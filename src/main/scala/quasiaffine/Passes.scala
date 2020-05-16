package quasiaffine

import PrimeFactorSet._

object Passes {
  def canonicalizeConsts(expr: Expr): Expr = expr match {
    case SumExpr(es) => {
      val es_opt = es.map(e => canonicalizeConsts(e))
      val consts_sum = es_opt.collect{case ConstExpr(c) => c}.sum
      val es_filtered = es_opt.filter(!_.isInstanceOf[ConstExpr])

      consts_sum match {
        case 0 => SumExpr(es_filtered)
        case _ => SumExpr(es_filtered :+ ConstExpr(consts_sum))
      }
    }

    case ProdExpr(es) => {
      val es_opt = es.map(e => canonicalizeConsts(e))
      val prod = es_opt.collect { case ConstExpr(c) => c }.product
      val es_filtered = es_opt.filter(!_.isInstanceOf[ConstExpr])

      prod match {
        case 0 => ConstExpr(0)
        case 1 => ProdExpr(es_filtered)
        case _ => ProdExpr(ConstExpr(prod) +: es_filtered)
      }
    }

    case DivExpr(e, c) => DivExpr(canonicalizeConsts(e), c)
    case ModExpr(e, c) => ModExpr(canonicalizeConsts(e), c)

    case _ => expr
  }

  def canonicalizeVars(expr: Expr): Expr = expr match {
    case v @ VarExpr(start, end, stride, _) => start match {
      case 0 => ProdExpr(Seq(ConstExpr(stride), v.copy(0, (end-start)/stride, 1)))
      case _ => SumExpr(Seq(ProdExpr(Seq(ConstExpr(stride), v.copy(0, (end-start)/stride, 1))), ConstExpr(start)))
    }
    case SumExpr(es) => SumExpr(es.map(canonicalizeVars))
    case ProdExpr(es) => ProdExpr(es.map(canonicalizeVars))
    case DivExpr(e, c) => DivExpr(canonicalizeVars(e), c)
    case ModExpr(e, c) => ModExpr(canonicalizeVars(e), c)
    case _ => expr
  }

  // Returns both an optimized Expr and the amount that the current denominator should be divided by
  def constFoldDiv(expr: Expr, denom: Int = 1): Expr = {
    if (expr.maxPossible < denom)
      ConstExpr(0)
    else expr match {
      case ConstExpr(c) => ConstExpr(c / denom)

      case v @ VarExpr(0, _, 1, _) => v
      case _: VarExpr => throw QuasiAffineException("not canonicalized var")

      case DivExpr(s: SumExpr, c) if gcd(s.nonConstPrimeFactors, PrimeFactorSet(c*denom)) == c*denom =>
        constFoldDiv(s, c*denom)
      case DivExpr(e, c) if gcd(e.primeFactors, PrimeFactorSet(c*denom)) == c*denom => constFoldDiv(e, c*denom)
      case DivExpr(e, c) => {
        val g = gcd(e.primeFactors, PrimeFactorSet(c*denom))
        DivExpr(constFoldDiv(e, g), c*denom / g)
      }

      case p @ ProdExpr(Seq(ConstExpr(c), e)) => {
        val g = gcd(p.primeFactors, PrimeFactorSet(denom))
        if (g != c) ProdExpr(Seq(ConstExpr(c / g), constFoldDiv(e, denom / g)))
        else constFoldDiv(e)
      }
      case p: ProdExpr => throw QuasiAffineException(s"not canonicalized prod, $p")

      case SumExpr(es) => SumExpr(es.map(e => constFoldDiv(e, denom)))

      case m: ModExpr if denom == 1 => m
      case m: ModExpr => throw QuasiAffineException(s"not canonicalized mod, $m")

      case _ => throw QuasiAffineException(s"Incorrect denominator $denom, $expr")
    }
  }

  def constFoldMod(expr: Expr): Expr = expr match {
    case ModExpr(e, c) if e.maxPossible < c => constFoldMod(e)

    case ModExpr(DivExpr(e, ci), c) =>
      constFoldMod(SumExpr(Seq(
        constFoldDiv(DivExpr(e, ci)), ProdExpr(Seq(ConstExpr(-c), DivExpr(e, c*ci))))))

    case ModExpr(SumExpr(es), c) if es.collect { case ConstExpr(i) => i % c == 0 }.contains(true) => {
      val es_filtered = es.filter(!_.isInstanceOf[ConstExpr])
      ModExpr(constFoldMod(SumExpr(es_filtered)), c)
    }

    case m @ ModExpr(e, _) => m.copy(constFoldMod(e))
    case d @ DivExpr(e, _) => d.copy(constFoldMod(e))
    case s @ SumExpr(es) => s.copy(es.map(constFoldMod))
    case p @ ProdExpr(es) => p.copy(es.map(constFoldMod))

    case _ => expr
  }

  def flattenChildren(expr: Expr): Expr = expr match {
    case e if e.isConst => ConstExpr(e.maxPossible)

    case SumExpr(Seq(e)) => flattenChildren(e)
    case SumExpr(es) => {
      val inner_sums = es.collect { case SumExpr(esi) => esi }.flatten
      val es_filtered = es.filter(!_.isInstanceOf[SumExpr])

      canonicalizeConsts(SumExpr((es_filtered ++ inner_sums).map(flattenChildren)))
    }

    case p @ ProdExpr(Seq(e)) => flattenChildren(e)
    case p @ ProdExpr(Seq(c: ConstExpr, SumExpr(es))) =>
      flattenChildren(SumExpr(es.map(e => flattenChildren(ProdExpr(Seq(c, e))))))
    case p @ ProdExpr(es) => p.copy(es.map(flattenChildren))

    case DivExpr(di: DivExpr, c) => flattenChildren(di.copy(c=c*di.c))
    case DivExpr(e, 1) => flattenChildren(e)
    case d @ DivExpr(e, _) => d.copy(flattenChildren(e))

    case ModExpr(mi @ ModExpr(e, ci), c) if c % ci == 0 => flattenChildren(mi)
    case ModExpr(ModExpr(e, ci), c) if ci % c == 0 => flattenChildren(ModExpr(e, c))
    case m @ ModExpr(e, _) => m.copy(flattenChildren(e))

    case _ => expr
  }

  def isQuasiAffinePass(expr: Expr): Expr =
    if (expr.isQuasiAffine) expr
    else throw QuasiAffineException("Is not quasi-affine")

  def apply(expr: Expr): Expr = {
    val passes = Seq(canonicalizeVars(_),
      canonicalizeConsts(_),
      (x: Expr) => constFoldDiv(x),
      constFoldMod(_)
    )
    passes.foldLeft(expr)((acc, p) => flattenChildren(p(acc)))
  }
}
