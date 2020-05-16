package quasiaffine

import PrimeFactorSet.PrimeFactorSet

sealed abstract class Expr {
  def minPossible: Int
  def maxPossible: Int
  def isConst: Boolean
  def isAffine: Boolean
  def isQuasiAffine: Boolean
  def internalVars: Int
  def internalDivsOrMods: Int

  // Assuming that all inner vars were canonicalized, what would be the factors that this expression would be divisible
  // by? The key is the prime factor, and the value is the count
  def primeFactors: PrimeFactorSet
}

sealed abstract class LeafExpr extends Expr

case class VarExpr(start: Int, end: Int, stride: Int, name: String) extends LeafExpr {
  override def minPossible: Int = start
  override def maxPossible: Int = ((end - start) / stride) * stride + start

  override def isConst: Boolean = false
  override def isAffine: Boolean = true
  override def isQuasiAffine: Boolean = true

  override def internalVars: Int = 1
  override def internalDivsOrMods: Int = 0

  override def primeFactors: PrimeFactorSet = start match {
    case 0 => PrimeFactorSet(stride)
    case _ => PrimeFactorSet.intersect(PrimeFactorSet(stride), PrimeFactorSet(start))
  }

  override def toString: String = {
    s"$name:[$start:$end:$stride]"
  }

  if ((end - start) % stride != 0)
    throw QuasiAffineException("Some of our code still doesn't support cases where the range is not a multiple of stride")
}

case class ConstExpr(c: Int) extends LeafExpr {
  override def minPossible: Int = c
  override def maxPossible: Int = c

  override def isConst: Boolean = true
  override def isAffine: Boolean = true
  override def isQuasiAffine: Boolean = true

  override def internalVars: Int = 0
  override def internalDivsOrMods: Int = 0

  override def primeFactors: PrimeFactorSet = PrimeFactorSet(c)

  override def toString: String = {
    s"$c"
  }
}

case class DivExpr(expr: Expr, c: Int) extends Expr {
  override def minPossible: Int = expr.minPossible / c
  override def maxPossible: Int = expr.maxPossible / c

  override def isConst: Boolean = expr.isConst || expr.maxPossible < c
  override def isAffine: Boolean = isConst
  override def isQuasiAffine: Boolean = expr.isQuasiAffine && expr.internalDivsOrMods == 0 && expr.internalVars <= 1

  override def internalVars: Int = expr.internalVars
  override def internalDivsOrMods: Int = 1 + expr.internalDivsOrMods

  override def primeFactors = {
    val totalChildFactor = PrimeFactorSet.totalFactor(expr match {
      case s: SumExpr => s.nonConstPrimeFactors
      case _ => expr.primeFactors
    })

    if (totalChildFactor % c == 0) PrimeFactorSet(totalChildFactor / c)
    else PrimeFactorSet(1)
  }

  override def toString: String = {
    s"div($expr, $c)"
  }

  if (c == 0) {
    throw QuasiAffineException("Can't divide by 0")
  }
}

case class ModExpr(expr: Expr, c: Int) extends Expr {
  val (totalChildFactor, start) = expr match {
    case s: SumExpr => (PrimeFactorSet.totalFactor(s.nonConstPrimeFactors), s.minPossible)
    case _ => (PrimeFactorSet.totalFactor(expr.primeFactors), 0)
  }

  override def isConst: Boolean = expr.isConst || totalChildFactor % c == 0
  override def isAffine: Boolean = isConst
  override def isQuasiAffine: Boolean = expr.isQuasiAffine && expr.internalDivsOrMods == 0 && expr.internalVars <= 1

  override def internalVars: Int = expr.internalVars
  override def internalDivsOrMods: Int = 1 + expr.internalDivsOrMods

  override def minPossible: Int = {
    if (expr.isConst)
      expr.minPossible % c
    else if (totalChildFactor % c == 0)
      start % c
    else if (internalVars == 1)
      (start to expr.maxPossible by totalChildFactor).map(_ % c).min // TODO inefficient and inelegant
    else
      0
  }

  override def maxPossible: Int = {
    if (expr.isConst)
      expr.maxPossible % c
    else if (totalChildFactor % c == 0)
      start % c
    else if (internalVars == 1)
      (start to expr.maxPossible by totalChildFactor).map(_ % c).max // TODO inefficient and inelegant
    else
      expr.maxPossible max (c - 1)
  }

  override def primeFactors: PrimeFactorSet = PrimeFactorSet(1)

  override def toString: String = {
    s"mod($expr, $c)"
  }

  if (c == 0) {
    throw QuasiAffineException("Can't modulo 0")
  }
}

case class ProdExpr(exprs: Seq[Expr]) extends Expr {
  override def minPossible: Int = exprs.map(_.minPossible).product
  override def maxPossible: Int = exprs.map(_.maxPossible).product

  override def isConst: Boolean = exprs.forall(_.isConst)

  override def isAffine: Boolean = internalVars <= 1 && exprs.forall(_.isAffine)
  override def isQuasiAffine: Boolean = internalVars <= 1 && internalDivsOrMods <= 1 && exprs.forall(_.isQuasiAffine)

  override def internalVars: Int = exprs.map(_.internalVars).sum
  override def internalDivsOrMods: Int = exprs.map(_.internalDivsOrMods).sum

  override def primeFactors: PrimeFactorSet = exprs.map(_.primeFactors).reduce(PrimeFactorSet.union)

  override def toString: String = {
    if (exprs.isEmpty) {
      ""
    } else {
      exprs.collect {
        case e @ SumExpr(es) => if (es.size > 1) s"($e)" else s"$e"
        case e => s"$e"
      }.reduce((acc, e) => s"$acc * $e")
    }
  }

  // Make sure there's only one term in here which isn't a constant
  val not_consts = exprs.count(!_.isInstanceOf[ConstExpr])
  if (not_consts > 1) {
    throw QuasiAffineException("Not quasi-affine expression")
  }

  // Make sure there's only two terms
  if (exprs.size > 2) {
    throw QuasiAffineException("Not quasi-affine expression")
  }
}

case class SumExpr(exprs: Seq[Expr]) extends Expr {
  override def minPossible: Int = exprs.map(_.minPossible).sum
  override def maxPossible: Int = exprs.map(_.maxPossible).sum

  override def isConst: Boolean = exprs.forall(_.isConst)
  override def isAffine: Boolean = exprs.forall(_.isAffine)
  override def isQuasiAffine: Boolean = exprs.forall(_.isQuasiAffine)

  override def internalVars: Int = exprs.map(_.internalVars).sum
  override def internalDivsOrMods: Int = exprs.map(_.internalDivsOrMods).sum

  override def primeFactors: PrimeFactorSet = exprs.map(_.primeFactors).reduce(PrimeFactorSet.intersect)

  def nonConstPrimeFactors: PrimeFactorSet = exprs.filter(!_.isInstanceOf[ConstExpr]).map(_.primeFactors).reduce(PrimeFactorSet.intersect)

  override def toString: String = {
    if (exprs.isEmpty) {
      ""
    } else {
      exprs.map(_.toString).reduce((acc, e) => s"$acc + $e")
    }
  }
}
