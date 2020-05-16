package quasiaffine

object Main {
  def main(args: Array[String]): Unit = {
    val exprs = Seq(
      VarExpr(0, 8, 2, "i"),

      DivExpr(VarExpr(0, 27, 3, "i"), 2),

      DivExpr(SumExpr(Seq(VarExpr(0, 8, 2, "j"), ConstExpr(3))), 2),

      ModExpr(VarExpr(0, 8, 2, "j"), 4),

      ModExpr(ModExpr(VarExpr(0, 8, 2, "j"), 16), 4),

      SumExpr(Seq(
        ProdExpr(Seq(ConstExpr(2), VarExpr(1, 4, 1, "i"))),
        VarExpr(1, 7, 2, "j"),
        ConstExpr(1),
      )),

      SumExpr(Seq(
        ProdExpr(Seq(ConstExpr(2), VarExpr(0, 4, 1, "i"))),
        VarExpr(0, 8, 2, "j"),
        ConstExpr(1),
      )),

      SumExpr(Seq(
        ProdExpr(Seq(ConstExpr(2), VarExpr(0, 4, 1, "i"))),
        ProdExpr(Seq(ConstExpr(4), DivExpr(SumExpr(Seq(VarExpr(0, 8, 2, "j"), ConstExpr(1))), 2))),
        ConstExpr(1),
      )),

      SumExpr(Seq(
        ProdExpr(Seq(ConstExpr(2), VarExpr(0, 4, 1, "i"))),
        ModExpr(SumExpr(Seq(VarExpr(0, 8, 2, "j"), ConstExpr(1))), 2),
        ConstExpr(1),
      )),
    )

    val cs = Seq(1, 2, 3, 100)

    for (expr <- exprs; c <- cs) {
      println("======================")

      println(s"expr:\t\t$expr")
      println(s"c:\t\t$c\n")

      println(s"optimized:\t${Passes(expr)}\n")

      val quotient = Passes(DivExpr(expr, c))
      val remainder = Passes(ModExpr(expr, c))

      println(s"expr // c:\t$quotient")
      println(s"expr % c:\t$remainder")

      try {
        Passes.isQuasiAffinePass(quotient)
      } catch {
        case _: QuasiAffineException => println("expr // c threw an exception because it's not quasi-affine")
      }

      try {
        Passes.isQuasiAffinePass(remainder)
      } catch {
        case _: QuasiAffineException => println("expr % c threw an exception because it's not quasi-affine")
      }

      println()
    }
  }
}
