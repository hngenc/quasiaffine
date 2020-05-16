package quasiaffine

import scala.math.{abs, pow}
import scala.annotation.tailrec

object PrimeFactorSet {
  type PrimeFactorSet = Map[Int, Int]

  def apply(x: Int): PrimeFactorSet = {
    @tailrec
    def helper(x: Int, p: Int = 2, prev: Seq[Int] = Seq()): Seq[Int] = {
      if (p == x) {
        prev :+ p
      } else if (x % p == 0) {
        helper(x / p, p, prev :+ p)
      } else {
        helper(x, p + 1, prev)
      }
    }

    val factors = if (abs(x) <= 1) Seq() else helper(abs(x))

    factors.groupBy(identity).mapValues(_.size)
  }

  def union(pfs1: PrimeFactorSet, pfs2: PrimeFactorSet): PrimeFactorSet = {
    val keys = pfs1.keySet union pfs2.keySet
    keys.map(k => k -> (pfs1.getOrElse(k, 0) + pfs2.getOrElse(k, 0))).toMap
  }

  def intersect(pfs1: PrimeFactorSet, pfs2: PrimeFactorSet): PrimeFactorSet = {
    val keys = pfs1.keySet intersect pfs2.keySet
    keys.map(k => k -> (pfs1(k) min pfs2(k))).toMap
  }

  def totalFactor(pfs: PrimeFactorSet): Int = pfs.map { case (p, c) => pow(p, c).toInt }.product

  def gcd(pfs1: PrimeFactorSet, pfs2: PrimeFactorSet): Int = totalFactor(intersect(pfs1, pfs2))
}
