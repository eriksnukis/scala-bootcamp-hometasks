package com.evolutiongaming.bootcamp.basics

import scala.annotation.tailrec

object Basics {
  // Homework.
  //
  // Implement functions that calculate https://en.wikipedia.org/wiki/Lowest_common_denominator and
  // https://en.wikipedia.org/wiki/Greatest_common_divisor for integers.
  //
  //

  def lcm(a: Int, b: Int): Int =
    (a, b) match {
      case (0, 0) => 0
      case _ => (a * b) / gcd(a, b)
    }

  @tailrec
  def gcd(a: Int, b: Int): Int = {
    if (b == 0)
      a
    else gcd(b, a % b)
  }
}
