package com.evolutiongaming.bootcamp.basics

import org.scalatest.flatspec.AnyFlatSpec
import Basics._
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class BasicsSpec extends AnyFlatSpec {

  "gcd" should "be calculated correctly" in {
    val sampleGcdList = List((21, 7, 7), (9, 6, 3), (25, 15, 5), (5, 14, 1))
    for ((a, b, expected) <- sampleGcdList) {
      gcd(a, b) shouldEqual expected
    }
  }

  "lcm" should "be calculated correctly" in {
    val sampleLcdList = List((21, 7, 21), (9, 6, 18), (0, 0, 0))
    for ((a, b, expected) <- sampleLcdList) {
      lcm(a, b) shouldEqual expected
    }
  }
}

