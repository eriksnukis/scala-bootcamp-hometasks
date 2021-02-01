package com.evolutiongaming.bootcamp.basics

import org.scalatest.flatspec.AnyFlatSpec
import Basics._
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class BasicsSpec extends AnyFlatSpec {

  "gcd" should "be calculated correctly" in {
    val sampleGcdList = List((21, 7, 7), (9, 6, 3), (25,15,5), (5,14,1))
    for (sample <- sampleGcdList) {
      gcd(sample._1, sample._2) shouldEqual sample._3
    }
  }

  "lcm" should "be calculated correctly" in {
    val sampleLcdList = List((21, 7, 21), (9, 6, 18))
    for (sample <- sampleLcdList) {
      lcm(sample._1, sample._2) shouldEqual sample._3
    }
  }
}

