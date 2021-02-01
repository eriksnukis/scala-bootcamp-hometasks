package com.evolutiongaming.bootcamp.basics

import org.scalatest.flatspec.AnyFlatSpec
import Basics._
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class BasicsSpec extends AnyFlatSpec {

  "gcd" should "be calculated correctly" in {
    val sampleLcdList = List((21, 7, 7), (9, 6, 3))
    for (sample <- sampleLcdList) {
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

