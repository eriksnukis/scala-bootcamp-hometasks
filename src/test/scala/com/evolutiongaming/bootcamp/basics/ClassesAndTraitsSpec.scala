package com.evolutiongaming.bootcamp.basics

import cats.implicits._
import com.evolutiongaming.bootcamp.basics.ClassesAndTraits.{Circle, _}
import org.scalacheck.Gen._
import org.scalacheck.cats.implicits._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ClassesAndTraitsSpec extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks {
  "Circle" should "be correct" in {
    val intervalGen = choose(1.0, 10)
    val gen = (intervalGen, intervalGen, intervalGen).tupled

    forAll(gen) { case (x, y, r) =>
      val circle = Circle(x, y, r)
      circle.minX shouldEqual x - r
      circle.maxX shouldEqual x + r
      circle.minY shouldEqual y - r
      circle.maxY shouldEqual y + r
    }
  }

  "minimumBoundingRectangle" should "be correct" in {
    val mbr = minimumBoundingRectangle(
      Set(
        Point(-12, -3),
        Point(-3, 7),
        Circle(0, 0, 5),
      )
    )

    mbr.minX shouldEqual -12
    mbr.maxX shouldEqual 5
    mbr.minY shouldEqual -5
    mbr.maxY shouldEqual 7
  }
}
