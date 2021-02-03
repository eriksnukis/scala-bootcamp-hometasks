package com.evolutiongaming.bootcamp.basics

// Homework
//
// Add additional 2D shapes such as triangle and square.
//
// In addition to the 2D shapes classes, add also 3D shapes classes
// (origin, point, sphere, cube, cuboid, 3D triangle - you can add
// others if you think they are a good fit).
//
// Add method `area` to 2D shapes.
//
// Add methods `surfaceArea` and `volume` to 3D shapes.
//
// If some of the implementation involves advanced math, it is OK
// to skip it (leave unimplemented), the primary intent of this
// exercise is modelling using case classes and traits, and not math.

object ClassesAndTraits {

  sealed trait Shape extends Located with Bounded with Movable {
    def area: Double
  }

  sealed trait Movable {
    def move(dx: Double, dy: Double): Movable
  }

  sealed trait Located {
    def x: Double

    def y: Double
  }

  sealed trait Bounded {
    def minX: Double

    def maxX: Double

    def minY: Double

    def maxY: Double
  }

  final case class Point(x: Double, y: Double) extends Shape {
    override def minX: Double = x

    override def maxX: Double = x

    override def minY: Double = y

    override def maxY: Double = y

    override def move(dx: Double, dy: Double): Point = Point(x + dx, y + dy)

    override def area: Double = 0
  }

  final case class Circle(centerX: Double, centerY: Double, radius: Double) extends Shape {
    override def x: Double = centerX

    override def y: Double = centerY

    override def minX: Double = centerX - radius

    override def maxX: Double = centerX + radius

    override def minY: Double = centerY - radius

    override def maxY: Double = centerY + radius

    override def move(dx: Double, dy: Double): Circle = Circle(centerX + dx, centerY + dy, radius)

    override def area: Double = Math.PI * radius * radius
  }

  final case class Rectangle(centerX: Double, centerY: Double, height: Double, width: Double) extends Shape {
    override def x: Double = centerX

    override def y: Double = centerY

    override def minX: Double = centerX - width / 2

    override def maxX: Double = centerX + width / 2

    override def minY: Double = centerY - height / 2

    override def maxY: Double = centerY + height / 2

    override def move(dx: Double, dy: Double): Rectangle = Rectangle(x + dx, y + dy, height, width)

    override def area: Double = height * width
  }

  final case class Square(centerX: Double, centerY: Double, height: Double) extends Shape {
    override def x: Double = centerX

    override def y: Double = centerY

    override def move(dx: Double, dy: Double): Square = Square(x + dx, y + dy, height)

    override def minX: Double = x - height / 2

    override def maxX: Double = x + height / 2

    override def minY: Double = y - height / 2

    override def maxY: Double = y + height / 2

    override def area: Double = height * height
  }

  final case class Triangle(x1: Double, y1: Double, x2: Double, y2: Double, x3: Double, y3: Double) extends Shape {
    override def x: Double = ???

    override def y: Double = ???

    override def move(dx: Double, dy: Double): Triangle = Triangle(x1 + dx, y1 + dy, x2 + dx, y2 + dy, x3 + dx, y3 + dy)

    override def minX: Double = List(x1, x2, x3).min

    override def maxX: Double = List(x1, x2, x3).max

    override def minY: Double = List(y1, y2, y3).min

    override def maxY: Double = List(y1, y2, y3).max

    //as per https://www.mathopenref.com/coordtrianglearea.html
    override def area: Double = Math.abs(x1 * (x2 - x3) + x2 * (y3 - y1) + x3 * (y1 - y2) / 2)
  }

  val point2: Point = Point(1, 2)
  println(point2.x)

  val shape: Shape = point2
  val point2Description: String = shape match {
    case Point(x, y) => s"x = $x, y = $y"
    case _ => "other shape"
  }

  val point3: Point = point2.copy(x = 3)
  println(point3.toString) // Point(3, 2)

  // Exercise. Implement an algorithm for finding the minimum bounding rectangle
  // (https://en.wikipedia.org/wiki/Minimum_bounding_rectangle) for a set of `Bounded` objects.
  //
  def minimumBoundingRectangle(objects: Set[Bounded]): Bounded =
    new Bounded {
      implicit private val doubleOrdering: Ordering[Double] = Ordering.Double.IeeeOrdering

      // if needed, fix the code to be correct
      override def minX: Double = objects.map(_.minX).min

      override def maxX: Double = objects.map(_.maxX).max

      override def minY: Double = objects.map(_.minY).min

      override def maxY: Double = objects.map(_.maxY).max
    }

  // Pattern matching and exhaustiveness checking
  def describe(x: Shape): String = x match {
    case Point(x, y) => s"Point(x = $x, y = $y)"
    case Circle(centerX, centerY, radius) => s"Circle(centerX = $centerX, centerY = $centerY, radius = $radius)"
    case Rectangle(centerX, centerY, height, width) => s"Rectangle(centerX = $centerX, centerY = $centerY, height = $height, width = $width"
    case Square(centerX, centerY, height) => s"Square(centerX = $centerX, centerY = $centerY, height = $height)"
    case Triangle(x1, y1, x2, y2, x3, y3) => s"Triangle(x1 = $x1, y1 = $y1, x2 = $x2, y2 = $y2, x3 = $x3, y3 = $y3"
  }

  // Singleton can extend classes and mix in traits
  object Origin extends Located {
    override def x: Double = 0

    override def y: Double = 0
  }

  object Bounded {
    def minimumBoundingRectangle(objects: Set[Bounded]): Bounded = ???
  }

}