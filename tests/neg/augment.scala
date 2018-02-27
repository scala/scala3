import Predef.{any2stringadd => _, _}
object augments {

// Simple extension methods

  case class Circle(x: Double, y: Double, radius: Double)

  augment Circle {
    def circumference = this.radius * math.Pi * 2
    private val p = math.Pi       // error: `def` expected
  }

// Trait implementations

  trait HasArea {
    def area: Double
  }

  augment Circle extends HasArea {
    def area = this.radius * this.radius * math.Pi
  }

// Generic trait implementations

  augment List[type T] {
    type I = Int                 // error: `def` expected
    def second = this.tail.head
  }

// Specific trait implementations

  augment List[Int] { self => // error: `def` expected
    import java.lang._ // error: `def` expected
    def maxx = (0 /: this)(_ `max` _)
  }

  augment Array[Int] {
    def maxx = (0 /: this)(_ `max` _)
  }

// Conditional extension methods

  case class Rectangle[T](x: T, y: T, width: T, height: T)

  trait Eql[T] {
    def eql (x: T, y: T): Boolean
  }

  augment Rectangle[type T: Eql] {
    def isSquare: Boolean = implicitly[Eql[T]].eql(this.width, this.height)
  }

// Simple generic augments

  augment (type T) {
    def ~[U](that: U): (T, U) = (this, that)
  }

// Conditional generic augments

  trait HasEql[T] {
    def === (that: T): Boolean
  }

  augment (type T: Eql) extends HasEql[T] {
    def === (that: T): Boolean = implicitly[Eql[T]].eql(this, that)
  }

  augment Rectangle[type T: Eql] extends HasEql[Rectangle[T]] {
    def === (that: Rectangle[T]) =
      this.x === that.x &&
      this.y === that.y &&
      this.width == that.width &&
      this.height == that.height
  }

// Generic augments with additional parameters

  augment List[List[type U]] {
    def flattened: List[U] = (this :\ (Nil: List[U]))(_ ++ _)
  }

  augment (type T: Eql, T) {
    def isSame = this._1 === this._2
  }
}

import augments._
object Test extends App {
  val c = Circle(0, 1, 2)
  println(c.area)

  implicit object IntHasEql extends Eql[Int] {
    def eql (x: Int, y: Int): Boolean = x == y
  }

  println(1 ~ "a")

  val r1 = Rectangle(0, 0, 2, 2)
  val r2 = Rectangle(0, 0, 2, 3)
  println(r1.isSquare)
  println(r2.isSquare)
  println(r1 === r1)
  println(r1 === r2)
  println(List(1, 2, 3).second)
  println(List(List(1), List(2, 3)).flattened)
  println(List(List(1), List(2, 3)).flattened.maxx)
  println(Array(1, 2, 3).maxx)
  println((2, 3).isSame)
  println((3, 3).isSame)
}