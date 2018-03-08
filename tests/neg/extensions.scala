import Predef.{any2stringadd => _, _}
object extensions {

// Simple extension methods

  case class Circle(x: Double, y: Double, radius: Double)

  extend Circle {
    def circumference = this.radius * math.Pi * 2
    private val p = math.Pi       // error: `def` expected
  }

  type Circle2 = Circle

  extend Circle2 {
    def circumference = radius * math.Pi * 2 // error: not found
  }

// Trait implementations

  trait HasArea {
    def area: Double
  }

  abstract class HasAreaClass extends HasArea

  extend Circle implements HasArea {
    def area = this.radius * this.radius * math.Pi
  }

  extend Circle2 extends HasArea {} // error: `implements` or `{` expected // error: `def` expected

  extend Circle implements HasAreaClass {
    def area = this.radius * this.radius * math.Pi
  }

// Generic trait implementations

  extend List[type T] {
    type I = Int                 // error: `def` expected
    def second = this.tail.head
  }

// Specific trait implementations

  extend List[Int] { self => // error: `def` expected
    import java.lang._ // error: `def` expected
    def maxx = (0 /: this)(_ `max` _)
  }

  extend Array[Int] {
    def maxx = (0 /: this)(_ `max` _)
  }

// Conditional extension methods

  case class Rectangle[T](x: T, y: T, width: T, height: T)

  trait Eql[T] {
    def eql (x: T, y: T): Boolean
  }

  extend Rectangle[type T: Eql] {
    def isSquare: Boolean = implicitly[Eql[T]].eql(this.width, this.height)
  }

// Simple generic extensions

  extend (type T) {
    def ~[U](that: U): (T, U) = (this, that)
  }
}

object extensions1 {
  extend List[List[type T]] {
    def flattened: List[T] = (this :\ (Nil: List[T]))(_ ++ _)
  }
}

object extensions2 {
  import extensions.Eql
  // Nested generic arguments

  extend List[List[type U]] {
    def flattened: List[U] = (this :\ (Nil: List[U]))(_ ++ _)
  }

  extend (type T: Eql, T) {
    def isSame: Boolean = this._1 === this._2 // error: === is not a member
  }

}

import extensions1._
import extensions2._
object Test extends App {
  println(List(List(1), List(2, 3)).flattened) // error: type error + note that implicit conversions are ambiguous
}