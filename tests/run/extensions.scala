import Predef.{any2stringadd => _, _}
object extensions {

// Simple extension methods

  case class Circle(x: Double, y: Double, radius: Double)

  extend Circle {
    def circumference = this.radius * math.Pi * 2
  }

// Trait implementations

  trait HasArea {
    def area: Double
  }

  extend Circle implements HasArea {
    def area = this.radius * this.radius * math.Pi
  }

// Generic extendations

  extend List[type T] {
    def second = this.tail.head
  }

// Specific extendations

  extend List[Int] {
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

  extend Rectangle[type T](implicit ev: Eql[T]) {
    def isNotSquare: Boolean = !implicitly[Eql[T]].eql(this.width, this.height)
  }

// Simple generic extensions

  extend (type T) {
    def ~[U](that: U): (T, U) = (this, that)
  }

// Conditional generic extensions

  trait HasEql[T] {
    def === (that: T): Boolean
  }

  extend (type S: Eql) implements HasEql[S] {
    def === (that: S): Boolean = implicitly[Eql[S]].eql(this, that)
  }

  extend (type T2)(implicit ev: Eql[T2]) {
    def ==== (that: T2): Boolean = implicitly[Eql[T2]].eql(this, that)
  }

  extend Rectangle[type T: Eql] implements HasEql[Rectangle[T]] {
    def === (that: Rectangle[T]) =
      this.x === that.x &&
      this.y === that.y &&
      this.width == that.width &&
      this.height == that.height
  }
}

object extensions2 {
  import extensions.{Eql, extend_type_S_Eql_S_implements_HasEql_S}
  // Nested generic arguments

  extend List[List[type U]] {
    def flattened: List[U] = (this :\ (Nil: List[U]))(_ ++ _)
  }

  extend (type T: Eql, T) {
    def isSame = this._1 === this._2
    def isSame2 = extend_type_S_Eql_S_implements_HasEql_S(this._1) == this._2
  }

}

object docs {
  extend Seq[String] {
    def longestStrings: Seq[String] = {
      val maxLength = this.map(_.length).max
      this.filter(_.length == maxLength)
    }
  }

  extend List[List[type T]] {
    def flattened: List[T] = this.foldLeft[List[T]](Nil)(_ ++ _)
  }

  extend Seq[type T: math.Ordering] {
    def indexOfLargest  = this.zipWithIndex.maxBy(_._1)._2
    def indexOfSmallest = this.zipWithIndex.minBy(_._1)._2
  }

  object PostConditions {
    opaque type EnsureResult[T] = T

    private object EnsureResult {
      def wrap[T](x: T): EnsureResult[T] = x
      def unwrap[T](x: EnsureResult[T]): T = x
    }

    def result[T](implicit er: EnsureResult[T]): T = EnsureResult.unwrap(er)

    extend (type T) {
      def ensuring[U](f: implicit EnsureResult[T] => Boolean): T = {
        assert(f(EnsureResult.wrap(this)))
        this
      }
    }
  }
  import PostConditions._

  val s = List(1, 2, 3).sum.ensuring(result == 6)

}

import extensions._
import extensions2._
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
  println(r2.isNotSquare)
  println(r1.isNotSquare)
  println(r1 === r1)
  println(r1 === r2)
  println(1 ==== 1)
  println(1 ==== 2)
  println(List(1, 2, 3).second)
  println(List(List(1), List(2, 3)).flattened)
  println(List(List(1), List(2, 3)).flattened.maxx)
  println(Array(1, 2, 3).maxx)
  println((2, 3).isSame)
  println(extend_type_T_Eql_T_T((3, 3)).isSame)
}