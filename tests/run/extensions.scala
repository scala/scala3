import Predef.{any2stringadd => _, _}
object extensions {

// Simple extension methods

  case class Circle(x: Double, y: Double, radius: Double)

  extension CircleOps for Circle {
    def circumference = this.radius * math.Pi * 2
  }

// Trait implementations

  trait HasArea {
    def area: Double
  }

  extension CircleHasArea for Circle : HasArea {
    def area = radius * this.radius * math.Pi
  }

// Generic extendations

  extension ListOps[T] for List[T] {
    def second = tail.head
  }

// Specific extendations

  extension IntListOps for List[Int] {
    def maxx = (0 /: this)(_ `max` _)
  }

  extension IntArrayOps for Array[Int] {
    def maxx = (0 /: this)(_ `max` _)
  }

// Conditional extension methods

  case class Rectangle[T](x: T, y: T, width: T, height: T)

  trait Eql[T] {
    def eql (x: T, y: T): Boolean
  }

  extension RectangleOps[T: Eql] for Rectangle[T] {
    def isSquare: Boolean = implicitly[Eql[T]].eql(this.width, this.height)
  }

  extension RectangleOps2[T](implicit ev: Eql[T]) for Rectangle[T] {
    def isNotSquare: Boolean = !implicitly[Eql[T]].eql(width, height)
  }

// Simple generic extensions

  extension Pairer[T] for T {
    def ~[U](that: U): (T, U) = (this, that)
  }

// Conditional generic extensions

  trait HasEql[T] {
    def === (that: T): Boolean
  }

  extension HasEqlDeco[T : Eql] for T : HasEql[T] {
    def === (that: T): Boolean = implicitly[Eql[T]].eql(this, that)
  }

  extension InfixEql[T](implicit ev: Eql[T]) for T {
    def ==== (that: T): Boolean = implicitly[Eql[T]].eql(this, that)
  }

  extension RectangleHasEql[T : Eql] for Rectangle[T] : HasEql[Rectangle[T]] {
    def === (that: Rectangle[T]) =
      this.x === that.x &&
      this.y === that.y &&
      this.width == that.width &&
      this.height == that.height
  }
}

object extensions2 {
  import extensions.{Eql, HasEqlDeco}
  // Nested generic arguments

  extension ListListOps[T] for List[List[T]] {
    def flattened: List[T] = (this :\ (Nil: List[T]))(_ ++ _)
  }

  extension EqlDeco[T : Eql] for (T, T) {
    def isSame = this._1 === this._2
    def isSame2 = HasEqlDeco(this._1) === this._2
  }

}

object docs {

  extension StringOps for Seq[String] {
    def longestStrings: Seq[String] = {
      val maxLength = map(_.length).max
      filter(_.length == maxLength)
    }
  }

  extension ListListOps[T] for List[List[T]] {
    def flattened: List[T] = foldLeft[List[T]](Nil)(_ ++ _)
  }

  extension OrdSeqOps[T : math.Ordering] for Seq[T] {
    def indexOfLargest  = zipWithIndex.maxBy(_._1)._2
    def indexOfSmallest = zipWithIndex.minBy(_._1)._2
  }

  object PostConditions {
    opaque type WrappedResult[T] = T

    private object WrappedResult {
      def wrap[T](x: T): WrappedResult[T] = x
      def unwrap[T](x: WrappedResult[T]): T = x
    }

    def result[T](implicit er: WrappedResult[T]): T = WrappedResult.unwrap(er)

    extension Ensuring[T] for T {
      def ensuring(condition: implicit WrappedResult[T] => Boolean): T = {
        implicit val wrapped = WrappedResult.wrap(this)
        assert(condition)
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
  println(EqlDeco((3, 3)).isSame)
}
