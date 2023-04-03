package dotty.tools.benchmarks.inlinetraits
package standard

import scala.annotation.tailrec

trait MatrixLib[T: Numeric]:
  opaque type Matrix = Vector[Vector[T]]

  object Matrix:
    def apply(rows: Seq[T]*): Matrix =
      @tailrec def rec(rows: Seq[Seq[T]], acc: Matrix): Matrix =
        rows match {
          case Seq() => throw IllegalArgumentException("a matrix cannot be empty")
          case Seq(Seq()) => throw IllegalArgumentException("a matrix cannot contain an empty row")
          case Seq(row) => acc :+ row.toVector
          case h +: t if h.length == t(0).length => rec(t, acc :+ h.toVector)
          case h +: _ => throw IllegalArgumentException(s"row ${h} does not have the same number of columns as the rest of the matrix")
        }

      rec(rows, Vector())

  extension (m: Matrix)
    def +(n: Matrix): Matrix =
      assert(
        m.length == n.length && m(0).length == n(0).length,
        "m and n do not have the same dimensions"
      )

      val num = summon[Numeric[T]]
      import num.plus

      for row <- (0 until m.length).toVector
      yield
        for col <- (0 until m(0).length).toVector
        yield plus(m(row)(col), n(row)(col))
    end +

    def *(n: Matrix): Matrix =
      assert(m(0).length == n.length, "m and n have incompatible dimensions")

      val num = summon[Numeric[T]]
      import num.{zero, plus, times}

      for i <- (0 until m.length).toVector
      yield
        for j <- (0 until n(0).length).toVector
        yield
          val mults = for k <- 0 until n.length yield times(m(i)(k), n(k)(j))
          mults.fold(zero)(plus(_, _))
    end *

object IntMatrixLib extends MatrixLib[Int]
object DoubleMatrixLib extends MatrixLib[Double]