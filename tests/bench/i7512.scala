import scala.compiletime.S

object InfiniteLoopMatchType {
    def main(args: Array[String]): Unit = {
        testProd(2, 3) // Infinite loop on Dotty 0.25.0-RC1
    }

    def testProd(a: Int, b: Int)(using ev: (a.type * b.type) =:= (b.type * a.type)) = true

    type *[A <: Int, B <: Int] <: Int = A match {
        case 0 => 0
        case _ => MultiplyLoop[A, B, 0]
    }

    type MultiplyLoop[A <: Int, B <: Int, Acc <: Int] <: Int = A match {
        case 0 => Acc
        case S[aMinusOne] => MultiplyLoop[aMinusOne, B, B + Acc]
    }

    type +[A <: Int, B <: Int] <: Int = A match {
        case 0 => B
        case S[aMinusOne] => aMinusOne + S[B]
    }
}

