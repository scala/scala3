import scala.annotation.targetName

object BugExp4Point2D {

  sealed trait ColumnType[T]
  case object DoubleT extends ColumnType[Double]
  case object IntT extends ColumnType[Int]

  object dsl {


    extension [T1:Numeric, T2:Numeric](x: T1)

      // N - N
      @targetName("point2DConstant")
      infix def º(y: T2): Point2D[T1,T2] = ???


      // N - C
      @targetName("point2DConstantData")
      infix def º(y: ColumnType[T2]): Point2D[T1,T2] = ???



    extension [T1:Numeric, T2:Numeric](x: ColumnType[T1])
      // C - C
      @targetName("point2DData")
      infix def º(y: ColumnType[T2]): Point2D[T1,T2] = ???

      // C - N
      @targetName("point2DDataConstant")
      infix def º(y: T2): Point2D[T1,T2] = ???


  }

  case class Point2D[T1:Numeric, T2:Numeric](x:T1, y:T2)

  import dsl.*

  def main(args: Array[String]): Unit = {
    val x = IntT
    val y = DoubleT

    val pos1: Point2D[Int,Double] = x º y       // error
    val pos2: Point2D[Int,Double] = 100 º 200.1 // ok
    val pos3: Point2D[Int,Double] = 101 º y     // ok
    val pos4: Point2D[Int,Double] = x º 201.1   // error

  }
}

class C

object Container:
  given C with {}

object Test:
  extension (x: String)(using C)
    def foo: String = x

  val y = "abc".foo  // error

