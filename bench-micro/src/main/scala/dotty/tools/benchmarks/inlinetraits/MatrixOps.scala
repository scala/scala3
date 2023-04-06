package dotty.tools.benchmarks.inlinetraits

import standard.IntMatrixLib.{Matrix => StdIntMatrix}
// import specialized.IntMatrixLib.{Matrix => SpeIntMatrix}
// import inline.IntMatrixLib.{Matrix => InlIntMatrix}

type BenchmarkMatrix = StdIntMatrix // | SpeIntMatrix | InlIntMatrix

class Matrix[M <: BenchmarkMatrix] private (private val matrix: M):
  def +(n: Matrix[M]): Matrix[BenchmarkMatrix] = (matrix, n.matrix) match {
    case (m: StdIntMatrix, n: StdIntMatrix) => Matrix(m + n)
    //case (m: SpeIntMatrix, n: SpeIntMatrix) => Matrix(m + n)
    //case (m: InlIntMatrix, n: InlIntMatrix) => Matrix(m + n)
    //case _ => ???
  }
  def *(n: Matrix[M]): Matrix[BenchmarkMatrix] = (matrix, n.matrix) match {
    case (m: StdIntMatrix, n: StdIntMatrix) => Matrix(m * n)
    //case (m: SpeIntMatrix, n: SpeIntMatrix) => Matrix(m * n)
    //case (m: InlIntMatrix, n: InlIntMatrix) => Matrix(m * n)
    //case _ => ???
  }

object Matrix:
  def ofType(tpe: String): Seq[Seq[Int]] => Matrix[BenchmarkMatrix] =
    (elems: Seq[Seq[Int]]) => tpe.toLowerCase() match {
      case "standard" => Matrix(StdIntMatrix(elems*))
    }

  val empty: Matrix[BenchmarkMatrix] = Matrix(StdIntMatrix(Nil))
