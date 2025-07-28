//> using options -experimental

import scala.util.TupledFunction
object Test {
  def main(args: Array[String]): Unit = {

    val f2: (x: (Int, Long)) => x._1.type = (args: (Int, Long)) => args._1
    val g2: (x: Int, y: Long) => x.type = f2.untupled
    println(g2(1, 3L))

  }

  /** Creates an untupled version of this function: instead of a single argument of type [[scala.Tuple]] with N elements,
    *  it accepts N arguments.
    *
    *  This is a generalization of [[scala.Function.untupled]] that work on functions of any arity
    *
    *  @tparam F the function type
    *  @tparam Args the tuple type with the same types as the function arguments of F
    *  @tparam R the return type of F
    */
  extension [F, Args <: Tuple, R](f: Args => R) def untupled(using tf: TupledFunction[F, Args => R]): F = tf.untupled(f)
}
