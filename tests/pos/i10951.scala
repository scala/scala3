import scala.language.dynamics

object Dyn extends Dynamic:
  def selectDynamic[T](name: String): Option[T] = None

val a: Option[(Int, Int)] = Dyn.asdf[Tuple2[Int, Int]]
val b: Option[(Int, Int)] = Dyn.selectDynamic[(Int, Int)]("asdf")
val c: Option[(Int, Int)] = Dyn.asdf[(Int, Int)]
