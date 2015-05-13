object Test extends dotty.runtime.LegacyApp {
  import scala.collection.generic.CanBuildFrom
  val cbf = implicitly[CanBuildFrom[Nothing, Nothing, Array[Nothing]]]
  println(cbf().result.getClass)
  println(new Array[Nothing](0).getClass)
  println(Array[Nothing]().getClass)
}
