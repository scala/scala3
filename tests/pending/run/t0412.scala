object Test extends dotty.runtime.LegacyApp {
  println(classOf[java.util.ArrayList[_]])
  println(classOf[java.util.ArrayList[T] forSome { type T }])
}
