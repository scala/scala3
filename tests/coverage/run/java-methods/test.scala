import java.util.function.Consumer

@main
def Test: Unit =
  StaticMethods.simple()
  StaticMethods.withTypeParam[Any](a => ())
  val obj = JavaObject()
  obj.f()
  println(obj.identity[Int](0))
  println("ok!")
