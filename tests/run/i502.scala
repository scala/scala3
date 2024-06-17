// scalajs: --skip

import scala.reflect.ClassTag

object Test extends App {
  Array[Int](1, 2)

  try {
    Array[Int](1, 2)(using null)
    ???
  } catch {
    case _: NullPointerException => println("Ok")
  }

  Array[Int](1, 2)(using {println("foo"); summon[ClassTag[Int]]})

  Array[Int](1, 2)(using ClassTag.apply({ println("bar"); classOf[Int]}))
}
