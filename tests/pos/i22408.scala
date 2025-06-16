object Obj:
  @scala.annotation.static
  val some_static_value: Int = {
    val some_local_value: Int = {
      val some_local_value_1 = ???
      some_local_value_1
    }
    some_local_value
  }

class Obj
