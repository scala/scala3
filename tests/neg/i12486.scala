val hello: String = "hello"

object MyObj {
  val a: Int = 123
  val b: Double = 456.789
  val c: String = "ABC"
}

val stringFromSingleton: String = new hello.type()    // error: not a class type
val myObjFromSingleton: MyObj.type = new MyObj.type() // error: not a class type
