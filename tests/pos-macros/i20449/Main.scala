
class Wrapper1[A]
val a = {
  getTypeInfo[Any]()
  val wrapper2 = Wrapper1[Any]()
}