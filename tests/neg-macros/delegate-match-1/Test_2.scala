
class Test1 extends App {

  implicit val a1: A = new A
  implicit val a2: A = new A
  f // error

}
