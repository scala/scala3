
class Test extends App {

  implicit def a1(implicit a: A): A = new A
  f // error

}
