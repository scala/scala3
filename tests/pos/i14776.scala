trait T1
trait T2 extends T1

trait Expr[T] { val data: T = ??? }
case class Tag2() extends Expr[T2]

def flag: Boolean = ???

def foo[T](e: Expr[T]): T1 = e match {
  case Tag2() =>
      if flag then
        new T2 {}
      else
        e.data
}

