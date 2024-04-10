//> using options -explain

trait A
trait B extends A

trait M[+T]

case class MA(id:Int) extends M[A]
class MB(id:Int) extends MA(id) with M[B] // error
