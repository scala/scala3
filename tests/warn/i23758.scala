//> using options -Wunused:imports

import scala.util.Try as _ // warn

class Promise(greeting: String):
  override def toString = greeting

@main def test = println:
  import scala.concurrent.{Promise as _, *}, ExecutionContext.Implicits.given
  val promise = new Promise("world")
  Future(s"hello, $promise")
