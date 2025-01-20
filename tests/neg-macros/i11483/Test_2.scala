package x

import scala.language.implicitConversions
import scala.concurrent.Future

given FutureAsyncMonad: CpsMonad[Future]:
  def pure[T](t:T): Future[T] = ???
  def impure[T](t:Future[T]): T = ???
  def map[A,B](x:Future[A])(f: A=>B): Future[B] = ???


object Api:

  def doSomething():Future[String] =
    Future.successful("doSomething")

  def println(x:String):Unit =
    Console.println(x)


object Main:

  def main(args: Array[String]): Unit =
    X.process[Future,Unit]{ // error
      Api.println(Api.doSomething())
    }
