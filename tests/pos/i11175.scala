package x

trait Printer[T]:
  def print(t:T):String

extension[T](t:T)(using Printer[T])
  def print():String = summon[Printer[T]].print(t)

object A

object B

given Printer[A.type]:
   def print(a:A.type):String = "a"

given Printer[B.type]:
   def print(b:B.type):String = "b"


object Main {

  def main(args:Array[String]):Unit =
    System.out.println(B.print())

}
