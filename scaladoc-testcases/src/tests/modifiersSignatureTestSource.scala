package tests.modifiersSignatureTestSource

case class Case()

final class Final()

abstract class Abstract()

class Empty()

sealed class Sealed()

open class Open

opaque type Opaque
  = Open

abstract class Methods()
{
  def method1(): Unit

  inline def inlineMethod(): Unit
    = ()

  implicit def toImplicitString(): String
   = "asd"

  inline def method2(inline name: String): String
  = "ala"
}

class ImplementedMethods() extends Methods/*<-*/()/*->*/
{
  override def method1(): Unit
    = ???

}

case class ReimplementedMethods() extends ImplementedMethods/*<-*/()/*->*/
{
  override def method1(): Unit
    = ???
}
