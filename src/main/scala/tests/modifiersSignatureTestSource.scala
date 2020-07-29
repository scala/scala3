package tests.modifiersSignatureTestSource

case class Case()

final class Final()

abstract class Abstract()

class Empty()

sealed class Sealed()

abstract class Methods()
{
    def method1(): Unit 

    inline def inlineMethod(): Unit
        = Unit
    
    implicit def toImplicitString(): String
     = "asd"
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


