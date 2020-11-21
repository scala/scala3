package example

import scala.quoted._

object MyClassMaker {
  inline def make: MyClass = ${ makeImpl }
  def makeImpl(using Quotes): Expr[MyClass] = {
    '{
      new MyClass {
        override def toString(): String = "MyClassMaker.make.MyClass"
      }
    }
  }
}