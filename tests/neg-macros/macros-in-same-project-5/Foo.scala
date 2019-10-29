import scala.quoted._
import Bar.aMacroImplementation

object Foo {

  inline def myMacro(): Unit = ${ aMacroImplementation }

}
