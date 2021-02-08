import scala.quoted.*
import Bar.aMacroImplementation

object Foo {

  inline def myMacro(): Unit = ${ aMacroImplementation }

}
