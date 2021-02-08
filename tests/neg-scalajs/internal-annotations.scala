import scala.scalajs.js
import scala.scalajs.js.annotation.*
import scala.scalajs.js.annotation.internal.*

@JSType trait A // error
@JSType class B { // error
  @JSType val a = ??? // error
  @JSType var b = ??? // error
  @JSType def c = ??? // error
  def d(@JSType i: Int) = ??? // error
  @JSType class X // error
  @JSType trait Y // error
}
@JSType object C // error

@ExposedJSMember class D // error
@JSOptional class E // error
