import annotation.{implicitNotFound, experimental}
import language.experimental.erasedDefinitions

@experimental
erased class CanThrow[-E <: Exception]

@experimental
object unsafeExceptions:
  given canThrowAny: CanThrow[Exception] = null

def test22: (CanThrow[Exception], Int) => Int = ???

trait Decoder[+T]:
  def apply(dummy: String): T

def deco: Decoder[CanThrow[Exception] ?=> Int] = new Decoder[CanThrow[Exception] ?=> Int]:
  def apply(dummy: String): CanThrow[Exception] ?=> Int = 1

@main def Test(): Unit =
  import unsafeExceptions.canThrowAny
  deco.apply("").apply(using canThrowAny)
//  summon[Decoder[CanThrow[Exception] ?=> Int]]()