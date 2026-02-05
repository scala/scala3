//> using options -Wvalue-discard

import scala.language.experimental.genericNumberLiterals

transparent inline def toto: Any = 1
def tata: Unit = toto // warn pure Discard

transparent inline def uhoh = 42: Unit // nowarn
def hmm: Unit = uhoh

class C
object C:
  val K = C()
class Converter extends scala.util.FromDigits[C]:
  inline transparent def fromDigits(digits: String): C = C.K
given Converter()

transparent inline def odd = (42: C): Unit // nowarn
transparent inline def even: Unit = (42: C) // warn impure discard

def literally: Unit = 42 // warn pure Discard
def funnily = 42: Unit // nowarn
def impure = ("*" * 42).length
def impurely: Unit = impure // warn impure discard

def i: Int = ???
def parenthetically: Int =
  () // warn pure expression (but not Discard)
  i
transparent inline def reduced = ()
def reductively: Int =
  reduced // no warn inlined literal unit, e.g., assert(2 + 2 == 4)
  i

// three different expansions, but warning is early at typer, so elaboration doesn't matter
transparent inline def bodily[A](body: => A): A = body
def embodied: Unit = bodily(toto) // warn pure Discard
def disembodied: Unit = bodily(reduced) // nowarn
def um: Unit = bodily(uhoh) // nowarn
