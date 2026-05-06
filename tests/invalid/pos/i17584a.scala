
import language.experimental.erasedDefinitions
trait A:
  erased def g = 1
trait B extends A:
  erased def f = super.g
class C extends B