//> using options -deprecation

@deprecatedInheritance("this class will be made final", "FooLib 12.0")
trait TFoo
class TBar1 extends TFoo // warn
trait TBar2 extends TFoo // warn
def TBar3 = new TFoo {}  // warn

@deprecatedInheritance(since = "FooLib 11.0")
class CFoo
class CBar1 extends CFoo // warn
trait CBar2 extends CFoo // warn
def CBar3 = new CFoo {}  // warn

@deprecatedInheritance(message = "this class will be made final")
abstract class AFoo
class ABar1 extends AFoo // warn
trait ABar2 extends AFoo // warn
def ABar3 = new AFoo {}  // warn

@deprecated
class DeprecatedFoo:
  class Foo extends AFoo // it shoudln't warn here (in deprecated context)
