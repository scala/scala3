//> using options -source 3.7-migration

trait Permit
class Foo:
  def run(implicit ev: Permit): Unit = ???
  def apply(implicit ev: Permit): Unit = ???

given Permit = ???
@main def Test = new Foo().run()

def ctorProxy = Foo().run()

def otherSyntax = new Foo()() // Foo().apply does not work

def kwazySyntax = new Foo() . run  (  /* your args here! */  ) // that was fun
