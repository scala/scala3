import scala.annotation.targetName

trait Ctx

def foo(f: Ctx => Int) = ???

@targetName("fooContextual")
def foo(f: Ctx ?=> Int) = ???

def bar1 = foo(ctx => 123)
def bar2 = foo((ctx: Ctx) => 123)
def bar3 = foo(ctx ?=> 123)
def bar4 = foo((ctx: Ctx) ?=> 123)
// def bar5 = foo(123)   does not work
