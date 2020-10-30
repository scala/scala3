import scala.annotation.compileTimeOnly

@compileTimeOnly("FooBar can not be used as an expression") trait FooBar

object foo extends FooBar // error

val fooAnon = new FooBar {} // error // error
val fooRef1: FooBar = ??? // error
def fooRef2: FooBar = ??? // error
def useFoo(foo: FooBar): foo.type = foo // error // error // error
val bar = fooRef2 // error

@compileTimeOnly("baz can not be used as an expression") val baz = 23
val qux = baz // error

@compileTimeOnly("quux can not be used as an expression") def quux = 47
val quxx = quux // error
