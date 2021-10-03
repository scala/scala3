package app

import lib.*

def boom(foo: Foo) = foo.foo(foo) // error: no implicit argument of type lib.Bar was found for parameter bar of method foo in class Foo
