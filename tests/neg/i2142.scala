object Foo {

class A
val a1 = new A()
val a2 = new A()

def f(x: A, y: x.type) = ()
f(a1, a1)           // ok
f(a1, a2)           // error
f(new A(), new A()) // error
f(new A(), a1)      // error

def g(x: A)(y: x.type) = ()
g(a1)(a1)           // ok
g(a1)(a2)           // error
g(new A())(new A()) // error
g(new A())(a1)      // error

val x0 = g(new A()) _
x0 (new A())          // error

class C[T]

def h(x: A): C[x.type] = ???
val x = h(a1)
val y = h(new A())

}
