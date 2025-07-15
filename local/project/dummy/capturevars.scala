package dummy

import language.experimental.captureChecking

trait Test:
    type T[-C^]
    type U[+C^]
    type C^
    type D^
    def foo[C^](x: T[C]): Unit
    def bar(x: T[{}]): Unit
    def baz(x: T[{caps.cap}]): Unit
    def foo2[C^](x: U[C]): Unit
    def bar2(x: U[{}]): Unit
    def baz2(x: U[{caps.cap}]): Unit
    def test[E^, F^ >: {caps.cap} <: {}](x: T[E], y: U[F]): Unit
