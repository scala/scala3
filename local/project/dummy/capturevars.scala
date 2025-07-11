package dummy

import language.experimental.captureChecking
import caps.*

trait Test:
    val a: AnyRef^
    val b: AnyRef^
    type Ordinary
    type Ordinary2 >: Int <: String
    type T[-C^ >: {a,b}]
    type U[+C^]
    type C^
    type D^ >: {C} <: {a,b}
    type E^ <: C
    type F^ <: {D,E}
    def foo[C^ >: {a,b}](x: T[C]): Unit
    def bar(x: T[{a,b}]): Unit
    def baz(x: T[{a,b,caps.cap}]): Unit
    def foo2[C^](x: U[C]): Unit
    def bar2(x: U[{a,b,cap}]): Unit
    def baz2(x: U[{caps.cap}]): Unit
    def test[E^, F^ >: {caps.cap} <: {}](x: T[{E,a,b}], y: U[F]): Unit
