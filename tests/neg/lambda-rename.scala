class Foo[T]
class Bar[T]

val a: (x: Int) => Bar[x.type] = ??? : ((x: Int) => Foo[x.type]) // error

trait HK[F <: AnyKind]
val b: HK[[X] =>> Foo[(X, X)]] = ??? : HK[[X] =>> Bar[(X, X)]] // error

class X
val c: HK[[X] =>> Foo[(X, X)]] = ??? : HK[[Y] =>> Foo[(X, X)]] // error

val d: HK[[Y] =>> Foo[(X, X)]] = ??? : HK[[X] =>> Foo[(X, X)]] // error
