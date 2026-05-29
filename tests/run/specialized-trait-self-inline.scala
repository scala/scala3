//> using options -language:experimental.specializedTraits
//> using scala 3.9.0-RC1-bin-SNAPSHOT-nonbootstrapped

import math.Numeric.Implicits.infixNumericOps
import math.Ordering.Implicits.infixOrderingOps

inline trait Foo[T: {Specialized, Numeric}]:
    inline def foo[S: Specialized](inline x: Int, inline y: Int, inline w: T, inline z: S): S = 
        inline if x < y then
            z
        else
            println(w)
            foo(x - 1, y, w - summon[Numeric[T]].one, z)

@main def Test =
    val x = new Foo[Double] {}
    println(x.foo(10, 6, 5, "Lift Off!"))
