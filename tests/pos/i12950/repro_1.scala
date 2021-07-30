package repro
object repro:
    object opq:
        opaque type Lift[T] = Int
        extension(v: Int)
            def lift[T]: Lift[T] = v
        extension[T](l: Lift[T])
            def value: Int = l

    export opq.Lift as Lift
    export opq.lift as lift

    final type Two

    extension[TL](l: Lift[TL])
        def repro[TR](using m: Mul[TL, TR]): Int = l.value + m.value

    abstract class Mul[TL, TR]:
        val value: Int

    transparent inline given mulGivenInt[TL <: Int & Singleton, TR <: Int & Singleton]: Mul[TL, TR] =
        val m: Int = scala.compiletime.constValue[TL] * scala.compiletime.constValue[TR]
        new Mul[TL, TR] { val value: Int = m }

    transparent inline given mulGivenTwo[TR <: Int & Singleton]: Mul[Two, TR] =
        val m: Int = 2 * scala.compiletime.constValue[TR]
        new Mul[Two, TR] { val value: Int = m }