import scala.util.TupledFunction

object Test {
  def main(args: Array[String]): Unit = {
    type T
    type R

    summon[TupledFunction[Nothing, ((T, T, T)) => R]] // error
    summon[TupledFunction[Any, ((T, T, T)) => R]] // error
    summon[TupledFunction[Tuple1[Int], ((T, T, T)) => R]] // error

    summon[TupledFunction[(T, T, T) => R, Nothing]] // error
    summon[TupledFunction[(T, T, T) => R, Any]] // error
    summon[TupledFunction[((T, T, T)) => R, Tuple1[Int]]] // error

    summon[TupledFunction[() => R, () => R]] // error
    summon[TupledFunction[() => Unit, () => Unit]] // error
    summon[TupledFunction[(T, T, T) => R, () => R]] // error
    summon[TupledFunction[(T, T, T) => R, (T, T) => R]] // error

    summon[TupledFunction[(T, T, T) => R, ((T, T, T)) ?=> R]] // error
    summon[TupledFunction[(T, T, T) => R, ((T, T, T)) ?=> R]] // error

  }
}