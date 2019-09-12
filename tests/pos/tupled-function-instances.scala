object Test {
  def main(args: Array[String]): Unit = {
    type T
    type R

    summon[TupledFunction[() => R, Unit => R]]
    summon[TupledFunction[T => R, Tuple1[T] => R]]
    summon[TupledFunction[(T, T) => R, ((T, T)) => R]]
    summon[TupledFunction[(T, T, T) => R, ((T, T, T)) => R]]
    summon[TupledFunction[(T, T, T, T) => R, ((T, T, T, T)) => R]]
    summon[TupledFunction[(T, T, T, T, T) => R, ((T, T, T, T, T)) => R]]
    summon[TupledFunction[(T, T, T, T, T, T) => R, ((T, T, T, T, T, T)) => R]]
    summon[TupledFunction[(T, T, T, T, T, T, T) => R, ((T, T, T, T, T, T, T)) => R]]
    summon[TupledFunction[(T, T, T, T, T, T, T, T) => R, ((T, T, T, T, T, T, T, T)) => R]]
    summon[TupledFunction[(T, T, T, T, T, T, T, T, T) => R, ((T, T, T, T, T, T, T, T, T)) => R]]
    summon[TupledFunction[(T, T, T, T, T, T, T, T, T, T) => R, ((T, T, T, T, T, T, T, T, T, T)) => R]]
    summon[TupledFunction[(T, T, T, T, T, T, T, T, T, T, T) => R, ((T, T, T, T, T, T, T, T, T, T, T)) => R]]
    summon[TupledFunction[(T, T, T, T, T, T, T, T, T, T, T, T) => R, ((T, T, T, T, T, T, T, T, T, T, T, T)) => R]]
    summon[TupledFunction[(T, T, T, T, T, T, T, T, T, T, T, T, T) => R, ((T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]]
    summon[TupledFunction[(T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, ((T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]]
    summon[TupledFunction[(T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]]
    summon[TupledFunction[(T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]]
    summon[TupledFunction[(T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]]
    summon[TupledFunction[(T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]]
    summon[TupledFunction[(T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]]
    summon[TupledFunction[(T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]]
    summon[TupledFunction[(T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]]
    summon[TupledFunction[(T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]]
    summon[TupledFunction[(T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]]
    summon[TupledFunction[(T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]]
    summon[TupledFunction[(T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]]

    summon[TupledFunction[given T => R, given Tuple1[T] => R]]
    summon[TupledFunction[given (T, T) => R, given ((T, T)) => R]]
    summon[TupledFunction[given (T, T, T) => R, given ((T, T, T)) => R]]
    summon[TupledFunction[given (T, T, T, T) => R, given ((T, T, T, T)) => R]]
    summon[TupledFunction[given (T, T, T, T, T) => R, given ((T, T, T, T, T)) => R]]
    summon[TupledFunction[given (T, T, T, T, T, T) => R, given ((T, T, T, T, T, T)) => R]]
    summon[TupledFunction[given (T, T, T, T, T, T, T) => R, given ((T, T, T, T, T, T, T)) => R]]
    summon[TupledFunction[given (T, T, T, T, T, T, T, T) => R, given ((T, T, T, T, T, T, T, T)) => R]]
    summon[TupledFunction[given (T, T, T, T, T, T, T, T, T) => R, given ((T, T, T, T, T, T, T, T, T)) => R]]
    summon[TupledFunction[given (T, T, T, T, T, T, T, T, T, T) => R, given ((T, T, T, T, T, T, T, T, T, T)) => R]]
    summon[TupledFunction[given (T, T, T, T, T, T, T, T, T, T, T) => R, given ((T, T, T, T, T, T, T, T, T, T, T)) => R]]
    summon[TupledFunction[given (T, T, T, T, T, T, T, T, T, T, T, T) => R, given ((T, T, T, T, T, T, T, T, T, T, T, T)) => R]]
    summon[TupledFunction[given (T, T, T, T, T, T, T, T, T, T, T, T, T) => R, given ((T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]]
    summon[TupledFunction[given (T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, given ((T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]]
    summon[TupledFunction[given (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, given ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]]
    summon[TupledFunction[given (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, given ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]]
    summon[TupledFunction[given (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, given ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]]
    summon[TupledFunction[given (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, given ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]]
    summon[TupledFunction[given (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, given ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]]
    summon[TupledFunction[given (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, given ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]]
    summon[TupledFunction[given (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, given ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]]
    summon[TupledFunction[given (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, given ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]]
    summon[TupledFunction[given (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, given ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]]
    summon[TupledFunction[given (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, given ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]]
    summon[TupledFunction[given (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, given ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]]


    type F2 = (T, T) => R
    type TF2 = ((T, T)) => R
    summon[TupledFunction[F2, ((T, T)) => R]]
    summon[TupledFunction[(T, T) => R, TF2]]
    summon[TupledFunction[F2, TF2]]

  }
}