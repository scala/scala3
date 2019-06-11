object Test {
  def main(args: Array[String]): Unit = {
    type T
    type R

    the[TupledFunction[() => R, Unit => R]]
    the[TupledFunction[T => R, Tuple1[T] => R]]
    the[TupledFunction[(T, T) => R, ((T, T)) => R]]
    the[TupledFunction[(T, T, T) => R, ((T, T, T)) => R]]
    the[TupledFunction[(T, T, T, T) => R, ((T, T, T, T)) => R]]
    the[TupledFunction[(T, T, T, T, T) => R, ((T, T, T, T, T)) => R]]
    the[TupledFunction[(T, T, T, T, T, T) => R, ((T, T, T, T, T, T)) => R]]
    the[TupledFunction[(T, T, T, T, T, T, T) => R, ((T, T, T, T, T, T, T)) => R]]
    the[TupledFunction[(T, T, T, T, T, T, T, T) => R, ((T, T, T, T, T, T, T, T)) => R]]
    the[TupledFunction[(T, T, T, T, T, T, T, T, T) => R, ((T, T, T, T, T, T, T, T, T)) => R]]
    the[TupledFunction[(T, T, T, T, T, T, T, T, T, T) => R, ((T, T, T, T, T, T, T, T, T, T)) => R]]
    the[TupledFunction[(T, T, T, T, T, T, T, T, T, T, T) => R, ((T, T, T, T, T, T, T, T, T, T, T)) => R]]
    the[TupledFunction[(T, T, T, T, T, T, T, T, T, T, T, T) => R, ((T, T, T, T, T, T, T, T, T, T, T, T)) => R]]
    the[TupledFunction[(T, T, T, T, T, T, T, T, T, T, T, T, T) => R, ((T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]]
    the[TupledFunction[(T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, ((T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]]
    the[TupledFunction[(T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]]
    the[TupledFunction[(T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]]
    the[TupledFunction[(T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]]
    the[TupledFunction[(T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]]
    the[TupledFunction[(T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]]
    the[TupledFunction[(T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]]
    the[TupledFunction[(T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]]
    the[TupledFunction[(T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]]
    the[TupledFunction[(T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]]
    the[TupledFunction[(T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]]
    the[TupledFunction[(T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]]

    the[TupledFunction[given T => R, given Tuple1[T] => R]]
    the[TupledFunction[given (T, T) => R, given ((T, T)) => R]]
    the[TupledFunction[given (T, T, T) => R, given ((T, T, T)) => R]]
    the[TupledFunction[given (T, T, T, T) => R, given ((T, T, T, T)) => R]]
    the[TupledFunction[given (T, T, T, T, T) => R, given ((T, T, T, T, T)) => R]]
    the[TupledFunction[given (T, T, T, T, T, T) => R, given ((T, T, T, T, T, T)) => R]]
    the[TupledFunction[given (T, T, T, T, T, T, T) => R, given ((T, T, T, T, T, T, T)) => R]]
    the[TupledFunction[given (T, T, T, T, T, T, T, T) => R, given ((T, T, T, T, T, T, T, T)) => R]]
    the[TupledFunction[given (T, T, T, T, T, T, T, T, T) => R, given ((T, T, T, T, T, T, T, T, T)) => R]]
    the[TupledFunction[given (T, T, T, T, T, T, T, T, T, T) => R, given ((T, T, T, T, T, T, T, T, T, T)) => R]]
    the[TupledFunction[given (T, T, T, T, T, T, T, T, T, T, T) => R, given ((T, T, T, T, T, T, T, T, T, T, T)) => R]]
    the[TupledFunction[given (T, T, T, T, T, T, T, T, T, T, T, T) => R, given ((T, T, T, T, T, T, T, T, T, T, T, T)) => R]]
    the[TupledFunction[given (T, T, T, T, T, T, T, T, T, T, T, T, T) => R, given ((T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]]
    the[TupledFunction[given (T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, given ((T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]]
    the[TupledFunction[given (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, given ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]]
    the[TupledFunction[given (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, given ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]]
    the[TupledFunction[given (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, given ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]]
    the[TupledFunction[given (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, given ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]]
    the[TupledFunction[given (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, given ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]]
    the[TupledFunction[given (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, given ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]]
    the[TupledFunction[given (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, given ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]]
    the[TupledFunction[given (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, given ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]]
    the[TupledFunction[given (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, given ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]]
    the[TupledFunction[given (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, given ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]]
    the[TupledFunction[given (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, given ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]]


    type F2 = (T, T) => R
    type TF2 = ((T, T)) => R
    the[TupledFunction[F2, ((T, T)) => R]]
    the[TupledFunction[(T, T) => R, TF2]]
    the[TupledFunction[F2, TF2]]

  }
}