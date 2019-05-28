object Test {
  def main(args: Array[String]): Unit = {
    type T
    type R

    the[TupledFunction[Nothing, ((T, T, T)) => R]] // error
    the[TupledFunction[Any, ((T, T, T)) => R]] // error
    the[TupledFunction[Tuple1[Int], ((T, T, T)) => R]] // error

    the[TupledFunction[(T, T, T))=> R, Nothing]] // error
    the[TupledFunction[(T, T, T) => R, Any]] // error
    the[TupledFunction[((T, T, T)) => R, Tuple1[Int]]] // error

    the[TupledFunction[() => R, () => R]] // error
    the[TupledFunction[() => Unit, () => Unit]] // error
    the[TupledFunction[(T, T, T) => R, () => R]] // error
    the[TupledFunction[(T, T, T) => R, (T, T) => R]] // error

    the[TupledFunction[(T, T, T) => R, given ((T, T, T)) => R]] // error
    the[TupledFunction[given (T, T, T) => R, ((T, T, T)) => R]] // error

    the[TupledFunction[erased T => R, erased Tuple1[T] => R]] // error
    the[TupledFunction[erased (T, T) => R, erased ((T, T)) => R]] // error
    the[TupledFunction[erased (T, T, T) => R, erased ((T, T, T)) => R]] // error
    the[TupledFunction[erased (T, T, T, T) => R, erased ((T, T, T, T)) => R]] // error
    the[TupledFunction[erased (T, T, T, T, T) => R, erased ((T, T, T, T, T)) => R]] // error
    the[TupledFunction[erased (T, T, T, T, T, T) => R, erased ((T, T, T, T, T, T)) => R]] // error
    the[TupledFunction[erased (T, T, T, T, T, T, T) => R, erased ((T, T, T, T, T, T, T)) => R]] // error
    the[TupledFunction[erased (T, T, T, T, T, T, T, T) => R, erased ((T, T, T, T, T, T, T, T)) => R]] // error
    the[TupledFunction[erased (T, T, T, T, T, T, T, T, T) => R, erased ((T, T, T, T, T, T, T, T, T)) => R]] // error
    the[TupledFunction[erased (T, T, T, T, T, T, T, T, T, T) => R, erased ((T, T, T, T, T, T, T, T, T, T)) => R]] // error
    the[TupledFunction[erased (T, T, T, T, T, T, T, T, T, T, T) => R, erased ((T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    the[TupledFunction[erased (T, T, T, T, T, T, T, T, T, T, T, T) => R, erased ((T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    the[TupledFunction[erased (T, T, T, T, T, T, T, T, T, T, T, T, T) => R, erased ((T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    the[TupledFunction[erased (T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, erased ((T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    the[TupledFunction[erased (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, erased ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    the[TupledFunction[erased (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, erased ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    the[TupledFunction[erased (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, erased ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    the[TupledFunction[erased (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, erased ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    the[TupledFunction[erased (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, erased ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    the[TupledFunction[erased (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, erased ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    the[TupledFunction[erased (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, erased ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    the[TupledFunction[erased (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, erased ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    the[TupledFunction[erased (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, erased ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    the[TupledFunction[erased (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, erased ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    the[TupledFunction[erased (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, erased ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error

    the[TupledFunction[given erased T => R, given erased Tuple1[T] => R]] // error
    the[TupledFunction[given erased (T, T) => R, given erased ((T, T)) => R]] // error
    the[TupledFunction[given erased (T, T, T) => R, given erased ((T, T, T)) => R]] // error
    the[TupledFunction[given erased (T, T, T, T) => R, given erased ((T, T, T, T)) => R]] // error
    the[TupledFunction[given erased (T, T, T, T, T) => R, given erased ((T, T, T, T, T)) => R]] // error
    the[TupledFunction[given erased (T, T, T, T, T, T) => R, given erased ((T, T, T, T, T, T)) => R]] // error
    the[TupledFunction[given erased (T, T, T, T, T, T, T) => R, given erased ((T, T, T, T, T, T, T)) => R]] // error
    the[TupledFunction[given erased (T, T, T, T, T, T, T, T) => R, given erased ((T, T, T, T, T, T, T, T)) => R]] // error
    the[TupledFunction[given erased (T, T, T, T, T, T, T, T, T) => R, given erased ((T, T, T, T, T, T, T, T, T)) => R]] // error
    the[TupledFunction[given erased (T, T, T, T, T, T, T, T, T, T) => R, given erased ((T, T, T, T, T, T, T, T, T, T)) => R]] // error
    the[TupledFunction[given erased (T, T, T, T, T, T, T, T, T, T, T) => R, given erased ((T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    the[TupledFunction[given erased (T, T, T, T, T, T, T, T, T, T, T, T) => R, given erased ((T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    the[TupledFunction[given erased (T, T, T, T, T, T, T, T, T, T, T, T, T) => R, given erased ((T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    the[TupledFunction[given erased (T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, given erased ((T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    the[TupledFunction[given erased (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, given erased ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    the[TupledFunction[given erased (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, given erased ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    the[TupledFunction[given erased (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, given erased ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    the[TupledFunction[given erased (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, given erased ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    the[TupledFunction[given erased (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, given erased ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    the[TupledFunction[given erased (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, given erased ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    the[TupledFunction[given erased (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, given erased ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    the[TupledFunction[given erased (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, given erased ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    the[TupledFunction[given erased (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, given erased ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    the[TupledFunction[given erased (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, given erased ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    the[TupledFunction[given erased (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, given erased ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
  }
}