object Test {
  def main(args: Array[String]): Unit = {
    type T
    type R

    summon[TupledFunction[Nothing, ((T, T, T)) => R]] // error
    summon[TupledFunction[Any, ((T, T, T)) => R]] // error
    summon[TupledFunction[Tuple1[Int], ((T, T, T)) => R]] // error

    summon[TupledFunction[(T, T, T))=> R, Nothing]] // error
    summon[TupledFunction[(T, T, T) => R, Any]] // error
    summon[TupledFunction[((T, T, T)) => R, Tuple1[Int]]] // error

    summon[TupledFunction[() => R, () => R]] // error
    summon[TupledFunction[() => Unit, () => Unit]] // error
    summon[TupledFunction[(T, T, T) => R, () => R]] // error
    summon[TupledFunction[(T, T, T) => R, (T, T) => R]] // error

    summon[TupledFunction[(T, T, T) => R, (given (T, T, T)) =>R]] // error
    summon[TupledFunction[(given T, T, T) => R, ((T, T, T)) =>R]] // error

    summon[TupledFunction[(erased T) => R, erased Tuple1[T] => R]] // error
    summon[TupledFunction[erased (T, T) => R, erased ((T, T)) => R]] // error
    summon[TupledFunction[erased (T, T, T) => R, erased ((T, T, T)) => R]] // error
    summon[TupledFunction[erased (T, T, T, T) => R, erased ((T, T, T, T)) => R]] // error
    summon[TupledFunction[erased (T, T, T, T, T) => R, erased ((T, T, T, T, T)) => R]] // error
    summon[TupledFunction[erased (T, T, T, T, T, T) => R, erased ((T, T, T, T, T, T)) => R]] // error
    summon[TupledFunction[erased (T, T, T, T, T, T, T) => R, erased ((T, T, T, T, T, T, T)) => R]] // error
    summon[TupledFunction[erased (T, T, T, T, T, T, T, T) => R, erased ((T, T, T, T, T, T, T, T)) => R]] // error
    summon[TupledFunction[erased (T, T, T, T, T, T, T, T, T) => R, erased ((T, T, T, T, T, T, T, T, T)) => R]] // error
    summon[TupledFunction[erased (T, T, T, T, T, T, T, T, T, T) => R, erased ((T, T, T, T, T, T, T, T, T, T)) => R]] // error
    summon[TupledFunction[erased (T, T, T, T, T, T, T, T, T, T, T) => R, erased ((T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    summon[TupledFunction[erased (T, T, T, T, T, T, T, T, T, T, T, T) => R, erased ((T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    summon[TupledFunction[erased (T, T, T, T, T, T, T, T, T, T, T, T, T) => R, erased ((T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    summon[TupledFunction[erased (T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, erased ((T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    summon[TupledFunction[erased (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, erased ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    summon[TupledFunction[erased (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, erased ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    summon[TupledFunction[erased (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, erased ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    summon[TupledFunction[erased (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, erased ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    summon[TupledFunction[erased (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, erased ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    summon[TupledFunction[erased (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, erased ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    summon[TupledFunction[erased (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, erased ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    summon[TupledFunction[erased (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, erased ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    summon[TupledFunction[erased (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, erased ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    summon[TupledFunction[erased (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, erased ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    summon[TupledFunction[erased (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, erased ((T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error

    summon[TupledFunction[(given erased T) => R, (given erased Tuple1[T]) => R]] // error
    summon[TupledFunction[(given erased T, T) => R, (given erased (T, T)) => R]] // error
    summon[TupledFunction[(given erased T, T, T) => R, (given erased (T, T, T)) => R]] // error
    summon[TupledFunction[(given erased T, T, T, T) => R, (given erased (T, T, T, T)) => R]] // error
    summon[TupledFunction[(given erased T, T, T, T, T) => R, (given erased (T, T, T, T, T)) => R]] // error
    summon[TupledFunction[(given erased T, T, T, T, T, T) => R, (given erased (T, T, T, T, T, T)) => R]] // error
    summon[TupledFunction[(given erased T, T, T, T, T, T, T) => R, (given erased (T, T, T, T, T, T, T)) => R]] // error
    summon[TupledFunction[(given erased T, T, T, T, T, T, T, T) => R, (given erased (T, T, T, T, T, T, T, T)) => R]] // error
    summon[TupledFunction[(given erased T, T, T, T, T, T, T, T, T) => R, (given erased (T, T, T, T, T, T, T, T, T)) => R]] // error
    summon[TupledFunction[(given erased T, T, T, T, T, T, T, T, T, T) => R, (given erased (T, T, T, T, T, T, T, T, T, T)) => R]] // error
    summon[TupledFunction[(given erased T, T, T, T, T, T, T, T, T, T, T) => R, (given erased (T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    summon[TupledFunction[(given erased T, T, T, T, T, T, T, T, T, T, T, T) => R, (given erased (T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    summon[TupledFunction[(given erased T, T, T, T, T, T, T, T, T, T, T, T, T) => R, (given erased (T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    summon[TupledFunction[(given erased T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, (given erased (T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    summon[TupledFunction[(given erased T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, (given erased (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    summon[TupledFunction[(given erased T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, (given erased (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    summon[TupledFunction[(given erased T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, (given erased (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    summon[TupledFunction[(given erased T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, (given erased (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    summon[TupledFunction[(given erased T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, (given erased (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    summon[TupledFunction[(given erased T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, (given erased (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    summon[TupledFunction[(given erased T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, (given erased (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    summon[TupledFunction[(given erased T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, (given erased (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    summon[TupledFunction[(given erased T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, (given erased (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    summon[TupledFunction[(given erased T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, (given erased (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    summon[TupledFunction[(given erased T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R, (given erased (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
  }
}