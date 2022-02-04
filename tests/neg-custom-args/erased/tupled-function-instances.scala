import scala.util.TupledFunction
object Test {
  def main(args: Array[String]): Unit = {
    type T
    type R

    summon[TupledFunction[(erased T) => R, erased Tuple1[T] => R]] // error
    summon[TupledFunction[(erased T, T) => R, (erased (T, T)) => R]] // error
    summon[TupledFunction[(erased T, T, T) => R,(erased (T, T, T)) => R]] // error
    summon[TupledFunction[(erased T, T, T, T) => R,(erased (T, T, T, T)) => R]] // error
    summon[TupledFunction[(erased T, T, T, T, T) => R,(erased (T, T, T, T, T)) => R]] // error
    summon[TupledFunction[(erased T, T, T, T, T, T) => R,(erased (T, T, T, T, T, T)) => R]] // error
    summon[TupledFunction[(erased T, T, T, T, T, T, T) => R,(erased (T, T, T, T, T, T, T)) => R]] // error
    summon[TupledFunction[(erased T, T, T, T, T, T, T, T) => R,(erased (T, T, T, T, T, T, T, T)) => R]] // error
    summon[TupledFunction[(erased T, T, T, T, T, T, T, T, T) => R,(erased (T, T, T, T, T, T, T, T, T)) => R]] // error
    summon[TupledFunction[(erased T, T, T, T, T, T, T, T, T, T) => R,(erased (T, T, T, T, T, T, T, T, T, T)) => R]] // error
    summon[TupledFunction[(erased T, T, T, T, T, T, T, T, T, T, T) => R,(erased (T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    summon[TupledFunction[(erased T, T, T, T, T, T, T, T, T, T, T, T) => R,(erased (T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    summon[TupledFunction[(erased T, T, T, T, T, T, T, T, T, T, T, T, T) => R,(erased (T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    summon[TupledFunction[(erased T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R,(erased (T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    summon[TupledFunction[(erased T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R,(erased (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    summon[TupledFunction[(erased T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R,(erased (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    summon[TupledFunction[(erased T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R,(erased (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    summon[TupledFunction[(erased T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R,(erased (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    summon[TupledFunction[(erased T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R,(erased (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    summon[TupledFunction[(erased T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R,(erased (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    summon[TupledFunction[(erased T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R,(erased (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    summon[TupledFunction[(erased T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R,(erased (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    summon[TupledFunction[(erased T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R,(erased (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    summon[TupledFunction[(erased T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R,(erased (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error
    summon[TupledFunction[(erased T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) => R,(erased (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => R]] // error

    summon[TupledFunction[(erased T) ?=> R, (erased Tuple1[T]) ?=> R]] // error
    summon[TupledFunction[(erased T, T) ?=> R, (erased T, T) ?=> R]] // error
    summon[TupledFunction[(erased T, T, T) ?=> R, (erased T, T, T) ?=> R]] // error
    summon[TupledFunction[(erased T, T, T, T) ?=> R, (erased T, T, T, T) ?=> R]] // error
    summon[TupledFunction[(erased T, T, T, T, T) ?=> R, (erased T, T, T, T, T) ?=> R]] // error
    summon[TupledFunction[(erased T, T, T, T, T, T) ?=> R, (erased T, T, T, T, T, T) ?=> R]] // error
    summon[TupledFunction[(erased T, T, T, T, T, T, T) ?=> R, (erased T, T, T, T, T, T, T) ?=> R]] // error
    summon[TupledFunction[(erased T, T, T, T, T, T, T, T) ?=> R, (erased T, T, T, T, T, T, T, T) ?=> R]] // error
    summon[TupledFunction[(erased T, T, T, T, T, T, T, T, T) ?=> R, (erased T, T, T, T, T, T, T, T, T) ?=> R]] // error
    summon[TupledFunction[(erased T, T, T, T, T, T, T, T, T, T) ?=> R, (erased T, T, T, T, T, T, T, T, T, T) ?=> R]] // error
    summon[TupledFunction[(erased T, T, T, T, T, T, T, T, T, T, T) ?=> R, (erased T, T, T, T, T, T, T, T, T, T, T) ?=> R]] // error
    summon[TupledFunction[(erased T, T, T, T, T, T, T, T, T, T, T, T) ?=> R, (erased T, T, T, T, T, T, T, T, T, T, T, T) ?=> R]] // error
    summon[TupledFunction[(erased T, T, T, T, T, T, T, T, T, T, T, T, T) ?=> R, (erased T, T, T, T, T, T, T, T, T, T, T, T, T) ?=> R]] // error
    summon[TupledFunction[(erased T, T, T, T, T, T, T, T, T, T, T, T, T, T) ?=> R, (erased T, T, T, T, T, T, T, T, T, T, T, T, T, T) ?=> R]] // error
    summon[TupledFunction[(erased T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) ?=> R, (erased T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) ?=> R]] // error
    summon[TupledFunction[(erased T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) ?=> R, (erased T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) ?=> R]] // error
    summon[TupledFunction[(erased T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) ?=> R, (erased T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) ?=> R]] // error
    summon[TupledFunction[(erased T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) ?=> R, (erased T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) ?=> R]] // error
    summon[TupledFunction[(erased T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) ?=> R, (erased T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) ?=> R]] // error
    summon[TupledFunction[(erased T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) ?=> R, (erased T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) ?=> R]] // error
    summon[TupledFunction[(erased T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) ?=> R, (erased T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) ?=> R]] // error
    summon[TupledFunction[(erased T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) ?=> R, (erased T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) ?=> R]] // error
    summon[TupledFunction[(erased T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) ?=> R, (erased T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) ?=> R]] // error
    summon[TupledFunction[(erased T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) ?=> R, (erased T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) ?=> R]] // error
    summon[TupledFunction[(erased T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) ?=> R, (erased T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) ?=> R]] // error
  }
}