class MySortedSet[T : Ord] extends SortedSet[T]

trait Ord[T]

trait Sorted[T]:
  given orrd: Ord[T] = compiletime.deferred

class SortedSet[T : Ord] extends Sorted[T]
