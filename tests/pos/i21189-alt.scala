//> using options -source:future -language:experimental.modularity

class MySortedSet[T : Ord] extends SortedSet[T]

trait Ord[T]

trait Sorted[T] extends ParentOfSorted[T]

trait ParentOfSorted[T]:
  given Ord[T] as ord = compiletime.deferred

class SortedSet[T : Ord] extends Sorted[T]
