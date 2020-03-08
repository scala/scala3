type HasThisB[T] = HasThis { type This <: T }
trait HasThis {
  type This >: this.type <: HasThisB[This]
}

type FB[T] = F { type This <: T }
class F extends HasThis {
  type This >: this.type <: FB[This]
}