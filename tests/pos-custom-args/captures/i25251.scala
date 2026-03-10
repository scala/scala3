import language.experimental.captureChecking
import language.experimental.modularity
import caps.*

trait Collection[+T] extends Stateful:
  self: Collection[T]^ =>
    type Index

    // slice should have its Index type member a subtype of the Collection's Index
    def getSlice(): (Collection[T] { type Index <: self.Index })^{this.rd} = new Slice(this)

class Slice[+T](tracked val coll: Collection[T]) extends Collection[T]:
  type Index = coll.Index