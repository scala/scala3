//> using options -language:experimental.modularity -source future
// A minimisation of pos/hylolib-cb that broke while fixing i15177
trait Value[Self]
trait Coll[Self]:
  type Pos: Value
  extension (self: Self) def pos: Pos
extension [Self: Coll](self: Self) def trigger = self.pos
class Slice[Base]
given SliceIsColl[T: Coll as c]: Coll[Slice[T]] with
  type Pos = c.Pos
  extension (self: Slice[T]) def pos: Pos = ???
