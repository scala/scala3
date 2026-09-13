class GenericCompanion[+CC[X] <: Iterable[X]]
object Test {
  val a : GenericCompanion[scala.collection.immutable.Seq] = ???
  val b : GenericCompanion[scala.collection.mutable.Seq] = ???
  List(a, b) // immutable.this.List.apply[GenericCompanion[Seq]](Test.this.a, Test.this.b)
}
