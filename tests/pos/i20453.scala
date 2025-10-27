import scala.language.experimental.namedTuples

object Test:
  val f: (name: String, age: Int) = ???
  val x: NamedTupleDecomposition.Names[f.type] = ("name", "age")
