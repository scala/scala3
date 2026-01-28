import scala.language.experimental.namedTuples

// Ensure we don't widen parameter singleton types
// which would cause unsoundness similar to #19746
object Test:
  def foo[T](f: (name: String, age: Int)): NamedTupleDecomposition.Names[f.type] =
    ("name", "age") // error
