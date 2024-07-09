import language.experimental.namedTuples
import NamedTuple.{NamedTuple, AnyNamedTuple}

// Repros for bugs or questions
class ClassToMap[A]()
abstract class ClassToFind[Rows <: AnyNamedTuple]:
  def mapped: NamedTuple.Map[Rows, ClassToMap]

given TDB: ClassToFind[(t1: Int, t2: String)] with
  override def mapped = (
    t1 = ClassToMap[Int](),
    t2 = ClassToMap[String]()
  )

type TypeAlias = (t1: Int, t2: String)
class Repro1_Pass(using val testDB: ClassToFind[TypeAlias]) {
  def query() =
    testDB.mapped.t1
}
class Repro1_Fail(using val testDB: ClassToFind[(t1: Int, t2: String)]) {
  def query() =
    testDB.mapped.t1 // fails to compile
}