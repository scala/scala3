case class Normal(value: String)
object Normal

case class ClassWithCaseCompanion(value: String)
case object ClassWithCaseCompanion

def instantiate[T](product: Product)(implicit mirror: scala.deriving.Mirror.ProductOf[T]) =
  mirror.fromProduct(product)

@main def Test: Unit = {
  assert(instantiate[Normal](Tuple1("a")) == Normal("a")) // works as expected

  assert(instantiate[ClassWithCaseCompanion.type](EmptyTuple) == ClassWithCaseCompanion) // works as expected

  val c = instantiate[ClassWithCaseCompanion](Tuple1("b")) // throws java.lang.ClassCastException: class ClassWithCaseCompanion$ cannot be cast to class ClassWithCaseCompanion
  assert(c == ClassWithCaseCompanion("b")) // desired behaviour

  val d = instantiate[ClassWithCaseCompanion.type](EmptyTuple)
  assert(d == ClassWithCaseCompanion)
}
