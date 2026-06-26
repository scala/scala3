// scalajs: --skip

object UnitAlias {
  type U = Unit
  type ConstU[T] = Unit
  type ID[T] = T
}

class Foo[W <: UnitAlias.U] {
  import UnitAlias.*

  type M <: U

  def bar(): U = ()
  def foobar(x: U): U = ()

  def generic[T <: U](): T = ???
  def classTypeParam(): W = ???
  def typeMember(): M = ???

  def constUnit(x: ConstU[Int]): ConstU[Int] = ()
  def idUnit(x: ID[Unit]): ID[Unit] = ()
  def idU(x: ID[U]): ID[U] = ()
  def idIDU(x: ID[ID[U]]): ID[ID[U]] = ()

  val valU: U = ()
  val valClassTypeParam: W = ().asInstanceOf[W]
  val valTypeMember: M = ().asInstanceOf[M]

  // Note: these differ from Scala 2. In Scala 2 the getter is void; in Scala 3 it is BoxedUnit
  val valUnit: Unit = ()
  val valConstUnit: ConstU[Int] = ()
  val valIDUnit: ID[Unit] = ()
  val valIDU: ID[U] = ()
  val valIDIDU: ID[ID[U]] = ()
}

object Test {
  def main(args: Array[String]): UnitAlias.U =
    val ms = classOf[Foo[Unit]].getDeclaredMethods().sortBy(_.getName())
    for m <- ms do
      println(m)
}
