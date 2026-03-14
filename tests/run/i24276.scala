// scalajs: --skip

class Box[A](value: A) extends AnyVal
class IBox(value: Int) extends AnyVal
class SBox(value: String) extends AnyVal

class Foo:
  def foo1[A](a: A): Box[A] = Box(a)
  def foo2(a: IBox): IBox = a
  def foo3(a: SBox): SBox = a
  def bar1[A](opt: Option[Box[A]]): Option[Box[A]] = opt
  def bar2[A](opt: Option[IBox]): Option[IBox] = opt
  def bar3[A](opt: Option[SBox]): Option[SBox] = opt

object Foo:
  def sfoo1[A](a: A): Box[A] = Box(a)
  def sfoo2(a: IBox): IBox = a
  def sfoo3(a: SBox): SBox = a
  def sbar1[A](opt: Option[Box[A]]): Option[Box[A]] = opt
  def sbar2[A](opt: Option[IBox]): Option[IBox] = opt
  def sbar3[A](opt: Option[SBox]): Option[SBox] = opt

@main def Test =
  for mtd <- classOf[Foo].getDeclaredMethods.sortBy(_.getName) do
    println(mtd.toGenericString)
