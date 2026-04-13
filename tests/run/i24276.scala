// scalajs: --skip
// (JVM-only, generic signatures)

class Box[A](value: A) extends AnyVal
class IBox(value: Int) extends AnyVal
class SBox(value: String) extends AnyVal
class Arr[A](value: Array[A]) extends AnyVal
class ArrR[A <: AnyRef](value: Array[A]) extends AnyVal

class Foo:
  def foo1[A](a: A): Box[A] = Box(a)
  def foo2(a: IBox): IBox = a
  def foo3(a: SBox): SBox = a
  def foo4(a: Box[Int]): Box[Int] = a
  def foo5(a: Box[String]): Box[String] = a
  def foo6(a: Arr[Int]): Arr[Int] = a
  def foo6b(a: ArrR[java.lang.Integer]): ArrR[java.lang.Integer] = a
  def foo7(a: Arr[String]): Arr[String] = a
  def foo8(a: Box[Array[Int]]): Box[Array[Int]] = a
  def foo9(a: Box[Array[String]]): Box[Array[String]] = a
  def bar1[A](opt: Option[Box[A]]): Option[Box[A]] = opt
  def bar2(opt: Option[IBox]): Option[IBox] = opt
  def bar3(opt: Option[SBox]): Option[SBox] = opt
  def bar4(opt: Option[Box[Int]]): Option[Box[Int]] = opt
  def bar5(opt: Option[Box[String]]): Option[Box[String]] = opt
  def bar6(opt: Option[Arr[Int]]): Option[Arr[Int]] = opt
  def bar7(opt: Option[Arr[String]]): Option[Arr[String]] = opt

object Foo:
  def sfoo1[A](a: A): Box[A] = Box(a)
  def sfoo2(a: IBox): IBox = a
  def sfoo3(a: SBox): SBox = a
  def sfoo4(a: Box[Int]): Box[Int] = a
  def sfoo5(a: Box[String]): Box[String] = a
  def sfoo6(a: Arr[Int]): Arr[Int] = a
  def sfoo6b(a: ArrR[java.lang.Integer]): ArrR[java.lang.Integer] = a
  def sfoo7(a: Arr[String]): Arr[String] = a
  def sfoo8(a: Box[Array[Int]]): Box[Array[Int]] = a
  def sfoo9(a: Box[Array[String]]): Box[Array[String]] = a
  def sbar1[A](opt: Option[Box[A]]): Option[Box[A]] = opt
  def sbar2(opt: Option[IBox]): Option[IBox] = opt
  def sbar3(opt: Option[SBox]): Option[SBox] = opt
  def sbar4(opt: Option[Box[Int]]): Option[Box[Int]] = opt
  def sbar5(opt: Option[Box[String]]): Option[Box[String]] = opt
  def sbar6(opt: Option[Arr[Int]]): Option[Arr[Int]] = opt
  def sbar7(opt: Option[Arr[String]]): Option[Arr[String]] = opt

@main def Test =
  for mtd <- classOf[Foo].getDeclaredMethods.sortBy(_.getName) do
    println(mtd)
    println(mtd.toGenericString)
    println("---")
