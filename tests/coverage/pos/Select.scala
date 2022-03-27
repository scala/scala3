package covtest

trait T:
  def print(): Unit

class A extends T:
  def print() = println("A")
  def instance = this

class B extends A:
  override def print() =
    super.print()
    println(this.instance)

def test(): Unit =
  val a = A()
  val aNew = new A

  a.instance.print() // should instrument `a.instance` and `(a.instance).print()`
  a.print()
