class Box[T](var value: T)

class A:
  var box = new Box[Int](10)

  def update(n: Int) =
    box.value = n

  def this(n: Int) =
    this()
    box.value = n

class B(n: Int) extends A(n):
  this.update(n * n)

object A:
  val a = new B(20)
