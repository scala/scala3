class A(a: String) extends B(new C {
  override def get(): String = a
})

class B(c: C)

trait C {
  def get(): String
}
