package example

trait A {
  def x1: Builder[?]
  def x2: Service

  def create: String =
    x1.addService(x2).build()
}
