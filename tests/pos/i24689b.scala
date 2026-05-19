trait A
trait B

object Test1:
  def foo(f: B => Unit) = ???
  def transfer(in: A): Unit =
    foo(in => transfer(in))
    foo(transfer)
    foo(transfer(_))
  def transfer(in: B): Unit = ???

object Test2:
  def foo[T <: (B => Unit)](f: T) = ???
  def transfer(in: A): Unit =
    foo(in => transfer(in))
    foo(transfer)
    foo(transfer(_))
  def transfer(in: B): Unit = ???
