trait Binding[+A]:
  def value: A = ???
object Binding:
  inline def apply[A](inline a: A): Binding[A] = ???
  final case class Constant[+A](override val value: A) extends Binding[A]

extension [A](inline binding: Binding[A])
    transparent inline def bind: A = null.asInstanceOf[A]

trait Vars[A] extends BindingSeq[A]
trait BindingSeq[+A]:
  def foreachBinding[U](f: A => Binding[U]): Binding[Unit] = ???

extension [A](inline bindingSeq: BindingSeq[A])
  transparent inline def foreach[U](inline f: A => U): Unit = ${ Macros.foreach('bindingSeq, 'f) }
