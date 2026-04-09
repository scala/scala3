import scala.quoted.*

trait Tag[A]
object Tag:
  inline def apply[A]: Tag[A] = ${ applyImpl[A] }
  def applyImpl[A: Type](using Quotes): Expr[Tag[A]] =
    quotes.reflect.report.errorAndAbort(s"Cannot create Tag for ${Type.show[A]}")

def useTag[A](tag: Tag[A]): Unit = ()

object Macro:
  inline def foo[A](): Unit = ${ fooImpl[A] }
  def fooImpl[A: Type](using Quotes): Expr[Unit] =
    '{ useTag(Tag.apply[A]) }
