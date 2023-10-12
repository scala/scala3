import scala.language.experimental.macros
import scala.quoted.{Quotes, Expr, Type}

trait TraitWithTypeParam[A]:
  inline def foo: Option[A] = ${ MacrosImpl.fooImpl[A] }
  def foo: Option[A] = macro MacrosImpl.compatFooImpl[A]

object MacrosImpl:
  def fooImpl[A: Type](using quotes: Quotes): Expr[Option[A]] = ???
  def compatFooImpl[A: c.WeakTypeTag](c: Context): c.Tree = ???

trait Context:
  type WeakTypeTag[A]
  type Tree