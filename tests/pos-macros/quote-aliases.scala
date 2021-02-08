import scala.quoted.*

object Test:

  def f1(using Quotes)(t: quotes.reflect.Tree): Unit = ()

  inline def q(using q: Quotes): q.type = q
  def f2(using Quotes)(t: q.reflect.Tree): Unit = ()

  inline def r(using q: Quotes): q.reflect.type = q.reflect
  def f3(using Quotes)(t: r.Tree): Unit = ()
