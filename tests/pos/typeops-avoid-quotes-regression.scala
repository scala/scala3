// Overeager TypeOps.avoid widening can result here in:
//-- [E008] Not Found Error: typeops-avoid-quotes-regression.scala:15:52 ----------------------------------
// 15 |  val code = '{ ${ Expr.ofSeq(instances.toSeq.map(_.asExprOf[T])) }.toSet }
//    |                                                  ^^^^^^^^^^
//    |                                value asExprOf is not a member of AnyRef
import scala.quoted._

private def wireCollInstances(using q: Quotes) =
  import q.reflect._
  val dependencyResolver = new DependencyResolver[q.type]
  dependencyResolver.resolveAll()

def wireSet_impl[T: Type](using q: Quotes): Unit =
  val instances = wireCollInstances
  val code = '{ ${ Expr.ofSeq(instances.toSeq.map(_.asExprOf[T])) }.toSet }
  ???

class DependencyResolver[Q <: Quotes](using val q: Q):
  import q.reflect.*
  def resolveAll(): Iterable[Tree] = ???
