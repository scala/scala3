import scala.quoted.*

case class Entity(value: String)
case class Input(ent: Entity)
case class Container(ents: List[Entity])

object Dsl {
  inline def container(inline c: Input):Container = ${ containerImpl('c) }
  def containerImpl(c: Expr[Input])(using Quotes): Expr[Container] =
    import quotes.reflect.*
    val entExpr = c match
      case '{ Input($ent) } => ent
      case _ => report.throwError("Cannot Extract Entity from Input")
    '{ Container(List($entExpr)) }


  given FromExpr[Entity] with
    def unapply(expr: Expr[Entity])(using Quotes) = expr match
      case '{ Entity(${Expr(value)}) } => Some(Entity(value))
      case _ => None

  inline def pull(inline c: Container): Entity = ${ pullImpl('c) }
  def pullImpl(c: Expr[Container])(using Quotes): Expr[Entity] =
    import quotes.reflect.*
    val inputs = c match
      case '{ Container($list) } =>
        list.valueOrError
      case _ => report.throwError("Cannot Extract List from Container")
    '{ Entity(${Expr(inputs.head.value)}) }
}
