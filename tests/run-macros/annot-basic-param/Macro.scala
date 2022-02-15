import scala.annotation.experimental
import scala.quoted._
import scala.annotation.MacroAnnotation
import scala.collection.mutable.Map

@experimental
class pos extends MacroAnnotation {
  override def transformParam(using Quotes)(paramTree: quotes.reflect.Definition, ownerTree: quotes.reflect.Definition): List[quotes.reflect.Definition] =
    import quotes.reflect._
    val n = Ref(paramTree.symbol).asExprOf[Int]
    val rhs = '{
      // assert($n >= 0)
      ${ownerTree.asInstanceOf[DefDef].rhs.get.asExprOf[List[Int]]}
    }.asTerm
    val newRepeat = ownerTree match
      case DefDef(name, params, tpt, _) =>
        DefDef.copy(ownerTree)(name, params, tpt, Some(rhs))
    List(newRepeat)
}
