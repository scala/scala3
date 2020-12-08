package dotty.communitybuild

import scala.quoted._

object FieldsImpl:
  inline def fieldsOfType[V, T](inline v: V): Seq[T] =
    ${ fieldsImpl[V, T]('v) }

  def fieldsImpl[V: Type, T: Type](from: Expr[V])(using Quotes): Expr[Seq[T]] =
    import quotes.reflect._
    val retType = TypeTree.of[T].tpe
    def isProjectField(s: Symbol) =
      s.isValDef && s.tree.asInstanceOf[ValDef].tpt.tpe <:< retType
    val projectsTree = Term.of(from)
    val symbols = TypeTree.of[V].symbol.fields.filter(isProjectField)
    val selects = symbols.map(Select(projectsTree, _).asExprOf[T])
    '{ println(${Expr(retType.show)}); ${Varargs(selects)} }
