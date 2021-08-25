package dotty.tools.scaladoc
package tasty

import collection.JavaConverters._
import dotty.tools.scaladoc._
import scala.quoted._

import SymOps._
import ScaladocSupport._

trait BasicSupport:
  self: TastyParser =>

  object SymOpsWithLinkCache extends SymOpsWithLinkCache
  export SymOpsWithLinkCache._


  def parseAnnotation(using Quotes)(annotTerm: reflect.Term): Annotation =
    import reflect._
    import dotty.tools.dotc.ast.Trees.{SeqLiteral}
    val dri = annotTerm.tpe.typeSymbol.dri
    def inner(t: Term): List[Annotation.AnnotationParameter] = t match {
        case i: Ident => List(Annotation.LinkParameter(None, i.tpe.typeSymbol.dri, i.name))
        case Typed(term, tpeTree) => inner(term)
        case SeqLiteral(args, tpeTree) => args.map(_.asInstanceOf[Term]).flatMap(inner)
        case Literal(constant) => List(Annotation.PrimitiveParameter(None, constant.show))
        case NamedArg(name, Literal(constant)) => List(Annotation.PrimitiveParameter(Some(name), constant.show))
        case x @ Select(qual, name) => List.empty
        case other => List(Annotation.UnresolvedParameter(None, other.show))
      }

    val params = annotTerm match
      case Apply(target, appliedWith) => {
        appliedWith.flatMap(inner)
      }

    Annotation(dri, params)

  def isValidPos(using Quotes)(pos: reflect.Position) =
    if pos.exists then pos.start != pos.end else false

  /* Heuristics solving a problem if the constructor of class in source code contains param clauses or not.
  It seems that this information is lost during source code processing */
  def constructorWithoutParamLists(using Quotes)(c: reflect.ClassDef): Boolean =
    !isValidPos(c.constructor.pos)  || {
      val end = c.constructor.pos.end
      val typesEnd =  c.constructor.leadingTypeParams.lastOption.fold(end - 1)(_.pos.end)
      val classDefTree = c.constructor.show
      c.constructor.leadingTypeParams.nonEmpty && end <= typesEnd + 1
    }

  extension (using Quotes)(sym: reflect.Symbol)
    def documentation = sym.docstring.map(parseComment(_, sym.tree))

    def getAnnotations(): List[Annotation] =
      sym.annotations.filterNot(_.symbol.packageName.startsWith("scala.annotation.internal")).map(parseAnnotation).reverse

    def isDeprecated(): Option[Annotation] =
      sym.annotations.find { a =>
        a.symbol.packageName == "scala" && a.symbol.className.contains("deprecated") ||
        a.symbol.packageName == "java.lang" && a.symbol.className.contains("Deprecated")
      }.map(parseAnnotation)

    def isLeftAssoc: Boolean = !sym.name.endsWith(":")
  end extension
