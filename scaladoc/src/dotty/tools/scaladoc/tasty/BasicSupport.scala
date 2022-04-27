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

  extension (using Quotes)(sym: reflect.Symbol)
    def documentation = parseComment(sym.docstring.getOrElse(""), sym.tree)

    def getAnnotations(): List[Annotation] =
      sym.annotations.filterNot(_.symbol.packageName.startsWith("scala.annotation.internal")).map(parseAnnotation).reverse

    def isDeprecated(): Option[Annotation] =
      sym.annotations.find { a =>
        a.symbol.packageName == "scala" && a.symbol.className.contains("deprecated") ||
        a.symbol.packageName == "java.lang" && a.symbol.className.contains("Deprecated")
      }.map(parseAnnotation)

    def isLeftAssoc: Boolean = !sym.name.endsWith(":")
  end extension
