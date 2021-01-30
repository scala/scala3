package dotty.dokka
package tasty

import collection.JavaConverters._
import dotty.dokka._
import dotty.dokka.model.api.Annotation
import dotty.dokka.model.api.TastyDocumentableSource
import scala.quoted._

trait BasicSupport:
  self: TastyParser =>
  import qctx.reflect._
  object SymOps extends SymOps[qctx.type](qctx)
  export SymOps._

  def parseAnnotation(annotTerm: Term): Annotation =
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

  extension (sym: Symbol)
    def documentation = sym.docstring.map(parseComment(_, sym.tree))

    def source(using Quotes) =
      val path = sym.pos.map(_.sourceFile.jpath).filter(_ != null).map(_.toAbsolutePath).map(_.toString)
      path.map(TastyDocumentableSource(_, sym.pos.get.startLine))

    def getAnnotations(): List[Annotation] =
      sym.annotations.filterNot(_.symbol.packageName.startsWith("scala.annotation.internal")).map(parseAnnotation).reverse

    def isLeftAssoc: Boolean = !sym.name.endsWith(":")


