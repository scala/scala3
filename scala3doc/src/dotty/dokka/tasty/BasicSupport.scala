package dotty.dokka
package tasty

import org.jetbrains.dokka.model._
import collection.JavaConverters._
import dotty.dokka._
import dotty.dokka.model.api.Annotation
import dotty.dokka.model.api.TastyDocumentableSource

trait BasicSupport:
  self: TastyParser =>
  import qctx.reflect._
  object SymOps extends SymOps[qctx.type](qctx)
  export SymOps._

  def parseAnnotation(annotTerm: Term): Annotation =
    val dri = annotTerm.tpe.typeSymbol.dri
    val params = annotTerm match
      case Apply(target, appliedWith) => {
        appliedWith.flatMap {
          case Literal(constant) => Some(Annotation.PrimitiveParameter(None, constant.value match {
            case s: String => "\"" + s"$s" + "\""
            case other => other.toString()
          }))
          case NamedArg(name, Literal(constant)) => Some(Annotation.PrimitiveParameter(Some(name), constant.value match
            case s: String => "\"" + s"$s" + "\""
            case other => other.toString()
          ))
          case x @ Select(qual, name) => None
          case other => Some(Annotation.UnresolvedParameter(None, other.show))
        }
      }

    Annotation(dri, params)


  extension (sym: Symbol)
    def documentation = sym.docstring match
      case Some(docstring) =>
          Map(ctx.sourceSet -> parseComment(docstring, sym.tree))
      case None =>
          Map.empty

    def source =
      val path = Some(sym.pos.get.sourceFile.jpath).filter(_ != null).map(_.toAbsolutePath).map(_.toString)
      path.map(TastyDocumentableSource(_, sym.pos.get.startLine))

    def getAnnotations(): List[Annotation] =
      sym.annotations.filterNot(_.symbol.packageName.startsWith("scala.annotation.internal")).map(parseAnnotation).reverse


