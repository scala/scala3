package dotty.dokka.tasty

import org.jetbrains.dokka.links._
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
        appliedWith.map {
          case Literal(constant) => Annotation.PrimitiveParameter(None, constant.value match {
            case s: String => "\"" + s"$s" + "\""
            case other => other.toString()
          })
          case Select(qual, name) =>
            val dri = qual.tpe.termSymbol.companionClass.dri
            Annotation.LinkParameter(None, dri, s"${dri.getClassNames}.$name") // TODO this is a nasty hack!

          case other => Annotation.UnresolvedParameter(None, other.show)
        }
      }

    Annotation(dri, params)


  extension (sym: Symbol):
    def documentation(using cxt: Context) = sym.documentation match
      case Some(comment) =>
          Map(sourceSet -> parseComment(comment, sym.tree))
      case None =>
          Map.empty

    def source(using ctx: Context) =
      val path = Some(sym.pos.sourceFile.jpath).filter(_ != null).map(_.toAbsolutePath).map(_.toString)
      path.map(TastyDocumentableSource(_, sym.pos.startLine))

    def getAnnotations(): List[Annotation] =
    sym.annots.filterNot(_.symbol.packageName.startsWith("scala.annotation.internal")).map(parseAnnotation).reverse

  private val emptyDRI = DRI.Companion.getTopLevel



