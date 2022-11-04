package dotty.tools.scaladoc
package tasty

import scala.jdk.CollectionConverters._
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
      // Custom annotations should be documented only if annotated by @java.lang.annotation.Documented
      // We allow also some special cases
      val fqNameWhitelist = Set(
        "scala.specialized",
        "scala.throws",
        "scala.transient",
        "scala.volatile",
        "scala.annotation.experimental",
        "scala.annotation.contructorOnly",
        "scala.annotation.static",
        "scala.annotation.targetName",
        "scala.annotation.threadUnsafe",
        "scala.annotation.varargs"
      )
      val documentedSymbol = summon[Quotes].reflect.Symbol.requiredClass("java.lang.annotation.Documented")
      val annotations = sym.annotations.filter { a =>
        a.tpe.typeSymbol.hasAnnotation(documentedSymbol) || fqNameWhitelist.contains(a.symbol.fullName)
      }
      annotations.map(parseAnnotation).reverse

    def isDeprecated(): Option[Annotation] =
      sym.annotations.find { a =>
        a.symbol.packageName == "scala" && a.symbol.className.contains("deprecated") ||
        a.symbol.packageName == "java.lang" && a.symbol.className.contains("Deprecated")
      }.map(parseAnnotation)

    def isLeftAssoc: Boolean = !sym.name.endsWith(":")
  end extension
