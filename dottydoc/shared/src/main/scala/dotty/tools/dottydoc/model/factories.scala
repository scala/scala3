package dotty.tools.dottydoc
package model

import comment._
import dotty.tools.dotc
import dotc.core.Types._
import dotc.core.Flags
import dotc.core.Contexts.Context
import dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.SymDenotations._
import dotty.tools.dotc.core.Names.TypeName
import dotc.core.{ Flags => DottyFlags }
import dotc.ast.Trees._


object factories {
  import dotty.tools.dotc.ast.tpd._
  import DottyFlags._

  type TypeTree = dotty.tools.dotc.ast.Trees.Tree[Type]

  def flags(t: Tree)(implicit ctx: Context): List[String] =
    (t.symbol.flags & SourceModifierFlags)
      .flagStrings.toList
      .filter(_ != "<trait>")
      .filter(_ != "interface")

  def path(sym: Symbol)(implicit ctx: Context): List[String] = sym match {
    case sym if sym.name.decode.toString == "<root>" => Nil
    case sym if sym is Flags.Module => path(sym.owner) :+ sym.name.decode.toString.dropRight(1)
    case sym => path(sym.owner) :+ sym.name.decode.toString
  }


  private val product = """Product[1-9][0-9]*""".r
  private def cleanTitle(title: String): String = title match {
    // matches Entity.this.Something
    case x if x matches "[^\\[]+\\.this\\..+" => x.split("\\.").last
    // Matches Entity[P, ...]
    case x if x matches "[^\\[]+\\[[^\\]]+\\]" =>
      val Array(tpe, params) = x.dropRight(1).split("\\[")
      s"""$tpe[${params.split(",").map(x => cleanTitle(x.trim)).mkString(", ")}]"""
    case _ => title
  }

  private def cleanQuery(query: String): String = query match {
    case x if x matches "[^\\[]+\\[[^\\]]+\\]" => x.takeWhile(_ != '[')
    case _ => query
  }

  def returnType(t: Type)(implicit ctx: Context): Reference = {
    def typeRef(name: String, query: String = "", params: List[MaterializableLink] = Nil) = {
      val realQuery = if (query != "") query else name
      TypeReference(name, UnsetLink(name, realQuery), params)
    }

    def expandTpe(t: Type, params: List[MaterializableLink] = Nil): Reference = t match {
      case ref @ RefinedType(parent, rn, info) => {
        val paramName = (info match {
          case ta: TypeAlias if ta.alias.isInstanceOf[NamedType] =>
            ta.alias.asInstanceOf[NamedType].name.show
          case _ => rn.show
        }).split("\\$").last
        val param = UnsetLink(paramName, paramName)
        expandTpe(parent, param :: params)
      }
      case HKApply(tycon, args) =>
        def paramName: Type => String = { tpe =>
          (tpe match {
            case ta: TypeAlias if ta.alias.isInstanceOf[NamedType] =>
              ta.alias.asInstanceOf[NamedType].name.show
            case _ => tpe.show
          }).split("\\$").last
        }
        expandTpe(tycon, args.map(paramName).map(x => UnsetLink(x,x)))
      case TypeRef(_, n) =>
        val name = n.decode.toString.split("\\$").last
        typeRef(name, params = params)
      case OrType(left, right) =>
        OrTypeReference(expandTpe(left), expandTpe(right))
      case AndType(left, right) =>
        AndTypeReference(expandTpe(left), expandTpe(right))
      case AnnotatedType(tpe, _) =>
        expandTpe(tpe)
      case ExprType(tpe) =>
        expandTpe(tpe)
      case c: ConstantType =>
        ConstantReference(c.show)
      case tt: ThisType =>
        expandTpe(tt.underlying)
      case ci: ClassInfo =>
        typeRef(ci.cls.show)
      case ta: TypeAlias =>
        expandTpe(ta.alias.widenDealias)
      case mt: MethodType =>
        expandTpe(mt.resultType)
      case pt: PolyType =>
        expandTpe(pt.resultType)
      case pp: PolyParam =>
        val paramName = pp.paramName.show
        val name =
          if (paramName.contains('$'))
            paramName.split("\\$\\$").last
          else paramName

        typeRef(name)
    }

    expandTpe(t)
  }

  def typeParams(sym: Symbol)(implicit ctx: Context): List[String] =
    sym.denot.info match {
      case pt: PolyType =>
        pt.paramNames.map(_.show.split("\\$").last)
      case _ => Nil
    }

  def paramLists(tpe: Type)(implicit ctx: Context): List[List[NamedReference]] = tpe match {
    case pt: PolyType =>
      paramLists(pt.resultType)

    case mt: MethodType =>
      mt.paramNames.zip(mt.paramTypes).map { case (name, tpe) =>
        NamedReference(name.decode.toString, returnType(tpe), tpe.isInstanceOf[ExprType])
      } :: paramLists(mt.resultType)

    case annot: AnnotatedType => paramLists(annot.tpe)
    case (_: PolyParam | _: RefinedType | _: TypeRef | _: ThisType |
          _: ExprType  | _: OrType      | _: AndType | _: HKApply) => Nil // return types should not be in the paramlist
  }

  def superTypes(t: Tree)(implicit ctx: Context): List[MaterializableLink] = t.symbol.denot match {
    case cd: ClassDenotation =>
      def isJavaLangObject(prefix: Type): Boolean =
        prefix match {
          case TypeRef(ThisType(TypeRef(NoPrefix, outerName)), innerName) =>
            outerName.toString == "lang" && innerName.toString == "Object"
          case _ => false
        }

      def isProductWithArity(prefix: Type): Boolean = prefix match {
        case TypeRef(TermRef(TermRef(NoPrefix, root), scala), prod) =>
          root.toString == "_root_" &&
          scala.toString == "scala" &&
          product.findFirstIn(prod.toString).isDefined
        case _ => false
      }

      cd.classParents.collect {
        case t: TypeRef if !isJavaLangObject(t) && !isProductWithArity(t) =>
          UnsetLink(t.name.toString, path(t.symbol).mkString("."))
      }
    case _ => Nil
  }
}
