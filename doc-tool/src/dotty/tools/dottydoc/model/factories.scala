package dotty.tools.dottydoc
package model

import comment._
import references._
import dotty.tools.dotc
import dotc.core.Types._
import dotc.core.TypeApplications._
import dotc.core.Contexts.Context
import dotc.core.Symbols.{ Symbol, ClassSymbol }
import dotty.tools.dotc.core.SymDenotations._
import dotty.tools.dotc.config.Printers.dottydoc
import dotty.tools.dotc.core.Names.TypeName
import dotc.ast.Trees._


object factories {
  import dotty.tools.dotc.ast.tpd._
  import dotty.tools.dottydoc.model.internal.ParamListImpl
  import dotc.core.Flags._

  type TypeTree = dotty.tools.dotc.ast.Trees.Tree[Type]

  def flags(t: Tree)(implicit ctx: Context): List[String] =
    (t.symbol.flags & SourceModifierFlags)
      .flagStrings.toList
      .filter(_ != "<trait>")
      .filter(_ != "interface")

  def path(sym: Symbol)(implicit ctx: Context): List[String] = sym match {
    case sym if sym.name.decode.toString == "<root>" => Nil
    case sym => path(sym.owner) :+ sym.name.show
  }

  def annotations(sym: Symbol)(implicit ctx: Context): List[String] =
    sym.annotations.map(_.symbol.showFullName)

  private val product = """Product[1-9][0-9]*""".r

  def returnType(t: Type)(implicit ctx: Context): Reference = {
    val defn = ctx.definitions

    def typeRef(name: String, query: String = "", params: List[Reference] = Nil) = {
      val realQuery = if (query != "") query else name
      TypeReference(name, UnsetLink(name, realQuery), params)
    }

    def expandTpe(t: Type, params: List[Reference] = Nil): Reference = t match {
      case tl: PolyType =>
        //FIXME: should be handled correctly
        // example, in `Option`:
        //
        // {{{
        //   def companion: GenericCompanion[collection.Iterable]
        // }}}
        //
        // Becomes: def companion: [+X0] -> collection.Iterable[X0]
        typeRef(tl.show + " (not handled)")
      case AppliedType(tycon, args) =>
        val cls = tycon.typeSymbol
        if (tycon.isRepeatedParam)
          expandTpe(args.head)
        else if (defn.isFunctionClass(cls))
          FunctionReference(args.init.map(expandTpe(_, Nil)), expandTpe(args.last))
        else if (defn.isTupleClass(cls))
          TupleReference(args.map(expandTpe(_, Nil)))
        else {
          val query = tycon.show
          val name  = query.split("\\.").last
          typeRef(name, query, params = args.map(expandTpe(_, Nil)))
        }

      case ref @ RefinedType(parent, rn, info) =>
        expandTpe(parent) //FIXME: will be a refined HK, aka class Foo[X] { def bar: List[X] } or similar
      case ref @ HKApply(tycon, args) =>
        expandTpe(tycon, args.map(expandTpe(_, params)))
      case TypeRef(_, n) =>
        val name = n.decode.toString.split("\\$").last
        typeRef(name, params = params)
      case ta: TypeAlias =>
        expandTpe(ta.alias.widenDealias)
      case OrType(left, right) =>
        OrTypeReference(expandTpe(left), expandTpe(right))
      case AndType(left, right) =>
        AndTypeReference(expandTpe(left), expandTpe(right))
      case tb @ TypeBounds(lo, hi) =>
        BoundsReference(expandTpe(lo), expandTpe(hi))
      case AnnotatedType(tpe, _) =>
        expandTpe(tpe)
      case ExprType(tpe) =>
        expandTpe(tpe)
      case c: ConstantType =>
        ConstantReference(c.show)
      case tt: ThisType =>
        expandTpe(tt.underlying)
      case ci: ClassInfo =>
        val query = path(ci.typeSymbol).mkString(".")
        typeRef(ci.cls.name.show, query = query)
      case mt: MethodType =>
        expandTpe(mt.resultType)
      case pp: PolyParam =>
        val paramName = pp.paramName.show
        val name =
          if (paramName.contains('$'))
            paramName.split("\\$\\$").last
          else paramName

        typeRef(name)
      case tr: TermRef =>
        /** A `TermRef` appears in the return type in e.g:
          * ```
          * def id[T](t: T): t.type = t
          * ```
          */
        val name = tr.show
        if (!name.endsWith(".type"))
          ctx.warning(s"unhandled return type found: $tr")

        typeRef(name, params = params)
    }

    expandTpe(t)
  }

  def typeParams(sym: Symbol)(implicit ctx: Context): List[String] =
    sym.info match {
      case pt: PolyType => // TODO: not sure if this case is needed anymore
        pt.paramNames.map(_.show.split("\\$").last)
      case ClassInfo(_, _, _, decls, _) =>
        decls.iterator
          .filter(_.flags is TypeParam)
          .map { tp =>
            val prefix =
              if (tp.flags is Covariant) "+"
              else if (tp.flags is Contravariant) "-"
              else ""
            prefix + tp.name.show.split("\\$").last
          }
          .toList
      case _ =>
        Nil
    }

  def constructors(sym: Symbol)(implicit ctx: Context): List[List[ParamList]] = sym match {
    case sym: ClassSymbol =>
      paramLists(sym.primaryConstructor.info) :: Nil
    case _ => Nil
  }

  def traitParameters(sym: Symbol)(implicit ctx: Context): List[ParamList] =
    constructors(sym).head

  def paramLists(tpe: Type)(implicit ctx: Context): List[ParamList] = tpe match {
    case pt: PolyType =>
      paramLists(pt.resultType)

    case mt: MethodType =>
      ParamListImpl(mt.paramNames.zip(mt.paramTypes).map { case (name, tpe) =>
        NamedReference(
          name.decode.toString,
          returnType(tpe),
          isByName = tpe.isInstanceOf[ExprType],
          isRepeated = tpe.isRepeatedParam
        )
      }, mt.isImplicit) :: paramLists(mt.resultType)

    case mp: MethodParam =>
      paramLists(mp.underlying)

    case annot: AnnotatedType =>
      paramLists(annot.tpe)

    case (_: PolyParam | _: RefinedType | _: TypeRef | _: ThisType |
          _: ExprType  | _: OrType      | _: AndType | _: HKApply  | _: TermRef) =>
      Nil // return types should not be in the paramlist
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
