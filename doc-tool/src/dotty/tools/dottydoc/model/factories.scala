package dotty.tools.dottydoc
package model

import references._
import dotty.tools.dotc
import dotc.core.Types
import Types._
import dotc.core.Contexts.Context
import dotc.core.Symbols.{ Symbol, ClassSymbol }
import dotty.tools.dotc.core.SymDenotations._

import scala.annotation.tailrec


object factories {
  import dotty.tools.dotc.ast.tpd._
  import dotty.tools.dottydoc.model.internal.ParamListImpl
  import dotc.core.Flags._

  type TypeTree = dotty.tools.dotc.ast.Trees.Tree[Type]

  def flags(t: Tree)(implicit ctx: Context): List[String] =
    (t.symbol.flags & (if (t.symbol.isType) TypeSourceModifierFlags else TermSourceModifierFlags))
      .flagStrings.toList
      .filter(_ != "<trait>")
      .filter(_ != "interface")
      .filter(_ != "case")

  def path(sym: Symbol)(implicit ctx: Context): List[String] = {
    @tailrec def go(sym: Symbol, acc: List[String]): List[String] =
      if (sym.isRoot)
        acc
      else
        go(sym.owner, sym.name.mangledString :: acc)
    go(sym, Nil)
  }

  def annotations(sym: Symbol)(implicit ctx: Context): List[String] =
    sym.annotations.collect {
      case ann if ann.symbol != ctx.definitions.SourceFileAnnot => ann.symbol.showFullName
    }

  private val product = """Product[1-9][0-9]*""".r

  def alias(t: Type)(implicit ctx: Context): Option[Reference] = {
    val defn = ctx.definitions
    t match {
      case TypeBounds(low, high) if (low eq defn.NothingType) && (high eq defn.AnyType) =>
        None
      case t => Some(returnType(t))
    }
  }

  def returnType(t: Type)(implicit ctx: Context): Reference = {
    val defn = ctx.definitions

    def typeRef(name: String, query: String = "", params: List[Reference] = Nil) = {
      val realQuery = if (query != "") query else name
      TypeReference(name, UnsetLink(name, realQuery), params)
    }

    def expandTpe(t: Type, params: List[Reference] = Nil): Reference = t match {
      case AppliedType(tycon, args) => {
        val cls = tycon.typeSymbol

        if (defn.isFunctionClass(cls))
          FunctionReference(args.init.map(expandTpe(_, Nil)), expandTpe(args.last), defn.isImplicitFunctionClass(cls))
        else if (defn.isTupleClass(cls))
          TupleReference(args.map(expandTpe(_, Nil)))
        else {
          val query = cls.showFullName
          val name = cls.name.show
          typeRef(name, query, params = args.map(expandTpe(_, Nil)))
        }
      }

      case t: TypeRef => {
        val cls = t.typeSymbol
        typeRef(cls.name.show.split("\\$\\$").last, query = cls.showFullName, params = params)
      }

      case TypeBounds(lo, hi) =>
        BoundsReference(expandTpe(lo), expandTpe(hi))

      case t: TypeParamRef =>
        typeRef(t.paramName.show, params = params)

      case ExprType(tpe) =>
        expandTpe(tpe)

      case t: ThisType =>
        expandTpe(t.underlying)

      case AnnotatedType(t, _) =>
        expandTpe(t)

      case t: MethodType =>
        expandTpe(t.finalResultType)

      case t: TermRef => {
        /** A `TermRef` appears in the return type in e.g:
          * ```
          * def id[T](t: T): t.type = t
          * ```
          */
        typeRef(t.name.show + ".type", params = params)
      }

      case ci: ClassInfo =>
        typeRef(ci.cls.name.show, query = ci.typeSymbol.showFullName)

      case tl: TypeLambda =>
        expandTpe(tl.resType)

      case OrType(left, right) =>
        OrTypeReference(expandTpe(left), expandTpe(right))

      case AndType(left, right) =>
        AndTypeReference(expandTpe(left), expandTpe(right))

      case c: ConstantType =>
        ConstantReference(c.value.show)

      case ref @ RefinedType(parent, rn, info) =>
        expandTpe(parent) //FIXME: will be a refined HK, aka class Foo[X] { def bar: List[X] } or similar

      case tp: LazyRef =>
        expandTpe(tp.ref)

      case MatchType(bound, scrutinee, cases) =>
        // See #6295
        ConstantReference("FIXME: MatchType support")
    }

    expandTpe(t)
  }

  def typeParams(sym: Symbol)(implicit ctx: Context): List[String] =
    sym.info match {
      case pt: TypeLambda => // TODO: not sure if this case is needed anymore
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
      case tp: Types.TypeAlias =>
        typeParams(tp.alias.typeSymbol)
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
    case pt: TypeLambda =>
      paramLists(pt.resultType)

    case mt: MethodType =>
      ParamListImpl(mt.paramNames.zip(mt.paramInfos).map { case (name, tpe) =>
        NamedReference(
          name.decode.toString,
          returnType(tpe),
          isByName = tpe.isInstanceOf[ExprType],
          isRepeated = tpe.isRepeatedParam
        )
      }, mt.isImplicitMethod) :: paramLists(mt.resultType)

    case mp: TermParamRef =>
      paramLists(mp.underlying)

    case annot: AnnotatedType =>
      paramLists(annot.parent)

    case _ =>
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

      cd.classParents.map(_.typeConstructor).collect {
        case t: TypeRef if !isJavaLangObject(t) && !isProductWithArity(t) =>
          UnsetLink(t.name.toString, path(t.symbol).mkString("."))
      }
    case _ => Nil
  }
}
