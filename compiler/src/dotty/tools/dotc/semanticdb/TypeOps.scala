package dotty.tools
package dotc
package semanticdb

import core.Symbols._
import core.Contexts.Context
import core.Types._
import core.Annotations.Annotation
import core.Flags
import core.Names.Name
import core.StdNames.tpnme
import ast.tpd._

import collection.mutable

import dotty.tools.dotc.{semanticdb => s}

class TypeOps:
  import SymbolScopeOps._
  import Scala3.given
  private val paramRefSymtab = mutable.Map[(LambdaType, Name), Symbol]()
  private val refinementSymtab = mutable.Map[(RefinedType, Name), Symbol]()
  given typeOps: TypeOps = this
  extension (tpe: Type)
    def toSemanticSig(using LinkMode, Context, SemanticSymbolBuilder)(sym: Symbol): s.Signature =
      def enterParamRef(tpe: Type): Unit =
        tpe match {
          case lam: LambdaType =>
            // Find the "actual" binder type for nested LambdaType
            // For example, `def foo(x: T)(y: T): T` and for `<y>.owner.info` would be like
            // `MethodType(...<x>, resType = MethodType(...<y>, resType = <T>))`.
            // (Let's say the outer `MethodType` "outer", and `MethodType` who is
            // `resType` of outer "inner")
            //
            // We try to find the "actual" binder of <y>: `inner`,
            // and register them to the symbol table with `(<y>, inner) -> <y>`
            // instead of `("y", outer) -> <y>`
            if lam.paramNames.contains(sym.name) then
              paramRefSymtab((lam, sym.name)) = sym
            else
              enterParamRef(lam.resType)

          // for class constructor
          // class C[T] { ... }
          case cls: ClassInfo if sym.info.isInstanceOf[LambdaType] =>
            val lam = sym.info.asInstanceOf[LambdaType]
            cls.cls.typeParams.foreach { param =>
              paramRefSymtab((lam, param.name)) = param
            }

          // type X[T] = ...
          case tb: TypeBounds =>
            enterParamRef(tb.lo)
            enterParamRef(tb.hi)

          case _ => ()
        }

      def enterRefined(tpe: Type): Unit =
        tpe match {
          case refined: RefinedType =>
            val key = (refined, sym.name)
            refinementSymtab(key) = sym

          case rec: RecType =>
            enterRefined(rec.parent)

          // Register symbol for opaque type,
          // opaque type alias will be stored into the refinement of
          // the self type of the enclosing class.
          // Key: the tuple of
          //   - self-type of enclosing class
          //   - name of the opaque type
          // Value: the symbol of the opaque type
          // See: SymDenotation.opaqueToBounds
          case cls: ClassInfo if sym.is(Flags.Opaque) =>
            cls.classSymbol.asClass.givenSelfType match
              case rt: RefinedType =>
                refinementSymtab((rt, sym.name)) = sym
              case _ => ()

          case cls: ClassInfo if (cls.cls.name == tpnme.REFINE_CLASS) =>
            enterRefined(sym.owner.owner.info)

          // type x = Person { refinement }
          case tb: TypeBounds =>
            // tb = TypeBounds(
            //   lo = RefinedType(...)
            //   hi = RefinedType(...)
            // )
            enterRefined(tb.lo)
            enterRefined(tb.hi)

          // def s(x: Int): { refinement } = ...
          case expr: ExprType =>
            enterRefined(expr.resType)
          case m: LambdaType =>
            enterRefined(m.resType)
          case _ => ()
        }
      enterParamRef(sym.owner.info)
      enterRefined(sym.owner.info)

      def loop(tpe: Type): s.Signature = tpe match {
        case mp: MethodOrPoly =>
          def flatten(
            t: Type,
            paramss: List[List[Symbol]],
            tparams: List[Symbol]
          ): (Type, List[List[Symbol]], List[Symbol]) = t match {
            case mt: MethodType =>
              val syms = mt.paramNames.flatMap { paramName =>
                val key = (mt, paramName)
                paramRefSymtab.get(key)
              }
              flatten(mt.resType, paramss :+ syms, tparams)
            case pt: PolyType =>
              val syms = pt.paramNames.flatMap { paramName =>
                val key = (pt, paramName)
                paramRefSymtab.get(key)
              }
              // there shouldn't multiple type params
              flatten(pt.resType, paramss, syms)
            case other =>
              (other, paramss, tparams)
          }
          val (resType, paramss, tparams) = flatten(mp, Nil, Nil)

          val sparamss = paramss.map(_.sscope)
          val stparams = tparams.sscopeOpt
          s.MethodSignature(
            stparams,
            sparamss,
            resType.toSemanticType
          )

        case cls: ClassInfo =>
          val stparams = cls.cls.typeParams.sscopeOpt
          val sparents = cls.parents.map(_.toSemanticType)
          val sself = cls.selfType.toSemanticType
          val decls = cls.decls.toList.sscopeOpt
          s.ClassSignature(stparams, sparents, sself, decls)

        case TypeBounds(lo, hi) =>
          // for `type X[T] = T` is equivalent to `[T] =>> T`
          def tparams(tpe: Type): (Type, List[Symbol]) = tpe match {
            case lambda: HKTypeLambda =>
              val paramSyms = lambda.paramNames.flatMap { paramName =>
                paramRefSymtab.get((lambda, paramName))
              }
              (lambda.resType, paramSyms)
            case _ => (tpe, Nil)
          }
          val (loRes, loParams) = tparams(lo)
          val (hiRes, hiParams) = tparams(hi)
          val params = (loParams ++ hiParams).distinctBy(_.name)
          val slo = loRes.toSemanticType
          val shi = hiRes.toSemanticType
          val stparams = params.sscopeOpt
          s.TypeSignature(stparams, slo, shi)

        case other =>
          s.ValueSignature(
            other.toSemanticType
          )
      }
      loop(tpe)

    private def toSemanticType(using LinkMode, SemanticSymbolBuilder, Context): s.Type =
      import ConstantOps._
      def loop(tpe: Type): s.Type = tpe match {
        case ExprType(tpe) =>
          val stpe = loop(tpe)
          s.ByNameType(stpe)

        case TypeRef(pre, sym: Symbol) =>
          val spre = if tpe.hasTrivialPrefix then s.Type.Empty else loop(pre)
          val ssym = sym.symbolName
          s.TypeRef(spre, ssym, Seq.empty)

        case TermRef(pre, sym: Symbol) =>
          val spre = if(tpe.hasTrivialPrefix) s.Type.Empty else loop(pre)
          val ssym = sym.symbolName
          s.SingleType(spre, ssym)

        case tref: ParamRef =>
          val key = (tref.binder, tref.paramName)
          paramRefSymtab.get(key) match {
            case Some(ref) =>
              val ssym = ref.symbolName
              tref match {
                case _: TypeParamRef => s.TypeRef(s.Type.Empty, ssym, Seq.empty)
                case _: TermParamRef => s.SingleType(s.Type.Empty, ssym)
              }
            case None => // shouldn't happen
              s.Type.Empty
          }

        case ThisType(TypeRef(_, sym: Symbol)) =>
          s.ThisType(sym.symbolName)

        case SuperType(thistpe, supertpe) =>
          val spre = loop(thistpe.typeSymbol.info)
          val ssym = supertpe.typeSymbol.symbolName
          s.SuperType(spre, ssym)

        // val clazzOf = classOf[...]
        case ConstantType(const) if const.tag == core.Constants.ClazzTag =>
          loop(const.typeValue)

        case ConstantType(const) =>
          s.ConstantType(const.toSemanticConst)

        case rt @ RefinedType(parent, name, info) =>
          // `X { def x: Int; def y: Int }`
          // RefinedType(
          //   parent = RefinedType(
          //     parent = TypeRef(..., X)
          //     ...
          //   )
          //   refinedName = x
          //   refinedInfo = TypeRef(..., Int)
          // )
          type RefinedInfo = (core.Names.Name, Type)
          def flatten(tpe: Type, acc: List[RefinedInfo]): (Type, List[RefinedInfo]) = tpe match {
            case RefinedType(parent, name, info) =>
              flatten(parent, acc :+ (name, info))
            case _ =>
              (tpe, acc)
          }

          // flatten parent types to list
          // e.g. `X with Y with Z { refined }`
          // RefinedType(parent = AndType(X, AndType(Y, Z)), ...)
          // => List(X, Y, Z)
          def flattenParent(parent: Type): List[s.Type] = parent match {
            case AndType(tp1, tp2) =>
              flattenParent(tp1) ++ flattenParent(tp2)
            case _ => List(loop(parent))
          }

          val (parent, refinedInfos) = flatten(rt, List.empty)
          val stpe = s.IntersectionType(flattenParent(parent))

          val decls = refinedInfos.flatMap { (name, _) =>
            refinementSymtab.get((rt, name))
          }
          val sdecls = decls.sscopeOpt(using LinkMode.HardlinkChildren)
          s.StructuralType(stpe, sdecls)

        case rec: RecType =>
          loop(rec.parent) // should be handled as RefinedType

        // repeated params: e.g. `Int*`, which is the syntax sugar of
        // `Seq[Int] @Repeated` (or `Array[Int] @Repeated`)
        // See: Desugar.scala and TypeApplications.scala
        case AnnotatedType(AppliedType(_, targs), annot)
          if (annot matches defn.RepeatedAnnot) && (targs.length == 1) =>
          val stpe = loop(targs(0))
          s.RepeatedType(stpe)

        case ann: AnnotatedType if ann.annot.symbol.info.isInstanceOf[ClassInfo] =>
          def flatten(tpe: Type, annots: List[Annotation]): (Type, List[Annotation]) = tpe match
            case AnnotatedType(parent, annot) if annot.symbol.info.isInstanceOf[ClassInfo] =>
              flatten(parent, annot +: annots)
            case other => (other, annots)

          val (parent, annots) = flatten(ann, List.empty)
          val sparent = loop(parent)
          val sannots = annots.map(a =>
            s.Annotation(loop(a.symbol.info.asInstanceOf[ClassInfo].selfType))
          )
          s.AnnotatedType(sannots, sparent)

        case app @ AppliedType(tycon, args) =>
          val targs = args.map { arg =>
            arg match
              // For wildcard type C[_ <: T], it's internal type representation will be
              // `AppliedType(TypeBounds(lo = <Nothing>, hi = <T>))`.
              //
              // As scalameta for Scala2 does, we'll convert the wildcard type to
              // `ExistentialType(TypeRef(NoPrefix, C, <local0>), Scope(hardlinks = List(<local0>)))`
              // where `<local0>` has
              // display_name: "_" and,
              // signature: type_signature(..., lo = <Nothing>, hi = <T>)
              case bounds: TypeBounds =>
                val wildcardSym = newSymbol(NoSymbol, tpnme.WILDCARD, Flags.EmptyFlags, bounds)
                val ssym = wildcardSym.symbolName
                (Some(wildcardSym), s.TypeRef(s.Type.Empty, ssym, Seq.empty))
              case other =>
                val sarg = loop(other)
                (None, sarg)
          }
          val wildcardSyms = targs.flatMap(_._1)
          val sargs = targs.map(_._2)

          val applied = loop(tycon) match
            case ref: s.TypeRef => ref.copy(typeArguments = sargs)
            case _ => s.Type.Empty

          if (wildcardSyms.isEmpty) applied
          else s.ExistentialType(
            applied,
            wildcardSyms.sscopeOpt(using LinkMode.HardlinkChildren)
          )

        case and: AndType =>
          def flatten(child: Type): List[Type] = child match
            case AndType(ct1, ct2) => flatten(ct1) ++ flatten(ct2)
            case other => List(other)
          val stpes = flatten(and).map(loop)
          s.IntersectionType(stpes)

        case or: OrType =>
          def flatten(child: Type): List[Type] = child match
            case OrType(ct1, ct2) => flatten(ct1) ++ flatten(ct2)
            case other => List(other)
          val stpes = flatten(or).map(loop)
          s.UnionType(stpes)

        case NoPrefix =>
          s.Type.Empty

        case _ => s.Type.Empty
      }
      loop(tpe)

    /** Return true if the prefix is like `_root_.this` */
    private def hasTrivialPrefix(using Context): Boolean =
      def checkTrivialPrefix(pre: Type, sym: Symbol)(using Context): Boolean =
        pre =:= sym.owner.thisType
      tpe match {
        case TypeRef(pre, sym: Symbol) =>
          checkTrivialPrefix(pre, sym)
        case TermRef(pre, sym: Symbol) =>
          checkTrivialPrefix(pre, sym)
        case _ => false
      }

object SymbolScopeOps:
  import Scala3.given
  extension (syms: List[Symbol])
    def sscope(using linkMode: LinkMode)(using SemanticSymbolBuilder, TypeOps, Context): s.Scope =
      linkMode match {
        case LinkMode.SymlinkChildren =>
          s.Scope(symlinks = syms.map(_.symbolName))
        case LinkMode.HardlinkChildren =>
          s.Scope(hardlinks = syms.map(_.symbolInfo(Set.empty)))
      }

    def sscopeOpt(using linkMode: LinkMode)(using SemanticSymbolBuilder, TypeOps, Context): Option[s.Scope] =
      if syms.nonEmpty then Some(syms.sscope) else None
