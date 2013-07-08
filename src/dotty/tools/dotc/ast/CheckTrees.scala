package dotty.tools
package dotc
package ast

import core._
import util.Positions._, Types._, Contexts._, Constants._, Names._, Flags._
import SymDenotations._, Symbols._, StdNames._, Annotations._, Trees._

object CheckTrees {

  import tpd.{TreeMapper, localSyms, TreeOps}

  def check(p: Boolean, msg: => String = "")(implicit ctx: Context): Unit = assert(p, msg)

  def checkTypeArg(arg: tpd.Tree, bounds: TypeBounds)(implicit ctx: Context): Unit = {
    check(arg.isValueType)
    check(bounds contains arg.tpe)
  }

  def checkType(tree: tpd.Tree)(implicit ctx: Context): Unit = tree match {
    case Ident(name) =>
    case Select(qualifier, name) =>
      check(qualifier.isValue)
      check(qualifier.tpe =:= tree.tpe.normalizedPrefix)
      val denot = qualifier.tpe.member(name)
      check(denot.exists)
      check(denot.hasAltWith(_.symbol == tree.symbol))
    case This(cls) =>
    case Super(qual, mixin) =>
      check(qual.isValue)
      val cls = qual.tpe.typeSymbol
      check(cls.isClass)
    case Apply(fn, args) =>
      def checkArg(arg: tpd.Tree, name: Name, formal: Type): Unit = {
        arg match {
          case NamedArg(argName, _) =>
            check(argName == name)
          case SeqLiteral(_, _) =>
            check(formal.isRepeatedParam)
          case _ =>
            check(arg.isValue)
        }
        check(arg.tpe <:< formal)
      }
      val MethodType(paramNames, paramTypes) = fn.tpe.widen // checked already at construction
      (args, paramNames, paramTypes).zipped foreach checkArg
    case TypeApply(fn, args) =>
      val pt @ PolyType(_) = fn.tpe.widen // checked already at construction
      (args, pt.instantiateBounds(args map (_.tpe))).zipped foreach checkTypeArg
    case Literal(const: Constant) =>
    case New(tpt) =>
      check(tpt.isValueType)
      val cls = tpt.tpe.typeSymbol
      check(cls.isClass)
      check(!(cls is AbstractOrTrait))
    case Pair(left, right) =>
      check(left.isValue)
      check(right.isValue)
    case Typed(expr, tpt) =>
      check(tpt.isValueType)
      expr.tpe.widen match {
        case tp: MethodType =>
          val cls = tpt.tpe.typeSymbol
          check(cls.isClass)
          check((cls is Trait) ||
                cls.primaryConstructor.info.paramTypess.flatten.isEmpty)
          val absMembers = tpt.tpe.abstractTermMembers
          check(absMembers.size == 1)
          check(tp <:< absMembers.head.info)
        case _ =>
          check(expr.isValueOrPattern)
          check(expr.tpe <:< tpt.tpe)
      }
    case NamedArg(name, arg) =>
    case Assign(lhs, rhs) =>
      check(lhs.isValue); check(rhs.isValue)
      lhs.tpe match {
        case ltpe: TermRef =>
          check(ltpe.symbol is Mutable)
        case _ =>
          check(false)
      }
      check(rhs.tpe <:< lhs.tpe.widen)
    case Block(stats, expr) =>
      var hoisted: Set[Symbol] = Set()
      lazy val locals = localSyms(stats).toSet
      check(expr.isValue)
      def isNonLocal(sym: Symbol): Boolean =
        !(locals contains sym) || isHoistableClass(sym)
      def isHoistableClass(sym: Symbol) =
        sym.isClass && {
          (hoisted contains sym) || {
            hoisted += sym
            noLeaksInClass(sym.asClass)
          }
        }
      def noLeaksIn(tp: Type): Boolean = tp forallParts {
        case tp: NamedType => isNonLocal(tp.symbol)
        case _ => true
      }
      def noLeaksInClass(sym: ClassSymbol): Boolean =
        (sym.info.parents forall noLeaksIn) &&
        (sym.decls.toList forall (t => noLeaksIn(t.info)))
      check(noLeaksIn(tree.tpe))
    case If(cond, thenp, elsep) =>
      check(cond.isValue); check(thenp.isValue); check(elsep.isValue)
      check(cond.tpe.derivesFrom(defn.BooleanClass))
    case Match(selector, cases) =>
      check(selector.isValue)
      // are any checks that relate selector and patterns desirable?
    case CaseDef(pat, guard, body) =>
      check(pat.isValueOrPattern); check(guard.isValue); check(body.isValue)
      check(guard.tpe.derivesFrom(defn.BooleanClass))
    case Return(expr, from) =>
      check(expr.isValue); check(from.isTerm)
      check(from.tpe.termSymbol.isSourceMethod)
    case Try(block, handler, finalizer) =>
      check(block.isTerm)
      check(finalizer.isTerm)
      check(handler.isTerm)
      check(handler.tpe derivesFrom defn.FunctionClass(1))
      check(handler.tpe.baseType(defn.FunctionClass(1)).typeArgs.head <:< defn.ThrowableType)
    case Throw(expr) =>
      check(expr.isValue)
      check(expr.tpe.derivesFrom(defn.ThrowableClass))
    case SeqLiteral(elemtpt, elems) =>
      check(elemtpt.isValueType);
      for (elem <- elems) {
        check(elem.isValue)
        check(elem.tpe <:< elemtpt.tpe)
      }
    case TypeTree(original) =>
      if (!original.isEmpty) {
        check(original.isValueType)
        check(original.tpe == tree.tpe)
      }
    case SingletonTypeTree(ref) =>
      check(ref.isValue)
      check(ref.symbol.isStable)
    case SelectFromTypeTree(qualifier, name) =>
      check(qualifier.isValueType)
      check(qualifier.tpe =:= tree.tpe.normalizedPrefix)
      val denot = qualifier.tpe.member(name)
      check(denot.exists)
      check(denot.symbol == tree.symbol)
    case AndTypeTree(left, right) =>
      check(left.isValueType); check(right.isValueType)
    case OrTypeTree(left, right) =>
      check(left.isValueType); check(right.isValueType)
    case RefinedTypeTree(tpt, refinements) =>
      check(tpt.isValueType)
      def checkRefinements(forbidden: Set[Symbol], rs: List[tpd.Tree]): Unit = rs match {
        case r :: rs1 =>
          val rsym = r.symbol
          check(rsym.isTerm || rsym.isAbstractOrAliasType)
          if (rsym.isAbstractType) check(tpt.tpe.member(rsym.name).exists)
          check(rsym.info forallParts {
            case nt: NamedType => !(forbidden contains nt.symbol)
            case _ => true
          })
          checkRefinements(forbidden - rsym, rs1)
        case nil =>
      }
      checkRefinements(localSyms(refinements).toSet, refinements)
    case AppliedTypeTree(tpt, args) =>
      check(tpt.isValueType)
      val tparams = tpt.tpe.typeParams
      check(sameLength(tparams, args))
      (args, tparams map (_.info.bounds)).zipped foreach checkTypeArg
    case TypeBoundsTree(lo, hi) =>
      check(lo.isValueType); check(hi.isValueType)
      check(lo.tpe <:< hi.tpe)
    case Bind(sym, body) =>
      check(body.isValueOrPattern)
      check(!(tree.symbol is Method))
      body match {
        case Ident(nme.WILDCARD) =>
        case _ => check(body.tpe.widen =:= tree.symbol.info)
      }
    case Alternative(alts) =>
      for (alt <- alts) check(alt.isValueOrPattern)
    case UnApply(fun, args) =>
      check(fun.isTerm)
      for (arg <- args) check(arg.isValueOrPattern)
      val funtpe @ MethodType(_, _) = fun.tpe.widen
      fun.symbol.name match { // check arg arity
        case nme.unapplySeq =>
          // args need to be wrapped in (...: _*)
          check(args.length == 1)
          check(args.head.tpe.isRepeatedParam)
        case nme.unapply =>
          val rtp = funtpe.resultType
          val rsym = rtp.dealias.typeSymbol
          if (rsym == defn.BooleanClass)
            check(args.isEmpty)
          else {
            check(rsym == defn.OptionClass)
            val normArgs = rtp.typeArgs match {
              case optionArg :: Nil =>
                optionArg.typeArgs match {
                  case Nil =>
                    optionArg :: Nil
                  case tupleArgs if defn.TupleClasses contains optionArg.dealias.typeSymbol =>
                    tupleArgs
                }
              case _ =>
                check(false)
                Nil
            }
            check(sameLength(normArgs, args))
          }
      }
    case ValDef(mods, name, tpt, rhs) =>
      check(!(tree.symbol is Method))
      if (!rhs.isEmpty) {
        check(rhs.isValue)
        check(rhs.tpe <:< tpt.tpe)
      }
    case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
      check(tree.symbol is Method)
      if (!rhs.isEmpty) {
        check(rhs.isValue)
        check(rhs.tpe <:< tpt.tpe)
      }
    case TypeDef(mods, name, tpt) =>
      check(tpt.isInstanceOf[Template[_]] || tpt.tpe.isInstanceOf[TypeBounds])
    case Template(constr, parents, selfType, body) =>
    case Import(expr, selectors) =>
      check(expr.isValue)
      check(expr.tpe.termSymbol.isStable)
    case PackageDef(pid, stats) =>
      check(pid.isTerm)
      check(pid.symbol is Package)
    case Annotated(annot, arg) =>
      check(annot.isInstantiation)
      check(annot.symbol.owner.isSubClass(defn.AnnotationClass))
      check(arg.isValueType || arg.isValue)
    case EmptyTree =>
    case SharedTree(shared) =>
      check(shared.isType || shared.isTerm)
  }
}

