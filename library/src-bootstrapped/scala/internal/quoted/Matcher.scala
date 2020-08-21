package scala.internal.quoted

import scala.annotation.internal.sharable
import scala.annotation.{Annotation, compileTimeOnly}

import scala.quoted._
import scala.internal.tasty.CompilerInterface.quoteContextWithCompilerInterface

/** Matches a quoted tree against a quoted pattern tree.
 *  A quoted pattern tree may have type and term holes in addition to normal terms.
 *
 *
 *  Semantics:
 *
 *  We use `'{..}` for expression, `'[..]` for types and `⟨..⟩` for patterns nested in expressions.
 *  The semantics are defined as a list of reduction rules that are tried one by one until one matches.
 *
 *   Operations:
 *   - `s =?= p` checks if a scrutinee `s` matches the pattern `p` while accumulating extracted parts of the code.
 *   - `isColosedUnder(x1, .., xn)('{e})` returns true if and only if all the references in `e` to names defined in the patttern are contained in the set `{x1, ... xn}`.
 *   - `lift(x1, .., xn)('{e})` returns `(y1, ..., yn) => [xi = $yi]'{e}` where `yi` is an `Expr` of the type of `xi`.
 *   - `withEnv(x1 -> y1, ..., xn -> yn)(matching)` evaluates mathing recording that `xi` is equivalent to `yi`.
 *   - `matched` denotes that the the match succedded and `matched('{e})` denotes that a matech succeded and extracts `'{e}`
 *   - `&&&` matches if both sides match. Concatenates the extracted expressions of both sides.
 *
 *   Note: that not all quoted terms bellow are valid expressions
 *
 *   ```scala
 *   /* Term hole */
 *   '{ e } =?= '{ hole[T] }  &&  typeOf('{e}) <:< T && isColosedUnder()('{e})  ===>   matched('{e})
 *
 *   /* Higher order term hole */
 *   '{ e } =?= '{ hole[(T1, ..., Tn) => T](x1, ..., xn) }  &&  isColosedUnder(x1, ... xn)('{e})  ===>   matched(lift(x1, ..., xn)('{e}))
 *
 *   /* Match literal */
 *   '{ lit } =?= '{ lit }   ===>   matched
 *
 *   /* Match type ascription (a) */
 *   '{ e: T } =?= '{ p }   ===>   '{e} =?= '{p}
 *
 *   /* Match type ascription (b) */
 *   '{ e } =?= '{ p: P }   ===>   '{e} =?= '{p}
 *
 *   /* Match selection */
 *   '{ e.x } =?= '{ p.x }   ===>   '{e} =?= '{p}
 *
 *   /* Match reference */
 *   '{ x } =?= '{ x }   ===>   matched
 *
 *   /* Match application */
 *   '{e0(e1, ..., en)} =?= '{p0(p1, ..., p2)}   ===>   '{e0} =?= '{p0} &&& '{e1} =?= '{p1} &&& ... %% '{en} =?= '{pn}
 *
 *   /* Match type application */
 *   '{e[T1, ..., Tn]} =?= '{p[P1, ..., Pn]}   ===>   '{e} =?= '{p} &&& '[T1] =?= '{P1} &&& ... %% '[Tn] =?= '[Pn]
 *
 *   /* Match block flattening */
 *   '{ {e0; e1; ...; en}; em } =?= '{ {p0; p1; ...; pm}; em }   ===>   '{ e0; {e1; ...; en; em} } =?= '{ p0; {p1; ...; pm; em} }
 *
 *   /* Match block */
 *   '{ e1; e2 } =?= '{ p1; p2 }   ===>   '{e1} =?= '{p1} &&& '{e2} =?= '{p2}
 *
 *   /* Match def block */
 *   '{ e1; e2 } =?= '{ p1; p2 }   ===>   withEnv(symOf(e1) -> symOf(p1))('{e1} =?= '{p1} &&& '{e2} =?= '{p2})
 *
 *   /* Match if */
 *   '{ if e0 then e1 else e2 } =?= '{ if p0 then p1 else p2 }   ===>  '{e0} =?= '{p0} &&& '{e1} =?= '{p1} &&& '{e2} =?= '{p2}
 *
 *   /* Match while */
 *   '{ while e0 do e1 } =?= '{ while p0 do p1 }   ===>  '{e0} =?= '{p0} &&& '{e1} =?= '{p1}
 *
 *   /* Match assign */
 *   '{ e0 = e1 } =?= '{ p0 = p1 }   ==>   '{e0} =?= '{p0} &&& '{e1} =?= '{p1}
 *
 *   /* Match new */
 *   '{ new T } =?= '{ new T }   ===>   matched
 *
 *   /* Match this */
 *   '{ C.this } =?= '{ C.this }   ===>   matched
 *
 *   /* Match super */
 *   '{ e.super } =?= '{ p.super }   ===>   '{e} =?= '{p}
 *
 *   /* Match varargs */
 *   '{ e: _* } =?= '{ p: _* }   ===>   '{e} =?= '{p}
 *
 *   /* Match val */
 *   '{ val x: T = e1; e2 } =?= '{ val y: P = p1; p2 }   ===>   withEnv(x -> y)('[T] =?= '[P] &&& '{e1} =?= '{p1} &&& '{e2} =?= '{p2})
 *
 *   /* Match def */
 *   '{ def x0(x1: T1, ..., xn: Tn): T0 = e1; e2 } =?= '{ def y0(y1: P1, ..., yn: Pn): P0 = p1; p2 }   ===>   withEnv(x0 -> y0, ..., xn -> yn)('[T0] =?= '[P0] &&& ... &&& '[Tn] =?= '[Pn] &&& '{e1} =?= '{p1} &&& '{e2} =?= '{p2})
 *
 *   // Types
 *
 *   /* Match type */
 *   '[T] =?= '[P] && T <:< P   ===>   matched
 *
 *   ```
 */
object Matcher {

  /** A splice in a quoted pattern is desugared by the compiler into a call to this method */
  @compileTimeOnly("Illegal reference to `scala.internal.quoted.CompileTime.patternHole`")
  def patternHole[T]: T = ???

  @compileTimeOnly("Illegal reference to `scala.internal.quoted.CompileTime.patternHigherOrderHole`")
  /** A higher order splice in a quoted pattern is desugared by the compiler into a call to this method */
  def patternHigherOrderHole[U](pat: Any, args: Any*): U = ???

  @compileTimeOnly("Illegal reference to `scala.internal.quoted.CompileTime.higherOrderHole`")
  /** A higher order splice in a quoted pattern is desugared by the compiler into a call to this method */
  def higherOrderHole[U](args: Any*): U = ???

  // TODO remove
  /** A splice of a name in a quoted pattern is desugared by wrapping getting this annotation */
  @compileTimeOnly("Illegal reference to `scala.internal.quoted.CompileTime.patternBindHole`")
  class patternBindHole extends Annotation

  /** A splice of a name in a quoted pattern is that marks the definition of a type splice */
  @compileTimeOnly("Illegal reference to `scala.internal.quoted.CompileTime.patternType`")
  class patternType extends Annotation

  /** A type pattern that must be aproximated from above */
  @compileTimeOnly("Illegal reference to `scala.internal.quoted.CompileTime.fromAbove`")
  class fromAbove extends Annotation

  class QuoteMatcher[QCtx <: QuoteContext & Singleton](val qctx0: QCtx) {
    val qctx = quoteContextWithCompilerInterface(qctx0)

    // TODO improve performance

    // TODO use flag from qctx.tasty.rootContext. Maybe -debug or add -debug-macros
    private final val debug = false

    import qctx.tasty._
    import Matching._

    /** A map relating equivalent symbols from the scrutinee and the pattern
     *  For example in
     *  ```
     *  '{val a = 4; a * a} match case '{ val x = 4; x * x }
     *  ```
     *  when matching `a * a` with `x * x` the environment will contain `Map(a -> x)`.
     */
    private type Env = Map[Symbol, Symbol]

    inline private def withEnv[T](env: Env)(inline body: Env ?=> T): T = body(using env)

    class SymBinding(val sym: Symbol, val fromAbove: Boolean)

    def termMatch(scrutineeTerm: Term, patternTerm: Term, hasTypeSplices: Boolean): Option[Tuple] = {
      given Env = Map.empty
      if (hasTypeSplices) {
        val ctx: Context = qctx.tasty.Constraints_init(rootContext)
        given Context = ctx
        val matchings = scrutineeTerm =?= patternTerm
        // After matching and doing all subtype checks, we have to approximate all the type bindings
        // that we have found and seal them in a quoted.Type
        matchings.asOptionOfTuple.map { tup =>
          Tuple.fromArray(tup.toArray.map { // TODO improve performance
            case x: SymBinding => qctx.tasty.Constraints_approximation(summon[Context])(x.sym, !x.fromAbove).seal
            case x => x
          })
        }
      }
      else {
        scrutineeTerm =?= patternTerm
      }
    }

    // TODO factor out common logic with `termMatch`
    def typeTreeMatch(scrutineeTypeTree: TypeTree, patternTypeTree: TypeTree, hasTypeSplices: Boolean): Option[Tuple] = {
      given Env = Map.empty
      if (hasTypeSplices) {
        val ctx: Context = qctx.tasty.Constraints_init(rootContext)
        given Context = ctx
        val matchings = scrutineeTypeTree =?= patternTypeTree
        // After matching and doing all subtype checks, we have to approximate all the type bindings
        // that we have found and seal them in a quoted.Type
        matchings.asOptionOfTuple.map { tup =>
          Tuple.fromArray(tup.toArray.map { // TODO improve performance
            case x: SymBinding => qctx.tasty.Constraints_approximation(summon[Context])(x.sym, !x.fromAbove).seal
            case x => x
          })
        }
      }
      else {
        scrutineeTypeTree =?= patternTypeTree
      }
    }

    private def hasPatternTypeAnnotation(sym: Symbol) = sym.annots.exists(isPatternTypeAnnotation)

    private def hasFromAboveAnnotation(sym: Symbol) = sym.annots.exists(isFromAboveAnnotation)

    private def isPatternTypeAnnotation(tree: Tree): Boolean = tree match {
      case New(tpt) => tpt.symbol == qctx.tasty.Definitions_InternalQuotedMatcher_patternTypeAnnot
      case annot => annot.symbol.owner == qctx.tasty.Definitions_InternalQuotedMatcher_patternTypeAnnot
    }

    private def isFromAboveAnnotation(tree: Tree): Boolean = tree match {
      case New(tpt) => tpt.symbol == qctx.tasty.Definitions_InternalQuotedMatcher_fromAboveAnnot
      case annot => annot.symbol.owner == qctx.tasty.Definitions_InternalQuotedMatcher_fromAboveAnnot
    }

    /** Check that all trees match with `mtch` and concatenate the results with &&& */
    private def matchLists[T](l1: List[T], l2: List[T])(mtch: (T, T) => Matching): Matching = (l1, l2) match {
      case (x :: xs, y :: ys) => mtch(x, y) &&& matchLists(xs, ys)(mtch)
      case (Nil, Nil) => matched
      case _ => notMatched
    }

    extension (scrutinees: List[Tree]):
      /** Check that all trees match with =?= and concatenate the results with &&& */
      private def =?= (patterns: List[Tree])(using Context, Env): Matching =
        matchLists(scrutinees, patterns)(_ =?= _)

    extension (scrutinee0: Tree):
      /** Check that the trees match and return the contents from the pattern holes.
       *  Return None if the trees do not match otherwise return Some of a tuple containing all the contents in the holes.
       *
       *  @param scrutinee The tree beeing matched
       *  @param pattern The pattern tree that the scrutinee should match. Contains `patternHole` holes.
       *  @param `summon[Env]` Set of tuples containing pairs of symbols (s, p) where s defines a symbol in `scrutinee` which corresponds to symbol p in `pattern`.
       *  @return `None` if it did not match or `Some(tup: Tuple)` if it matched where `tup` contains the contents of the holes.
       */
      private def =?= (pattern0: Tree)(using Context, Env): Matching = {

        /* Match block flattening */ // TODO move to cases
        /** Normalize the tree */
        def normalize(tree: Tree): Tree = tree match {
          case Block(Nil, expr) => normalize(expr)
          case Block(stats1, Block(stats2, expr)) =>
            expr match
              case _: Closure => tree
              case _ => normalize(Block(stats1 ::: stats2, expr))
          case Inlined(_, Nil, expr) => normalize(expr)
          case _ => tree
        }

        val scrutinee = normalize(scrutinee0)
        val pattern = normalize(pattern0)

        /** Check that both are `val` or both are `lazy val` or both are `var` **/
        def checkValFlags(): Boolean = {
          import Flags._
          val sFlags = scrutinee.symbol.flags
          val pFlags = pattern.symbol.flags
          sFlags.is(Lazy) == pFlags.is(Lazy) && sFlags.is(Mutable) == pFlags.is(Mutable)
        }

        (scrutinee, pattern) match {

          /* Term hole */
          // Match a scala.internal.Quoted.patternHole typed as a repeated argument and return the scrutinee tree
          case (scrutinee @ Typed(s, tpt1), Typed(TypeApply(patternHole, tpt :: Nil), tpt2))
              if patternHole.symbol == qctx.tasty.Definitions_InternalQuotedMatcher_patternHole &&
                 s.tpe <:< tpt.tpe &&
                 tpt2.tpe.derivesFrom(defn.RepeatedParamClass) =>
            matched(scrutinee.seal)

          /* Term hole */
          // Match a scala.internal.Quoted.patternHole and return the scrutinee tree
          case (ClosedPatternTerm(scrutinee), TypeApply(patternHole, tpt :: Nil))
              if patternHole.symbol == qctx.tasty.Definitions_InternalQuotedMatcher_patternHole &&
                 scrutinee.tpe <:< tpt.tpe =>
            matched(scrutinee.seal)

          /* Higher order term hole */
          // Matches an open term and wraps it into a lambda that provides the free variables
          case (scrutinee, pattern @ Apply(TypeApply(Ident("higherOrderHole"), List(Inferred())), Repeated(args, _) :: Nil))
              if pattern.symbol == qctx.tasty.Definitions_InternalQuotedMatcher_higherOrderHole =>

            def bodyFn(lambdaArgs: List[Tree]): Tree = {
              val argsMap = args.map(_.symbol).zip(lambdaArgs.asInstanceOf[List[Term]]).toMap
              new TreeMap {
                override def transformTerm(tree: Term)(using ctx: Context): Term =
                  tree match
                    case tree: Ident => summon[Env].get(tree.symbol).flatMap(argsMap.get).getOrElse(tree)
                    case tree => super.transformTerm(tree)
              }.transformTree(scrutinee)
            }
            val names = args.map {
              case Block(List(DefDef("$anonfun", _, _, _, Some(Apply(Ident(name), _)))), _) => name
              case arg => arg.symbol.name
            }
            val argTypes = args.map(x => x.tpe.widenTermRefExpr)
            val resType = pattern.tpe
            val res = Lambda(MethodType(names)(_ => argTypes, _ => resType), bodyFn)
            matched(res.seal)

          //
          // Match two equivalent trees
          //

          /* Match literal */
          case (Literal(constant1), Literal(constant2)) if constant1 == constant2 =>
            matched

          /* Match type ascription (a) */
          case (Typed(expr1, _), pattern) =>
            expr1 =?= pattern

          /* Match type ascription (b) */
          case (scrutinee, Typed(expr2, _)) =>
            scrutinee =?= expr2

          /* Match selection */
          case (ref: Ref, Select(qual2, _)) if scrutinee.symbol == pattern.symbol || summon[Env].get(scrutinee.symbol).contains(pattern.symbol) =>
            ref match
              case Select(qual1, _) => qual1 =?= qual2
              case ref: Ident =>
                ref.tpe match
                  case TermRef(qual: TermRef, _) => Ref.term(qual) =?= qual2
                  case _ => matched

          /* Match reference */
          case (_: Ref, _: Ident) if scrutinee.symbol == pattern.symbol || summon[Env].get(scrutinee.symbol).contains(pattern.symbol) =>
            matched

          /* Match application */
          case (Apply(fn1, args1), Apply(fn2, args2)) =>
            fn1 =?= fn2 &&& args1 =?= args2

          /* Match type application */
          case (TypeApply(fn1, args1), TypeApply(fn2, args2)) =>
            fn1 =?= fn2 &&& args1 =?= args2

          case (Block(stats1, expr1), Block(binding :: stats2, expr2)) if isTypeBinding(binding) =>
            qctx.tasty.Constraints_add(summon[Context])(binding.symbol :: Nil)
            matched(new SymBinding(binding.symbol, hasFromAboveAnnotation(binding.symbol))) &&& Block(stats1, expr1) =?= Block(stats2, expr2)

          /* Match block */
          case (Block(stat1 :: stats1, expr1), Block(stat2 :: stats2, expr2)) =>
            val newEnv = (stat1, stat2) match {
              case (stat1: Definition, stat2: Definition) =>
                summon[Env] + (stat1.symbol -> stat2.symbol)
              case _ =>
                summon[Env]
            }
            withEnv(newEnv) {
              stat1 =?= stat2 &&& Block(stats1, expr1) =?= Block(stats2, expr2)
            }

          case (scrutinee, Block(typeBindings, expr2)) if typeBindings.forall(isTypeBinding) =>
            val bindingSymbols = typeBindings.map(_.symbol)
            qctx.tasty.Constraints_add(summon[Context])(bindingSymbols)
            bindingSymbols.foldRight(scrutinee =?= expr2)((x, acc) => matched(new SymBinding(x, hasFromAboveAnnotation(x))) &&& acc)

          /* Match if */
          case (If(cond1, thenp1, elsep1), If(cond2, thenp2, elsep2)) =>
            cond1 =?= cond2 &&& thenp1 =?= thenp2 &&& elsep1 =?= elsep2

          /* Match while */
          case (While(cond1, body1), While(cond2, body2)) =>
            cond1 =?= cond2 &&& body1 =?= body2

          /* Match assign */
          case (Assign(lhs1, rhs1), Assign(lhs2, rhs2)) =>
            lhs1 =?= lhs2 &&& rhs1 =?= rhs2

          /* Match new */
          case (New(tpt1), New(tpt2)) if tpt1.tpe.typeSymbol == tpt2.tpe.typeSymbol =>
            matched

          /* Match this */
          case (This(_), This(_)) if scrutinee.symbol == pattern.symbol =>
            matched

          /* Match super */
          case (Super(qual1, mix1), Super(qual2, mix2)) if mix1 == mix2 =>
            qual1 =?= qual2

          /* Match varargs */
          case (Repeated(elems1, _), Repeated(elems2, _)) if elems1.size == elems2.size =>
            elems1 =?= elems2

          /* Match type */
          // TODO remove this?
          case (scrutinee: TypeTree, pattern: TypeTree) if scrutinee.tpe <:< pattern.tpe =>
            matched

          /* Match val */
          case (ValDef(_, tpt1, rhs1), ValDef(_, tpt2, rhs2)) if checkValFlags() =>
            def rhsEnv = summon[Env] + (scrutinee.symbol -> pattern.symbol)
            tpt1 =?= tpt2 &&& treeOptMatches(rhs1, rhs2)(using summon[Context], rhsEnv)

          /* Match def */
          case (DefDef(_, typeParams1, paramss1, tpt1, Some(rhs1)), DefDef(_, typeParams2, paramss2, tpt2, Some(rhs2))) =>
            def rhsEnv =
              val oldEnv: Env = summon[Env]
              val newEnv: List[(Symbol, Symbol)] =
                (scrutinee.symbol -> pattern.symbol) :: typeParams1.zip(typeParams2).map((tparam1, tparam2) => tparam1.symbol -> tparam2.symbol) :::
                paramss1.flatten.zip(paramss2.flatten).map((param1, param2) => param1.symbol -> param2.symbol)
              oldEnv ++ newEnv

            typeParams1 =?= typeParams2
              &&& matchLists(paramss1, paramss2)(_ =?= _)
              &&& tpt1 =?= tpt2
              &&& withEnv(rhsEnv)(rhs1 =?= rhs2)

          case (Closure(_, tpt1), Closure(_, tpt2)) =>
            // TODO match tpt1 with tpt2?
            matched

          case (NamedArg(name1, arg1), NamedArg(name2, arg2)) if name1 == name2 =>
            arg1 =?= arg2

          // No Match
          case _ =>
            if (debug)
              println(
                s""">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                   |Scrutinee
                   |  ${scrutinee.show}
                   |
                   |${scrutinee.showExtractors}
                   |
                   |did not match pattern
                   |  ${pattern.show}
                   |
                   |${pattern.showExtractors}
                   |
                   |with environment: ${summon[Env]}
                   |
                   |
                   |""".stripMargin)
            notMatched
        }
      }
    end extension

    private object ClosedPatternTerm {
      /** Matches a term that does not contain free variables defined in the pattern (i.e. not defined in `Env`) */
      def unapply(term: Term)(using Context, Env): Option[term.type] =
        if freePatternVars(term).isEmpty then Some(term) else None

      /** Return all free variables of the term defined in the pattern (i.e. defined in `Env`) */
      def freePatternVars(term: Term)(using ctx: Context, env: Env): Set[Symbol] =
        val accumulator = new TreeAccumulator[Set[Symbol]] {
          def foldTree(x: Set[Symbol], tree: Tree)(using ctx: Context): Set[Symbol] =
            tree match
              case tree: Ident if env.contains(tree.symbol) => foldOverTree(x + tree.symbol, tree)
              case _ => foldOverTree(x, tree)
        }
        accumulator.foldTree(Set.empty, term)
    }

    private object IdentArgs {
      def unapply(args: List[Term])(using Context): Option[List[Ident]] =
        args.foldRight(Option(List.empty[Ident])) {
          case (id: Ident, Some(acc)) => Some(id :: acc)
          case (Block(List(DefDef("$anonfun", Nil, List(params), Inferred(), Some(Apply(id: Ident, args)))), Closure(Ident("$anonfun"), None)), Some(acc))
              if params.zip(args).forall(_.symbol == _.symbol) =>
            Some(id :: acc)
          case _ => None
        }
    }

    private def treeOptMatches(scrutinee: Option[Tree], pattern: Option[Tree])(using Context, Env): Matching = {
      (scrutinee, pattern) match {
        case (Some(x), Some(y)) => x =?= y
        case (None, None) => matched
        case _ => notMatched
      }
    }

    private def isTypeBinding(tree: Tree): Boolean = tree match {
      case tree: TypeDef => hasPatternTypeAnnotation(tree.symbol)
      case _ => false
    }
  }

  /** Result of matching a part of an expression */
  private opaque type Matching = Option[Tuple]

  private object Matching {

    def notMatched: Matching = None
    val matched: Matching = Some(Tuple())
    def matched(x: Any): Matching = Some(Tuple1(x))

    extension (self: Matching):
      def asOptionOfTuple: Option[Tuple] = self

      /** Concatenates the contents of two successful matchings or return a `notMatched` */
      def &&& (that: => Matching): Matching = self match {
        case Some(x) =>
          that match {
            case Some(y) => Some(x ++ y)
            case _ => None
          }
        case _ => None
      }
    end extension

  }

}
