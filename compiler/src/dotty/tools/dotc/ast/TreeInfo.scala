package dotty.tools
package dotc
package ast

import core._
import Flags._, Trees._, Types._, Contexts._
import Names._, StdNames._, NameOps._, Symbols._
import typer.ConstFold
import reporting.trace
import dotty.tools.dotc.transform.SymUtils._

import scala.annotation.tailrec

trait TreeInfo[T >: Untyped <: Type] { self: Trees.Instance[T] =>

  // Note: the <: Type constraint looks necessary (and is needed to make the file compile in dotc).
  // But Scalac accepts the program happily without it. Need to find out why.

  def unsplice(tree: Trees.Tree[T]): Trees.Tree[T] = tree

  def isDeclarationOrTypeDef(tree: Tree): Boolean = unsplice(tree) match {
    case DefDef(_, _, _, _, EmptyTree)
      | ValDef(_, _, EmptyTree)
      | TypeDef(_, _) => true
    case _ => false
  }

  def isOpAssign(tree: Tree): Boolean = unsplice(tree) match {
    case Apply(fn, _ :: _) =>
      unsplice(fn) match {
        case Select(_, name) if name.isOpAssignmentName => true
        case _ => false
      }
    case _ => false
  }

  class MatchingArgs(params: List[Symbol], args: List[Tree])(implicit ctx: Context) {
    def foreach(f: (Symbol, Tree) => Unit): Boolean = {
      def recur(params: List[Symbol], args: List[Tree]): Boolean = params match {
        case Nil => args.isEmpty
        case param :: params1 =>
          if (param.info.isRepeatedParam) {
            for (arg <- args) f(param, arg)
            true
          } else args match {
            case Nil => false
            case arg :: args1 =>
              f(param, args.head)
              recur(params1, args1)
          }
      }
      recur(params, args)
    }
    def zipped: List[(Symbol, Tree)] = map((_, _))
    def map[R](f: (Symbol, Tree) => R): List[R] = {
      val b = List.newBuilder[R]
      foreach(b += f(_, _))
      b.result()
    }
  }

  /** The method part of an application node, possibly enclosed in a block
   *  with only valdefs as statements. the reason for also considering blocks
   *  is that named arguments can transform a call into a block, e.g.
   *   <init>(b = foo, a = bar)
   * is transformed to
   *   { val x$1 = foo
   *     val x$2 = bar
   *     <init>(x$2, x$1)
   *   }
   */
  def methPart(tree: Tree): Tree = stripApply(tree) match {
    case TypeApply(fn, _) => methPart(fn)
    case AppliedTypeTree(fn, _) => methPart(fn) // !!! should not be needed
    case Block(stats, expr) => methPart(expr)
    case mp => mp
  }

  /** If this is an application, its function part, stripping all
   *  Apply nodes (but leaving TypeApply nodes in). Otherwise the tree itself.
   */
  def stripApply(tree: Tree): Tree = unsplice(tree) match {
    case Apply(fn, _) => stripApply(fn)
    case _ => tree
  }

  /** If this is a block, its expression part */
  def stripBlock(tree: Tree): Tree = unsplice(tree) match {
    case Block(_, expr) => stripBlock(expr)
    case _ => tree
  }

  /** The number of arguments in an application */
  def numArgs(tree: Tree): Int = unsplice(tree) match {
    case Apply(fn, args) => numArgs(fn) + args.length
    case TypeApply(fn, _) => numArgs(fn)
    case Block(_, expr) => numArgs(expr)
    case _ => 0
  }

  /** The (last) list of arguments of an application */
  def arguments(tree: Tree): List[Tree] = unsplice(tree) match {
    case Apply(_, args) => args
    case TypeApply(fn, _) => arguments(fn)
    case Block(_, expr) => arguments(expr)
    case _ => Nil
  }

  /** Is tree a path? */
  def isPath(tree: Tree): Boolean = unsplice(tree) match {
    case Ident(_) | This(_) | Super(_, _) => true
    case Select(qual, _) => isPath(qual)
    case _ => false
  }

  /** Is tree a self constructor call this(...)? I.e. a call to a constructor of the
   *  same object?
   */
  def isSelfConstrCall(tree: Tree): Boolean = methPart(tree) match {
    case Ident(nme.CONSTRUCTOR) | Select(This(_), nme.CONSTRUCTOR) => true
    case _ => false
  }

  /** Is tree a super constructor call?
   */
  def isSuperConstrCall(tree: Tree): Boolean = methPart(tree) match {
    case Select(Super(_, _), nme.CONSTRUCTOR) => true
    case _ => false
  }

  def isSuperSelection(tree: Tree): Boolean = unsplice(tree) match {
    case Select(Super(_, _), _) => true
    case _ => false
  }

  def isSelfOrSuperConstrCall(tree: Tree): Boolean = methPart(tree) match {
    case Ident(nme.CONSTRUCTOR)
       | Select(This(_), nme.CONSTRUCTOR)
       | Select(Super(_, _), nme.CONSTRUCTOR) => true
    case _ => false
  }

  /** Is tree a variable pattern? */
  def isVarPattern(pat: Tree): Boolean = unsplice(pat) match {
    case x: BackquotedIdent => false
    case x: Ident => x.name.isVariableName
    case _  => false
  }

  /** The first constructor definition in `stats` */
  def firstConstructor(stats: List[Tree]): Tree = stats match {
    case (meth: DefDef) :: _ if meth.name.isConstructorName => meth
    case stat :: stats => firstConstructor(stats)
    case nil => EmptyTree
  }

  /** The arguments to the first constructor in `stats`. */
  def firstConstructorArgs(stats: List[Tree]): List[Tree] = firstConstructor(stats) match {
    case DefDef(_, _, args :: _, _, _) => args
    case _                                => Nil
  }

  /** Is tpt a vararg type of the form T* or => T*? */
  def isRepeatedParamType(tpt: Tree)(implicit ctx: Context): Boolean = tpt match {
    case ByNameTypeTree(tpt1) => isRepeatedParamType(tpt1)
    case tpt: TypeTree => tpt.typeOpt.isRepeatedParam
    case AppliedTypeTree(Select(_, tpnme.REPEATED_PARAM_CLASS), _) => true
    case _ => false
  }

  /** Is name a left-associative operator? */
  def isLeftAssoc(operator: Name): Boolean = !operator.isEmpty && (operator.toSimpleName.last != ':')

  /** Is this argument node of the form <expr> : _*, or is it a reference to
   *  such an argument ? The latter case can happen when an argument is lifted.
   */
  def isWildcardStarArg(tree: Tree)(implicit ctx: Context): Boolean = unbind(tree) match {
    case Typed(Ident(nme.WILDCARD_STAR), _) => true
    case Typed(_, Ident(tpnme.WILDCARD_STAR)) => true
    case Typed(_, tpt: TypeTree) => tpt.typeOpt.isRepeatedParam
    case NamedArg(_, arg) => isWildcardStarArg(arg)
    case arg => arg.typeOpt.widen.isRepeatedParam
  }

  /** If this tree has type parameters, those.  Otherwise Nil.
  def typeParameters(tree: Tree): List[TypeDef] = tree match {
    case DefDef(_, _, tparams, _, _, _) => tparams
    case ClassDef(_, _, tparams, _)     => tparams
    case TypeDef(_, _, tparams, _)      => tparams
    case _                              => Nil
  }*/

  /** Does this argument list end with an argument of the form <expr> : _* ? */
  def isWildcardStarArgList(trees: List[Tree])(implicit ctx: Context): Boolean =
    trees.nonEmpty && isWildcardStarArg(trees.last)

  /** Is the argument a wildcard argument of the form `_` or `x @ _`?
   */
  def isWildcardArg(tree: Tree): Boolean = unbind(tree) match {
    case Ident(nme.WILDCARD) => true
    case _                   => false
  }

  /** Does this list contain a named argument tree? */
  def hasNamedArg(args: List[Any]): Boolean = args exists isNamedArg
  val isNamedArg: Any => Boolean = (arg: Any) => arg.isInstanceOf[Trees.NamedArg[_]]

  /** Is this pattern node a catch-all (wildcard or variable) pattern? */
  def isDefaultCase(cdef: CaseDef): Boolean = cdef match {
    case CaseDef(pat, EmptyTree, _) => isWildcardArg(pat)
    case _                            => false
  }

  /** Is this pattern node a synthetic catch-all case, added during PartialFuction synthesis before we know
    * whether the user provided cases are exhaustive. */
  def isSyntheticDefaultCase(cdef: CaseDef): Boolean = unsplice(cdef) match {
    case CaseDef(Bind(nme.DEFAULT_CASE, _), EmptyTree, _) => true
    case _                                                  => false
  }

  /** Does this CaseDef catch Throwable? */
  def catchesThrowable(cdef: CaseDef)(implicit ctx: Context): Boolean =
    catchesAllOf(cdef, defn.ThrowableType)

  /** Does this CaseDef catch everything of a certain Type? */
  def catchesAllOf(cdef: CaseDef, threshold: Type)(implicit ctx: Context): Boolean =
    isDefaultCase(cdef) ||
    cdef.guard.isEmpty && {
      unbind(cdef.pat) match {
        case Typed(Ident(nme.WILDCARD), tpt) => threshold <:< tpt.typeOpt
        case _                               => false
      }
    }

  /** Is this case guarded? */
  def isGuardedCase(cdef: CaseDef): Boolean = cdef.guard ne EmptyTree

  /** The underlying pattern ignoring any bindings */
  def unbind(x: Tree): Tree = unsplice(x) match {
    case Bind(_, y) => unbind(y)
    case y          => y
  }

  /**  The largest subset of {NoInits, PureInterface} that a
   *   trait or class enclosing this statement can have as flags.
   */
  def defKind(tree: Tree)(implicit ctx: Context): FlagSet = unsplice(tree) match {
    case EmptyTree | _: Import => NoInitsInterface
    case tree: TypeDef => if (tree.isClassDef) NoInits else NoInitsInterface
    case tree: DefDef =>
      if (tree.unforcedRhs == EmptyTree &&
          tree.vparamss.forall(_.forall(_.rhs.isEmpty))) NoInitsInterface
      else NoInits
    case tree: ValDef => if (tree.unforcedRhs == EmptyTree) NoInitsInterface else EmptyFlags
    case _ => EmptyFlags
  }

  /**  The largest subset of {NoInits, PureInterface} that a
   *   trait or class with these parents can have as flags.
   */
  def parentsKind(parents: List[Tree])(implicit ctx: Context): FlagSet = parents match {
    case Nil => NoInitsInterface
    case Apply(_, _ :: _) :: _ => EmptyFlags
    case _ :: parents1 => parentsKind(parents1)
  }

  /**  The largest subset of {NoInits, PureInterface} that a
   *   trait or class with this body can have as flags.
   */
  def bodyKind(body: List[Tree])(implicit ctx: Context): FlagSet =
    (NoInitsInterface /: body)((fs, stat) => fs & defKind(stat))

  /** Checks whether predicate `p` is true for all result parts of this expression,
   *  where we zoom into Ifs, Matches, and Blocks.
   */
  def forallResults(tree: Tree, p: Tree => Boolean): Boolean = tree match {
    case If(_, thenp, elsep) => forallResults(thenp, p) && forallResults(elsep, p)
    case Match(_, cases) => cases forall (c => forallResults(c.body, p))
    case Block(_, expr) => forallResults(expr, p)
    case _ => p(tree)
  }
}

trait UntypedTreeInfo extends TreeInfo[Untyped] { self: Trees.Instance[Untyped] =>
  import untpd._

  /** The underlying tree when stripping any TypedSplice or Parens nodes */
  override def unsplice(tree: Tree): Tree = tree match {
    case TypedSplice(tree1) => tree1
    case Parens(tree1) => unsplice(tree1)
    case _ => tree
  }

  /** True iff definition is a val or def with no right-hand-side, or it
   *  is an abstract typoe declaration
   */
  def lacksDefinition(mdef: MemberDef)(implicit ctx: Context): Boolean = mdef match {
    case mdef: ValOrDefDef =>
      mdef.unforcedRhs == EmptyTree && !mdef.name.isConstructorName && !mdef.mods.is(TermParamOrAccessor)
    case mdef: TypeDef =>
      def isBounds(rhs: Tree): Boolean = rhs match {
        case _: TypeBoundsTree => true
        case _: MatchTypeTree => true // Typedefs with Match rhs classify as abstract
        case LambdaTypeTree(_, body) => isBounds(body)
        case _ => false
      }
      mdef.rhs.isEmpty || isBounds(mdef.rhs)
    case _ => false
  }

  def functionWithUnknownParamType(tree: Tree): Option[Tree] = tree match {
    case Function(args, _) =>
      if (args.exists {
        case ValDef(_, tpt, _) => tpt.isEmpty
        case _ => false
      }) Some(tree)
      else None
    case Match(EmptyTree, _) =>
      Some(tree)
    case Block(Nil, expr) =>
      functionWithUnknownParamType(expr)
    case _ =>
      None
  }

  def isFunctionWithUnknownParamType(tree: Tree): Boolean =
    functionWithUnknownParamType(tree).isDefined

  /** Is `tree` an implicit function or closure, possibly nested in a block? */
  def isContextualClosure(tree: Tree)(implicit ctx: Context): Boolean = unsplice(tree) match {
    case tree: FunctionWithMods => tree.mods.is(Given)
    case Function((param: untpd.ValDef) :: _, _) => param.mods.is(Given)
    case Closure(_, meth, _) => true
    case Block(Nil, expr) => isContextualClosure(expr)
    case Block(DefDef(nme.ANON_FUN, _, params :: _, _, _) :: Nil, cl: Closure) =>
      params match {
        case param :: _ => param.mods.is(Given)
        case Nil => cl.tpt.eq(untpd.ContextualEmptyTree) || defn.isImplicitFunctionType(cl.tpt.typeOpt)
      }
    case _ => false
  }

  // todo: fill with other methods from TreeInfo that only apply to untpd.Tree's
}

trait TypedTreeInfo extends TreeInfo[Type] { self: Trees.Instance[Type] =>
  import TreeInfo._
  import tpd._

  /** The purity level of this statement.
   *  @return   Pure        if statement has no side effects
   *            Idempotent  if running the statement a second time has no side effects
   *            Impure      otherwise
   */
  def statPurity(tree: Tree)(implicit ctx: Context): PurityLevel = unsplice(tree) match {
    case EmptyTree
       | TypeDef(_, _)
       | Import(_, _, _)
       | DefDef(_, _, _, _, _) =>
      Pure
    case vdef @ ValDef(_, _, _) =>
      if (vdef.symbol.flags is Mutable) Impure else exprPurity(vdef.rhs) `min` Pure
    case _ =>
      Impure
      // TODO: It seem like this should be exprPurity(tree)
      // But if we do that the repl/vars test break. Need to figure out why that's the case.
  }

  /** The purity level of this expression. See docs for PurityLevel for what that means
   *
   *  Note that purity and idempotency are treated differently.
   *  References to modules and lazy vals are impure (side-effecting) both because
   *  side-effecting code may be executed and because the first reference
   *  takes a different code path than all to follow; but they are idempotent
   *  because running the expression a second time gives the cached result.
   */
  def exprPurity(tree: Tree)(implicit ctx: Context): PurityLevel = unsplice(tree) match {
    case EmptyTree
       | This(_)
       | Super(_, _)
       | Literal(_) =>
      PurePath
    case Ident(_) =>
      refPurity(tree)
    case Select(qual, _) =>
      if (tree.symbol.is(Erased)) Pure
      else refPurity(tree) `min` exprPurity(qual)
    case New(_) | Closure(_, _, _) =>
      Pure
    case TypeApply(fn, _) =>
      if (fn.symbol.is(Erased)) Pure else exprPurity(fn)
    case Apply(fn, args) =>
      def isKnownPureOp(sym: Symbol) =
        sym.owner.isPrimitiveValueClass || sym.owner == defn.StringClass
      if (tree.tpe.isInstanceOf[ConstantType] && isKnownPureOp(tree.symbol) // A constant expression with pure arguments is pure.
          || (fn.symbol.isStableMember && !fn.symbol.is(Lazy))
          || fn.symbol.isPrimaryConstructor && fn.symbol.owner.isNoInitsClass) // TODO: include in isStable?
        minOf(exprPurity(fn), args.map(exprPurity)) `min` Pure
      else if (fn.symbol.is(Erased)) Pure
      else if (fn.symbol.isStableMember /* && fn.symbol.is(Lazy) */)
        minOf(exprPurity(fn), args.map(exprPurity)) `min` Idempotent
      else Impure
    case Typed(expr, _) =>
      exprPurity(expr)
    case Block(stats, expr) =>
      minOf(exprPurity(expr), stats.map(statPurity))
    case Inlined(_, bindings, expr) =>
      minOf(exprPurity(expr), bindings.map(statPurity))
    case NamedArg(_, expr) =>
      exprPurity(expr)
    case _ =>
      Impure
  }

  private def minOf(l0: PurityLevel, ls: List[PurityLevel]) = (l0 /: ls)(_ `min` _)

  def isPurePath(tree: Tree)(implicit ctx: Context): Boolean = tree.tpe match {
    case tpe: ConstantType => exprPurity(tree) >= Pure
    case _ => exprPurity(tree) == PurePath
  }

  def isPureExpr(tree: Tree)(implicit ctx: Context): Boolean =
    exprPurity(tree) >= Pure

  def isIdempotentPath(tree: Tree)(implicit ctx: Context): Boolean = tree.tpe match {
    case tpe: ConstantType => exprPurity(tree) >= Idempotent
    case _ => exprPurity(tree) >= IdempotentPath
  }

  def isIdempotentExpr(tree: Tree)(implicit ctx: Context): Boolean =
    exprPurity(tree) >= Idempotent

  def isPureBinding(tree: Tree)(implicit ctx: Context): Boolean = statPurity(tree) >= Pure

  /** The purity level of this reference.
   *  @return
   *    PurePath        if reference is (nonlazy and stable) or to a parameterized function
   *    IdempotentPath  if reference is lazy and stable
   *    Impure          otherwise
   *  @DarkDimius: need to make sure that lazy accessor methods have Lazy and Stable
   *               flags set.
   */
  def refPurity(tree: Tree)(implicit ctx: Context): PurityLevel = {
    val sym = tree.symbol
    if (!tree.hasType) Impure
    else if (!tree.tpe.widen.isParameterless || sym.isEffectivelyErased) PurePath
    else if (!sym.isStableMember) Impure
    else if (sym.is(Module))
      if (sym.moduleClass.isNoInitsClass) PurePath else IdempotentPath
    else if (sym.is(Lazy)) IdempotentPath
    else PurePath
  }

  def isPureRef(tree: Tree)(implicit ctx: Context): Boolean =
    refPurity(tree) == PurePath
  def isIdempotentRef(tree: Tree)(implicit ctx: Context): Boolean =
    refPurity(tree) >= IdempotentPath

  /** (1) If `tree` is a constant expression, its value as a Literal,
   *  or `tree` itself otherwise.
   *
   *  Note: Demanding idempotency instead of purity in literalize is strictly speaking too loose.
   *  Example
   *
   *    object O { final val x = 42; println("43") }
   *    O.x
   *
   *  Strictly speaking we can't replace `O.x` with `42`.  But this would make
   *  most expressions non-constant. Maybe we can change the spec to accept this
   *  kind of eliding behavior. Or else enforce true purity in the compiler.
   *  The choice will be affected by what we will do with `inline` and with
   *  Singleton type bounds (see SIP 23). Presumably
   *
   *     object O1 { val x: Singleton = 42; println("43") }
   *     object O2 { inline val x = 42; println("43") }
   *
   *  should behave differently.
   *
   *     O1.x  should have the same effect as   { println("43"); 42 }
   *
   *  whereas
   *
   *     O2.x = 42
   *
   *  Revisit this issue once we have standardized on `inline`. Then we can demand
   *  purity of the prefix unless the selection goes to a inline val.
   *
   *  Note: This method should be applied to all term tree nodes that are not literals,
   *        that can be idempotent, and that can have constant types. So far, only nodes
   *        of the following classes qualify:
   *
   *        Ident
   *        Select
   *        TypeApply
   *
   *  (2) A primitive unary operator expression `pre.op` where `op` is one of `+`, `-`, `~`, `!`
   *  that has a constant type `ConstantType(v)` but that is not a constant expression
   *  (i.e. `pre` has side-effects) is translated to
   *
   *     { pre; v }
   *
   *  This avoids the situation where we have a Select node that does not have a symbol.
   */
  def constToLiteral(tree: Tree)(implicit ctx: Context): Tree = {
    val tree1 = ConstFold(tree)
    tree1.tpe.widenTermRefExpr match {
      case ConstantType(value) =>
        if (isIdempotentExpr(tree1)) Literal(value).withSpan(tree.span)
        else tree1 match {
          case Select(qual, _) if tree1.tpe.isInstanceOf[ConstantType] =>
            // it's a primitive unary operator; Simplify `pre.op` to `{ pre; v }` where `v` is the value of `pre.op`
            Block(qual :: Nil, Literal(value)).withSpan(tree.span)
          case _ =>
            tree1
        }
      case _ => tree1
    }
  }

  /** Is symbol potentially a getter of a mutable variable?
   */
  def mayBeVarGetter(sym: Symbol)(implicit ctx: Context): Boolean = {
    def maybeGetterType(tpe: Type): Boolean = tpe match {
      case _: ExprType => true
      case tpe: MethodType => tpe.isImplicitMethod
      case tpe: PolyType => maybeGetterType(tpe.resultType)
      case _ => false
    }
    sym.owner.isClass && !sym.isStableMember && maybeGetterType(sym.info)
  }

  /** Is tree a reference to a mutable variable, or to a potential getter
   *  that has a setter in the same class?
   */
  def isVariableOrGetter(tree: Tree)(implicit ctx: Context): Boolean = {
    def sym = tree.symbol
    def isVar    = sym is Mutable
    def isGetter =
      mayBeVarGetter(sym) && sym.owner.info.member(sym.name.asTermName.setterName).exists

    unsplice(tree) match {
      case Ident(_) => isVar
      case Select(_, _) => isVar || isGetter
      case Apply(_, _) =>
        methPart(tree) match {
          case Select(qual, nme.apply) => qual.tpe.member(nme.update).exists
          case _ => false
        }
      case _ => false
    }
  }

  /** Is tree a `this` node which belongs to `enclClass`? */
  def isSelf(tree: Tree, enclClass: Symbol)(implicit ctx: Context): Boolean = unsplice(tree) match {
    case This(_) => tree.symbol == enclClass
    case _ => false
  }

  /** Is tree a compiler-generated `.apply` node that refers to the
   *  apply of a function class?
   */
  def isSyntheticApply(tree: Tree): Boolean = tree match {
    case Select(qual, nme.apply) => tree.span.end == qual.span.end
    case _ => false
  }

  /** Strips layers of `.asInstanceOf[T]` / `_.$asInstanceOf[T]()` from an expression */
  def stripCast(tree: Tree)(implicit ctx: Context): Tree = {
    def isCast(sel: Tree) = sel.symbol.isTypeCast
    unsplice(tree) match {
      case TypeApply(sel @ Select(inner, _), _) if isCast(sel) =>
        stripCast(inner)
      case Apply(TypeApply(sel @ Select(inner, _), _), Nil) if isCast(sel) =>
        stripCast(inner)
      case t =>
        t
    }
  }

  /** Decompose a call fn[targs](vargs_1)...(vargs_n)
   *  into its constituents (fn, targs, vargss).
   *
   *  Note: targ and vargss may be empty
   */
  def decomposeCall(tree: Tree): (Tree, List[Tree], List[List[Tree]]) = {
    @tailrec
    def loop(tree: Tree, targss: List[Tree], argss: List[List[Tree]]): (Tree, List[Tree], List[List[Tree]]) =
      tree match {
        case Apply(fn, args) =>
          loop(fn, targss, args :: argss)
        case TypeApply(fn, targs) =>
          loop(fn, targs ::: targss, argss)
        case _ =>
          (tree, targss, argss)
      }
    loop(tree, Nil, Nil)
  }

  /** An extractor for closures, either contained in a block or standalone.
   */
  object closure {
    def unapply(tree: Tree): Option[(List[Tree], Tree, Tree)] = tree match {
      case Block(_, expr) => unapply(expr)
      case Closure(env, meth, tpt) => Some(env, meth, tpt)
      case Typed(expr, _)  => unapply(expr)
      case _ => None
    }
  }

  /** An extractor for def of a closure contained the block of the closure. */
  object closureDef {
    def unapply(tree: Tree)(implicit ctx: Context): Option[DefDef] = tree match {
      case Block((meth @ DefDef(nme.ANON_FUN, _, _, _, _)) :: Nil, closure: Closure) =>
        Some(meth)
      case Block(Nil, expr) =>
        unapply(expr)
      case Inlined(_, bindings, expr) if bindings.forall(isPureBinding) =>
        unapply(expr)
      case _ =>
        None
    }
  }

  /** If tree is a closure, its body, otherwise tree itself */
  def closureBody(tree: Tree)(implicit ctx: Context): Tree = tree match {
    case closureDef(meth) => meth.rhs
    case _ => tree
  }

  /** The variables defined by a pattern, in reverse order of their appearance. */
  def patVars(tree: Tree)(implicit ctx: Context): List[Symbol] = {
    val acc = new TreeAccumulator[List[Symbol]] {
      def apply(syms: List[Symbol], tree: Tree)(implicit ctx: Context) = tree match {
        case Bind(_, body) => apply(tree.symbol :: syms, body)
        case Annotated(tree, id @ Ident(tpnme.BOUNDTYPE_ANNOT)) => apply(id.symbol :: syms, tree)
        case _ => foldOver(syms, tree)
      }
    }
    acc(Nil, tree)
  }

  /** Is this pattern node a catch-all or type-test pattern? */
  def isCatchCase(cdef: CaseDef)(implicit ctx: Context): Boolean = cdef match {
    case CaseDef(Typed(Ident(nme.WILDCARD), tpt), EmptyTree, _) =>
      isSimpleThrowable(tpt.tpe)
    case CaseDef(Bind(_, Typed(Ident(nme.WILDCARD), tpt)), EmptyTree, _) =>
      isSimpleThrowable(tpt.tpe)
    case _ =>
      isDefaultCase(cdef)
  }

  private def isSimpleThrowable(tp: Type)(implicit ctx: Context): Boolean = tp match {
    case tp @ TypeRef(pre, _) =>
      (pre == NoPrefix || pre.widen.typeSymbol.isStatic) &&
      (tp.symbol derivesFrom defn.ThrowableClass) && !(tp.symbol is Trait)
    case _ =>
      false
  }

  /** The symbols defined locally in a statement list */
  def localSyms(stats: List[Tree])(implicit ctx: Context): List[Symbol] =
    for (stat <- stats if stat.isDef && stat.symbol.exists) yield stat.symbol

  /** If `tree` is a DefTree, the symbol defined by it, otherwise NoSymbol */
  def definedSym(tree: Tree)(implicit ctx: Context): Symbol =
    if (tree.isDef) tree.symbol else NoSymbol

  /** Going from child to parent, the path of tree nodes that starts
   *  with a definition of symbol `sym` and ends with `root`, or Nil
   *  if no such path exists.
   *  Pre: `sym` must have a position.
   */
  def defPath(sym: Symbol, root: Tree)(implicit ctx: Context): List[Tree] = trace.onDebug(s"defpath($sym with position ${sym.span}, ${root.show})") {
    require(sym.span.exists, sym)
    object accum extends TreeAccumulator[List[Tree]] {
      def apply(x: List[Tree], tree: Tree)(implicit ctx: Context): List[Tree] = {
        if (tree.span.contains(sym.span))
          if (definedSym(tree) == sym) tree :: x
          else {
            val x1 = foldOver(x, tree)
            if (x1 ne x) tree :: x1 else x1
          }
        else x
      }
    }
    accum(Nil, root)
  }

  /** The top level classes in this tree, including only those module classes that
   *  are not a linked class of some other class in the result.
   */
  def topLevelClasses(tree: Tree)(implicit ctx: Context): List[ClassSymbol] = tree match {
    case PackageDef(_, stats) => stats.flatMap(topLevelClasses)
    case tdef: TypeDef if tdef.symbol.isClass => tdef.symbol.asClass :: Nil
    case _ => Nil
  }

  /** The tree containing only the top-level classes and objects matching either `cls` or its companion object */
  def sliceTopLevel(tree: Tree, cls: ClassSymbol)(implicit ctx: Context): List[Tree] = tree match {
    case PackageDef(pid, stats) =>
      val slicedStats = stats.flatMap(sliceTopLevel(_, cls))
      if (!slicedStats.isEmpty)
        cpy.PackageDef(tree)(pid, slicedStats) :: Nil
      else
        Nil
    case tdef: TypeDef =>
      val sym = tdef.symbol
      assert(sym.isClass)
      if (cls == sym || cls == sym.linkedClass) tdef :: Nil
      else Nil
    case vdef: ValDef =>
      val sym = vdef.symbol
      assert(sym is Module)
      if (cls == sym.companionClass || cls == sym.moduleClass) vdef :: Nil
      else Nil
    case tree =>
      tree :: Nil
  }

  /** The statement sequence that contains a definition of `sym`, or Nil
   *  if none was found.
   *  For a tree to be found, The symbol must have a position and its definition
   *  tree must be reachable from come tree stored in an enclosing context.
   */
  def definingStats(sym: Symbol)(implicit ctx: Context): List[Tree] =
    if (!sym.span.exists || (ctx eq NoContext) || ctx.compilationUnit == null) Nil
    else defPath(sym, ctx.compilationUnit.tpdTree) match {
      case defn :: encl :: _ =>
        def verify(stats: List[Tree]) =
          if (stats exists (definedSym(_) == sym)) stats else Nil
        encl match {
          case Block(stats, _) => verify(stats)
          case encl: Template => verify(encl.body)
          case PackageDef(_, stats) => verify(stats)
          case _ => Nil
        }
      case nil =>
        Nil
    }

  /** If `tree` is an instance of `TupleN[...](e1, ..., eN)`, the arguments `e1, ..., eN`
   *  otherwise the empty list.
   */
  def tupleArgs(tree: Tree)(implicit ctx: Context): List[Tree] = tree match {
    case Block(Nil, expr) => tupleArgs(expr)
    case Inlined(_, Nil, expr) => tupleArgs(expr)
    case Apply(fn, args)
    if fn.symbol.name == nme.apply &&
        fn.symbol.owner.is(Module) &&
        defn.isTupleClass(fn.symbol.owner.companionClass) => args
    case _ => Nil
  }

  /** The qualifier part of a Select or Ident.
   *  For an Ident, this is the `This` of the current class.
   */
  def qualifier(tree: Tree)(implicit ctx: Context): Tree = tree match {
    case Select(qual, _) => qual
    case tree: Ident => desugarIdentPrefix(tree)
    case _ => This(ctx.owner.enclosingClass.asClass)
  }

  /** Is this a (potentially applied) selection of a member of a structural type
   *  that is not a member of an underlying class or trait?
   */
  def isStructuralTermSelectOrApply(tree: Tree)(implicit ctx: Context): Boolean = {
    def isStructuralTermSelect(tree: Select) = {
      def hasRefinement(qualtpe: Type): Boolean = qualtpe.dealias match {
        case RefinedType(parent, rname, rinfo) =>
          rname == tree.name || hasRefinement(parent)
        case tp: TypeProxy =>
          hasRefinement(tp.underlying)
        case tp: AndType =>
          hasRefinement(tp.tp1) || hasRefinement(tp.tp2)
        case tp: OrType =>
          hasRefinement(tp.tp1) || hasRefinement(tp.tp2)
        case _ =>
          false
      }
      !tree.symbol.exists && tree.isTerm && hasRefinement(tree.qualifier.tpe)
    }
    def loop(tree: Tree): Boolean = tree match {
      case Apply(fun, _) =>
        loop(fun)
      case tree: Select =>
        isStructuralTermSelect(tree)
      case _ =>
        false
    }
    loop(tree)
  }

  /** Structural tree comparison (since == on trees is reference equality).
   *  For the moment, only Ident, Select, Literal, Apply and TypeApply are supported
   */
  implicit class StructuralEqDeco(t1: Tree) {
    def === (t2: Tree)(implicit ctx: Context): Boolean = (t1, t2) match {
      case (t1: Ident, t2: Ident) =>
        t1.symbol == t2.symbol
      case (t1 @ Select(q1, _), t2 @ Select(q2, _)) =>
        t1.symbol == t2.symbol && q1 === q2
      case (Literal(c1), Literal(c2)) =>
        c1 == c2
      case (Apply(f1, as1), Apply(f2, as2)) =>
        f1 === f2 && as1.corresponds(as2)(_ === _)
      case (TypeApply(f1, ts1), TypeApply(f2, ts2)) =>
        f1 === f2 && ts1.tpes.corresponds(ts2.tpes)(_ =:= _)
      case _ =>
        false
    }
    def hash(implicit ctx: Context): Int =
      t1.getClass.hashCode * 37 + {
        t1 match {
          case t1: Ident => t1.symbol.hashCode
          case t1 @ Select(q1, _) => t1.symbol.hashCode * 41 + q1.hash
          case Literal(c1) => c1.hashCode
          case Apply(f1, as1) => (f1.hash /: as1)((h, arg) => h * 41 + arg.hash)
          case TypeApply(f1, ts1) => (f1.hash /: ts1)((h, arg) => h * 41 + arg.tpe.hash)
          case _ => t1.hashCode
        }
      }
  }

  /** Extractors for quotes */
  object Quoted {
    /** Extracts the content of a quoted tree.
     *  The result can be the contents of a term or type quote, which
     *  will return a term or type tree respectively.
     */
    def unapply(tree: tpd.Tree)(implicit ctx: Context): Option[tpd.Tree] = tree match {
      case tree: GenericApply[Type] if tree.symbol.isQuote => Some(tree.args.head)
      case _ => None
    }
  }

  /** Extractors for splices */
  object Spliced {
    /** Extracts the content of a spliced tree.
     *  The result can be the contents of a term or type splice, which
     *  will return a term or type tree respectively.
     */
    def unapply(tree: tpd.Tree)(implicit ctx: Context): Option[tpd.Tree] = tree match {
      case tree: tpd.Apply if tree.symbol.isSplice => Some(tree.args.head)
      case tree: tpd.Select if tree.symbol.isSplice => Some(tree.qualifier)
      case _ => None
    }
  }
}

object TreeInfo {
  /** A purity level is represented as a bitset (expressed as an Int) */
  class PurityLevel(val x: Int) extends AnyVal {
    /** `this` contains the bits of `that` */
    def >= (that: PurityLevel): Boolean = (x & that.x) == that.x

    /** The intersection of the bits of `this` and `that` */
    def min(that: PurityLevel): PurityLevel = new PurityLevel(x & that.x)
  }

  /** An expression is a stable path. Requires that expression is at least idempotent */
  val Path: PurityLevel = new PurityLevel(4)

  /** The expression has no side effects */
  val Pure: PurityLevel = new PurityLevel(3)

  /** Running the expression a second time has no side effects. Implied by `Pure`. */
  val Idempotent: PurityLevel = new PurityLevel(1)

  val Impure: PurityLevel = new PurityLevel(0)

  /** A stable path that is evaluated without side effects */
  val PurePath: PurityLevel = new PurityLevel(Pure.x | Path.x)

  /** A stable path that is also idempotent */
  val IdempotentPath: PurityLevel = new PurityLevel(Idempotent.x | Path.x)
}
