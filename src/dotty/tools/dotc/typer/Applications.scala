package dotty.tools
package dotc
package typer

import core._
import ast.{Trees, untpd, tpd, TreeInfo}
import util.Positions._
import util.Stats.track
import Trees.Untyped
import Mode.ImplicitsEnabled
import Contexts._
import Flags._
import Denotations._
import NameOps._
import Symbols._
import Types._
import Decorators._
import ErrorReporting._
import Trees._
import Names._
import StdNames._
import Inferencing._
import EtaExpansion._
import collection.mutable
import reflect.ClassTag
import config.Printers._
import language.implicitConversions

object Applications {
  import tpd._
  private val isNamedArg = (arg: Any) => arg.isInstanceOf[Trees.NamedArg[_]]
  def hasNamedArg(args: List[Any]) = args exists isNamedArg

  def wrapDefs(defs: mutable.ListBuffer[Tree], tree: Tree)(implicit ctx: Context): Tree =
    if (defs != null && defs.nonEmpty) tpd.Block(defs.toList, tree) else tree
}

import Applications._

trait Applications extends Compatibility { self: Typer =>

  import Applications._
  import tpd.{ cpy => _, _ }
  import untpd.cpy

  /** @param Arg        the type of arguments, could be tpd.Tree, untpd.Tree, or Type
   *  @param methRef    the reference to the method of the application
   *  @param funType    the type of the function part of the application
   *  @param args       the arguments of the application
   *  @param resultType the expected result type of the application
   */
  abstract class Application[Arg](methRef: TermRef, funType: Type, args: List[Arg], resultType: Type)(implicit ctx: Context) {

    /** The type of typed arguments: either tpd.Tree or Type */
    type TypedArg

    /** Given an original argument and the type of the corresponding formal
     *  parameter, produce a typed argument.
     */
    protected def typedArg(arg: Arg, formal: Type): TypedArg

    /** Turn a typed tree into an argument */
    protected def treeToArg(arg: Tree): Arg

    /** Check that argument corresponds to type `formal` and
     *  possibly add it to the list of adapted arguments
     */
    protected def addArg(arg: TypedArg, formal: Type): Unit

    /** Is this an argument of the form `expr: _*` or a RepeatedParamType
     *  derived from such an argument?
     */
    protected def isVarArg(arg: Arg): Boolean

    /** If constructing trees, turn last `n` processed arguments into a
     *  `SeqLiteral` tree with element type `elemFormal`.
     */
    protected def makeVarArg(n: Int, elemFormal: Type): Unit

    /** Signal failure with given message at position of given argument */
    protected def fail(msg: => String, arg: Arg): Unit

    /** Signal failure with given message at position of the application itself */
    protected def fail(msg: => String): Unit

    protected def appPos: Position

    /** If constructing trees, the current function part, which might be
     *  affected by lifting. EmptyTree otherwise.
     */
    protected def normalizedFun: Tree

    /** If constructing trees, pull out all parts of the function
     *  which are not idempotent into separate prefix definitions
     */
    protected def liftFun(): Unit = ()

    /** A flag signalling that the typechecking the application was so far succesful */
    private[this] var _ok = true

    def ok = _ok
    def ok_=(x: Boolean) = {
      assert(x || ctx.errorsReported || !ctx.typerState.isCommittable) // !!! DEBUG
      _ok = x
    }

    /** The function's type after widening and instantiating polytypes
     *  with polyparams in constraint set
     */
    val methType = funType.widen match {
      case funType: MethodType => funType
      case funType: PolyType => constrained(funType).resultType
      case tp => tp //was: funType
    }

    /** The arguments re-ordered so that each named argument matches the
     *  same-named formal parameter.
     */
    lazy val orderedArgs =
      if (hasNamedArg(args))
        reorder(args.asInstanceOf[List[untpd.Tree]]).asInstanceOf[List[Arg]]
      else
        args

    protected def init() = methType match {
      case methType: MethodType =>
        // apply the result type constraint, unless method type is dependent
        if (!methType.isDependent)
          if (!constrainResult(methType.resultType, resultType))
            fail(err.typeMismatchStr(methType.resultType, resultType))
        // match all arguments with corresponding formal parameters
        matchArgs(orderedArgs, methType.paramTypes, 0)
      case _ =>
        if (methType.isError) ok = false
        else fail(s"$methString does not take parameters")
    }

    /** The application was succesful */
    def success = ok

    protected def methodType = methType.asInstanceOf[MethodType]
    private def methString: String = s"method ${methRef.name}: ${methType.show}"

    /** Re-order arguments to correctly align named arguments */
    def reorder[T >: Untyped](args: List[Trees.Tree[T]]): List[Trees.Tree[T]] = {

      /** @param pnames    The list of parameter names that are missing arguments
       *  @param args      The list of arguments that are not yet passed, or that are waiting to be dropped
       *  @param nameToArg A map from as yet unseen names to named arguments
       *  @param todrop    A set of names that have aready be passed as named arguments
       *
       *  For a well-typed application we have the invariants
       *
       *  1. `(args diff toDrop)` can be reordered to match `pnames`
       *  2. For every `(name -> arg)` in `nameToArg`, `arg` is an element of `args`
       */
      def recur(pnames: List[Name], args: List[Trees.Tree[T]],
                nameToArg: Map[Name, Trees.NamedArg[T]], toDrop: Set[Name]): List[Trees.Tree[T]] = pnames match {
        case pname :: pnames1 if nameToArg contains pname =>
          // there is a named argument for this parameter; pick it
          nameToArg(pname) :: recur(pnames1, args, nameToArg - pname, toDrop + pname)
        case _ =>
          def pnamesRest = if (pnames.isEmpty) pnames else pnames.tail
          args match {
            case (arg @ NamedArg(aname, _)) :: args1 =>
              if (toDrop contains aname) // argument is already passed
                recur(pnames, args1, nameToArg, toDrop - aname)
              else if ((nameToArg contains aname) && pnames.nonEmpty) // argument is missing, pass an empty tree
                genericEmptyTree :: recur(pnames.tail, args, nameToArg, toDrop)
              else { // name not (or no longer) available for named arg
                def msg =
                  if (methodType.paramNames contains aname)
                    s"parameter $aname of $methString is already instantiated"
                  else
                    s"$methString does not have a parameter $aname"
                fail(msg, arg.asInstanceOf[Arg])
                arg :: recur(pnamesRest, args1, nameToArg, toDrop)
              }
            case arg :: args1 =>
              arg :: recur(pnamesRest, args1, nameToArg, toDrop) // unnamed argument; pick it
            case Nil => // no more args, continue to pick up any preceding named args
              if (pnames.isEmpty) Nil
              else recur(pnamesRest, args, nameToArg, toDrop)
          }
      }
      val nameAssocs = for (arg @ NamedArg(name, _) <- args) yield (name, arg)
      recur(methodType.paramNames, args, nameAssocs.toMap, Set())
    }

    /** Splice new method reference into existing application */
    def spliceMeth(meth: Tree, app: Tree): Tree = app match {
      case Apply(fn, args) => Apply(spliceMeth(meth, fn), args)
      case TypeApply(fn, targs) => TypeApply(spliceMeth(meth, fn), targs)
      case _ => meth
    }

    /** Find reference to default parameter getter for parameter #n in current
     *  parameter list, or NoType if none was found
     */
    def findDefaultGetter(n: Int)(implicit ctx: Context): Type = {
      val meth = methRef.symbol
      val prefix =
        if ((meth is Synthetic) && meth.name == nme.apply) nme.CONSTRUCTOR else methRef.name
      def getterName = prefix.defaultGetterName(n)
      def ref(pre: Type, sym: Symbol): Type =
        if (pre.exists && sym.isTerm) pre select sym else NoType
      if (meth.hasDefaultParams)
        methRef.prefix match {
          case NoPrefix =>
            def findDefault(cx: Context): Type = {
              if (cx eq NoContext) NoType
              else if (cx.scope != cx.outer.scope &&
                       cx.denotNamed(methRef.name).hasAltWith(_.symbol == meth)) {
                val denot = cx.denotNamed(getterName)
                assert(denot.exists)
                cx.owner.thisType.select(getterName, denot)
              } else findDefault(cx.outer)
            }
            findDefault(ctx)
          case mpre =>
            val cls = meth.owner
            val pre =
              if (meth.isClassConstructor) {
                // default getters for class constructors are found in the companion object
                mpre.baseType(cls) match {
                  case TypeRef(clspre, _) => ref(clspre, cls.companionModule)
                  case _ => NoType
                }
              } else mpre
            val getter = pre.member(getterName)
            ref(pre, getter.symbol)
        }
      else NoType
    }

    /** Match re-ordered arguments against formal parameters
     *  @param n   The position of the first parameter in formals in `methType`.
     */
    def matchArgs(args: List[Arg], formals: List[Type], n: Int): Unit = {
      if (success) formals match {
        case formal :: formals1 =>

          def addTyped(arg: Arg, formal: Type) =
            addArg(typedArg(arg, formal), formal)

          def missingArg(n: Int): Unit = {
            val pname = methodType.paramNames(n)
            fail(
              if (pname contains '$') s"not enough arguments for $methString"
              else s"missing argument for parameter $pname of $methString")
          }

          def tryDefault(n: Int, args1: List[Arg]): Unit = {
            findDefaultGetter(n + numArgs(normalizedFun)) match {
              case dref: NamedType =>
                liftFun()
                addTyped(treeToArg(spliceMeth(Ident(dref) withPos appPos, normalizedFun)), formal)
                matchArgs(args1, formals1, n + 1)
              case _ =>
                missingArg(n)
            }
          }

          if (formal.isRepeatedParam)
            args match {
              case arg :: Nil if isVarArg(arg) =>
                addTyped(arg, formal)
              case _ =>
                val elemFormal = formal.typeArgs.head
                args foreach (addTyped(_, elemFormal))
                makeVarArg(args.length, elemFormal)
            }
          else args match {
            case EmptyTree :: args1 =>
              tryDefault(n, args1)
            case arg :: args1 =>
              addTyped(arg, formal)
              matchArgs(args1, formals1, n + 1)
            case nil =>
              tryDefault(n, args)
          }

        case nil =>
          args match {
            case arg :: args1 => fail(s"too many arguments for $methString", arg)
            case nil =>
          }
      }
    }
  }

  /** Subclass of Application for the cases where we are interested only
   *  in a "can/cannot apply" answer, without needing to construct trees or
   *  issue error messages.
   */
  abstract class TestApplication[Arg](methRef: TermRef, funType: Type, args: List[Arg], resultType: Type)(implicit ctx: Context)
  extends Application[Arg](methRef, funType, args, resultType) {
    type TypedArg = Arg
    type Result = Unit

    /** The type of the given argument */
    protected def argType(arg: Arg, formal: Type): Type

    def typedArg(arg: Arg, formal: Type): Arg = arg
    def addArg(arg: TypedArg, formal: Type) =
      ok = ok & isCompatible(argType(arg, formal), formal)
    def makeVarArg(n: Int, elemFormal: Type) = {}
    def fail(msg: => String, arg: Arg) =
      ok = false
    def fail(msg: => String) =
      ok = false
    def appPos = NoPosition
    def normalizedFun = EmptyTree
    init()
  }

  /** Subclass of Application for applicability tests with type arguments and value
   *  argument trees.
   */
  class ApplicableToTrees(methRef: TermRef, targs: List[Type], args: List[Tree], resultType: Type)(implicit ctx: Context)
  extends TestApplication(methRef, methRef.widen.appliedTo(targs), args, resultType) {
    def argType(arg: Tree, formal: Type): Type = normalize(arg.tpe, formal)
    def treeToArg(arg: Tree): Tree = arg
    def isVarArg(arg: Tree): Boolean = tpd.isWildcardStarArg(arg)
  }

  /** Subclass of Application for applicability tests with value argument types. */
  class ApplicableToTypes(methRef: TermRef, args: List[Type], resultType: Type)(implicit ctx: Context)
  extends TestApplication(methRef, methRef, args, resultType) {
    def argType(arg: Type, formal: Type): Type = arg
    def treeToArg(arg: Tree): Type = arg.tpe
    def isVarArg(arg: Type): Boolean = arg.isRepeatedParam
  }

  /** Subclass of Application for type checking an Apply node, where
   *  types of arguments are either known or unknown.
   */
  abstract class TypedApply[T >: Untyped](
    app: untpd.Apply, fun: Tree, methRef: TermRef, args: List[Trees.Tree[T]], resultType: Type)(implicit ctx: Context)
  extends Application(methRef, fun.tpe, args, resultType) {
    type TypedArg = Tree
    def isVarArg(arg: Trees.Tree[T]): Boolean = untpd.isWildcardStarArg(arg)
    private var typedArgBuf = new mutable.ListBuffer[Tree]
    private var liftedDefs: mutable.ListBuffer[Tree] = null
    private var myNormalizedFun: Tree = fun
    init()

    def addArg(arg: Tree, formal: Type): Unit =
      typedArgBuf += adaptInterpolated(arg, formal.widenExpr)

    def makeVarArg(n: Int, elemFormal: Type): Unit = {
      val args = typedArgBuf.takeRight(n).toList
      typedArgBuf.trimEnd(n)
      val seqLit = if (methodType.isJava) JavaSeqLiteral(args) else SeqLiteral(args)
      typedArgBuf += seqToRepeated(seqLit)
    }

    override def appPos = app.pos

    def fail(msg: => String, arg: Trees.Tree[T]) = {
      ctx.error(msg, arg.pos)
      ok = false
    }

    def fail(msg: => String) = {
      ctx.error(msg, app.pos)
      ok = false
    }

    def normalizedFun = myNormalizedFun

    override def liftFun(): Unit =
      if (liftedDefs == null) {
        liftedDefs = new mutable.ListBuffer[Tree]
        myNormalizedFun = liftApp(liftedDefs, myNormalizedFun)
      }

    /** The index of the first difference between lists of trees `xs` and `ys`,
     *  where `EmptyTree`s in the second list are skipped.
     *  -1 if there are no differences.
     */
    private def firstDiff[T <: Trees.Tree[_]](xs: List[T], ys: List[T], n: Int = 0): Int = xs match {
      case x :: xs1 =>
        ys match {
          case EmptyTree :: ys1 => firstDiff(xs1, ys1, n)
          case y :: ys1 => if (x ne y) n else firstDiff(xs1, ys1, n + 1)
          case nil => n
        }
      case nil =>
        ys match {
          case EmptyTree :: ys1 => firstDiff(xs, ys1, n)
          case y :: ys1 => n
          case nil => -1
        }
    }
    private def sameSeq[T <: Trees.Tree[_]](xs: List[T], ys: List[T]): Boolean = firstDiff(xs, ys) < 0

    val result = {
      var typedArgs = typedArgBuf.toList
      val ownType =
        if (!success) ErrorType
        else {
          if (!sameSeq(args, orderedArgs)) {
            // need to lift arguments to maintain evaluation order in the
            // presence of argument reorderings.
            liftFun()
            val eqSuffixLength = firstDiff(app.args.reverse, orderedArgs.reverse)
            val (liftable, rest) = typedArgs splitAt (typedArgs.length - eqSuffixLength)
            typedArgs = liftArgs(liftedDefs, methType, liftable) ++ rest
          }
          if (sameSeq(typedArgs, args)) // trick to cut down on tree copying
            typedArgs = args.asInstanceOf[List[Tree]]
          methodType.instantiate(typedArgs.tpes)
        }
      wrapDefs(liftedDefs, cpy.Apply(app, normalizedFun, typedArgs).withType(ownType))
    }
  }

  /** Subclass of Application for type checking an Apply node with untyped arguments. */
  class ApplyToUntyped(app: untpd.Apply, fun: Tree, methRef: TermRef, proto: FunProto, resultType: Type)(implicit ctx: Context)
  extends TypedApply(app, fun, methRef, proto.args, resultType) {
    def typedArg(arg: untpd.Tree, formal: Type): TypedArg = proto.typedArg(arg, formal.widenExpr)
    def treeToArg(arg: Tree): untpd.Tree = untpd.TypedSplice(arg)
  }

  /** Subclass of Application for type checking an Apply node with typed arguments. */
  class ApplyToTyped(app: untpd.Apply, fun: Tree, methRef: TermRef, args: List[Tree], resultType: Type)(implicit ctx: Context)
  extends TypedApply(app, fun, methRef, args, resultType) {
    def typedArg(arg: Tree, formal: Type): TypedArg = arg
    def treeToArg(arg: Tree): Tree = arg
  }

  def typedApply(tree: untpd.Apply, pt: Type)(implicit ctx: Context): Tree = {

    def realApply(implicit ctx: Context): Tree = track("realApply") {
      val proto = new FunProto(tree.args, pt, this)
      val fun1 = typedExpr(tree.fun, proto)
      methPart(fun1).tpe match {
        case funRef: TermRef =>
          tryEither { implicit ctx =>
            val app =
              if (proto.argsAreTyped) new ApplyToTyped(tree, fun1, funRef, proto.typedArgs, pt)
              else new ApplyToUntyped(tree, fun1, funRef, proto, pt)
            val result = app.result
            ConstFold(result)
          } { (failedVal, failedState) => fun1 match {
              case Select(qual, name) =>
                // try with prototype `[].name(args)`, this might succeed by inserting an
                // implicit conversion around []. (an example is Int + BigInt).
                tryEither { implicit ctx =>
                  val simpleFunProto = new FunProto(tree.args, WildcardType, this) // drop result type, because views are disabled
                  val selProto = new SelectionProto(name, simpleFunProto, NoViewsAllowed)
                  val qual1 = adaptInterpolated(qual, selProto)
                  if (qual eq qual1) ctx.error("no progress")
                  if (ctx.reporter.hasErrors) qual1
                  else
                    typedApply(
                      cpy.Apply(tree,
                        cpy.Select(fun1, untpd.TypedSplice(qual1), name),
                        proto.typedArgs map untpd.TypedSplice),
                      pt)
                } { (_, _) =>
                  failedState.commit()
                  failedVal
                }
              case _ =>
                failedState.commit()
                failedVal
            }
          }
        case _ =>
          fun1.tpe match {
            case ErrorType =>
              tree.withType(ErrorType)
            case tp =>
              throw new Error(s"unexpected type.\n fun1 = $fun1,\n methPart(fun1) = ${methPart(fun1)},\n methPart(fun1).tpe = ${methPart(fun1).tpe},\n tpe = $tp")
          }
      }
    }

    /** Convert expression like
     *
     *     e += (args)
     *
     *  where the lifted-for-assignment version of e is { val xs = es; e' } to
     *
     *     { val xs = es; e' = e' + args }
     */
     def typedOpAssign: Tree = track("typedOpAssign") {
      val Apply(Select(lhs, name), rhss) = tree
      val lhs1 = typedExpr(lhs)
      val liftedDefs = new mutable.ListBuffer[Tree]
      val lhs2 = untpd.TypedSplice(liftAssigned(liftedDefs, lhs1))
      val assign = untpd.Assign(lhs2, untpd.Apply(untpd.Select(lhs2, name.init), rhss))
      wrapDefs(liftedDefs, typed(assign))
    }

    if (untpd.isOpAssign(tree))
      tryEither {
        implicit ctx => realApply
      } { (failedVal, failedState) =>
        tryEither {
          implicit ctx => typedOpAssign
        } { (_, _) =>
          failedState.commit()
          failedVal
        }
      }
    else realApply
  }

  def typedTypeApply(tree: untpd.TypeApply, pt: Type)(implicit ctx: Context): Tree = track("typedTypeApply") {
    val typedArgs = tree.args mapconserve (typedType(_))
    val typedFn = typedExpr(tree.fun, PolyProto(typedArgs.tpes, pt))
    val ownType = typedFn.tpe.widen match {
      case pt: PolyType =>
        checkBounds(typedArgs, pt, tree.pos)
        val argTypes = typedArgs.tpes
        if (argTypes.length == pt.paramNames.length)
          pt.resultType.substParams(pt, typedArgs.tpes)
        else {
          ctx.error(i"wrong number of type parameters for ${typedFn.tpe}; expected: ${pt.paramNames.length}", tree.pos)
          ErrorType
        }
      case _ =>
        ctx.error(s"${err.exprStr(typedFn)} does not take type parameters", tree.pos)
        ErrorType
    }
    cpy.TypeApply(tree, typedFn, typedArgs).withType(ownType)
  }

  def typedUnApply(tree: untpd.Apply, pt: Type)(implicit ctx: Context): Tree = track("typedUnApply") {
    val Apply(qual, args) = tree

    def notAnExtractor(tree: Tree) =
      errorTree(tree, s"${qual.show} cannot be used as an extractor in a pattern because it lacks an unapply or unapplySeq method")

    /** If this is a term ref tree, try to typecheck with its type name.
     *  If this refers to a type alias, follow the alias, and if
     *  one finds a class, reference the class companion module.
     */
    def followTypeAlias(tree: untpd.Tree): untpd.Tree = {
      tree match {
        case tree: untpd.RefTree =>
          val ttree = typedType(tree.withName(tree.name.toTypeName))
          ttree.tpe match {
            case alias: TypeRef if alias.info.isAlias =>
              companionRef(alias) match {
                case companion: TermRef => return untpd.ref(companion) withPos tree.pos
                case _ =>
              }
            case _ =>
          }
        case _ =>
      }
      untpd.EmptyTree
    }

    /** A typed qual.unappy or qual.unappySeq tree, if this typechecks.
     *  Otherwise fallBack with (maltyped) qual.unapply as argument
     */
    def trySelectUnapply(qual: untpd.Tree)(fallBack: Tree => Tree): Tree = {
      val unappProto = new UnapplyFunProto(this)
      tryEither {
        implicit ctx => typedExpr(untpd.Select(qual, nme.unapply), unappProto)
      } {
        (sel, _) =>
          tryEither {
            implicit ctx => typedExpr(untpd.Select(qual, nme.unapplySeq), unappProto) // for backwards compatibility; will be dropped
          } {
            (_, _) => fallBack(sel)
          }
      }
    }

    /** Produce a typed qual.unappy or qual.unappySeq tree, or
     *  else if this fails follow a type alias and try again.
     */
    val unapply = trySelectUnapply(qual) { sel =>
      val qual1 = followTypeAlias(qual)
      if (qual1.isEmpty) notAnExtractor(sel)
      else trySelectUnapply(qual1)(_ => notAnExtractor(sel))
    }

    def fromScala2x = unapply.symbol.exists && (unapply.symbol.owner is Scala2x)

    def unapplyArgs(unapplyResult: Type)(implicit ctx: Context): List[Type] = {
      def extractorMemberType(tp: Type, name: Name) = {
        val ref = tp member name
        if (ref.isOverloaded)
          errorType(s"Overloaded reference to ${ref.show} is not allowed in extractor", tree.pos)
        else if (ref.info.isInstanceOf[PolyType])
          errorType(s"Reference to polymorphic ${ref.show}: ${ref.info.show} is not allowed in extractor", tree.pos)
        else
          ref.info.widenExpr.dealias
      }

      def productSelectors(tp: Type): List[Type] = {
        val sels = for (n <- Iterator.from(0)) yield extractorMemberType(tp, nme.selectorName(n))
        sels.takeWhile(_.exists).toList
      }
      def seqSelector = defn.RepeatedParamType.appliedTo(unapplyResult.elemType :: Nil)
      def getSelectors(tp: Type): List[Type] =
        if (defn.isProductSubType(tp) && args.length > 1) productSelectors(tp)
        else tp :: Nil
      def getTp = extractorMemberType(unapplyResult, nme.get)

      // println(s"unapply $unapplyResult ${extractorMemberType(unapplyResult, nme.isDefined)}")
      if (extractorMemberType(unapplyResult, nme.isDefined) isRef defn.BooleanClass) {
        if (getTp.exists)
          if (unapply.symbol.name == nme.unapplySeq) {
            val seqArg = getTp.firstBaseTypeArg(defn.SeqClass)
            if (seqArg.exists) return args map Function.const(seqArg)
          }
          else return getSelectors(getTp)
        else if (defn.isProductSubType(unapplyResult)) return productSelectors(unapplyResult)
      }
      if (unapplyResult derivesFrom defn.SeqClass) seqSelector :: Nil
      else if (unapplyResult isRef defn.BooleanClass) Nil
      else {
        ctx.error(s"${unapplyResult.show} is not a valid result type of an unapply method of an extractor", tree.pos)
        Nil
      }
    }

    /** Can `subtp` be made to be a subtype of `tp`, possibly by dropping some
     *  refinements in `tp`?
     */
    def isSubTypeOfParent(subtp: Type, tp: Type): Boolean =
      if (subtp <:< tp) true
      else tp match {
        case RefinedType(parent, _) => isSubTypeOfParent(subtp, parent)
        case _ => false
      }

    unapply.tpe.widen match {
      case mt: MethodType if mt.paramTypes.length == 1 && !mt.isDependent =>
        val unapplyArgType = mt.paramTypes.head
        unapp.println(s"unapp arg tpe = ${unapplyArgType.show}, pt = ${pt.show}")
        def wpt = widenForMatchSelector(pt) // needed?
        val ownType =
          if (pt <:< unapplyArgType) {
            fullyDefinedType(unapplyArgType, "extractor argument", tree.pos)
            unapp.println(i"case 1 $unapplyArgType ${ctx.typerState.constraint}")
            pt
          } else if (isSubTypeOfParent(unapplyArgType, wpt)) {
            maximizeType(unapplyArgType) match {
              case Some(tvar) =>
                def msg =
                  i"""There is no best instantiation of pattern type $unapplyArgType
                     |that makes it a subtype of selector type $pt.
                     |Non-variant type variable ${tvar.origin} cannot be uniquely instantiated.""".stripMargin
                if (fromScala2x) {
                  // We can't issue an error here, because in Scala 2, ::[B] is invariant
                  // whereas List[+T] is covariant. According to the strict rule, a pattern
                  // match of a List[C] against a case x :: xs is illegal, because
                  // B cannot be uniquely instantiated. Of course :: should have been
                  // covariant in the first place, but in the Scala libraries it isn't.
                  // So for now we allow these kinds of patterns, even though they
                  // can open unsoundness holes. See SI-7952 for an example of the hole this opens.
                  if (ctx.settings.verbose.value) ctx.warning(msg, tree.pos)
                } else {
                  unapp.println(s" ${unapply.symbol.owner} ${unapply.symbol.owner is Scala2x}")
                  ctx.error(msg, tree.pos)
                }
              case _ =>
            }
            unapp.println(i"case 2 $unapplyArgType ${ctx.typerState.constraint}")
            unapplyArgType
          } else {
            unapp.println("Neither sub nor super")
            unapp.println(TypeComparer.explained(implicit ctx => unapplyArgType <:< wpt))
            errorType(
              i"Pattern type $unapplyArgType is neither a subtype nor a supertype of selector type $wpt",
              tree.pos)
          }

        val dummyArg = dummyTreeOfType(unapplyArgType)
        val unapplyApp = typedExpr(untpd.TypedSplice(Apply(unapply, dummyArg :: Nil)))
        val unapplyImplicits = unapplyApp match {
          case Apply(Apply(unapply, `dummyArg` :: Nil), args2) => assert(args2.nonEmpty); args2
          case Apply(unapply, `dummyArg` :: Nil) => Nil
        }

        var argTypes = unapplyArgs(unapplyApp.tpe)
        val bunchedArgs = argTypes match {
          case argType :: Nil if argType.isRepeatedParam => untpd.SeqLiteral(args) :: Nil
          case _ => args
        }
        if (argTypes.length != bunchedArgs.length) {
          ctx.error(i"wrong number of argument patterns for $qual; expected: ($argTypes%, %)", tree.pos)
          argTypes = argTypes.take(args.length) ++
            List.fill(argTypes.length - args.length)(WildcardType)
        }
        val unapplyPatterns = (bunchedArgs, argTypes).zipped map (typed(_, _))
        val result = cpy.UnApply(tree, unapply, unapplyImplicits, unapplyPatterns) withType ownType
        unapp.println(s"unapply patterns = $unapplyPatterns")
        if ((ownType eq pt) || ownType.isError) result
        else Typed(result, TypeTree(ownType))
      case tp =>
        val unapplyErr = if (tp.isError) unapply else notAnExtractor(unapply)
        val typedArgsErr = args mapconserve (typed(_, defn.AnyType))
        cpy.UnApply(tree, unapplyErr, Nil, typedArgsErr) withType ErrorType
    }
  }

  /** Is given method reference applicable to type arguments `targs` and argument trees `args`?
   *  @param  resultType   The expected result type of the application
   */
  def isApplicable(methRef: TermRef, targs: List[Type], args: List[Tree], resultType: Type)(implicit ctx: Context): Boolean = {
    val nestedContext = ctx.fresh.withExploreTyperState
    new ApplicableToTrees(methRef, targs, args, resultType)(nestedContext).success
  }

  /** Is given method reference applicable to argument types `args`?
   *  @param  resultType   The expected result type of the application
   */
  def isApplicable(methRef: TermRef, args: List[Type], resultType: Type)(implicit ctx: Context): Boolean = {
    val nestedContext = ctx.fresh.withExploreTyperState
    new ApplicableToTypes(methRef, args, resultType)(nestedContext).success
  }

  /** Is given type applicable to type arguments `targs` and argument trees `args`,
   *  possibly after inserting an `apply`?
   *  @param  resultType   The expected result type of the application
   */
  def isApplicable(tp: Type, targs: List[Type], args: List[Tree], resultType: Type)(implicit ctx: Context): Boolean =
    onMethod(tp, isApplicable(_, targs, args, resultType))

  /** Is given type applicable to argument types `args`, possibly after inserting an `apply`?
   *  @param  resultType   The expected result type of the application
   */
  def isApplicable(tp: Type, args: List[Type], resultType: Type)(implicit ctx: Context): Boolean =
    onMethod(tp, isApplicable(_, args, resultType))

  private def onMethod(tp: Type, p: TermRef => Boolean)(implicit ctx: Context): Boolean = tp match {
    case methRef: TermRef if methRef.widenSingleton.isInstanceOf[SignedType] =>
      p(methRef)
    case mt: SignedType =>
      p(mt.narrow)
    case _ =>
      tp.member(nme.apply).hasAltWith(d => p(TermRef(tp, nme.apply, d)))
  }

  /** In a set of overloaded applicable alternatives, is `alt1` at least as good as
   *  `alt2`? `alt1` and `alt2` are nonoverloaded references.
   */
  def isAsGood(alt1: TermRef, alt2: TermRef)(implicit ctx: Context): Boolean = track("isAsGood") { ctx.traceIndented(i"isAsGood($alt1, $alt2)", overload) {

    assert(alt1 ne alt2)

    /** Is class or module class `sym1` derived from class or module class `sym2`?
     *  Module classes also inherit the relationship from their companions.
     */
    def isDerived(sym1: Symbol, sym2: Symbol): Boolean =
      if (sym1 isSubClass sym2) true
      else if (sym2 is Module) isDerived(sym1, sym2.companionClass)
      else (sym1 is Module) && isDerived(sym1.companionClass, sym2)

    /** Is alternative `alt1` with type `tp1` as specific as alternative
     *  `alt2` with type `tp2` ? This is the case if
     *
     *    1. `tp2` is a method or poly type but `tp1` isn't, or `tp1` is nullary.
     *    2. `tp2` and `tp1` are method or poly types and `tp2` can be applied to the parameters of `tp1`.
     *    3. Neither `tp1` nor `tp2` are method or poly types and `tp1` is compatible with `tp2`.
     */
    def isAsSpecific(alt1: TermRef, tp1: Type, alt2: TermRef, tp2: Type): Boolean = ctx.traceIndented(i"isAsSpecific $tp1 $tp2", overload) { tp1 match {
      case tp1: PolyType =>
        def bounds(tparamRefs: List[TypeRef]) = tp1.paramBounds map (_.substParams(tp1, tparamRefs))
        val tparams = ctx.newTypeParams(alt1.symbol.owner, tp1.paramNames, EmptyFlags, bounds)
        isAsSpecific(alt1, tp1.instantiate(tparams map (_.typeRef)), alt2, tp2)
      case tp1: MethodType =>
        def repeatedToSingle(tp: Type) = if (tp.isRepeatedParam) tp.typeArgs.head else tp
        isApplicable(alt2, tp1.paramTypes map repeatedToSingle, WildcardType) ||
        tp1.paramTypes.isEmpty && tp2.isInstanceOf[MethodOrPoly]
      case _ =>
        tp2 match {
          case tp2: MethodOrPoly => true
          case _ => isCompatible(tp1, tp2)
        }
    }}

    /** Drop any implicit parameter section */
    def stripImplicit(tp: Type) = tp match {
      case mt: ImplicitMethodType if !mt.isDependent => mt.resultType // todo: make sure implicit method types are not dependent
      case _ => tp
    }

    val owner1 = alt1.symbol.owner
    val owner2 = alt2.symbol.owner
    val tp1 = stripImplicit(alt1.widen)
    val tp2 = stripImplicit(alt2.widen)

    def winsOwner1 = isDerived(owner1, owner2)
    def winsType1  = isAsSpecific(alt1, tp1, alt2, tp2)
    def winsOwner2 = isDerived(owner2, owner1)
    def winsType2  = isAsSpecific(alt2, tp2, alt1, tp1)

    // Assume the following probabilities:
    //
    // P(winsOwnerX) = 2/3
    // P(winsTypeX) = 1/3
    //
    // Then the call probabilities of the 4 basic operations are as follows:
    //
    // winsOwner1: 1/1
    // winsOwner2: 1/1
    // winsType1 : 7/9
    // winsType2 : 4/9

    if (winsOwner1) /* 6/9 */ !winsOwner2 || /* 4/9 */ winsType1 || /* 8/27 */ !winsType2
    else if (winsOwner2) /* 2/9 */ winsType1 && /* 2/27 */ !winsType2
    else /* 1/9 */ winsType1 || /* 2/27 */ !winsType2
  }}

  def narrowMostSpecific(alts: List[TermRef])(implicit ctx: Context): List[TermRef] = track("narrowMostSpecific") {
    (alts: @unchecked) match {
      case alt :: alts1 =>
        def winner(bestSoFar: TermRef, alts: List[TermRef]): TermRef = alts match {
          case alt :: alts1 =>
            winner(if (isAsGood(alt, bestSoFar)) alt else bestSoFar, alts1)
          case nil =>
            bestSoFar
        }
        val best = winner(alt, alts1)
        def asGood(alts: List[TermRef]): List[TermRef] = alts match {
          case alt :: alts1 =>
            if ((alt eq best) || !isAsGood(alt, best)) asGood(alts1)
            else alt :: asGood(alts1)
          case nil =>
            Nil
        }
        best :: asGood(alts)
    }
  }

  /** Resolve overloaded alternative `alts`, given expected type `pt` and
   *  possibly also type argument `targs` that need to be applied to each alternative
   *  to form the method type.
   *  todo: use techniques like for implicits to pick candidates quickly?
   */
  def resolveOverloaded(alts: List[TermRef], pt: Type, targs: List[Type] = Nil)(implicit ctx: Context): List[TermRef] = track("resolveOverloaded") {

    def isDetermined(alts: List[TermRef]) = alts.isEmpty || alts.tail.isEmpty

    /** The shape of given tree as a type; cannot handle named arguments. */
    def typeShape(tree: untpd.Tree): Type = tree match {
      case untpd.Function(args, body) =>
        defn.FunctionType(args map Function.const(defn.AnyType), typeShape(body))
      case _ =>
        defn.NothingType
    }

    /** The shape of given tree as a type; is more expensive than
     *  typeShape but can can handle named arguments.
     */
    def treeShape(tree: untpd.Tree): Tree = tree match {
      case NamedArg(name, arg) =>
        val argShape = treeShape(arg)
        cpy.NamedArg(tree, name, argShape).withType(argShape.tpe)
      case _ =>
        dummyTreeOfType(typeShape(tree))
    }

    def narrowByTypes(alts: List[TermRef], argTypes: List[Type], resultType: Type): List[TermRef] =
      alts filter (isApplicable(_, argTypes, resultType))

    val candidates = pt match {
      case pt @ FunProto(args, resultType, _) =>
        val numArgs = args.length

        def sizeFits(alt: TermRef, tp: Type): Boolean = tp match {
          case tp: PolyType => sizeFits(alt, tp.resultType)
          case MethodType(_, ptypes) =>
            val numParams = ptypes.length
            def isVarArgs = ptypes.nonEmpty && ptypes.last.isRepeatedParam
            def hasDefault = alt.symbol.hasDefaultParams
            if (numParams == numArgs) true
            else if (numParams < numArgs) isVarArgs
            else if (numParams > numArgs + 1) hasDefault
            else isVarArgs || hasDefault
          case _ =>
            numArgs == 0
        }

        def narrowBySize(alts: List[TermRef]): List[TermRef] =
          alts filter (alt => sizeFits(alt, alt.widen))

        def narrowByShapes(alts: List[TermRef]): List[TermRef] =
          if (args exists (_.isInstanceOf[untpd.Function]))
            if (args exists (_.isInstanceOf[Trees.NamedArg[_]]))
              narrowByTrees(alts, args map treeShape, resultType)
            else
              narrowByTypes(alts, args map typeShape, resultType)
          else
            alts

        def narrowByTrees(alts: List[TermRef], args: List[Tree], resultType: Type): List[TermRef] =
          alts filter (isApplicable(_, targs, args, resultType))

        val alts1 = narrowBySize(alts)
        if (isDetermined(alts1)) alts1
        else {
          val alts2 = narrowByShapes(alts1)
          if (isDetermined(alts2)) alts2
          else narrowByTrees(alts2, pt.typedArgs, resultType)
        }

      case pt @ PolyProto(targs, pt1) =>
        val alts1 = alts filter pt.isMatchedBy
        resolveOverloaded(alts1, pt1, targs)

      case defn.FunctionType(args, resultType) =>
        narrowByTypes(alts, args, resultType)

      case pt =>
        alts filter (normalizedCompatible(_, pt))
    }
    if (isDetermined(candidates)) candidates
    else narrowMostSpecific(candidates)
  }
}

/*
  def typedApply(app: untpd.Apply, fun: Tree, methRef: TermRef, args: List[Tree], resultType: Type)(implicit ctx: Context): Tree = track("typedApply") {
    new ApplyToTyped(app, fun, methRef, args, resultType).result
  }

  def typedApply(fun: Tree, methRef: TermRef, args: List[Tree], resultType: Type)(implicit ctx: Context): Tree =
    typedApply(untpd.Apply(untpd.TypedSplice(fun), args), fun, methRef, args, resultType)
*/