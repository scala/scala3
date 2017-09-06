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
import config.Config
import Names._
import StdNames._
import NameKinds.DefaultGetterName
import ProtoTypes._
import EtaExpansion._
import Inferencing._

import collection.mutable
import config.Printers.{typr, unapp, overload}
import TypeApplications._

import language.implicitConversions
import reporting.diagnostic.Message
import Constants.{Constant, IntTag, LongTag}

import scala.collection.mutable.ListBuffer

object Applications {
  import tpd._

  def extractorMember(tp: Type, name: Name)(implicit ctx: Context) =
    tp.member(name).suchThat(_.info.isParameterless)

  def extractorMemberType(tp: Type, name: Name, errorPos: Position = NoPosition)(implicit ctx: Context) = {
    val ref = extractorMember(tp, name)
    if (ref.isOverloaded)
      errorType(i"Overloaded reference to $ref is not allowed in extractor", errorPos)
    ref.info.widenExpr.dealias
  }

  /** Does `tp` fit the "product match" conditions as an unapply result type
   *  for a pattern with `numArgs` subpatterns?
   *  This is the case of `tp` has members `_1` to `_N` where `N == numArgs`.
   */
  def isProductMatch(tp: Type, numArgs: Int)(implicit ctx: Context) =
    numArgs > 0 && productArity(tp) == numArgs

  /** Does `tp` fit the "get match" conditions as an unapply result type?
   *  This is the case of `tp` has a `get` member as well as a
   *  parameterless `isDefined` member of result type `Boolean`.
   */
  def isGetMatch(tp: Type, errorPos: Position = NoPosition)(implicit ctx: Context) =
    extractorMemberType(tp, nme.isEmpty, errorPos).isRef(defn.BooleanClass) &&
    extractorMemberType(tp, nme.get, errorPos).exists

  def productSelectorTypes(tp: Type, errorPos: Position = NoPosition)(implicit ctx: Context): List[Type] = {
    val sels = for (n <- Iterator.from(0)) yield extractorMemberType(tp, nme.selectorName(n), errorPos)
    sels.takeWhile(_.exists).toList
  }

  def productArity(tp: Type)(implicit ctx: Context) =
    if (defn.isProductSubType(tp)) productSelectorTypes(tp).size else -1

  def productSelectors(tp: Type)(implicit ctx: Context): List[Symbol] = {
    val sels = for (n <- Iterator.from(0)) yield
      tp.member(nme.selectorName(n)).suchThat(_.info.isParameterless).symbol
    sels.takeWhile(_.exists).toList
  }

  def getUnapplySelectors(tp: Type, args: List[untpd.Tree], pos: Position = NoPosition)(implicit ctx: Context): List[Type] =
    if (args.length > 1 && !(tp.derivesFrom(defn.SeqClass))) {
      val sels = productSelectorTypes(tp, pos)
      if (sels.length == args.length) sels
      else tp :: Nil
    } else tp :: Nil

  def unapplyArgs(unapplyResult: Type, unapplyFn: Tree, args: List[untpd.Tree], pos: Position = NoPosition)(implicit ctx: Context): List[Type] = {

    val unapplyName = unapplyFn.symbol.name
    def seqSelector = defn.RepeatedParamType.appliedTo(unapplyResult.elemType :: Nil)
    def getTp = extractorMemberType(unapplyResult, nme.get, pos)

    def fail = {
      ctx.error(i"$unapplyResult is not a valid result type of an $unapplyName method of an extractor", pos)
      Nil
    }

    if (unapplyName == nme.unapplySeq) {
      if (unapplyResult derivesFrom defn.SeqClass) seqSelector :: Nil
      else if (isGetMatch(unapplyResult, pos)) {
        val seqArg = boundsToHi(getTp.elemType)
        if (seqArg.exists) args.map(Function.const(seqArg))
        else fail
      }
      else fail
    }
    else {
      assert(unapplyName == nme.unapply)
      if (isProductMatch(unapplyResult, args.length))
        productSelectorTypes(unapplyResult)
      else if (isGetMatch(unapplyResult, pos))
        getUnapplySelectors(getTp, args, pos)
      else if (unapplyResult isRef defn.BooleanClass)
        Nil
      else if (defn.isProductSubType(unapplyResult))
        productSelectorTypes(unapplyResult)
          // this will cause a "wrong number of arguments in pattern" error later on,
          // which is better than the message in `fail`.
      else fail
    }
  }

  def wrapDefs(defs: mutable.ListBuffer[Tree], tree: Tree)(implicit ctx: Context): Tree =
    if (defs != null && defs.nonEmpty) tpd.Block(defs.toList, tree) else tree
}

import Applications._

trait Applications extends Compatibility { self: Typer with Dynamic =>

  import Applications._
  import tpd.{ cpy => _, _ }
  import untpd.cpy
  import Dynamic.isDynamicMethod

  /** @tparam Arg       the type of arguments, could be tpd.Tree, untpd.Tree, or Type
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

    /** If all `args` have primitive numeric types, make sure it's the same one */
    protected def harmonizeArgs(args: List[TypedArg]): List[TypedArg]

    /** Signal failure with given message at position of given argument */
    protected def fail(msg: => Message, arg: Arg): Unit

    /** Signal failure with given message at position of the application itself */
    protected def fail(msg: => Message): Unit

    protected def appPos: Position

    /** The current function part, which might be affected by lifting.
     */
    protected def normalizedFun: Tree

    protected def typeOfArg(arg: Arg): Type

    /** If constructing trees, pull out all parts of the function
     *  which are not idempotent into separate prefix definitions
     */
    protected def liftFun(): Unit = ()

    /** A flag signalling that the typechecking the application was so far successful */
    private[this] var _ok = true

    def ok = _ok
    def ok_=(x: Boolean) = {
      assert(x || ctx.reporter.errorsReported || !ctx.typerState.isCommittable) // !!! DEBUG
      _ok = x
    }

    /** The function's type after widening and instantiating polytypes
     *  with TypeParamRefs in constraint set
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
        val resultApprox = resultTypeApprox(methType)
        if (!constrainResult(resultApprox, resultType))
          if (ctx.typerState.isCommittable)
            // defer the problem until after the application;
            // it might be healed by an implicit conversion
            ()
          else
            fail(err.typeMismatchMsg(methType.resultType, resultType))
        // match all arguments with corresponding formal parameters
        matchArgs(orderedArgs, methType.paramInfos, 0)
      case _ =>
        if (methType.isError) ok = false
        else fail(s"$methString does not take parameters")
    }

    /** The application was successful */
    def success = ok

    protected def methodType = methType.asInstanceOf[MethodType]
    private def methString: String = i"${methRef.symbol}: ${methType.show}"

    /** Re-order arguments to correctly align named arguments */
    def reorder[T >: Untyped](args: List[Trees.Tree[T]]): List[Trees.Tree[T]] = {

      /** @param pnames    The list of parameter names that are missing arguments
       *  @param args      The list of arguments that are not yet passed, or that are waiting to be dropped
       *  @param nameToArg A map from as yet unseen names to named arguments
       *  @param toDrop    A set of names that have already be passed as named arguments
       *
       *  For a well-typed application we have the invariants
       *
       *  1. `(args diff toDrop)` can be reordered to match `pnames`
       *  2. For every `(name -> arg)` in `nameToArg`, `arg` is an element of `args`
       */
      def handleNamed(pnames: List[Name], args: List[Trees.Tree[T]],
                      nameToArg: Map[Name, Trees.NamedArg[T]], toDrop: Set[Name]): List[Trees.Tree[T]] = pnames match {
        case pname :: pnames1 if nameToArg contains pname =>
          // there is a named argument for this parameter; pick it
          nameToArg(pname) :: handleNamed(pnames1, args, nameToArg - pname, toDrop + pname)
        case _ =>
          def pnamesRest = if (pnames.isEmpty) pnames else pnames.tail
          args match {
            case (arg @ NamedArg(aname, _)) :: args1 =>
              if (toDrop contains aname) // argument is already passed
                handleNamed(pnames, args1, nameToArg, toDrop - aname)
              else if ((nameToArg contains aname) && pnames.nonEmpty) // argument is missing, pass an empty tree
                genericEmptyTree :: handleNamed(pnames.tail, args, nameToArg, toDrop)
              else { // name not (or no longer) available for named arg
                def msg =
                  if (methodType.paramNames contains aname)
                    s"parameter $aname of $methString is already instantiated"
                  else
                    s"$methString does not have a parameter $aname"
                fail(msg, arg.asInstanceOf[Arg])
                arg :: handleNamed(pnamesRest, args1, nameToArg, toDrop)
              }
            case arg :: args1 =>
              arg :: handleNamed(pnamesRest, args1, nameToArg, toDrop) // unnamed argument; pick it
            case Nil => // no more args, continue to pick up any preceding named args
              if (pnames.isEmpty) Nil
              else handleNamed(pnamesRest, args, nameToArg, toDrop)
          }
      }

      def handlePositional(pnames: List[Name], args: List[Trees.Tree[T]]): List[Trees.Tree[T]] =
        args match {
          case (arg: NamedArg @unchecked) :: _ =>
            val nameAssocs = for (arg @ NamedArg(name, _) <- args) yield (name, arg)
            handleNamed(pnames, args, nameAssocs.toMap, Set())
          case arg :: args1 =>
            arg :: handlePositional(if (pnames.isEmpty) Nil else pnames.tail, args1)
          case Nil => Nil
        }

      handlePositional(methodType.paramNames, args)
    }

    /** Splice new method reference into existing application */
    def spliceMeth(meth: Tree, app: Tree): Tree = app match {
      case Apply(fn, args) =>
        spliceMeth(meth, fn).appliedToArgs(args)
      case TypeApply(fn, targs) =>
        // Note: It is important that the type arguments `targs` are passed in new trees
        // instead of being spliced in literally. Otherwise, a type argument to a default
        // method could be constructed as the definition site of the type variable for
        // that default constructor. This would interpolate type variables too early,
        // causing lots of tests (among them tasty_unpickleScala2) to fail.
        //
        // The test case is in i1757.scala. Here we have a variable `s` and a method `cpy`
        // defined like this:
        //
        //      var s
        //      def cpy[X](b: List[Int] = b): B[X] = new B[X](b)
        //
        // The call `s.cpy()` then gets expanded to
        //
        //      { val $1$: B[Int] = this.s
        //        $1$.cpy[X']($1$.cpy$default$1[X']
        //      }
        //
        // A type variable gets interpolated if it does not appear in the type
        // of the current tree and the current tree contains the variable's "definition".
        // Previously, the polymorphic function tree to which the variable was first added
        // was taken as the variable's definition. But that fails here because that
        // tree was `s.cpy` but got transformed into `$1$.cpy`. We now take the type argument
        // [X'] of the variable as its definition tree, which is more robust. But then
        // it's crucial that the type tree is not copied directly as argument to
        // `cpy$default$1`. If it was, the variable `X'` would already be interpolated
        // when typing the default argument, which is too early.
        spliceMeth(meth, fn).appliedToTypes(targs.tpes)
      case _ => meth
    }

    /** Find reference to default parameter getter for parameter #n in current
     *  parameter list, or NoType if none was found
     */
    def findDefaultGetter(n: Int)(implicit ctx: Context): Tree = {
      val meth = methRef.symbol.asTerm
      val receiver: Tree = methPart(normalizedFun) match {
        case Select(receiver, _) => receiver
        case mr => mr.tpe.normalizedPrefix match {
          case mr: TermRef => ref(mr)
          case mr =>
            if (this.isInstanceOf[TestApplication[_]])
              // In this case it is safe to skolemize now; we will produce a stable prefix for the actual call.
              ref(mr.narrow)
            else
              EmptyTree
        }
      }
      val getterPrefix =
        if ((meth is Synthetic) && meth.name == nme.apply) nme.CONSTRUCTOR else meth.name
      def getterName = DefaultGetterName(getterPrefix, n)
      if (!meth.hasDefaultParams)
        EmptyTree
      else if (receiver.isEmpty) {
        def findGetter(cx: Context): Tree = {
          if (cx eq NoContext) EmptyTree
          else if (cx.scope != cx.outer.scope &&
            cx.denotNamed(meth.name).hasAltWith(_.symbol == meth)) {
            val denot = cx.denotNamed(getterName)
            if (denot.exists) ref(TermRef(cx.owner.thisType, getterName, denot))
            else {
              assert(ctx.mode.is(Mode.Interactive),
                s"non-existent getter denotation ($denot) for getter($getterName)")
              findGetter(cx.outer)
            }
          } else findGetter(cx.outer)
        }
        findGetter(ctx)
      }
      else {
        def selectGetter(qual: Tree): Tree = {
          val getterDenot = qual.tpe.member(getterName)
          if (getterDenot.exists) qual.select(TermRef(qual.tpe, getterName, getterDenot))
          else EmptyTree
        }
        if (!meth.isClassConstructor)
          selectGetter(receiver)
        else {
          // default getters for class constructors are found in the companion object
          val cls = meth.owner
          val companion = cls.companionModule
          receiver.tpe.baseTypeRef(cls) match {
            case tp: TypeRef if companion.isTerm =>
              selectGetter(ref(TermRef(tp.prefix, companion.asTerm)))
            case _ =>
              EmptyTree
          }
        }
      }
    }

    /** Is `sym` a constructor of a Java-defined annotation? */
    def isJavaAnnotConstr(sym: Symbol) =
      sym.is(JavaDefined) && sym.isConstructor && sym.owner.derivesFrom(defn.AnnotationClass)

    /** Match re-ordered arguments against formal parameters
     *  @param n   The position of the first parameter in formals in `methType`.
     */
    def matchArgs(args: List[Arg], formals: List[Type], n: Int): Unit = {
      if (success) formals match {
        case formal :: formals1 =>

          /** Add result of typing argument `arg` against parameter type `formal`.
           *  @return  A type transformation to apply to all arguments following this one.
           */
          def addTyped(arg: Arg, formal: Type): Type => Type = {
            addArg(typedArg(arg, formal), formal)
            if (methodType.isParamDependent)
              safeSubstParam(_, methodType.paramRefs(n), typeOfArg(arg))
            else identity
          }

          def missingArg(n: Int): Unit = {
            val pname = methodType.paramNames(n)
            fail(
              if (pname.firstPart contains '$') s"not enough arguments for $methString"
              else s"missing argument for parameter $pname of $methString")
          }

          def tryDefault(n: Int, args1: List[Arg]): Unit = {
            if (!isJavaAnnotConstr(methRef.symbol)) liftFun()
            val getter = findDefaultGetter(n + numArgs(normalizedFun))
            if (getter.isEmpty) missingArg(n)
            else {
              val substParam = addTyped(
                  treeToArg(spliceMeth(getter withPos normalizedFun.pos, normalizedFun)),
                  formal)
              matchArgs(args1, formals1.mapconserve(substParam), n + 1)
            }
          }

          if (formal.isRepeatedParam)
            args match {
              case arg :: Nil if isVarArg(arg) =>
                addTyped(arg, formal)
              case _ =>
                val elemFormal = formal.widenExpr.argTypesLo.head
                val origConstraint = ctx.typerState.constraint
                var typedArgs = args.map(typedArg(_, elemFormal))
                val harmonizedArgs = harmonizeArgs(typedArgs)
                if (harmonizedArgs ne typedArgs) {
                  ctx.typerState.constraint = origConstraint
                    // reset constraint, we will re-establish constraint anyway when we
                    // compare against the seqliteral. The reset is needed
                    // otherwise pos/harmonize.scala would fail on line 40.
                  typedArgs = harmonizedArgs
                }
                typedArgs.foreach(addArg(_, elemFormal))
                makeVarArg(args.length, elemFormal)
            }
          else args match {
            case EmptyTree :: args1 =>
              tryDefault(n, args1)
            case arg :: args1 =>
              val substParam = addTyped(arg, formal)
              matchArgs(args1, formals1.mapconserve(substParam), n + 1)
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

    protected def argOK(arg: TypedArg, formal: Type) = argType(arg, formal) match {
      case ref: TermRef if ref.denot.isOverloaded =>
        // in this case we could not resolve overloading because no alternative
        // matches expected type
        false
      case argtpe =>
        def SAMargOK = formal match {
          case SAMType(meth) => argtpe <:< meth.info.toFunctionType()
          case _ => false
        }
        isCompatible(argtpe, formal) || ctx.mode.is(Mode.ImplicitsEnabled) && SAMargOK
    }

    /** The type of the given argument */
    protected def argType(arg: Arg, formal: Type): Type

    def typedArg(arg: Arg, formal: Type): Arg = arg
    final def addArg(arg: TypedArg, formal: Type) = ok = ok & argOK(arg, formal)
    def makeVarArg(n: Int, elemFormal: Type) = {}
    def fail(msg: => Message, arg: Arg) =
      ok = false
    def fail(msg: => Message) =
      ok = false
    def appPos = NoPosition
    lazy val normalizedFun = ref(methRef)
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
    def typeOfArg(arg: Tree): Type = arg.tpe
    def harmonizeArgs(args: List[Tree]) = harmonize(args)
  }

  /** Subclass of Application for applicability tests with type arguments and value
   * argument trees.
   */
  class ApplicableToTreesDirectly(methRef: TermRef, targs: List[Type], args: List[Tree], resultType: Type)(implicit ctx: Context) extends ApplicableToTrees(methRef, targs, args, resultType)(ctx) {
    override def argOK(arg: TypedArg, formal: Type) = argType(arg, formal) <:< formal
  }

  /** Subclass of Application for applicability tests with value argument types. */
  class ApplicableToTypes(methRef: TermRef, args: List[Type], resultType: Type)(implicit ctx: Context)
  extends TestApplication(methRef, methRef, args, resultType) {
    def argType(arg: Type, formal: Type): Type = arg
    def treeToArg(arg: Tree): Type = arg.tpe
    def isVarArg(arg: Type): Boolean = arg.isRepeatedParam
    def typeOfArg(arg: Type): Type = arg
    def harmonizeArgs(args: List[Type]) = harmonizeTypes(args)
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
      val elemtpt = TypeTree(elemFormal)
      val seqLit =
        if (methodType.isJava) JavaSeqLiteral(args, elemtpt)
        else SeqLiteral(args, elemtpt)
      typedArgBuf += seqToRepeated(seqLit)
    }

    def harmonizeArgs(args: List[TypedArg]) = harmonize(args)

    override def appPos = app.pos

    def fail(msg: => Message, arg: Trees.Tree[T]) = {
      ctx.error(msg, arg.pos)
      ok = false
    }

    def fail(msg: => Message) = {
      ctx.error(msg, app.pos)
      ok = false
    }

    def normalizedFun = myNormalizedFun

    override def liftFun(): Unit =
      if (liftedDefs == null) {
        liftedDefs = new mutable.ListBuffer[Tree]
        myNormalizedFun = liftApp(liftedDefs, myNormalizedFun)
      }

    /** The index of the first difference between lists of trees `xs` and `ys`
     *  -1 if there are no differences.
     */
    private def firstDiff[T <: Trees.Tree[_]](xs: List[T], ys: List[T], n: Int = 0): Int = xs match {
      case x :: xs1 =>
        ys match {
          case y :: ys1 => if (x ne y) n else firstDiff(xs1, ys1, n + 1)
          case nil => n
        }
      case nil =>
        ys match {
          case y :: ys1 => n
          case nil => -1
        }
    }
    private def sameSeq[T <: Trees.Tree[_]](xs: List[T], ys: List[T]): Boolean = firstDiff(xs, ys) < 0

    val result = {
      var typedArgs = typedArgBuf.toList
      def app0 = cpy.Apply(app)(normalizedFun, typedArgs) // needs to be a `def` because typedArgs can change later
      val app1 =
        if (!success) app0.withType(UnspecifiedErrorType)
        else {
          if (!sameSeq(args, orderedArgs.dropWhile(_ eq EmptyTree)) && !isJavaAnnotConstr(methRef.symbol)) {
            // need to lift arguments to maintain evaluation order in the
            // presence of argument reorderings.

            liftFun()

            // lift arguments in the definition order
            val argDefBuf = mutable.ListBuffer.empty[Tree]
            typedArgs = liftArgs(argDefBuf, methType, typedArgs)

            // Lifted arguments ordered based on the original order of typedArgBuf and
            // with all non-explicit default parameters at the end in declaration order.
            val orderedArgDefs = {
              // List of original arguments that are lifted by liftArgs
              val impureArgs = typedArgBuf.filterNot(isPureExpr)
              // Assuming stable sorting all non-explicit default parameters will remain in the end with the same order
              val defaultParamIndex = args.size
              // Mapping of index of each `liftable` into original args ordering
              val indices = impureArgs.map { arg =>
                val idx = args.indexOf(arg)
                if (idx >= 0) idx // original index skipping pure arguments
                else defaultParamIndex
              }
              scala.util.Sorting.stableSort[(Tree, Int), Int](argDefBuf zip indices, x => x._2).map(_._1)
            }

            liftedDefs ++= orderedArgDefs
          }
          if (sameSeq(typedArgs, args)) // trick to cut down on tree copying
            typedArgs = args.asInstanceOf[List[Tree]]
          assignType(app0, normalizedFun, typedArgs)
        }
      wrapDefs(liftedDefs, app1)
    }
  }

  /** Subclass of Application for type checking an Apply node with untyped arguments. */
  class ApplyToUntyped(app: untpd.Apply, fun: Tree, methRef: TermRef, proto: FunProto, resultType: Type)(implicit ctx: Context)
  extends TypedApply(app, fun, methRef, proto.args, resultType) {
    def typedArg(arg: untpd.Tree, formal: Type): TypedArg = proto.typedArg(arg, formal.widenExpr)
    def treeToArg(arg: Tree): untpd.Tree = untpd.TypedSplice(arg)
    def typeOfArg(arg: untpd.Tree) = proto.typeOfArg(arg)
  }

  /** Subclass of Application for type checking an Apply node with typed arguments. */
  class ApplyToTyped(app: untpd.Apply, fun: Tree, methRef: TermRef, args: List[Tree], resultType: Type)(implicit ctx: Context)
  extends TypedApply[Type](app, fun, methRef, args, resultType) {
      // Dotty deviation: Dotc infers Untyped for the supercall. This seems to be according to the rules
      // (of both Scala and Dotty). Untyped is legal, and a subtype of Typed, whereas TypeApply
      // is invariant in the type parameter, so the minimal type should be inferred. But then typedArg does
      // not match the abstract method in Application and an abstract class error results.
    def typedArg(arg: tpd.Tree, formal: Type): TypedArg = arg
    def treeToArg(arg: Tree): Tree = arg
    def typeOfArg(arg: Tree) = arg.tpe
  }

  /** If `app` is a `this(...)` constructor call, the this-call argument context,
   *  otherwise the current context.
   */
  def argCtx(app: untpd.Tree)(implicit ctx: Context): Context =
    if (untpd.isSelfConstrCall(app)) ctx.thisCallArgContext else ctx

  def typedApply(tree: untpd.Apply, pt: Type)(implicit ctx: Context): Tree = {

    def realApply(implicit ctx: Context): Tree = track("realApply") {
      val originalProto = new FunProto(tree.args, IgnoredProto(pt), this)(argCtx(tree))
      val fun1 = typedExpr(tree.fun, originalProto)

      // Warning: The following lines are dirty and fragile. We record that auto-tupling was demanded as
      // a side effect in adapt. If it was, we assume the tupled proto-type in the rest of the application,
      // until, possibly, we have to fall back to insert an implicit on the qualifier.
      // This crucially relies on he fact that `proto` is used only in a single call of `adapt`,
      // otherwise we would get possible cross-talk between different `adapt` calls using the same
      // prototype. A cleaner alternative would be to return a modified prototype from `adapt` together with
      // a modified tree but this would be more convoluted and less efficient.
      val proto = if (originalProto.isTupled) originalProto.tupled else originalProto

      // If some of the application's arguments are function literals without explicitly declared
      // parameter types, relate the normalized result type of the application with the
      // expected type through `constrainResult`. This can add more constraints which
      // help sharpen the inferred parameter types for the argument function literal(s).
      // This tweak is needed to make i1378 compile.
      if (tree.args.exists(untpd.isFunctionWithUnknownParamType(_)))
        if (!constrainResult(fun1.tpe.widen, proto.derivedFunProto(resultType = pt)))
          typr.println(i"result failure for $tree with type ${fun1.tpe.widen}, expected = $pt")

      /** Type application where arguments come from prototype, and no implicits are inserted */
      def simpleApply(fun1: Tree, proto: FunProto)(implicit ctx: Context): Tree =
        methPart(fun1).tpe match {
          case funRef: TermRef =>
            val app =
              if (proto.allArgTypesAreCurrent())
                new ApplyToTyped(tree, fun1, funRef, proto.typedArgs, pt)
              else
                new ApplyToUntyped(tree, fun1, funRef, proto, pt)(argCtx(tree))
            convertNewGenericArray(ConstFold(app.result))
          case _ =>
            handleUnexpectedFunType(tree, fun1)
        }

      /** Try same application with an implicit inserted around the qualifier of the function
       *  part. Return an optional value to indicate success.
       */
      def tryWithImplicitOnQualifier(fun1: Tree, proto: FunProto)(implicit ctx: Context): Option[Tree] =
        tryInsertImplicitOnQualifier(fun1, proto) flatMap { fun2 =>
          tryEither {
            implicit ctx => Some(simpleApply(fun2, proto)): Option[Tree]
          } {
            (_, _) => None
          }
        }

      fun1.tpe match {
        case err: ErrorType => untpd.cpy.Apply(tree)(fun1, tree.args).withType(err)
        case TryDynamicCallType => typedDynamicApply(tree, pt)
        case _ =>
          if (originalProto.isDropped) fun1
          else
            tryEither {
              implicit ctx => simpleApply(fun1, proto)
            } {
              (failedVal, failedState) =>
                def fail = { failedState.commit(); failedVal }
                // Try once with original prototype and once (if different) with tupled one.
                // The reason we need to try both is that the decision whether to use tupled
                // or not was already taken but might have to be revised when an implicit
                // is inserted on the qualifier.
                tryWithImplicitOnQualifier(fun1, originalProto).getOrElse(
                  if (proto eq originalProto) fail
                  else tryWithImplicitOnQualifier(fun1, proto).getOrElse(fail))
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
    def typedOpAssign(implicit ctx: Context): Tree = track("typedOpAssign") {
      val Apply(Select(lhs, name), rhss) = tree
      val lhs1 = typedExpr(lhs)
      val liftedDefs = new mutable.ListBuffer[Tree]
      val lhs2 = untpd.TypedSplice(liftAssigned(liftedDefs, lhs1))
      val assign = untpd.Assign(lhs2,
          untpd.Apply(untpd.Select(lhs2, name.asSimpleName.dropRight(1)), rhss))
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
    else {
      val app = realApply
      app match {
        case Apply(fn @ Select(left, _), right :: Nil) if fn.hasType =>
          val op = fn.symbol
          if (op == defn.Any_== || op == defn.Any_!=)
            checkCanEqual(left.tpe.widen, right.tpe.widen, app.pos)
        case _ =>
      }
      app
    }
  }

  /** Overridden in ReTyper to handle primitive operations that can be generated after erasure */
  protected def handleUnexpectedFunType(tree: untpd.Apply, fun: Tree)(implicit ctx: Context): Tree =
    throw new Error(i"unexpected type.\n fun = $fun,\n methPart(fun) = ${methPart(fun)},\n methPart(fun).tpe = ${methPart(fun).tpe},\n tpe = ${fun.tpe}")

  def typedNamedArgs(args: List[untpd.Tree])(implicit ctx: Context) =
    for (arg @ NamedArg(id, argtpt) <- args) yield {
      val argtpt1 = typedType(argtpt)
      cpy.NamedArg(arg)(id, argtpt1).withType(argtpt1.tpe)
    }

  def typedTypeApply(tree: untpd.TypeApply, pt: Type)(implicit ctx: Context): Tree = track("typedTypeApply") {
    val isNamed = hasNamedArg(tree.args)
    val typedArgs = if (isNamed) typedNamedArgs(tree.args) else tree.args.mapconserve(typedType(_))
    val typedFn = typedExpr(tree.fun, PolyProto(typedArgs.tpes, pt))
    typedFn.tpe.widen match {
      case pt: PolyType =>
        if (typedArgs.length <= pt.paramInfos.length && !isNamed)
          if (typedFn.symbol == defn.Predef_classOf && typedArgs.nonEmpty) {
            val arg = typedArgs.head
            checkClassType(arg.tpe, arg.pos, traitReq = false, stablePrefixReq = false)
          }
      case _ =>
    }
    def tryDynamicTypeApply(): Tree = typedFn match {
      case typedFn: Select if !pt.isInstanceOf[FunProto] => typedDynamicSelect(typedFn, typedArgs, pt)
      case _                                             => tree.withType(TryDynamicCallType)
    }
    if (typedFn.tpe eq TryDynamicCallType) tryDynamicTypeApply()
    else assignType(cpy.TypeApply(tree)(typedFn, typedArgs), typedFn, typedArgs)
  }

  /** Rewrite `new Array[T](....)` if T is an unbounded generic to calls to newGenericArray.
   *  It is performed during typer as creation of generic arrays needs a classTag.
   *  we rely on implicit search to find one.
   */
  def convertNewGenericArray(tree: tpd.Tree)(implicit ctx: Context): tpd.Tree = tree match {
    case Apply(TypeApply(tycon, targs@(targ :: Nil)), args) if tycon.symbol == defn.ArrayConstructor =>
      fullyDefinedType(tree.tpe, "array", tree.pos)

      def newGenericArrayCall =
        ref(defn.DottyArraysModule)
          .select(defn.newGenericArrayMethod).withPos(tree.pos)
          .appliedToTypeTrees(targs).appliedToArgs(args)

      if (TypeErasure.isUnboundedGeneric(targ.tpe))
        newGenericArrayCall
      else tree
    case _ =>
      tree
  }

  def typedUnApply(tree: untpd.Apply, selType: Type)(implicit ctx: Context): Tree = track("typedUnApply") {
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
          val nestedCtx = ctx.fresh.setNewTyperState
          val ttree =
            typedType(untpd.rename(tree, tree.name.toTypeName))(nestedCtx)
          ttree.tpe match {
            case alias: TypeRef if alias.info.isAlias && !nestedCtx.reporter.hasErrors =>
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

    /** A typed qual.unapply or qual.unapplySeq tree, if this typechecks.
     *  Otherwise fallBack with (maltyped) qual.unapply as argument
     *  Note: requires special handling for overloaded occurrences of
     *  unapply or unapplySeq. We first try to find a non-overloaded
     *  method which matches any type. If that fails, we try to find an
     *  overloaded variant which matches one of the argument types.
     *  In fact, overloaded unapply's are problematic because a non-
     *  overloaded unapply does *not* need to be applicable to its argument
     *  whereas overloaded variants need to have a conforming variant.
     */
    def trySelectUnapply(qual: untpd.Tree)(fallBack: Tree => Tree): Tree = {
      // try first for non-overloaded, then for overloaded ocurrences
      def tryWithName(name: TermName)(fallBack: Tree => Tree)(implicit ctx: Context): Tree =
        tryEither { implicit ctx =>
          val specificProto = new UnapplyFunProto(selType, this)
          typedExpr(untpd.Select(qual, name), specificProto)
        } {
          (sel, _) =>
            tryEither { implicit ctx =>
              val genericProto = new UnapplyFunProto(WildcardType, this)
              typedExpr(untpd.Select(qual, name), genericProto)
            } {
              (_, _) => fallBack(sel)
            }
        }
      // try first for unapply, then for unapplySeq
      tryWithName(nme.unapply) {
        sel => tryWithName(nme.unapplySeq)(_ => fallBack(sel)) // for backwards compatibility; will be dropped
      }
    }

    /** Produce a typed qual.unapply or qual.unapplySeq tree, or
     *  else if this fails follow a type alias and try again.
     */
    val unapplyFn = trySelectUnapply(qual) { sel =>
      val qual1 = followTypeAlias(qual)
      if (qual1.isEmpty) notAnExtractor(sel)
      else trySelectUnapply(qual1)(_ => notAnExtractor(sel))
    }

    def fromScala2x = unapplyFn.symbol.exists && (unapplyFn.symbol.owner is Scala2x)

    /** Is `subtp` a subtype of `tp` or of some generalization of `tp`?
     *  The generalizations of a type T are the smallest set G such that
     *
     *   - T is in G
     *   - If a typeref R in G represents a class or trait, R's superclass is in G.
     *   - If a type proxy P is not a reference to a class, P's supertype is in G
     */
    def isSubTypeOfParent(subtp: Type, tp: Type)(implicit ctx: Context): Boolean =
      if (subtp <:< tp) true
      else tp match {
        case tp: TypeRef if tp.symbol.isClass => isSubTypeOfParent(subtp, tp.firstParent)
        case tp: TypeProxy => isSubTypeOfParent(subtp, tp.superType)
        case _ => false
      }

    unapplyFn.tpe.widen match {
      case mt: MethodType if mt.paramInfos.length == 1 =>
        val unapplyArgType = mt.paramInfos.head
        unapp.println(i"unapp arg tpe = $unapplyArgType, pt = $selType")
        val ownType =
          if (selType <:< unapplyArgType) {
            unapp.println(i"case 1 $unapplyArgType ${ctx.typerState.constraint}")
            selType
          } else if (isSubTypeOfParent(unapplyArgType, selType)(ctx.addMode(Mode.GADTflexible))) {
            maximizeType(unapplyArgType) match {
              case Some(tvar) =>
                def msg =
                  ex"""There is no best instantiation of pattern type $unapplyArgType
                      |that makes it a subtype of selector type $selType.
                      |Non-variant type variable ${tvar.origin} cannot be uniquely instantiated."""
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
                  unapp.println(s" ${unapplyFn.symbol.owner} ${unapplyFn.symbol.owner is Scala2x}")
                  ctx.strictWarning(msg, tree.pos)
                }
              case _ =>
            }
            unapp.println(i"case 2 $unapplyArgType ${ctx.typerState.constraint}")
            unapplyArgType
          } else {
            unapp.println("Neither sub nor super")
            unapp.println(TypeComparer.explained(implicit ctx => unapplyArgType <:< selType))
            errorType(
              ex"Pattern type $unapplyArgType is neither a subtype nor a supertype of selector type $selType",
              tree.pos)
          }

        val dummyArg = dummyTreeOfType(ownType)
        val unapplyApp = typedExpr(untpd.TypedSplice(Apply(unapplyFn, dummyArg :: Nil)))
        val unapplyImplicits = unapplyApp match {
          case Apply(Apply(unapply, `dummyArg` :: Nil), args2) => assert(args2.nonEmpty); args2
          case Apply(unapply, `dummyArg` :: Nil) => Nil
        }

        var argTypes = unapplyArgs(unapplyApp.tpe, unapplyFn, args, tree.pos)
        for (argType <- argTypes) assert(!argType.isInstanceOf[TypeBounds], unapplyApp.tpe.show)
        val bunchedArgs = argTypes match {
          case argType :: Nil =>
            if (argType.isRepeatedParam) untpd.SeqLiteral(args, untpd.TypeTree()) :: Nil
            else if (args.lengthCompare(1) > 0 && ctx.canAutoTuple) untpd.Tuple(args) :: Nil
            else args
          case _ => args
        }
        if (argTypes.length != bunchedArgs.length) {
          ctx.error(em"wrong number of argument patterns for $qual; expected: ($argTypes%, %)", tree.pos)
          argTypes = argTypes.take(args.length) ++
            List.fill(argTypes.length - args.length)(WildcardType)
        }
        val unapplyPatterns = (bunchedArgs, argTypes).zipped map (typed(_, _))
        val result = assignType(cpy.UnApply(tree)(unapplyFn, unapplyImplicits, unapplyPatterns), ownType)
        unapp.println(s"unapply patterns = $unapplyPatterns")
        if ((ownType eq selType) || ownType.isError) result
        else tryWithClassTag(Typed(result, TypeTree(ownType)), selType)
      case tp =>
        val unapplyErr = if (tp.isError) unapplyFn else notAnExtractor(unapplyFn)
        val typedArgsErr = args mapconserve (typed(_, defn.AnyType))
        cpy.UnApply(tree)(unapplyErr, Nil, typedArgsErr) withType unapplyErr.tpe
    }
  }

  /** A typed unapply hook, can be overridden by re any-typers between frontend
   *  and pattern matcher.
   */
  def typedUnApply(tree: untpd.UnApply, selType: Type)(implicit ctx: Context): UnApply =
    throw new UnsupportedOperationException("cannot type check an UnApply node")

  /** Is given method reference applicable to type arguments `targs` and argument trees `args`?
   *  @param  resultType   The expected result type of the application
   */
  def isApplicable(methRef: TermRef, targs: List[Type], args: List[Tree], resultType: Type)(implicit ctx: Context): Boolean =
    ctx.typerState.test(new ApplicableToTrees(methRef, targs, args, resultType).success)

  /** Is given method reference applicable to type arguments `targs` and argument trees `args` without inferring views?
    *  @param  resultType   The expected result type of the application
    */
  def isDirectlyApplicable(methRef: TermRef, targs: List[Type], args: List[Tree], resultType: Type)(implicit ctx: Context): Boolean =
    ctx.typerState.test(new ApplicableToTreesDirectly(methRef, targs, args, resultType).success)

  /** Is given method reference applicable to argument types `args`?
   *  @param  resultType   The expected result type of the application
   */
  def isApplicable(methRef: TermRef, args: List[Type], resultType: Type)(implicit ctx: Context): Boolean =
    ctx.typerState.test(new ApplicableToTypes(methRef, args, resultType).success)

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
    case methRef: TermRef if methRef.widenSingleton.isInstanceOf[MethodicType] =>
      p(methRef)
    case mt: MethodicType =>
      p(mt.narrow)
    case _ =>
      tp.member(nme.apply).hasAltWith(d => p(TermRef(tp, nme.apply, d)))
  }

  /** In a set of overloaded applicable alternatives, is `alt1` at least as good as
   *  `alt2`? Also used for implicits disambiguation.
   *
   *  @param  alt1, alt2      Non-overloaded references indicating the two choices
   *  @param  level1, level2  If alternatives come from a comparison of two contextual
   *                          implicit candidates, the nesting levels of the candidates.
   *                          In all other cases the nesting levels are both 0.
   *
   *  An alternative A1 is "as good as" an alternative A2 if it wins or draws in a tournament
   *  that awards one point for each of the following
   *
   *   - A1 is nested more deeply than A2
   *   - The nesting levels of A1 and A2 are the same, and A1's owner derives from A2's owner
   *   - A1's type is more specific than A2's type.
   */
  def isAsGood(alt1: TermRef, alt2: TermRef, nesting1: Int = 0, nesting2: Int = 0)(implicit ctx: Context): Boolean = track("isAsGood") { ctx.traceIndented(i"isAsGood($alt1, $alt2)", overload) {

    assert(alt1 ne alt2)

    /** Is class or module class `sym1` derived from class or module class `sym2`?
     *  Module classes also inherit the relationship from their companions.
     */
    def isDerived(sym1: Symbol, sym2: Symbol): Boolean =
      if (sym1 isSubClass sym2) true
      else if (sym2 is Module) isDerived(sym1, sym2.companionClass)
      else (sym1 is Module) && isDerived(sym1.companionClass, sym2)

    /** Is alternative `alt1` with type `tp1` as specific as alternative
     *  `alt2` with type `tp2` ?
     *
     *    1. A method `alt1` of type (p1: T1, ..., pn: Tn)U is as specific as `alt2`
     *       if `alt2` is applicable to arguments (p1, ..., pn) of types T1,...,Tn
     *       or if `alt1` is nullary.
     *    2. A polymorphic member of type [a1 >: L1 <: U1, ..., an >: Ln <: Un]T is as
     *       specific as `alt2` of type `tp2` if T is as specific as `tp2` under the
     *       assumption that for i = 1,...,n each ai is an abstract type name bounded
     *       from below by Li and from above by Ui.
     *    3. A member of any other type `tp1` is:
     *       a. always as specific as a method or a polymorphic method.
     *       b. as specific as a member of any other type `tp2` if `tp1` is compatible
     *          with `tp2`.
     */
    def isAsSpecific(alt1: TermRef, tp1: Type, alt2: TermRef, tp2: Type): Boolean = ctx.traceIndented(i"isAsSpecific $tp1 $tp2", overload) { tp1 match {
      case tp1: MethodType => // (1)
        val formals1 =
          if (tp1.isVarArgsMethod && tp2.isVarArgsMethod) tp1.paramInfos.map(_.repeatedToSingle)
          else tp1.paramInfos
        isApplicable(alt2, formals1, WildcardType) ||
        tp1.paramInfos.isEmpty && tp2.isInstanceOf[LambdaType]
      case tp1: PolyType => // (2)
        val tparams = ctx.newTypeParams(alt1.symbol, tp1.paramNames, EmptyFlags, tp1.instantiateBounds)
        isAsSpecific(alt1, tp1.instantiate(tparams map (_.typeRef)), alt2, tp2)
      case _ => // (3)
        tp2 match {
          case tp2: MethodType => true // (3a)
          case tp2: PolyType if tp2.resultType.isInstanceOf[MethodType] => true // (3a)
          case tp2: PolyType => // (3b)
            ctx.typerState.test(isAsSpecificValueType(tp1, constrained(tp2).resultType))
          case _ => // (3b)
            isAsSpecificValueType(tp1, tp2)
        }
    }}

    /** Test whether value type `tp1` is as specific as value type `tp2`.
     *  Let's abbreviate this to `tp1 <:s tp2`.
     *  Previously, `<:s` was the same as `<:`. This behavior is still
     *  available under mode `Mode.OldOverloadingResolution`. The new behavior
     *  is different, however. Here, `T <:s U` iff
     *
     *    flip(T) <: flip(U)
     *
     *  where `flip` changes top-level contravariant type aliases to covariant ones.
     *  Intuitively `<:s` means subtyping `<:`, except that all top-level arguments
     *  to contravariant parameters are compared as if they were covariant. E.g. given class
     *
     *     class Cmp[-X]
     *
     *  `Cmp[T] <:s Cmp[U]` if `T <: U`. On the other hand, nested occurrences
     *  of parameters are not affected.
     *  So `T <: U` would imply `List[Cmp[U]] <:s List[Cmp[T]]`, as usual.
     *
     *  This relation might seem strange, but it models closely what happens for methods.
     *  Indeed, if we integrate the existing rules for methods into `<:s` we have now that
     *
     *     (T)R  <:s  (U)R
     *
     *  iff
     *
     *     T => R  <:s  U => R
     */
    def isAsSpecificValueType(tp1: Type, tp2: Type)(implicit ctx: Context) =
      if (ctx.mode.is(Mode.OldOverloadingResolution))
        isCompatible(tp1, tp2)
      else {
        val flip = new TypeMap {
          def apply(t: Type) = t match {
            case t: TypeAlias if variance > 0 && t.variance < 0 => t.derivedTypeAlias(t.alias, 1)
            case t: TypeBounds => t
            case _ => mapOver(t)
          }
        }
        isCompatible(flip(tp1), flip(tp2))
      }

    /** Drop any implicit parameter section */
    def stripImplicit(tp: Type): Type = tp match {
      case mt: ImplicitMethodType =>
        resultTypeApprox(mt)
      case pt: PolyType =>
        pt.derivedLambdaType(pt.paramNames, pt.paramInfos, stripImplicit(pt.resultType))
      case _ =>
        tp
    }

    val owner1 = if (alt1.symbol.exists) alt1.symbol.owner else NoSymbol
    val owner2 = if (alt2.symbol.exists) alt2.symbol.owner else NoSymbol
    val tp1 = stripImplicit(alt1.widen)
    val tp2 = stripImplicit(alt2.widen)

    def winsOwner1 =
      nesting1 > nesting2 || nesting1 == nesting2 && isDerived(owner1, owner2)
    def winsType1  = isAsSpecific(alt1, tp1, alt2, tp2)
    def winsOwner2 =
      nesting2 > nesting1 || nesting1 == nesting2 && isDerived(owner2, owner1)
    def winsType2  = isAsSpecific(alt2, tp2, alt1, tp1)

    overload.println(i"isAsGood($alt1, $alt2)? $tp1 $tp2 $winsOwner1 $winsType1 $winsOwner2 $winsType2")

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
    alts match {
      case Nil => alts
      case _ :: Nil => alts
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
   *  Two trials: First, without implicits or SAM conversions enabled. Then,
   *  if the fist finds no eligible candidates, with implicits and SAM conversions enabled.
   */
  def resolveOverloaded(alts: List[TermRef], pt: Type)(implicit ctx: Context): List[TermRef] = track("resolveOverloaded") {

    /** Is `alt` a method or polytype whose result type after the first value parameter
     *  section conforms to the expected type `resultType`? If `resultType`
     *  is a `IgnoredProto`, pick the underlying type instead.
     */
    def resultConforms(alt: Type, resultType: Type)(implicit ctx: Context): Boolean = resultType match {
      case IgnoredProto(ignored) => resultConforms(alt, ignored)
      case _: ValueType =>
        alt.widen match {
          case tp: PolyType => resultConforms(constrained(tp).resultType, resultType)
          case tp: MethodType => constrainResult(tp.resultType, resultType)
          case _ => true
        }
      case _ => true
    }

    /** If the `chosen` alternative has a result type incompatible with the expected result
     *  type `pt`, run overloading resolution again on all alternatives that do match `pt`.
     *  If the latter succeeds with a single alternative, return it, otherwise
     *  fallback to `chosen`.
     *
     *  Note this order of events is done for speed. One might be tempted to
     *  preselect alternatives by result type. But this is slower, because it discriminates
     *  less. The idea is when searching for a best solution, as is the case in overloading
     *  resolution, we should first try criteria which are cheap and which have a high
     *  probability of pruning the search. result type comparisons are neither cheap nor
     *  do they prune much, on average.
     */
    def adaptByResult(chosen: TermRef) = pt match {
      case pt: FunProto if !ctx.typerState.test(resultConforms(chosen, pt.resultType)) =>
        val conformingAlts = alts.filter(alt =>
          (alt ne chosen) && ctx.typerState.test(resultConforms(alt, pt.resultType)))
        conformingAlts match {
          case Nil => chosen
          case alt2 :: Nil => alt2
          case alts2 =>
            resolveOverloaded(alts2, pt) match {
              case alt2 :: Nil => alt2
              case _ => chosen
            }
        }
      case _ => chosen
    }

    var found = resolveOverloaded(alts, pt, Nil)(ctx.retractMode(Mode.ImplicitsEnabled))
    if (found.isEmpty && ctx.mode.is(Mode.ImplicitsEnabled))
      found = resolveOverloaded(alts, pt, Nil)
    found match {
      case alt :: Nil => adaptByResult(alt) :: Nil
      case _ => found
    }
  }

  /** This private version of `resolveOverloaded` does the bulk of the work of
   *  overloading resolution, but does not do result adaptation. It might be
   *  called twice from the public `resolveOverloaded` method, once with
   *  implicits and SAM conversions enabled, and once without.
   */
  private def resolveOverloaded(alts: List[TermRef], pt: Type, targs: List[Type])(implicit ctx: Context): List[TermRef] = track("resolveOverloaded") {

    def isDetermined(alts: List[TermRef]) = alts.isEmpty || alts.tail.isEmpty

    /** The shape of given tree as a type; cannot handle named arguments. */
    def typeShape(tree: untpd.Tree): Type = tree match {
      case untpd.Function(args, body) =>
        defn.FunctionOf(args map Function.const(defn.AnyType), typeShape(body))
      case Match(EmptyTree, _) =>
        defn.PartialFunctionType.appliedTo(defn.AnyType :: defn.NothingType :: Nil)
      case _ =>
        defn.NothingType
    }

    /** The shape of given tree as a type; is more expensive than
     *  typeShape but can can handle named arguments.
     */
    def treeShape(tree: untpd.Tree): Tree = tree match {
      case NamedArg(name, arg) =>
        val argShape = treeShape(arg)
        cpy.NamedArg(tree)(name, argShape).withType(argShape.tpe)
      case _ =>
        dummyTreeOfType(typeShape(tree))
    }

    def narrowByTypes(alts: List[TermRef], argTypes: List[Type], resultType: Type): List[TermRef] =
      alts filter (isApplicable(_, argTypes, resultType))

    val candidates = pt match {
      case pt @ FunProto(args, resultType, _) =>
        val numArgs = args.length
        val normArgs = args.mapConserve {
          case Block(Nil, expr) => expr
          case x => x
        }

        def sizeFits(alt: TermRef, tp: Type): Boolean = tp.stripPoly match {
          case tp: MethodType =>
            val ptypes = tp.paramInfos
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

        def narrowByShapes(alts: List[TermRef]): List[TermRef] = {
          if (normArgs exists untpd.isFunctionWithUnknownParamType)
            if (hasNamedArg(args)) narrowByTrees(alts, args map treeShape, resultType)
            else narrowByTypes(alts, normArgs map typeShape, resultType)
          else
            alts
        }

        def narrowByTrees(alts: List[TermRef], args: List[Tree], resultType: Type): List[TermRef] = {
          val alts2 = alts.filter(alt =>
            isDirectlyApplicable(alt, targs, args, resultType)
          )
          if (alts2.isEmpty && !ctx.isAfterTyper)
            alts.filter(alt =>
              isApplicable(alt, targs, args, resultType)
            )
          else
            alts2
        }

        val alts1 = narrowBySize(alts)
        //ctx.log(i"narrowed by size: ${alts1.map(_.symbol.showDcl)}%, %")
        if (isDetermined(alts1)) alts1
        else {
          val alts2 = narrowByShapes(alts1)
          //ctx.log(i"narrowed by shape: ${alts2.map(_.symbol.showDcl)}%, %")
          if (isDetermined(alts2)) alts2
          else {
            pretypeArgs(alts2, pt)
            narrowByTrees(alts2, pt.typedArgs, resultType)
          }
        }

      case pt @ PolyProto(targs1, pt1) =>
        assert(targs.isEmpty)
        val alts1 = alts filter pt.isMatchedBy
        resolveOverloaded(alts1, pt1, targs1)

      case defn.FunctionOf(args, resultType, _) =>
        narrowByTypes(alts, args, resultType)

      case pt =>
        alts filter (normalizedCompatible(_, pt))
    }
    val found = narrowMostSpecific(candidates)
    if (found.length <= 1) found
    else {
      val noDefaults = alts.filter(!_.symbol.hasDefaultParams)
      if (noDefaults.length == 1) noDefaults // return unique alternative without default parameters if it exists
      else {
        val deepPt = pt.deepenProto
        if (deepPt ne pt) resolveOverloaded(alts, deepPt, targs)
        else alts
      }
    }
  }

  /** Try to typecheck any arguments in `pt` that are function values missing a
   *  parameter type. If the formal parameter types corresponding to a closure argument
   *  all agree on their argument types, typecheck the argument with an expected
   *  function or partial function type that contains these argument types,
   *  The result of the typecheck is stored in `pt`, to be retrieved when its `typedArgs` are selected.
   *  The benefit of doing this is to allow idioms like this:
   *
   *     def map(f: Char => Char): String = ???
   *     def map[U](f: Char => U): Seq[U] = ???
   *     map(x => x.toUpper)
   *
   *  Without `pretypeArgs` we'd get a "missing parameter type" error for `x`.
   *  With `pretypeArgs`, we use the `Char => ?` as the expected type of the
   *  closure `x => x.toUpper`, which makes the code typecheck.
   */
  private def pretypeArgs(alts: List[TermRef], pt: FunProto)(implicit ctx: Context): Unit = {
    def recur(altFormals: List[List[Type]], args: List[untpd.Tree]): Unit = args match {
      case arg :: args1 if !altFormals.exists(_.isEmpty) =>
        def isUnknownParamType(t: untpd.Tree) = t match {
          case ValDef(_, tpt, _) => tpt.isEmpty
          case _ => false
        }
        val fn = untpd.functionWithUnknownParamType(arg)
        if (fn.isDefined) {
          def isUniform[T](xs: List[T])(p: (T, T) => Boolean) = xs.forall(p(_, xs.head))
          val formalsForArg: List[Type] = altFormals.map(_.head)
          def argTypesOfFormal(formal: Type): List[Type] =
            formal match {
              case defn.FunctionOf(args, result, isImplicit) => args
              case defn.PartialFunctionOf(arg, result) => arg :: Nil
              case _ => Nil
            }
          val formalParamTypessForArg: List[List[Type]] =
            formalsForArg.map(argTypesOfFormal)
          if (formalParamTypessForArg.forall(_.nonEmpty) &&
              isUniform(formalParamTypessForArg)((x, y) => x.length == y.length)) {
            val commonParamTypes = formalParamTypessForArg.transpose.map(ps =>
              // Given definitions above, for i = 1,...,m,
              //   ps(i) = List(p_i_1, ..., p_i_n)  -- i.e. a column
              // If all p_i_k's are the same, assume the type as formal parameter
              // type of the i'th parameter of the closure.
              if (isUniform(ps)(ctx.typeComparer.isSameTypeWhenFrozen(_, _))) ps.head
              else WildcardType)
            def isPartial = // we should generate a partial function for the arg
              fn.get.isInstanceOf[untpd.Match] &&
              formalsForArg.exists(_.isRef(defn.PartialFunctionClass))
            val commonFormal =
              if (isPartial) defn.PartialFunctionOf(commonParamTypes.head, newTypeVar(TypeBounds.empty))
              else defn.FunctionOf(commonParamTypes, WildcardType)
            overload.println(i"pretype arg $arg with expected type $commonFormal")
            pt.typedArg(arg, commonFormal)(ctx.addMode(Mode.ImplicitsEnabled))
          }
        }
        recur(altFormals.map(_.tail), args1)
      case _ =>
    }
    recur(alts.map(_.widen.firstParamTypes), pt.args)
  }

  private def harmonizeWith[T <: AnyRef](ts: List[T])(tpe: T => Type, adapt: (T, Type) => T)(implicit ctx: Context): List[T] = {
    def numericClasses(ts: List[T], acc: Set[Symbol]): Set[Symbol] = ts match {
      case t :: ts1 =>
        val sym = tpe(t).widen.classSymbol
        if (sym.isNumericValueClass) numericClasses(ts1, acc + sym)
        else Set()
      case Nil =>
        acc
    }
    val clss = numericClasses(ts, Set())
    if (clss.size > 1) {
      def isCompatible(cls: Symbol, sup: TypeRef) =
        defn.isValueSubType(cls.typeRef, sup) &&
        !(cls == defn.LongClass && sup.isRef(defn.FloatClass))
          // exclude Long <: Float from list of allowable widenings
          // TODO: should we do this everywhere we ask for isValueSubType?

      val lub = defn.ScalaNumericValueTypeList.find(lubTpe =>
        clss.forall(cls => isCompatible(cls, lubTpe))).get

      def lossOfPrecision(ct: Constant): Boolean =
        ct.tag == IntTag && lub.isRef(defn.FloatClass) &&
          ct.intValue.toFloat.toInt != ct.intValue ||
        ct.tag == LongTag && lub.isRef(defn.DoubleClass) &&
          ct.longValue.toDouble.toLong != ct.longValue

      val ts1 = ts.mapConserve { t =>
        tpe(t).widenTermRefExpr match {
          case ct: ConstantType if !lossOfPrecision(ct.value) => adapt(t, lub)
          case _ => t
        }
      }
      if (numericClasses(ts1, Set()).size == 1) ts1 else ts
    }
    else ts
  }

  /** If `trees` all have numeric value types, and they do not have all the same type,
   *  pick a common numeric supertype and convert all constant trees to this type.
   *  If the resulting trees all have the same type, return them instead of the original ones.
   */
  def harmonize(trees: List[Tree])(implicit ctx: Context): List[Tree] = {
    def adapt(tree: Tree, pt: Type): Tree = tree match {
      case cdef: CaseDef => tpd.cpy.CaseDef(cdef)(body = adapt(cdef.body, pt))
      case _ => adaptInterpolated(tree, pt)
    }
    if (ctx.isAfterTyper) trees else harmonizeWith(trees)(_.tpe, adapt)
  }

  /** If all `types` are numeric value types, and they are not all the same type,
   *  pick a common numeric supertype and widen any constant types in `tpes` to it.
   *  If the resulting types are all the same, return them instead of the original ones.
   */
  private def harmonizeTypes(tpes: List[Type])(implicit ctx: Context): List[Type] =
    harmonizeWith(tpes)(identity, (tp, pt) => pt)
}

