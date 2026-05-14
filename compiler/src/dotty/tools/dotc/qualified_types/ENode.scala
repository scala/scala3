package dotty.tools.dotc.qualified_types

import scala.collection.mutable.ListBuffer

import dotty.tools.dotc.ast.{tpd, untpd}
import dotty.tools.dotc.ast.tpd.TreeOps
import dotty.tools.dotc.config.Printers
import dotty.tools.dotc.config.Settings.Setting.value
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Contexts.ctx
import dotty.tools.dotc.core.Decorators.i
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Flags.EmptyFlags
import dotty.tools.dotc.core.Hashable.Binders
import dotty.tools.dotc.core.Names.{termName, Name}
import dotty.tools.dotc.core.Names.Designator
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols.{defn, NoSymbol, Symbol}
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Types.{
  AndType,
  AnnotatedType,
  AppliedType,
  CachedProxyType,
  ClassInfo,
  ConstantType,
  LambdaType,
  MethodType,
  NamedType,
  NoPrefix,
  ParamRef,
  Range,
  SingletonType,
  SkolemType,
  TermParamRef,
  TermRef,
  ThisType,
  Type,
  TypeMap,
  TypeProxy,
  TypeRef,
  TypeVar,
  ValueType
}
import dotty.tools.dotc.parsing
import dotty.tools.dotc.printing.{Printer, Showable}
import dotty.tools.dotc.printing.GlobalPrec
import dotty.tools.dotc.printing.Texts.{Text, given}
import dotty.tools.dotc.qualified_types.ENode.Op
import dotty.tools.dotc.reporting.trace
import dotty.tools.dotc.transform.TreeExtractors.{BinaryOp, UnaryOp}

enum ENode extends Showable:
  import ENode.*

  case Atom(tp: SingletonType)
  case Constructor(constr: Symbol)(val fields: List[Symbol])
  case Select(qual: ENode, member: Symbol)
  case Apply(fn: ENode, args: List[ENode])
  case OpApply(fn: ENode.Op, args: List[ENode])
  case TypeApply(fn: ENode, args: List[Type])
  case Lambda(paramTps: List[Type], retTp: Type, body: ENode)

  // require(
  //  this match
  //    case Constructor(constr) =>
  //      constr.lastKnownDenotation.isConstructor
  //    case Lambda(paramTps, retTp, body) =>
  //      paramTps.zipWithIndex.forall: (tp, index) =>
  //        tp match
  //          case ENodeVar.BoundParam(i) => i < index
  //          case _ => true
  //    case _ => true
  // )

  def prettyString(printFullPaths: Boolean = false): String =

    def rec(n: ENode): String =
      n.prettyString(printFullPaths)

    def printTp(tp: Type): String =
      tp match
        case tp: NamedType =>
          val prefixString = if isEmptyPrefix(tp.prefix) then "" else printTp(tp.prefix) + "."
          prefixString + printDesignator(tp.designator) // + s"#${System.identityHashCode(tp).toHexString}"
        case tp: ConstantType =>
          tp.value.value.toString // + s"#${System.identityHashCode(tp).toHexString}"
        case tp: SkolemType =>
          "(?" + tp.hashCode + ": " + printTp(tp.info) + ")"
        case tp: ThisType =>
          printTp(tp.tref) + ".this"
        case tp: TypeVar =>
          tp.origin.paramName.toString()
        case tp: ENodeVar.BoundParam => s"arg${tp.index}"
        case tp: ENodeVar.OpenedParam => s"(op${tp.index}: ${printTp(tp.rawUnderlying)})"
        case tp: ENodeVar.Skolem => s"(sk${tp.index}: ${printTp(tp.rawUnderlying)})"
        case tp: AppliedType =>
          val argsString = tp.args.map(printTp).mkString(", ")
          s"${printTp(tp.tycon)}[$argsString]"
        case _ =>
          tp.toString

    def printDesignator(d: Designator): String =
      d match
        case d: Symbol => d.lastKnownDenotation.name.toString
        case _ => d.toString

    this match
      case Atom(tp) =>
        printTp(tp)
      case Constructor(constr) =>
        s"new ${printDesignator(constr.lastKnownDenotation.owner)}"
      case Select(qual, member) =>
        s"${rec(qual)}.${printDesignator(member)}"
      case Apply(fn, args) =>
        s"${rec(fn)}(${args.map(rec).mkString(", ")})"
      case OpApply(Op.IfThenElse, List(cond, thenp, elsep)) =>
        s"(if ${rec(cond)} then ${rec(thenp)} else ${rec(elsep)})"
      case OpApply(op, args) =>
        s"(${args.map(rec).mkString(" " + op.operatorName().toString() + " ")})"
      case TypeApply(fn, args) =>
        s"${rec(fn)}[${args.map(printTp).mkString(", ")}]"
      case Lambda(paramTps, retTp, body) =>
        val paramsString = paramTps.map(p => "_: " + printTp(p)).mkString(", ")
        s"($paramsString): ${printTp(retTp)} => ${rec(body)}"

  override def toText(p: Printer): Text =
    p.printerContext.base.qualifiedTypesStats.record("ENode.toText"):
      toText(p, false)

  private def toText(p: Printer, printAddresses: Boolean): Text =
    given Context = p.printerContext

    def withAddress(obj: Any, text: Text): Text =
      if printAddresses then
        "<" ~ text ~ s"#${System.identityHashCode(obj).toHexString}" ~ ">"
      else
        text

    def listToText[T](xs: List[T], fn: T => Text, sep: Text): Text =
      xs.map(fn).reduceLeftOption(_ ~ sep ~ _).getOrElse("")

    withAddress(
      this,
      this match
        case Atom(tp) =>
          p.toTextRef(tp)
        case Constructor(constr) =>
          "new " ~ p.toText(constr.lastKnownDenotation.owner.name)
        case Select(qual, member) =>
          qual.toText(p) ~ "." ~ p.toText(member.name)
        case Apply(fn, args) =>
          fn.toText(p) ~ "(" ~ listToText(args, arg => p.atPrec(GlobalPrec)(arg.toText(p)), ", ") ~ ")"
        case OpApply(op, args) =>
          assert(args.nonEmpty)
          op match
            case Op.IfThenElse =>
              assert(args.length == 3)
              "if " ~ p.atPrec(GlobalPrec)(args(0).toText(p)) ~ " then " ~ p.atPrec(GlobalPrec)(
                args(1).toText(p)
              ) ~ " else " ~ p.atPrec(GlobalPrec)(args(2).toText(p))
            // All operators with arity >= 2
            case Op.IntSum | Op.IntMinus | Op.IntProduct | Op.IntDiv | Op.IntMod |
                Op.IntLessThan | Op.IntLessEqual | Op.IntGreaterThan | Op.IntGreaterEqual |
                Op.LongSum | Op.LongMinus | Op.LongProduct |
                Op.And | Op.Or | Op.Equal | Op.NotEqual =>
              val opPrec = parsing.precedence(op.operatorName())
              val isRightAssoc = false
              val leftPrec = if isRightAssoc then opPrec + 1 else opPrec
              val rightPrec = if !isRightAssoc then opPrec + 1 else opPrec
              p.changePrec(opPrec):
                args.map(_.toText(p)).reduceLeft: (l, r) =>
                  p.atPrec(leftPrec)(l) ~ " " ~ p.toText(op.operatorName()) ~ " " ~ p.atPrec(rightPrec)(r)
            // Unary operators
            case _ =>
              assert(args.length == 1)
              val opPrec = parsing.precedence(op.operatorName())
              p.changePrec(opPrec):
                p.toText(op.operatorName()) ~ p.atPrec(opPrec + 1)(args.head.toText(p))
        case TypeApply(fn, args) =>
          fn.toText(p) ~ "[" ~ listToText(args, p.toText, ",") ~ "]"
        case Lambda(paramTps, retTp, body) =>
          p.enodeLambdaDepth += paramTps.length
          try
            val paramsText =
              listToText(paramTps.zipWithIndex, (tp, i) => p.enodeLambdaParamName(i, tp) ~ ": " ~ p.toText(tp), ", ")
            "(" ~ paramsText ~ ")" ~ " => " ~ p.atPrec(GlobalPrec)(body.toText(p))
          finally p.enodeLambdaDepth -= paramTps.length
    )

  def showNoBreak(using Context): String =
    toText(ctx.printer).mkString()

  def dotId() =
    "n" + System.identityHashCode(this).toHexString.substring(1)

  def toDot()(using _ctx: Context): String =
    given Context = _ctx.withoutColors
    val id = dotId()
    val fields: List[ENode | String] =
      this match
        case Atom(tp) => this.showNoBreak :: Nil
        case Constructor(constr) => this.showNoBreak :: Nil
        case Select(qual, member) => qual :: member.name.show :: Nil
        case Apply(fn, args) => fn :: args
        case OpApply(op, args) => op.operatorName().toString() :: args
        case TypeApply(fn, args) => fn :: args.map(_.show)
        case Lambda(paramTps, retTp, body) =>
          val paramsString = paramTps.map(p => "_:" + p.show).mkString(", ")
          "(" + paramsString + ")  => " + retTp.show :: body :: Nil
    val fieldStrings =
      fields.zipWithIndex.map: (field, i) =>
        field match
          case child: ENode => s"<p${i}>"
          case str: String => str.replace("<", "\\<").replace(">", "\\>")
    val nodeString = s"$id [label=\"${fieldStrings.mkString("|")}\"];\n"
    val edgesString =
      fields.zipWithIndex.map: (field, i) =>
        field match
          case child: ENode => s"$id:p$i -> ${child.dotId()};\n"
          case _ => ""
    nodeString + edgesString.mkString

  def mapTypes(f: Type => Type)(using Context): ENode =
    ctx.base.qualifiedTypesStats.record("ENode.mapTypes"):
      mapTypesRec(f)

  private def mapTypesRec(f: Type => Type)(using Context): ENode =
    this match
      case Atom(tp) =>
        val mappedTp =
          f(tp) match
            case Range(lo, hi) => hi
            case mappedTp => mappedTp
        if mappedTp eq tp then
          this
        else
          ENode.singleton(mappedTp)
      case Constructor(constr) =>
        this
      case node @ Select(qual, member) =>
        node.derived(qual.mapTypesRec(f), member)
      case node @ Apply(fn, args) =>
        node.derived(fn.mapTypesRec(f), args.mapConserve(_.mapTypesRec(f)))
      case node @ OpApply(op, args) =>
        node.derived(op, args.mapConserve(_.mapTypesRec(f)))
      case node @ TypeApply(fn, args) =>
        node.derived(fn.mapTypesRec(f), args.mapConserve(f))
      case node @ Lambda(paramTps, retTp, body) =>
        node.derived(paramTps.mapConserve(f), f(retTp), body.mapTypesRec(f))

  def foreachType(f: Type => Unit)(using Context): Unit =
    ctx.base.qualifiedTypesStats.record("ENode.foreachType"):
      foreachTypeRec(f)

  private def foreachTypeRec(f: Type => Unit)(using Context): Unit =
    this match
      case Atom(tp) => f(tp)
      case Constructor(_) => ()
      case Select(qual, _) => qual.foreachTypeRec(f)
      case Apply(fn, args) =>
        fn.foreachTypeRec(f)
        args.foreach(_.foreachTypeRec(f))
      case OpApply(_, args) => args.foreach(_.foreachTypeRec(f))
      case TypeApply(fn, args) =>
        fn.foreachTypeRec(f)
        args.foreach(f)
      case Lambda(paramTps, retTp, body) =>
        paramTps.foreach(f)
        f(retTp)
        body.foreachTypeRec(f)

  def normalizeTypes()(using Context): ENode =
    trace(i"normalizeTypes($this)", Printers.qualifiedTypes):
      ctx.base.qualifiedTypesStats.record("ENode.normalizeTypes"):
        mapTypes(NormalizeMap()).promoteAnyEquals()

  /** After type normalization, `Any_==` calls whose receiver type is now
   *  known to be a value type or case class can be promoted to `OpApply(Equal, ...)`.
   *  This handles qualifiers written with type parameters (e.g., `g(f(a)) == a`
   *  in a trait) that are later instantiated to concrete types
   *  (see tests/pos-custom-args/qualified-types/bijection.scala).
   */
  private def promoteAnyEquals()(using Context): ENode =
    map:
      case Apply(Select(qual, sym), List(arg))
          if (sym == defn.Any_== || sym == defn.Int_== || sym == defn.Boolean_==)
            && ENode.isValidEqualClass(qual.resultType) =>
        OpApply(Op.Equal, List(qual, arg))
      case Apply(Select(qual, sym), List(arg))
          if (sym == defn.Any_!= || sym == defn.Int_!= || sym == defn.Boolean_!=)
            && ENode.isValidEqualClass(qual.resultType) =>
        OpApply(Op.NotEqual, List(qual, arg))
      case node => node

  /** Try to infer the result type of this ENode, returning NoType if unknown. */
  private def resultType(using Context): Type =
    this match
      case Atom(tp) => tp.widen
      case Apply(fn, _) =>
        fn.resultType match
          case mt: MethodType => mt.resultType
          case _ => NoPrefix
      case OpApply(op, _) =>
        import Op.*
        op match
          case IntSum | IntMinus | IntProduct | IntDiv | IntMod => defn.IntType
          case LongSum | LongMinus | LongProduct => defn.LongType
          case _ => defn.BooleanType
      case _ => ENode.resolvedInfo(this)

  private class NormalizeMap(using Context) extends TypeMap:
    def apply(tp: Type): Type =
      tp match
        case tp: TypeVar if tp.isInstantiated =>
          apply(tp.stripTypeVar)
        case tp: NamedType =>
          val dealiased = tp.dealiasKeepAnnotsAndOpaques
          if dealiased ne tp then
            apply(dealiased)
          else if tp.symbol.isStatic || ((tp.prefix eq NoPrefix) && tp.symbol.owner.isClass) then
            tp.symbol.namedType
          else
            derivedSelect(tp, apply(tp.prefix))
        case _ =>
          mapOver(tp)

  def substEParamRefs(from: Int, to: List[Type])(using Context): ENode =
    this match
      case Atom(tp) =>
        mapTypes(SubstEParamsMap(from, to))
      case Constructor(_) =>
        this
      case node @ Select(qual, member) =>
        node.derived(qual.substEParamRefs(from, to), member)
      case node @ Apply(fn, args) =>
        node.derived(fn.substEParamRefs(from, to), args.mapConserve(_.substEParamRefs(from, to)))
      case node @ OpApply(op, args) =>
        node.derived(op, args.mapConserve(_.substEParamRefs(from, to)))
      case node @ TypeApply(fn, args) =>
        node.derived(fn.substEParamRefs(from, to), args.mapConserve(SubstEParamsMap(from, to)))
      case node @ Lambda(paramTps, retTp, body) =>
        node.derived(
          paramTps.mapConserve(SubstEParamsMap(from, to)),
          SubstEParamsMap(from, to)(retTp),
          body.substEParamRefs(from + paramTps.length, to)
        )

  private class SubstEParamsMap(from: Int, to: List[Type])(using Context) extends TypeMap:
    override def apply(tp: Type): Type =
      tp match
        case ENodeVar.BoundParam(i) if i >= from && i < from + to.length => to(i - from)
        case tp @ AnnotatedType(parent, annot @ QualifiedAnnotation(qualifier)) =>
          // Use substEParamRefs on the qualifier lambda to properly shift
          // de Bruijn indices and avoid substituting bound variables.
          val parent1 = apply(parent)
          val qualifier1 = qualifier.substEParamRefs(from, to).asInstanceOf[ENode.Lambda]
          tp.derivedAnnotatedType(parent1, annot.derivedAnnotation(qualifier1))
        case _ => mapOver(tp)

  /** Apply `f` to all child ENodes, preserving structure identity when unchanged. */
  def mapChildren(f: ENode => ENode): ENode =
    this match
      case Atom(_) | Constructor(_) => this
      case node @ Select(qual, member) =>
        node.derived(f(qual), member)
      case node @ Apply(fn, args) =>
        node.derived(f(fn), args.mapConserve(f))
      case node @ OpApply(op, args) =>
        node.derived(op, args.mapConserve(f))
      case node @ TypeApply(fn, args) =>
        node.derived(f(fn), args)
      case node @ Lambda(paramTps, retTp, body) =>
        node.derived(paramTps, retTp, f(body))

  /** Bottom-up transformation: apply `f` to every node after transforming its children. */
  def map(f: ENode => ENode): ENode =
    f(mapChildren(_.map(f)))

  def foreach(f: ENode => Unit): Unit =
    f(this)
    this match
      case Atom(_) => ()
      case Constructor(_) => ()
      case Select(qual, _) =>
        qual.foreach(f)
      case Apply(fn, args) =>
        fn.foreach(f)
        args.foreach(_.foreach(f))
      case OpApply(_, args) =>
        args.foreach(_.foreach(f))
      case TypeApply(fn, args) =>
        fn.foreach(f)
      case Lambda(_, _, body) =>
        body.foreach(f)

  def contains(that: ENode): Boolean =
    var found = false
    foreach: node =>
      if node eq that then
        found = true
    found

  // -----------------------------------
  // Conversion from E-Nodes to Trees
  // -----------------------------------

  /** Like `tpd.singleton`, but produces a placeholder tree for types that
   *  contain `SkolemType`s or free `ENodeVar`s (which cannot be represented
   *  as real trees).
   *
   *  This is fine because these trees only appear inside `@qualified`
   *  annotations where only the type matters.
   */
  private def singletonOrPlaceholder(tp: Type)(using Context): tpd.Tree =
    def hasSkolemOrFreeVar(tp: Type): Boolean = tp match
      case tp: SkolemType => true
      case tp: ENodeVar => tp.isFree
      case tp: TermRef => hasSkolemOrFreeVar(tp.prefix)
      case _ => false
    tp.dealias match
      case tp: SkolemType =>
        tpd.ref(defn.Predef_undefined).withType(tp)
      case tp: ENodeVar if tp.isFree =>
        tpd.ref(defn.QualifiedTypesInternal_skolem)
          .appliedToType(tp.underlying)
          .appliedTo(tpd.Literal(Constant(tp.index)))
      case tp: TermRef if hasSkolemOrFreeVar(tp) =>
        tpd.ref(defn.Predef_undefined).withType(tp.underlying)
      case tp => tpd.singleton(tp)

  def toTree(paramRefs: List[Type] = Nil)(using Context): tpd.Tree =
    ctx.base.qualifiedTypesStats.record("ENode.toTree"):
      toTreeRec(paramRefs)

  private def toTreeRec(paramRefs: List[Type] = Nil)(using Context): tpd.Tree =
    def mapType(tp: Type): Type = SubstEParamsMap(0, paramRefs)(tp)

    trace(i"ENode.toTreeRec $this, paramRefs: $paramRefs", Printers.qualifiedTypes):
      this match
        case Atom(tp) =>
          mapType(tp) match
            case tp1: TermParamRef => untpd.Ident(tp1.paramName).withType(tp1)
            case tp1 => singletonOrPlaceholder(tp1)
        case Constructor(sym) =>
          val tycon = sym.owner.asClass.classDenot.classInfo.selfType
          tpd.New(tycon).select(TermRef(tycon, sym))
        case Select(qual, member) =>
          qual.toTreeRec(paramRefs).select(member)
        case Apply(fn, args) =>
          tpd.Apply(fn.toTreeRec(paramRefs), args.map(_.toTreeRec(paramRefs)))
        case OpApply(op, args) =>
          def unaryOp(symbol: Symbol): tpd.Tree =
            require(args.length == 1)
            args(0).toTreeRec(paramRefs).select(symbol).appliedToNone
          def binaryOp(symbol: Symbol): tpd.Tree =
            require(args.length == 2)
            args(0).toTreeRec(paramRefs).select(symbol).appliedTo(args(1).toTreeRec(paramRefs))
          op match
            case Op.IntSum =>
              args.map(_.toTreeRec(paramRefs)).reduceLeft(_.select(defn.Int_+).appliedTo(_))
            case Op.IntMinus =>
              binaryOp(defn.Int_-)
            case Op.IntProduct =>
              args.map(_.toTreeRec(paramRefs)).reduceLeft(_.select(defn.Int_*).appliedTo(_))
            case Op.IntDiv =>
              binaryOp(defn.Int_/)
            case Op.IntMod =>
              binaryOp(defn.Int_%)
            case Op.LongSum =>
              ???
            case Op.LongMinus =>
              ???
            case Op.LongProduct =>
              ???
            case Op.Equal =>
              args(0).toTreeRec(paramRefs).equal(args(1).toTreeRec(paramRefs))
            case Op.NotEqual =>
              val lhs = args(0).toTreeRec(paramRefs)
              val rhs = args(1).toTreeRec(paramRefs)
              tpd.applyOverloaded(lhs, nme.NE, rhs :: Nil, Nil, defn.BooleanType)
            case Op.Not => unaryOp(defn.Boolean_!)
            case Op.And => binaryOp(defn.Boolean_&&)
            case Op.Or => binaryOp(defn.Boolean_||)
            case Op.IntLessThan => binaryOp(defn.Int_<)
            case Op.IntLessEqual => binaryOp(defn.Int_<=)
            case Op.IntGreaterThan => binaryOp(defn.Int_>)
            case Op.IntGreaterEqual => binaryOp(defn.Int_>=)
            case Op.IfThenElse =>
              require(args.length == 3)
              tpd.If(args(0).toTreeRec(paramRefs), args(1).toTreeRec(paramRefs), args(2).toTreeRec(paramRefs))
        case TypeApply(fn, args) =>
          tpd.TypeApply(fn.toTreeRec(paramRefs), args.map(tp => tpd.TypeTree(mapType(tp), false)))
        case Lambda(paramTps, retTp, body) =>
          val myParamNames = paramTps.zipWithIndex.map((tp, i) => termName("param" + (paramRefs.size + i)))
          def computeParamTypes(mt: MethodType) =
            val reversedParamRefs = mt.paramRefs.reverse
            paramTps.zipWithIndex.map((tp, i) => SubstEParamsMap(0, reversedParamRefs.take(i) ::: paramRefs)(tp))
          val mt = MethodType(myParamNames)(computeParamTypes, _ => retTp)
          tpd.Lambda(
            mt,
            myParamRefTrees =>
              val myParamRefs = myParamRefTrees.map(_.tpe).reverse
              body.toTreeRec(myParamRefs ::: paramRefs)
          )

object ENode:
  /** The types allowed in `Atom` nodes. */
  type AtomType = TermRef | ThisType | ConstantType | TermParamRef | ENodeVar | SkolemType

  /** Smart constructor that builds an ENode from a `SingletonType`.
   *
   *  - `TermRef`s with non-trivial prefixes are decomposed into
   *    `Select(singleton(prefix), member)`, so that the prefix is shared
   *    structurally rather than wrapped in a fresh skolem each time.
   *  - Leaf types (`ConstantType`, `ThisType`, `TermParamRef`, `ENodeVar`,
   *    `SkolemType`, static `TermRef`s) become `Atom` nodes directly.
   */
  def singleton(tp: SingletonType)(using Context): ENode = tp match
    case tp: TermRef =>
      val pre = tp.prefix
      pre match
        case _: NoPrefix.type => Atom(tp)
        case _ if tp.symbol.isStatic => Atom(tp.symbol.termRef)
        case pre: SingletonType => Select(singleton(pre), tp.symbol)
        case _ =>
          // Non-singleton prefix: wrap in a fresh skolem and decompose
          Select(Atom(SkolemType(pre)), tp.symbol)
    case tp: (ThisType | ConstantType | TermParamRef | ENodeVar | SkolemType) => Atom(tp)

  /** Like `singleton`, but for types that may not be `SingletonType`s.
   *  Non-singleton types are wrapped in a fresh `SkolemType`.
   */
  def singleton(tp: Type)(using Context): ENode = tp match
    case tp: SingletonType => singleton(tp)
    case _ =>
      Atom(SkolemType(tp))

  private def isEmptyPrefix(tp: Type): Boolean =
    tp match
      case tp: NoPrefix.type =>
        true
      case tp: ThisType =>
        tp.tref.designator match
          case d: Symbol => d.lastKnownDenotation.name.toTermName == nme.EMPTY_PACKAGE
          case _ => false
      case _ => false

  enum Op:
    case IntSum
    case IntMinus
    case IntProduct
    case IntDiv
    case IntMod
    case LongSum
    case LongMinus
    case LongProduct
    case Equal
    case NotEqual
    case Not
    case And
    case Or
    case IntLessThan
    case IntLessEqual
    case IntGreaterThan
    case IntGreaterEqual
    case IfThenElse

    def operatorName(): Name =
      this match
        case IntSum => nme.Plus
        case IntMinus => nme.Minus
        case IntProduct => nme.Times
        case IntDiv => nme.DIV
        case IntMod => nme.MOD
        case LongSum => nme.Plus
        case LongMinus => nme.Minus
        case LongProduct => nme.Times
        case Equal => nme.Equals
        case NotEqual => nme.NotEquals
        case Not => nme.Not
        case And => nme.And
        case Or => nme.Or
        case IntLessThan => nme.Lt
        case IntLessEqual => nme.Le
        case IntGreaterThan => nme.Gt
        case IntGreaterEqual => nme.Ge
        case IfThenElse => termName("if")

  // -----------------------------------
  // Conversion from Trees to E-Nodes
  // -----------------------------------

  def fromTree(
      tree: tpd.Tree,
      paramSyms: List[Symbol] = Nil,
      paramTps: List[Type] = Nil,
      valBindings: Map[Symbol, ENode] = Map.empty
  )(using Context): Option[ENode] =
    ctx.base.qualifiedTypesStats.record("ENode.fromTree"):
      fromTreeRec(tree, paramSyms, paramTps, valBindings)

  private def fromTreeRec(
      tree: tpd.Tree,
      paramSyms: List[Symbol] = Nil,
      paramTps: List[Type] = Nil,
      valBindings: Map[Symbol, ENode] = Map.empty
  )(using Context): Option[ENode] =
    val d = defn // Need a stable path to match on `defn` members

    def rec(
        tree: tpd.Tree,
        paramSyms: List[Symbol] = paramSyms,
        paramTps: List[Type] = paramTps,
        valBindings: Map[Symbol, ENode] = valBindings
    ): Option[ENode] =
      fromTreeRec(tree, paramSyms, paramTps, valBindings)

    def binaryOpNode(op: ENode.Op, lhs: tpd.Tree, rhs: tpd.Tree): Option[ENode] =
      for
        lhsNode <- rec(lhs)
        rhsNode <- rec(rhs)
      yield OpApply(op, List(lhsNode, rhsNode))

    def unaryOpNode(op: ENode.Op, arg: tpd.Tree): Option[ENode] =
      for argNode <- rec(arg) yield OpApply(op, List(argNode))

    def isValidEqual(sym: Symbol, lhs: tpd.Tree, rhs: tpd.Tree): Boolean =
      def lhsClass = lhs.tpe.classSymbol
      sym == defn.Int_==
      || sym == defn.Boolean_==
      || sym == defn.Any_== && lhsClass == defn.StringClass
      || sym.name == nme.EQ && lhsClass.exists && hasCaseClassEquals(lhsClass)

    trace(s"ENode.fromTreeRec $tree", Printers.qualifiedTypes):
      tree match
        case tpd.Ident(_) if valBindings.contains(tree.symbol) =>
          Some(valBindings(tree.symbol))
        case tpd.Literal(_) | tpd.Ident(_) | tpd.This(_)
            if tree.tpe.isInstanceOf[SingletonType] && tpd.isIdempotentExpr(tree) =>
          Some(singleton(substParamRefs(tree.tpe, paramSyms, paramTps).asInstanceOf[SingletonType]))
        case tpd.Literal(Constant(null)) => // null does not have a SingletonType
          Some(Atom(ConstantType(Constant(null))))
        case tpd.Select(tpd.New(_), nme.CONSTRUCTOR) =>
          constructorNode(tree.symbol)
        case tree: tpd.Select if isCaseClassApply(tree.symbol) =>
          constructorNode(tree.symbol.owner.linkedClass.primaryConstructor)
        case tpd.Select(qual, name) =>
          for qualNode <- rec(qual) yield Select(qualNode, tree.symbol)
        case BinaryOp(lhs, sym, rhs) if isValidEqual(sym, lhs, rhs) => binaryOpNode(ENode.Op.Equal, lhs, rhs)
        case BinaryOp(lhs, d.Int_!= | d.Boolean_!=, rhs) => binaryOpNode(ENode.Op.NotEqual, lhs, rhs)
        case UnaryOp(d.Boolean_!, arg) => unaryOpNode(ENode.Op.Not, arg)
        case BinaryOp(lhs, d.Boolean_&&, rhs) => binaryOpNode(ENode.Op.And, lhs, rhs)
        case BinaryOp(lhs, d.Boolean_||, rhs) => binaryOpNode(ENode.Op.Or, lhs, rhs)
        case BinaryOp(lhs, d.Int_+, rhs) => binaryOpNode(ENode.Op.IntSum, lhs, rhs)
        case BinaryOp(lhs, d.Int_-, rhs) => binaryOpNode(ENode.Op.IntMinus, lhs, rhs)
        case BinaryOp(lhs, d.Int_*, rhs) => binaryOpNode(ENode.Op.IntProduct, lhs, rhs)
        case BinaryOp(lhs, d.Int_/, rhs) => binaryOpNode(ENode.Op.IntDiv, lhs, rhs)
        case BinaryOp(lhs, d.Int_%, rhs) => binaryOpNode(ENode.Op.IntMod, lhs, rhs)
        case BinaryOp(lhs, d.Int_<, rhs) => binaryOpNode(ENode.Op.IntLessThan, lhs, rhs)
        case BinaryOp(lhs, d.Int_<=, rhs) => binaryOpNode(ENode.Op.IntLessEqual, lhs, rhs)
        case BinaryOp(lhs, d.Int_>, rhs) => binaryOpNode(ENode.Op.IntGreaterThan, lhs, rhs)
        case BinaryOp(lhs, d.Int_>=, rhs) => binaryOpNode(ENode.Op.IntGreaterEqual, lhs, rhs)
        case tpd.If(cond, thenp, elsep) =>
          for
            condNode  <- rec(cond)
            thenpNode <- rec(thenp)
            elsepNode <- rec(elsep)
          yield OpApply(ENode.Op.IfThenElse, List(condNode, thenpNode, elsepNode))
        // Decode the inverse of `toTree`'s encoding of a free skolem:
        // `skolem[T](idx)` → `ENodeVar.Skolem(ctx.owner, idx)(T)`. The owner
        // is recovered from the surrounding `ctx.owner`, which by invariant
        // matches the owner used at encoding time (typer-time `ctx.owner`).
        case tpd.Apply(tpd.TypeApply(fn, List(tArg)), List(tpd.Literal(Constant(idx: Int))))
            if fn.symbol == defn.QualifiedTypesInternal_skolem =>
          val underlying = substParamRefs(tArg.tpe, paramSyms, paramTps)
          Some(ENode.Atom(ENodeVar.Skolem(ctx.owner, idx)(underlying)))
        case tpd.Apply(fun, args) =>
          for
            funNode   <- rec(fun)
            argsNodes <- args.map(rec(_)).sequence
          yield ENode.Apply(funNode, argsNodes)
        // Strip asInstanceOf/$asInstanceOf casts: they don't change the
        // runtime value, and encoding them would introduce types that are
        // not properly hash-consed, breaking EGraph identity invariants.
        case tpd.TypeApply(tpd.Select(qual, _), _)
            if tree.symbol == defn.Any_asInstanceOf || tree.symbol == defn.Any_typeCast =>
          rec(qual)
        case tpd.TypeApply(fun, args) =>
          for funNode <- rec(fun)
          yield ENode.TypeApply(funNode, args.map(tp => substParamRefs(tp.tpe, paramSyms, paramTps)))
        case tpd.closureDef(defDef) =>
          defDef.symbol.info.dealias match
            case mt: MethodType =>
              assert(defDef.termParamss.size == 1, "closure is expected to have a single parameter list")
              var newParamSyms: List[Symbol] = paramSyms
              var newParamTps: List[Type] = paramTps
              val myParamSyms: List[Symbol] = defDef.termParamss.head.map(_.symbol)
              val myParamTps: List[Type] = mt.paramInfos
              for (myParamSym, myParamTp) <- myParamSyms.zip(myParamTps) do
                newParamTps = substParamRefs(myParamTp, newParamSyms, newParamTps) :: newParamTps
                newParamSyms = myParamSym :: newParamSyms
              val myRetTp = substParamRefs(mt.resType, newParamSyms, newParamTps)
              for body <- fromTreeRec(defDef.rhs, newParamSyms, newParamTps, valBindings)
              yield ENode.Lambda(newParamTps.take(myParamTps.size), myRetTp, body)
            case _ => None
        case tpd.Block(stats, expr) =>
          // Process val defs, collecting bindings for those whose RHS can be
          // converted to ENodes. When an Ident references a bound val, we
          // return its ENode directly. Non-ValDef statements (e.g., coverage
          // instrumentation calls, local defs) are skipped as they don't
          // affect the block's result value.
          //
          // TODO(mbovel): types embedded in the tree (e.g., TypeApply args) may
          // still contain TermRefs to inlined vals.
          var bindings = valBindings
          var ok = true
          for stat <- stats if ok do
            stat match
              case vdef: tpd.ValDef =>
                fromTreeRec(vdef.rhs, paramSyms, paramTps, bindings) match
                  case Some(node) => bindings = bindings.updated(vdef.symbol, node)
                  case None => ok = false
              case _ => ()
          if ok then rec(expr, valBindings = valBindings ++ bindings)
          else None
        case tpd.Inlined(_, Nil, expr) =>
          rec(expr)
        case _ =>
          None

  private def constructorNode(constr: Symbol)(using Context): Option[ENode.Constructor] =
    val clazz = constr.owner
    if hasCaseClassEquals(clazz) then
      val isPrimaryConstructor = constr.denot.isPrimaryConstructor
      val fieldsRaw = clazz.denot.asClass.paramAccessors.filter(isPrimaryConstructor && _.isStableMember)
      val constrParams = constr.paramSymss.flatten.filter(_.isTerm)
      val fields = constrParams.map(p => fieldsRaw.find(_.name == p.name).getOrElse(NoSymbol))
      Some(ENode.Constructor(constr)(fields))
    else
      None

  /** Resolve the info of a function-like ENode (Select or Atom TermRef),
   *  looking through the receiver type to pick up overrides
   *  (see tests/pos-custom-args/qualified-types/bijection.scala).
   */
  private def resolvedInfo(node: ENode)(using Context): Type =
    node match
      case Atom(tp: TermRef) => tp.symbol.info
      case Select(Atom(qualTp), member) =>
        qualTp.member(member.name).info
      case Select(_, member) => member.info
      case _ => NoPrefix

  private def isValidEqualClass(tp: Type)(using Context): Boolean =
    val clazz = tp.classSymbol
    clazz == defn.IntClass
    || clazz == defn.BooleanClass
    || clazz == defn.StringClass
    || clazz.exists && hasCaseClassEquals(clazz)

  private def hasCaseClassEquals(clazz: Symbol)(using Context): Boolean =
    val equalsMethod = clazz.info.decls.lookup(nme.equals_)
    val equalsNotOverriden = !equalsMethod.exists || equalsMethod.is(Flags.Synthetic)
    clazz.isClass && clazz.is(Flags.Case) && equalsNotOverriden

  private def isCaseClassApply(meth: Symbol)(using Context): Boolean =
    meth.name == nme.apply
      && meth.flags.is(Flags.Synthetic)
      && meth.owner.linkedClass.is(Flags.Case)

  def substParamRefs(tp: Type, paramSyms: List[Symbol], paramTps: List[Type])(using Context): Type =
    trace(i"substParamRefs($tp, $paramSyms, $paramTps)", Printers.qualifiedTypes):
      tp.subst(paramSyms, paramTps.zipWithIndex.map((tp, i) => ENodeVar.BoundParam(i)(tp)).toList)

  def selfify(tree: tpd.Tree)(using Context): Option[ENode.Lambda] =
    trace(i"ENode.selfify $tree", Printers.qualifiedTypes):
      fromTree(tree) match
        case Some(treeNode) =>
          Some(ENode.Lambda(
            List(tree.tpe),
            defn.BooleanType,
            OpApply(ENode.Op.Equal, List(treeNode, ENode.Atom(ENodeVar.BoundParam(0)(tree.tpe))))
          ))
        case None => None

  // -----------------------------------
  // Assumptions retrieval
  // -----------------------------------

  def assumptions(node: ENode)(using Context): List[ENode] =
    trace(i"assumptions($node)", Printers.qualifiedTypes):
      node match
        case n: Atom => typeAssumptions(n.tp)
        case n: Constructor => Nil
        case n: Select => assumptions(n.qual)
        case n: Apply => resultTypeAssumptions(n) ++ assumptions(n.fn) ++ n.args.flatMap(assumptions)
        case n: OpApply => n.args.flatMap(assumptions)
        case n: TypeApply => assumptions(n.fn)
        case n: Lambda => Nil

  private def termAssumptions(tp: SingletonType)(using Context): List[ENode] =
    trace(i"termAssumptions($tp)", Printers.qualifiedTypes):
      tp match
        case tp: TermRef =>
          val skolemInfo = ctx.base.qualifierSkolemIndexBySymbol.get(tp.symbol)
          val skolemAssumptions = skolemInfo.toList.map: (owner, id) =>
            OpApply(ENode.Op.Equal, List(Atom(ENodeVar.Skolem(owner, id)(SkolemType(tp.widen))), singleton(tp)))
          val defTree = tp.symbol.defTree
          val valAssumptions = defTree match
            case valDef: tpd.ValDef if !valDef.rhs.isEmpty && !valDef.symbol.is(Flags.Lazy) =>
              fromTree(valDef.rhs) match
                case Some(treeNode) => OpApply(ENode.Op.Equal, List(treeNode, singleton(tp))) :: assumptions(treeNode)
                case None => Nil
            case _ => Nil
          valAssumptions ++ skolemAssumptions
        case _ => Nil

  private def typeAssumptions(rootTp: SingletonType)(using Context): List[ENode] =
    def selfify(tp: SingletonType): List[ENode] =
      if tp frozen_=:= rootTp then Nil
      else List(OpApply(ENode.Op.Equal, List(singleton(tp), singleton(rootTp))))
    def recPrefix(tp: SingletonType): List[ENode] =
      tp match
        case TermRef(prefix: SingletonType, _) => termAssumptions(tp) ::: typeAssumptions(prefix)
        case _ => Nil
    def rec(tp: Type): List[ENode] =
      tp match
        case QualifiedType(parent, qualifier) =>
          qualifier.body.substEParamRefs(0, List(rootTp)) :: assumptions(qualifier.body) ::: rec(parent)
        case tp: SingletonType => termAssumptions(tp) ::: selfify(tp) ::: recPrefix(tp) ::: rec(tp.underlying)
        case tp: TypeProxy => rec(tp.underlying)
        case AndType(tp1, tp2) => rec(tp1) ++ rec(tp2)
        case _ => Nil
    trace(i"typeAssumptions($rootTp)", Printers.qualifiedTypes):
      rec(rootTp)

  /** For an Apply node, extract assumptions from the function's return type qualifier.
   *  If the function has a qualified return type like `{res: T with qualifier(res)}`,
   *  substitute method params with actual args and the result self-ref with the Apply node.
   *  Only handles simple (non-curried, non-generic) methods.
   */
  private def resultTypeAssumptions(applyNode: Apply)(using Context): List[ENode] =
    resolvedInfo(applyNode.fn) match
      case mt: MethodType =>
        mt.resultType match
          case QualifiedType(_, qualifier) =>
            val args = applyNode.args
            if args.length != mt.paramInfos.length then return Nil
            def substBody(node: ENode): ENode =
              node match
                case Atom(tp: TermParamRef) if tp.binder eq mt =>
                  args(tp.paramNum)
                case Atom(ENodeVar.BoundParam(0)) =>
                  applyNode
                case node => node.mapChildren(substBody)
            List(substBody(qualifier.body))
          case _ => Nil
      case _ => Nil

  // -----------------------------------
  // Utils
  // -----------------------------------

  extension (n: Atom)
    def derived(tp: SingletonType)(using Context): ENode =
      if n.tp eq tp then n
      else ENode.singleton(tp)

  extension (n: Constructor)
    def derived(constr: Symbol): ENode.Constructor =
      if n.constr eq constr then n
      else ENode.Constructor(constr)(n.fields)

  extension (n: Select)
    def derived(qual: ENode, member: Symbol): ENode.Select =
      if (n.qual eq qual) && (n.member eq member) then n
      else ENode.Select(qual, member)

  extension (n: Apply)
    def derived(fn: ENode, args: List[ENode]): ENode.Apply =
      if (n.fn eq fn) && (n.args eq args) then n
      else ENode.Apply(fn, args)

  extension (n: OpApply)
    def derived(op: ENode.Op, args: List[ENode]): ENode.OpApply =
      if (n.fn eq op) && (n.args eq args) then n
      else ENode.OpApply(op, args)

  extension (n: TypeApply)
    def derived(fn: ENode, args: List[Type]): ENode.TypeApply =
      if (n.fn eq fn) && (n.args eq args) then n
      else ENode.TypeApply(fn, args)

  extension (n: Lambda)
    def derived(paramTps: List[Type], retTp: Type, body: ENode): ENode.Lambda =
      if (n.paramTps eq paramTps) && (n.retTp eq retTp) && (n.body eq body) then n
      else ENode.Lambda(paramTps, retTp, body)

  // -----------------------------------
  // Utils
  // -----------------------------------

  extension [T](xs: List[Option[T]])
    private def sequence: Option[List[T]] =
      var result = List.newBuilder[T]
      var current = xs
      while current.nonEmpty do
        current.head match
          case Some(x) =>
            result += x
            current = current.tail
          case None =>
            return None
      Some(result.result())
