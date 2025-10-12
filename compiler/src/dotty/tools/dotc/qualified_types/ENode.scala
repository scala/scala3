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
  AppliedType,
  CachedProxyType,
  ClassInfo,
  ConstantType,
  LambdaType,
  MethodType,
  NamedType,
  NoPrefix,
  ParamRef,
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
import dotty.tools.dotc.printing.Texts.{stringToText, Text}
import dotty.tools.dotc.qualified_types.ENode.Op
import dotty.tools.dotc.reporting.trace
import dotty.tools.dotc.transform.TreeExtractors.{BinaryOp, UnaryOp}
import dotty.tools.dotc.util.Spans.Span

enum ENode extends Showable:
  import ENode.*

  case Atom(tp: SingletonType)
  case Constructor(constr: Symbol)(val fields: List[Symbol])
  case Select(qual: ENode, member: Symbol)
  case Apply(fn: ENode, args: List[ENode])
  case OpApply(fn: ENode.Op, args: List[ENode])
  case TypeApply(fn: ENode, args: List[Type])
  case Lambda(paramTps: List[Type], retTp: Type, body: ENode)

  require(
    this match
      case Constructor(constr) =>
        constr.lastKnownDenotation.isConstructor
      case Lambda(paramTps, retTp, body) =>
        paramTps.zipWithIndex.forall: (tp, index) =>
          tp match
            case ENodeParamRef(i, _) => i < index
            case _ => true
      case _ => true
  )

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
        case tp: ENodeParamRef =>
          s"arg${tp.index}"
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
        case OpApply(op, args) =>
          s"(${args.map(rec).mkString(" " + op.operatorName().toString() + " ")})"
        case TypeApply(fn, args) =>
          s"${rec(fn)}[${args.map(printTp).mkString(", ")}]"
        case Lambda(paramTps, retTp, body) =>
          val paramsString = paramTps.map(p => "_: " + printTp(p)).mkString(", ")
          s"($paramsString): ${printTp(retTp)} => ${rec(body)}"


  override def toText(p: Printer): Text = toText(p, false)

  def toText(p: Printer, printAddresses: Boolean): Text =
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
          "new" ~ p.toText(constr.lastKnownDenotation.owner)
        case Select(qual, member) =>
          qual.toText(p) ~ "." ~ p.toText(member.name)
        case Apply(fn, args) =>
          fn.toText(p) ~ "(" ~ listToText(args, arg => p.atPrec(GlobalPrec)(arg.toText(p)), ", ") ~ ")"
        case OpApply(op, args) =>
          assert(args.nonEmpty)
          op match
            // All operators with arity >= 2
            case Op.IntSum | Op.IntMinus | Op.IntProduct |
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
          val paramsText = listToText(paramTps, "_: " ~ p.toText(_), ", ")
          "(" ~ paramsText ~ ")" ~ " => " ~ p.atPrec(GlobalPrec)(body.toText(p))
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
    this match
      case Atom(tp) =>
        val mappedTp = f(tp)
        if mappedTp eq tp then
          this
        else
          mappedTp match
            case mappedTp: SingletonType => Atom(mappedTp)
            case _ => Atom(SkolemType(mappedTp))
      case Constructor(constr) =>
        this
      case node @ Select(qual, member) =>
        node.derived(qual.mapTypes(f), member)
      case node @ Apply(fn, args) =>
        node.derived(fn.mapTypes(f), args.mapConserve(_.mapTypes(f)))
      case node @ OpApply(op, args) =>
        node.derived(op, args.mapConserve(_.mapTypes(f)))
      case node @ TypeApply(fn, args) =>
        node.derived(fn.mapTypes(f), args.mapConserve(f))
      case node @ Lambda(paramTps, retTp, body) =>
        node.derived(paramTps.mapConserve(f), f(retTp), body.mapTypes(f))

  def foreachType(f: Type => Unit)(using Context): Unit =
    this match
      case Atom(tp) => f(tp)
      case Constructor(_) => ()
      case Select(qual, _) => qual.foreachType(f)
      case Apply(fn, args) =>
        fn.foreachType(f)
        args.foreach(_.foreachType(f))
      case OpApply(_, args) => args.foreach(_.foreachType(f))
      case TypeApply(fn, args) =>
        fn.foreachType(f)
        args.foreach(f)
      case Lambda(paramTps, retTp, body) =>
        paramTps.foreach(f)
        f(retTp)
        body.foreachType(f)

  def normalizeTypes()(using Context): ENode =
    trace(i"normalizeTypes($this)", Printers.qualifiedTypes):
      mapTypes(NormalizeMap())

  private class NormalizeMap(using Context) extends TypeMap:
    def apply(tp: Type): Type =
      tp match
        case tp: TypeVar if tp.isPermanentlyInstantiated =>
          apply(tp.permanentInst)
        case tp: NamedType =>
          val dealiased = tp.dealiasKeepAnnotsAndOpaques
          if dealiased ne tp then
            apply(dealiased)
          else if tp.symbol.isStatic then
            if tp.isInstanceOf[TermRef] then tp.symbol.termRef
            else tp.symbol.typeRef
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
        node.derived(paramTps.mapConserve(SubstEParamsMap(from, to)), SubstEParamsMap(from, to)(retTp), body.substEParamRefs(from + paramTps.length, to))

  private class SubstEParamsMap(from: Int, to: List[Type])(using Context) extends TypeMap:
    override def apply(tp: Type): Type =
      tp match
        case ENodeParamRef(i, _) if i >= from && i < from + to.length => to(i - from)
        case _ => mapOver(tp)

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

  def toTree(paramRefs: List[Type] = Nil)(using Context): tpd.Tree =
    def mapType(tp: Type): Type = SubstEParamsMap(0, paramRefs)(tp)

    trace(i"ENode.toTree $this, paramRefs: $paramRefs", Printers.qualifiedTypes):
      this match
        case Atom(tp) =>
          mapType(tp) match
            case tp1: TermParamRef => untpd.Ident(tp1.paramName).withType(tp1)
            case tp1 => tpd.singleton(tp1)
        case Constructor(sym) =>
          val tycon = sym.owner.asClass.classDenot.classInfo.selfType
          tpd.New(tycon).select(TermRef(tycon, sym))
        case Select(qual, member) =>
          qual.toTree(paramRefs).select(member)
        case Apply(fn, args) =>
          tpd.Apply(fn.toTree(paramRefs), args.map(_.toTree(paramRefs)))
        case OpApply(op, args) =>
          def unaryOp(symbol: Symbol): tpd.Tree =
            require(args.length == 1)
            args(0).toTree(paramRefs).select(symbol).appliedToNone
          def binaryOp(symbol: Symbol): tpd.Tree =
            require(args.length == 2)
            args(0).toTree(paramRefs).select(symbol).appliedTo(args(1).toTree(paramRefs))
          op match
            case Op.IntSum =>
              args.map(_.toTree(paramRefs)).reduceLeft(_.select(defn.Int_+).appliedTo(_))
            case Op.IntMinus =>
              binaryOp(defn.Int_-)
            case Op.IntProduct =>
              args.map(_.toTree(paramRefs)).reduceLeft(_.select(defn.Int_*).appliedTo(_))
            case Op.LongSum =>
              ???
            case Op.LongMinus =>
              ???
            case Op.LongProduct =>
              ???
            case Op.Equal =>
              args(0).toTree(paramRefs).equal(args(1).toTree(paramRefs))
            case Op.NotEqual =>
              val lhs = args(0).toTree(paramRefs)
              val rhs = args(1).toTree(paramRefs)
              tpd.applyOverloaded(lhs, nme.NE, rhs :: Nil, Nil, defn.BooleanType)
            case Op.Not => unaryOp(defn.Boolean_!)
            case Op.And => binaryOp(defn.Boolean_&&)
            case Op.Or => binaryOp(defn.Boolean_||)
            case Op.IntLessThan => binaryOp(defn.Int_<)
            case Op.IntLessEqual => binaryOp(defn.Int_<=)
            case Op.IntGreaterThan => binaryOp(defn.Int_>)
            case Op.IntGreaterEqual => binaryOp(defn.Int_>=)
        case TypeApply(fn, args) =>
          tpd.TypeApply(fn.toTree(paramRefs), args.map(tp => tpd.TypeTree(mapType(tp), false)))
        case Lambda(paramTps, retTp, body) =>
          val myParamNames = paramTps.zipWithIndex.map((tp, i) => termName("param" + (paramRefs.size + i)))
          def computeParamTypes(mt: MethodType) =
            val reversedParamRefs = mt.paramRefs.reverse
            paramTps.zipWithIndex.map((tp, i) => SubstEParamsMap(0, reversedParamRefs.take(i) ::: paramRefs)(tp))
          val mt = MethodType(myParamNames)(computeParamTypes, _ => retTp)
          tpd.Lambda(mt, myParamRefTrees =>
            val myParamRefs = myParamRefTrees.map(_.tpe).reverse
            body.toTree(myParamRefs ::: paramRefs)
          )

object ENode:
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

    def operatorName(): Name =
      this match
        case IntSum => nme.Plus
        case IntMinus => nme.Minus
        case IntProduct => nme.Times
        case LongSum => nme.Plus
        case LongMinus => nme.Minus
        case LongProduct => nme.Times
        case Equal => nme.Equals
        case NotEqual => nme.NotEquals
        case Not => nme.Not
        case And => nme.And
        case Or => nme.Or
        case IntLessThan => nme.Le
        case IntLessEqual => nme.Lt
        case IntGreaterThan => nme.Gt
        case IntGreaterEqual => nme.Ge

  // -----------------------------------
  // Conversion from Trees to E-Nodes
  // -----------------------------------

  def fromTree(
      tree: tpd.Tree,
      paramSyms: List[Symbol] = Nil,
      paramTps: List[Type] = Nil
  )(using Context): Option[ENode] =
    val d = defn // Need a stable path to match on `defn` members

    def binaryOpNode(op: ENode.Op, lhs: tpd.Tree, rhs: tpd.Tree): Option[ENode] =
      for
        lhsNode <- fromTree(lhs, paramSyms, paramTps)
        rhsNode <- fromTree(rhs, paramSyms, paramTps)
      yield OpApply(op, List(lhsNode, rhsNode))

    def unaryOpNode(op: ENode.Op, arg: tpd.Tree): Option[ENode] =
      for argNode <- fromTree(arg, paramSyms, paramTps) yield OpApply(op, List(argNode))

    def isValidEqual(sym: Symbol, lhs: tpd.Tree, rhs: tpd.Tree): Boolean =
      def lhsClass = lhs.tpe.classSymbol
      sym == defn.Int_==
      || sym == defn.Boolean_==
      || sym == defn.Any_== && lhsClass == defn.StringClass
      || sym.name == nme.EQ && lhsClass.exists && hasCaseClassEquals(lhsClass)

    trace(s"ENode.fromTree $tree", Printers.qualifiedTypes):
      tree match
        case tpd.Literal(_) | tpd.Ident(_) | tpd.This(_)
            if tree.tpe.isInstanceOf[SingletonType] && tpd.isIdempotentExpr(tree) =>
          Some(Atom(substParamRefs(tree.tpe, paramSyms, paramTps).asInstanceOf[SingletonType]))
        case tpd.Select(tpd.New(_), nme.CONSTRUCTOR) =>
          constructorNode(tree.symbol)
        case tree: tpd.Select if isCaseClassApply(tree.symbol) =>
          constructorNode(tree.symbol.owner.linkedClass.primaryConstructor)
        case tpd.Select(qual, name) =>
          for qualNode <- fromTree(qual, paramSyms, paramTps) yield Select(qualNode, tree.symbol)
        case BinaryOp(lhs, sym, rhs) if isValidEqual(sym, lhs, rhs) => binaryOpNode(ENode.Op.Equal, lhs, rhs)
        case BinaryOp(lhs, d.Int_!= | d.Boolean_!=, rhs) => binaryOpNode(ENode.Op.NotEqual, lhs, rhs)
        case UnaryOp(d.Boolean_!, arg) => unaryOpNode(ENode.Op.Not, arg)
        case BinaryOp(lhs, d.Boolean_&&, rhs) => binaryOpNode(ENode.Op.And, lhs, rhs)
        case BinaryOp(lhs, d.Boolean_||, rhs) => binaryOpNode(ENode.Op.Or, lhs, rhs)
        case BinaryOp(lhs, d.Int_+, rhs) => binaryOpNode(ENode.Op.IntSum, lhs, rhs)
        case BinaryOp(lhs, d.Int_-, rhs) => binaryOpNode(ENode.Op.IntMinus, lhs, rhs)
        case BinaryOp(lhs, d.Int_*, rhs) => binaryOpNode(ENode.Op.IntProduct, lhs, rhs)
        case BinaryOp(lhs, d.Int_<, rhs) => binaryOpNode(ENode.Op.IntLessThan, lhs, rhs)
        case BinaryOp(lhs, d.Int_<=, rhs) => binaryOpNode(ENode.Op.IntLessEqual, lhs, rhs)
        case BinaryOp(lhs, d.Int_>, rhs) => binaryOpNode(ENode.Op.IntGreaterThan, lhs, rhs)
        case BinaryOp(lhs, d.Int_>=, rhs) => binaryOpNode(ENode.Op.IntGreaterEqual, lhs, rhs)
        case tpd.Apply(fun, args) =>
          for
            funNode <- fromTree(fun, paramSyms, paramTps)
            argsNodes <- args.map(fromTree(_, paramSyms, paramTps)).sequence
          yield ENode.Apply(funNode, argsNodes)
        case tpd.TypeApply(fun, args) =>
          for funNode <- fromTree(fun, paramSyms, paramTps)
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
              for body <- fromTree(defDef.rhs, newParamSyms, newParamTps)
              yield ENode.Lambda(newParamTps.take(myParamTps.size), myRetTp, body)
            case _ => None
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
      tp.subst(paramSyms, paramTps.zipWithIndex.map((tp, i) => ENodeParamRef(i, tp)).toList)

  def selfify(tree: tpd.Tree)(using Context): Option[ENode.Lambda] =
    trace(i"ENode.selfify $tree", Printers.qualifiedTypes):
      fromTree(tree) match
        case Some(treeNode) =>
          Some(ENode.Lambda(
            List(tree.tpe),
            defn.BooleanType,
            OpApply(ENode.Op.Equal, List(treeNode, ENode.Atom(ENodeParamRef(0, tree.tpe))))
          ))
        case None => None

  // -----------------------------------
  // Assumptions retrieval
  // -----------------------------------

  def assumptions(node: ENode)(using Context): List[ENode] =
    trace(i"assumptions($node)", Printers.qualifiedTypes):
      node match
        case n: Atom => termAssumptions(n.tp) ++ typeAssumptions(n.tp)
        case n: Constructor => Nil
        case n: Select => assumptions(n.qual)
        case n: Apply => assumptions(n.fn) ++ n.args.flatMap(assumptions)
        case n: OpApply => n.args.flatMap(assumptions)
        case n: TypeApply => assumptions(n.fn)
        case n: Lambda => Nil

  private def termAssumptions(tp: SingletonType)(using Context): List[ENode] =
    trace(i"termAssumptions($tp)", Printers.qualifiedTypes):
      tp match
        case tp: TermRef =>
          tp.symbol.info match
            case _ =>
              tp.symbol.defTree match
                case valDef: tpd.ValDef if !valDef.rhs.isEmpty && !valDef.symbol.is(Flags.Lazy) =>
                  fromTree(valDef.rhs) match
                    case Some(treeNode) => OpApply(ENode.Op.Equal, List(treeNode, Atom(tp))) :: assumptions(treeNode)
                    case None => Nil
                case _ => Nil
        case _ => Nil

  private def typeAssumptions(rootTp: SingletonType)(using Context): List[ENode] =
    def rec(tp: Type): List[ENode] =
      tp match
        case QualifiedType(parent, qualifier) => qualifier.body.substEParamRefs(0, List(rootTp)) :: assumptions(qualifier.body) ::: rec(parent)
        case tp: SingletonType if tp ne rootTp => List(OpApply(ENode.Op.Equal, List(Atom(tp), Atom(rootTp))))
        case tp: TypeProxy => rec(tp.underlying)
        case AndType(tp1, tp2) => rec(tp1) ++ rec(tp2)
        case _ => Nil
    trace(i"typeAssumptions($rootTp)", Printers.qualifiedTypes):
      rec(rootTp)

  // -----------------------------------
  // Utils
  // -----------------------------------

  extension (n: Atom)
    def derived(tp: SingletonType): ENode.Atom =
      if n.tp eq tp then n
      else ENode.Atom(tp)

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
