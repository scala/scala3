package dotty.tools.dotc.qualified_types

import dotty.tools.dotc.ast.{tpd, untpd}
import dotty.tools.dotc.config.Printers
import dotty.tools.dotc.config.Settings.Setting.value
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Contexts.ctx
import dotty.tools.dotc.core.Decorators.i
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Flags.EmptyFlags
import dotty.tools.dotc.core.Hashable.Binders
import dotty.tools.dotc.core.Names.Designator
import dotty.tools.dotc.core.Names.{termName, Name}
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols.{defn, NoSymbol, Symbol}
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Types.{
  AndType,
  AppliedType,
  CachedProxyType,
  ConstantType,
  ClassInfo,
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
  TypeRef,
  TypeVar,
  TypeProxy,
  ValueType
}
import dotty.tools.dotc.ast.tpd.TreeOps
import dotty.tools.dotc.qualified_types.ENode.Op
import dotty.tools.dotc.reporting.trace
import dotty.tools.dotc.transform.TreeExtractors.{BinaryOp, UnaryOp}
import dotty.tools.dotc.util.Spans.Span
import scala.collection.mutable.ListBuffer

enum ENode:
  import ENode.*

  case Atom(tp: SingletonType)
  case Constructor(constr: Symbol)(val fields: List[Symbol])
  case Select(qual: ENode, member: Symbol)
  case Apply(fn: ENode, args: List[ENode])
  case OpApply(fn: ENode.Op, args: List[ENode])
  case TypeApply(fn: ENode, args: List[Type])
  case Lambda(paramTps: List[ENodeParamRef], retTp: Type, body: ENode)

  override def toString(): String =
    val res =
      this match
        case Atom(tp) => typeToString(tp)
        case Constructor(constr) => s"new ${designatorToString(constr.lastKnownDenotation.owner)}"
        case Select(qual, member) => s"$qual.${designatorToString(member)}"
        case Apply(fn, args) => s"$fn(${args.mkString(", ")})"
        case OpApply(op, args) => s"(${args.mkString(" " + op.operatorString() + " ")})"
        case TypeApply(fn, args) => s"$fn[${args.map(typeToString).mkString(", ")}]"
        case Lambda(paramTps, retTp, body) => s"${lambdaSignatureString(paramTps, retTp)} => $body"
    //(this).toHexString}>"
    res

  def dotId() =
    "n" + System.identityHashCode(this).toHexString.substring(1)

  def toDot(): String =
    val id = dotId()
    val fields: List[ENode | String] =
      this match
        case Atom(tp) => this.toString :: Nil
        case Constructor(constr) => this.toString :: Nil
        case Select(qual, member) => qual :: designatorToString(member) :: Nil
        case Apply(fn, args) => fn :: args
        case OpApply(op, args) => op.operatorString() :: args
        case TypeApply(fn, args) => this.toString :: Nil
        case Lambda(paramTps, retTp, body) => lambdaSignatureString(paramTps, retTp) :: body :: Nil
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
            case _ =>
              //throw new Error(s"Warning: ENode Atom type mapped to non-singleton type: $mappedTp")
              Atom(SkolemType(mappedTp))
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
        val paramTpsMapped =
          paramTps.mapConserve: tp =>
            f(tp) match
              case tp: ENodeParamRef => tp
              case tp => throw new Error(s"Warning: ENode Lambda parameter type mapped to non-ENodeParamRef type: $tp")
        node.derived(paramTpsMapped, f(retTp), body.mapTypes(f))

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


  def instantiateTypeVars()(using Context): ENode =
    mapTypes(InstantiateMap())

  /** Instantiates type variables when possible.
    *
    * See `TypeMap.mapOverTypeVar`.
    */
  private class InstantiateMap(using Context)  extends TypeMap:
    override def mapOverTypeVar(tp: TypeVar): Type =
      val res = super.mapOverTypeVar(tp)
      if res eq tp then res else ENode.normalizeType(res)

    def apply(tp: Type): Type = mapOver(tp)

  def substArgRefs(startIndex: Int, to: List[Type])(using Context): ENode =
    mapTypes(SubstENodeParamsMap(startIndex, to))

  private class SubstENodeParamsMap(startIndex: Int, to: List[Type])(using Context) extends InstantiateMap:
    override def apply(tp: Type): Type =
      tp match
        case ENodeParamRef(i, _) if i >= startIndex && i < startIndex + to.size => to(i - startIndex)
        case _ => super.apply(tp)

  // -----------------------------------
  // Conversion from E-Nodes to Trees
  // -----------------------------------

  def toTree(paramRefs: List[Type] = Nil)(using Context): tpd.Tree =
    def mapType(tp: Type): Type = SubstENodeParamsMap(0, paramRefs)(tp)

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
              args.foldLeft(tpd.Literal(Constant(0)): tpd.Tree): (acc, arg) =>
                acc.select(defn.Int_+).appliedTo(arg.toTree(paramRefs))
            case Op.IntMinus =>
              binaryOp(defn.Int_-)
            case Op.IntProduct =>
              args.foldLeft(tpd.Literal(Constant(1)): tpd.Tree): (acc, arg) =>
                acc.select(defn.Int_*).appliedTo(arg.toTree(paramRefs))
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
          val paramNames = paramTps.map(arg => termName("arg" + arg.index))
          val mt = MethodType(paramNames)(
            mt => paramTps.map(paramTp =>
              val underlying = paramTp.underlying
              if paramTps.nonEmpty then SubstENodeParamsMap(paramTps.head.index, mt.paramRefs)(underlying)
              else underlying
            ),
            mt => retTp
          )
          tpd.Lambda(mt, myParamRefs => body.toTree(paramRefs ++ myParamRefs.map(_.tpe)))


object ENode:
  private def paramToString(param: Type): String =
    param match
      case tp: ENodeParamRef =>
        s"arg${tp.index}: ${typeToString(tp.underlying)}"
      case _ =>
        typeToString(param)

  private def typeToString(tp: Type): String =
    tp match
      case tp: NamedType =>
        val prefixString = if isEmptyPrefix(tp.prefix) then "" else typeToString(tp.prefix) + "."
        prefixString + designatorToString(tp.designator) //+ s"#${System.identityHashCode(tp).toHexString}"
      case tp: ConstantType =>
        tp.value.value.toString //+ s"#${System.identityHashCode(tp).toHexString}"
      case tp: SkolemType =>
        "(?" + tp.hashCode + ": " + typeToString(tp.info) + ")"
      case tp: ThisType =>
        typeToString(tp.tref) + ".this"
      case tp: TypeVar =>
        tp.origin.paramName.toString()
      case tp: ENodeParamRef =>
        s"arg${tp.index}"
      case tp: AppliedType =>
        val argsString = tp.args.map(typeToString).mkString(", ")
        s"${typeToString(tp.tycon)}[$argsString]"
      case _ =>
        tp.toString

  private def lambdaSignatureString(paramTps: List[ENodeParamRef], retTp: Type): String =
    val paramsString = paramTps.map(paramToString).mkString(", ")
    s"($paramsString) => ${typeToString(retTp)}"

  private def isEmptyPrefix(tp: Type): Boolean =
    tp match
      case tp: NoPrefix.type =>
        true
      case tp: ThisType =>
        tp.tref.designator match
          case d: Symbol => d.lastKnownDenotation.name.toTermName == nme.EMPTY_PACKAGE
          case _ => false
      case _ => false

  private def designatorToString(d: Designator): String =
    d match
      case d: Symbol => d.lastKnownDenotation.name.toString
      case _ => d.toString

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

    def operatorString(): String =
      this match
        case IntSum => "+"
        case IntMinus => "-"
        case IntProduct => "*"
        case LongSum => "+"
        case LongMinus => "-"
        case LongProduct => "*"
        case Equal => "=="
        case NotEqual => "!="
        case Not => "!"
        case And => "&&"
        case Or => "||"
        case IntLessThan => "<"
        case IntLessEqual => "<="
        case IntGreaterThan => ">"
        case IntGreaterEqual => ">="

  // -----------------------------------
  // Conversion from Trees to E-Nodes
  // -----------------------------------

  def fromTree(
      tree: tpd.Tree,
      paramSyms: List[Symbol] = Nil,
      paramTps: List[ENodeParamRef] = Nil
  )(using Context): Option[ENode] =
    val d = defn // Need a stable path to match on `defn` members

    def mapType(tp: Type): Type =
      normalizeType(tp.subst(paramSyms, paramTps))

    def binaryOpNode(op: ENode.Op, lhs: tpd.Tree, rhs: tpd.Tree): Option[ENode] =
      for
        lhsNode <- fromTree(lhs, paramSyms, paramTps)
        rhsNode <- fromTree(rhs, paramSyms, paramTps)
      yield OpApply(op, List(lhsNode, rhsNode))

    def unaryOpNode(op: ENode.Op, arg: tpd.Tree): Option[ENode] =
      for argNode <- fromTree(arg, paramSyms, paramTps) yield
        OpApply(op, List(argNode))

    def isValidEqual(sym: Symbol, lhs: tpd.Tree, rhs: tpd.Tree): Boolean =
      def lhsClass = lhs.tpe.classSymbol
      sym == defn.Int_==
      || sym == defn.Boolean_==
      || sym == defn.Any_== && lhsClass == defn.StringClass
      || sym.name == nme.EQ && lhsClass.exists && hasCaseClassEquals(lhsClass)

    trace(s"ENode.fromTree $tree", Printers.qualifiedTypes):
      tree match
        case tpd.Literal(_) | tpd.Ident(_) | tpd.This(_) if tree.tpe.isInstanceOf[SingletonType] && tpd.isIdempotentExpr(tree) =>
          Some(Atom(mapType(tree.tpe).asInstanceOf[SingletonType]))
        case tpd.Select(tpd.New(_), nme.CONSTRUCTOR) =>
          constructorNode(tree.symbol)
        case tree: tpd.Select if isCaseClassApply(tree.symbol) =>
          constructorNode(tree.symbol.owner.linkedClass.primaryConstructor)
        case tpd.Select(qual, name) =>
          for qualNode <- fromTree(qual, paramSyms, paramTps) yield
            Select(qualNode, tree.symbol)
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
          yield ENode.TypeApply(funNode, args.map(tp => mapType(tp.tpe)))
        case tpd.closureDef(defDef) =>
          defDef.symbol.info.dealias match
            case mt: MethodType =>
              assert(defDef.termParamss.size == 1, "closures have a single parameter list, right?")
              val myParamSyms: List[Symbol] = defDef.termParamss.head.map(_.symbol)
              val myParamTps: ListBuffer[ENodeParamRef] = ListBuffer.empty
              val paramTpsSize = paramTps.size
              for myParamSym <- myParamSyms do
                val underlying = mapType(myParamSym.info.subst(myParamSyms.take(myParamTps.size), myParamTps.toList))
                myParamTps += ENodeParamRef(paramTpsSize + myParamTps.size, underlying)
              val myRetTp = mapType(defDef.tpt.tpe.subst(myParamSyms, myParamTps.toList))
              for body <- fromTree(defDef.rhs, myParamSyms ::: paramSyms, myParamTps.toList ::: paramTps)
              yield ENode.Lambda(myParamTps.toList, myRetTp, body)
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

  def normalizeType(tp: Type)(using Context): Type =
    tp match
      case tp: TypeVar if tp.isPermanentlyInstantiated =>
        tp.permanentInst
      case tp: NamedType =>
        if tp.symbol.isStatic then
          if tp.isInstanceOf[TermRef] then tp.symbol.termRef else tp.symbol.typeRef
        else
          normalizeType(tp.prefix).select(tp.symbol)
      case tp => tp

  def selfify(tree: tpd.Tree)(using Context): Option[ENode.Lambda] =
    trace(i"ENode.selfify $tree", Printers.qualifiedTypes):
      fromTree(tree) match
        case Some(treeNode) =>
          val paramType = ENodeParamRef(0, tree.tpe)
          Some(ENode.Lambda(
            List(paramType),
            defn.BooleanType,
            OpApply(ENode.Op.Equal, List(treeNode, ENode.Atom(paramType)))
          ))
        case None => None

  // -----------------------------------
  // Assumptions retrieval
  // -----------------------------------

  def assumptions(node: ENode)(using Context): List[ENode] =
    trace(i"assumptions($node)", Printers.qualifiedTypes):
      node match
        case Atom(tp: SingletonType) => termAssumptions(tp) ++ typeAssumptions(tp)
        case n: Constructor => Nil
        case n: Select => assumptions(n.qual)
        case n: Apply => assumptions(n.fn) ++ n.args.flatMap(assumptions)
        case n: OpApply => n.args.flatMap(assumptions)
        case n: TypeApply => assumptions(n.fn)
        case n: Lambda => Nil

  private def termAssumptions(tp: SingletonType)(using Context): List[ENode] =
    trace(i"termAssumptions($tp)", Printers.qualifiedTypes):
      tp match
        case tp: NamedType =>
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
        case QualifiedType(underlying, qualifier) => qualifier.body.substArgRefs(0, List(rootTp)) :: rec(underlying)
        case tp: SingletonType if tp ne rootTp => List(OpApply(ENode.Op.Equal, List(Atom(tp), Atom(rootTp))))
        case tp: TypeProxy => rec(tp.underlying)
        case AndType(tp1, tp2) => rec(tp1) ++ rec(tp2)
        case _ => Nil
    trace(i"typeAssumptions($rootTp)", Printers.qualifiedTypes):
      rec(rootTp)

  // -----------------------------------
  // Utils
  // -----------------------------------

  extension (n: Atom) def derived(tp: SingletonType): ENode.Atom =
    if n.tp eq tp then n
    else ENode.Atom(tp)

  extension (n: Constructor) def derived(constr: Symbol): ENode.Constructor =
    if n.constr eq constr then n
    else ENode.Constructor(constr)(n.fields)

  extension (n: Select) def derived(qual: ENode, member: Symbol): ENode.Select =
    if (n.qual eq qual) && (n.member eq member) then n
    else ENode.Select(qual, member)

  extension (n: Apply) def derived(fn: ENode, args: List[ENode]): ENode.Apply =
    if (n.fn eq fn) && (n.args eq args) then n
    else ENode.Apply(fn, args)

  extension (n: OpApply) def derived(op: ENode.Op, args: List[ENode]): ENode.OpApply =
    if (n.fn eq op) && (n.args eq args) then n
    else ENode.OpApply(op, args)

  extension (n: TypeApply) def derived(fn: ENode, args: List[Type]): ENode.TypeApply =
    if (n.fn eq fn) && (n.args eq args) then n
    else ENode.TypeApply(fn, args)

  extension (n: Lambda) def derived(paramTps: List[ENodeParamRef], retTp: Type, body: ENode): ENode.Lambda =
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
