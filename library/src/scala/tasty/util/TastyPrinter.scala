package scala.tasty.util

import scala.tasty.Tasty

class TastyPrinter[T <: Tasty with Singleton](val tasty: T) {
  import tasty._

  def stringOfTree(tree: Tree)(implicit ctx: Context): String =
    new Buffer().visitTree(tree).result()

  def stringOfTypeTree(tree: TypeOrBoundsTree)(implicit ctx: Context): String =
    new Buffer().visitTypeTree(tree).result()

  def stringOfType(tpe: TypeOrBounds)(implicit ctx: Context): String =
    new Buffer().visitType(tpe).result()

  def stringOfModifier(mod: Modifier)(implicit ctx: Context): String =
    new Buffer().visitModifier(mod).result()

  def stringOfConstant(const: Constant)(implicit ctx: Context): String =
    new Buffer().visitConstant(const).result()

  private class Buffer(implicit ctx: Context) { self =>

    private val sb: StringBuilder = new StringBuilder

    def result(): String = sb.result()

    def visitTree(x: Tree): Buffer = x match {
      case Ident(name) =>
        this += "Ident(" += name += ")"
      case Select(qualifier, name, signature) =>
        this += "Select(" += qualifier += ", " += name += ", " += signature += ")"
      case This(qual) =>
        this += "This(" += qual += ")"
      case Super(qual, mix) =>
        this += "TypeApply(" += qual += ", " += mix += ")"
      case Apply(fun, args) =>
        this += "Apply(" += fun += ", " ++= args += ")"
      case TypeApply(fun, args) =>
        this += "TypeApply(" += fun += ", " ++= args += ")"
      case Literal(const) =>
        this += "Literal(" += const += ")"
      case New(tpt) =>
        this += "New(" += tpt += ")"
      case Typed(expr, tpt) =>
        this += "Typed(" += expr += ", "  += tpt += ")"
      case NamedArg(name, arg) =>
        this += "NamedArg(" += name += ", " += arg += ")"
      case Assign(lhs, rhs) =>
        this += "Assign(" += lhs += ", " += rhs += ")"
      case Block(stats, expr) =>
        this += "Block(" ++= stats += ", " += expr += ")"
      case If(cond, thenp, elsep) =>
        this += "If(" += cond += ", " += thenp += ", " += elsep += ")"
      case Lambda(meth, tpt) =>
        this += "Lambda(" += meth += ", " += tpt += ")"
      case Match(selector, cases) =>
        this += "Match(" += selector += ", " ++= cases += ")"
      case Return(expr) =>
        this += "Return(" += expr += ")"
      case Try(block, handlers, finalizer) =>
        this += "Try(" += block += ", " ++= handlers += ", " += finalizer += ")"
      case Repeated(elems) =>
        this += "Repeated(" ++= elems += ")"
      case Inlined(call, bindings, expansion) =>
        this += "Inlined(" += call += ", " ++= bindings += ", " += expansion += ")"
      case ValDef(name, tpt, rhs) =>
        this += "ValDef(" += name += ", " += tpt += ", " += rhs += ")"
      case DefDef(name, typeParams, paramss, returnTpt, rhs) =>
        this += "DefDef(" += name += ", " ++= typeParams += ", " +++= paramss += ", " += returnTpt += ", " += rhs += ")"
      case TypeDef(name, rhs) =>
        this += "TypeDef(" += name += ", " += rhs += ")"
      case ClassDef(name, constr, parents, self, body) =>
        this += "ClassDef(" += name += ", " += constr += ", "
        visitList[Parent](parents, {
          case parent @ Term() => this += parent
          case parent @ TypeTree() => this += parent
        })
        this += ", " += self += ", " ++= body += ")"
      case PackageDef(name, members) =>
        this += "PackageDef(" += name += ", " ++= members += ")"
      case Import(expr, selectors) =>
        this += "Import(" += expr += ", " ++= selectors += ")"
      case PackageClause(pid, stats) =>
        this += "PackageClause(" += pid += ", " ++= stats += ")"
    }

    def visitTypeTree(x: TypeOrBoundsTree): Buffer = x match {
      case Synthetic() =>
        this += "Synthetic()"
      case TypeIdent(name) =>
        this += "TypeIdent(" += name += ")"
      case TypeSelect(qualifier, name) =>
        this += "TypeSelect(" += qualifier += ", " += name += ")"
      case Singleton(ref) =>
        this += "Singleton(" += ref += ")"
      case And(left, right) =>
        this += "And(" += left += ", " += right += ")"
      case Or(left, right) =>
        this += "Or(" += left += ", " += right += ")"
      case Refined(tpt, refinements) =>
        this += "Refined(" += tpt += ", " ++= refinements += ")"
      case Applied(tpt, args) =>
        this += "Applied(" += tpt += ", " ++= args += ")"
      case ByName(result) =>
        this += "ByName(" += result += ")"
      case Annotated(arg, annot) =>
        this += "Annotated(" += arg += ", " += annot += ")"
      case TypeBoundsTree(lo, hi) =>
        this += "TypeBoundsTree(" += lo += ", " += hi += ")"
    }

    def visitCaseDef(x: CaseDef): Buffer = {
      val CaseDef(pat, guard, body) = x
      this += "CaseDef(" += pat += ", " += guard += ", " += body += ")"
    }

    def visitPattern(x: Pattern): Buffer = x match {
      case Value(v) =>
        this += "Value(" += v += ")"
      case Bind(name, body) =>
        this += "Bind(" += name += ", " += body += ")"
      case Unapply(fun, implicits, patterns) =>
        this += "Unapply(" += fun += ", " ++= implicits += ", " ++= patterns += ")"
      case Alternative(patterns) =>
        this += "Alternative(" ++= patterns += ")"
      case TypeTest(tpt) =>
        this += "TypeTest(" += tpt += ")"
    }

    def visitConstant(x: Constant): Buffer = x match {
      case UnitConstant() => this += "Unit()"
      case NullConstant() => this += "Null()"
      case BooleanConstant(value) => this += "Boolean(" += value += ")"
      case ByteConstant(value) => this += "Byte(" += value += ")"
      case ShortConstant(value) => this += "Short(" += value += ")"
      case CharConstant(value) => this += "Char(" += value += ")"
      case IntConstant(value) => this += "Int(" += value.toString += ")"
      case LongConstant(value) => this += "Long(" += value += ")"
      case FloatConstant(value) => this += "Float(" += value += ")"
      case DoubleConstant(value) => this += "Double(" += value += ")"
      case StringConstant(value) => this += "String(" += value += ")"
    }

    def visitType(x: TypeOrBounds): Buffer = x match {
      case ConstantType(value) =>
        this += "ConstantType(" += value += ")"
      case SymRef(sym, qual) =>
        def visitName(sym: Definition): Buffer = sym match {
          case ValDef(name, _, _) => this += name
          case DefDef(name, _, _, _, _) => this += name
          case TypeDef(name, _) => this += name
          case ClassDef(name, _, _, _, _) => this += name
          case PackageDef(name, _) => this += name
          case _ => this += "#"
        }
        this += "SymRef("
        visitName(sym)
        this += ", " += qual += ")"
      case TermRef(name, qual) =>
        this += "TermRef(" += name += ", " += qual += ")"
      case TypeRef(name, qual) =>
        this += "TypeRef(" += name += ", " += qual += ")"
      case Refinement(parent, name, info) =>
        this += "Refinement(" += parent += ", " += name += ", " += info += ")"
      case AppliedType(tycon, args) =>
        this += "AppliedType(" += tycon += ", " ++= args += ")"
      case AnnotatedType(underlying, annot) =>
        this += "AnnotatedType(" += underlying += ", " += annot += ")"
      case AndType(left, right) =>
        this += "AndType(" += left += ", " += right += ")"
      case OrType(left, right) =>
        this += "OrType(" += left += ", " += right += ")"
      case ByNameType(underlying) =>
        this += "ByNameType(" += underlying += ")"
      case ParamRef(binder, idx) =>
        this += "ParamRef(" += binder+= ", " += idx += ")"
      case ThisType(tp) =>
        this += "ThisType(" += tp += ")"
      case RecursiveThis(binder) =>
        this += "RecursiveThis(" += binder += ")"
      case MethodType(argNames, argTypes, resType) =>
        this += "MethodType(" ++= argNames += ", " ++= argTypes += ", " += resType += ")"
      case PolyType(argNames, argBounds, resType) =>
        this += "PolyType(" ++= argNames += ", " ++= argBounds += ", " += resType += ")"
      case TypeLambda(argNames, argBounds, resType) =>
        this += "TypeLambda(" ++= argNames += ", " ++= argBounds += ", " += resType += ")"
      case TypeBounds(lo, hi) =>
        this += "TypeBounds(" += lo += ", " += hi += ")"
      case NoPrefix() =>
        this += "NoPrefix"
    }

    def visitModifier(x: Modifier): Buffer = x match {
      case Flags(flags) => this += "Flags(" += flags.toString += ")"
      case QualifiedPrivate(tp) => this += "QualifiedPrivate(" += tp += ")"
      case QualifiedProtected(tp) => this += "QualifiedProtected(" += tp += ")"
      case Annotation(tree) => this += "Annotation(" += tree += ")"
    }

    def visitId(x: Id): Buffer = {
      val Id(name) = x
      this += "Id(" += name += ")"
    }

    def visitSignature(sig: Signature): Buffer = {
      val Signature(params, res) = sig
      this += "Signature(" ++= params += ", " += res += ")"
    }

    def visitImportSelector(sel: ImportSelector): Buffer = sel match {
      case SimpleSelector(id) => this += "SimpleSelector(" += id += ")"
      case RenameSelector(id1, id2) => this += "RenameSelector(" += id1 += ", " += id2 += ")"
      case OmitSelector(id) => this += "OmitSelector(" += id += ")"
    }

    def +=(x: Boolean): Buffer = { sb.append(x); this }
    def +=(x: Byte): Buffer = { sb.append(x); this }
    def +=(x: Short): Buffer = { sb.append(x); this }
    def +=(x: Int): Buffer = { sb.append(x); this }
    def +=(x: Long): Buffer = { sb.append(x); this }
    def +=(x: Float): Buffer = { sb.append(x); this }
    def +=(x: Double): Buffer = { sb.append(x); this }
    def +=(x: Char): Buffer = { sb.append(x); this }
    def +=(x: String): Buffer = { sb.append(x); this }

    def ++=(xs: List[String]): Buffer = visitList[String](xs, +=)

    private implicit class TreeOps(buff: Buffer) {
      def +=(x: Tree): Buffer = { visitTree(x); buff }
      def +=(x: Option[Tree]): Buffer = { visitOption(x, visitTree); buff }
      def ++=(x: List[Tree]): Buffer = { visitList(x, visitTree); buff }
      def +++=(x: List[List[Tree]]): Buffer = { visitList(x, ++=); buff }
    }

    private implicit class CaseDefOps(buff: Buffer) {
      def +=(x: CaseDef): Buffer = { visitCaseDef(x); buff }
      def ++=(x: List[CaseDef]): Buffer = { visitList(x, visitCaseDef); buff }
    }

    private implicit class PatternOps(buff: Buffer) {
      def +=(x: Pattern): Buffer = { visitPattern(x); buff }
      def ++=(x: List[Pattern]): Buffer = { visitList(x, visitPattern); buff }
    }

    private implicit class ConstantOps(buff: Buffer) {
      def +=(x: Constant): Buffer = { visitConstant(x); buff }
    }

    private implicit class TypeTreeOps(buff: Buffer) {
      def +=(x: TypeOrBoundsTree): Buffer = { visitTypeTree(x); buff }
      def +=(x: Option[TypeTree]): Buffer = { visitOption(x, visitTypeTree); buff }
      def ++=(x: List[TypeTree]): Buffer = { visitList(x, visitTypeTree); buff }
    }

    private implicit class TypeOps(buff: Buffer) {
      def +=(x: TypeOrBounds): Buffer = { visitType(x); buff }
      def ++=(x: List[TypeOrBounds]): Buffer = { visitList(x, visitType); buff }
    }

    private implicit class IdOps(buff: Buffer) {
      def +=(x: Id): Buffer = { visitId(x); buff }
      def +=(x: Option[Id]): Buffer = { visitOption(x, visitId); buff }
    }

    private implicit class SignatureOps(buff: Buffer) {
      def +=(x: Option[Signature]): Buffer = { visitOption(x, visitSignature); buff }
    }

    private implicit class ImportSelectorOps(buff: Buffer) {
      def ++=(x: List[ImportSelector]): Buffer = { visitList(x, visitImportSelector); buff }
    }

    private def visitOption[U](opt: Option[U], visit: U => Buffer): Buffer = opt match {
      case Some(x) =>
        this += "Some("
        visit(x)
        this += ")"
      case _ =>
        this += "None"
    }

    private def visitList[U](list: List[U], visit: U => Buffer): Buffer = list match {
      case x0 :: xs =>
        this += "List("
        visit(x0)
        def visitNext(xs: List[U]): Unit = xs match {
          case y :: ys =>
            this += ", "
            visit(y)
            visitNext(ys)
          case Nil =>
        }
        visitNext(xs)
        this += ")"
      case Nil =>
        this += "Nil"
    }
  }

}
