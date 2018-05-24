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
      case Term.Ident(name) =>
        this += "Ident(" += name += ")"
      case Term.Select(qualifier, name, signature) =>
        this += "Select(" += qualifier += ", " += name += ", " += signature += ")"
      case Term.This(qual) =>
        this += "This(" += qual += ")"
      case Term.Super(qual, mix) =>
        this += "TypeApply(" += qual += ", " += mix += ")"
      case Term.Apply(fun, args) =>
        this += "Apply(" += fun += ", " ++= args += ")"
      case Term.TypeApply(fun, args) =>
        this += "TypeApply(" += fun += ", " ++= args += ")"
      case Term.Literal(const) =>
        this += "Literal(" += const += ")"
      case Term.New(tpt) =>
        this += "New(" += tpt += ")"
      case Term.Typed(expr, tpt) =>
        this += "Typed(" += expr += ", "  += tpt += ")"
      case Term.NamedArg(name, arg) =>
        this += "NamedArg(" += name += ", " += arg += ")"
      case Term.Assign(lhs, rhs) =>
        this += "Assign(" += lhs += ", " += rhs += ")"
      case Term.Block(stats, expr) =>
        this += "Block(" ++= stats += ", " += expr += ")"
      case Term.If(cond, thenp, elsep) =>
        this += "If(" += cond += ", " += thenp += ", " += elsep += ")"
      case Term.Lambda(meth, tpt) =>
        this += "Lambda(" += meth += ", " += tpt += ")"
      case Term.Match(selector, cases) =>
        this += "Match(" += selector += ", " ++= cases += ")"
      case Term.Return(expr) =>
        this += "Return(" += expr += ")"
      case Term.Try(block, handlers, finalizer) =>
        this += "Try(" += block += ", " ++= handlers += ", " += finalizer += ")"
      case Term.Repeated(elems) =>
        this += "Repeated(" ++= elems += ")"
      case Term.Inlined(call, bindings, expansion) =>
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
      case TypeTree.Synthetic() =>
        this += "Synthetic()"
      case TypeTree.TypeIdent(name) =>
        this += "TypeIdent(" += name += ")"
      case TypeTree.TypeSelect(qualifier, name) =>
        this += "TypeSelect(" += qualifier += ", " += name += ")"
      case TypeTree.Singleton(ref) =>
        this += "Singleton(" += ref += ")"
      case TypeTree.And(left, right) =>
        this += "And(" += left += ", " += right += ")"
      case TypeTree.Or(left, right) =>
        this += "Or(" += left += ", " += right += ")"
      case TypeTree.Refined(tpt, refinements) =>
        this += "Refined(" += tpt += ", " ++= refinements += ")"
      case TypeTree.Applied(tpt, args) =>
        this += "Applied(" += tpt += ", " ++= args += ")"
      case TypeTree.ByName(result) =>
        this += "ByName(" += result += ")"
      case TypeTree.Annotated(arg, annot) =>
        this += "Annotated(" += arg += ", " += annot += ")"
      case TypeBoundsTree(lo, hi) =>
        this += "TypeBoundsTree(" += lo += ", " += hi += ")"
    }

    def visitCaseDef(x: CaseDef): Buffer = {
      val CaseDef(pat, guard, body) = x
      this += "CaseDef(" += pat += ", " += guard += ", " += body += ")"
    }

    def visitPattern(x: Pattern): Buffer = x match {
      case Pattern.Value(v) =>
        this += "Value(" += v += ")"
      case Pattern.Bind(name, body) =>
        this += "Bind(" += name += ", " += body += ")"
      case Pattern.Unapply(fun, implicits, patterns) =>
        this += "Unapply(" += fun += ", " ++= implicits += ", " ++= patterns += ")"
      case Pattern.Alternative(patterns) =>
        this += "Alternative(" ++= patterns += ")"
      case Pattern.TypeTest(tpt) =>
        this += "TypeTest(" += tpt += ")"
    }

    def visitConstant(x: Constant): Buffer = x match {
      case Constant.Unit() => this += "Unit()"
      case Constant.Null() => this += "Null()"
      case Constant.Boolean(value) => this += "Boolean(" += value += ")"
      case Constant.Byte(value) => this += "Byte(" += value += ")"
      case Constant.Short(value) => this += "Short(" += value += ")"
      case Constant.Char(value) => this += "Char(" += value += ")"
      case Constant.Int(value) => this += "Int(" += value.toString += ")"
      case Constant.Long(value) => this += "Long(" += value += ")"
      case Constant.Float(value) => this += "Float(" += value += ")"
      case Constant.Double(value) => this += "Double(" += value += ")"
      case Constant.String(value) => this += "String(" += value += ")"
    }

    def visitType(x: TypeOrBounds): Buffer = x match {
      case Type.ConstantType(value) =>
        this += "ConstantType(" += value += ")"
      case Type.SymRef(sym, qual) =>
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
      case Type.TermRef(name, qual) =>
        this += "TermRef(" += name += ", " += qual += ")"
      case Type.TypeRef(name, qual) =>
        this += "TypeRef(" += name += ", " += qual += ")"
      case Type.Refinement(parent, name, info) =>
        this += "Refinement(" += parent += ", " += name += ", " += info += ")"
      case Type.AppliedType(tycon, args) =>
        this += "AppliedType(" += tycon += ", " ++= args += ")"
      case Type.AnnotatedType(underlying, annot) =>
        this += "AnnotatedType(" += underlying += ", " += annot += ")"
      case Type.AndType(left, right) =>
        this += "AndType(" += left += ", " += right += ")"
      case Type.OrType(left, right) =>
        this += "OrType(" += left += ", " += right += ")"
      case Type.ByNameType(underlying) =>
        this += "ByNameType(" += underlying += ")"
      case Type.ParamRef(binder, idx) =>
        this += "ParamRef(" += binder+= ", " += idx += ")"
      case Type.ThisType(tp) =>
        this += "ThisType(" += tp += ")"
      case Type.RecursiveThis(binder) =>
        this += "RecursiveThis(" += binder += ")"
      case Type.MethodType(argNames, argTypes, resType) =>
        this += "MethodType(" ++= argNames += ", " ++= argTypes += ", " += resType += ")"
      case Type.PolyType(argNames, argBounds, resType) =>
        this += "PolyType(" ++= argNames += ", " ++= argBounds += ", " += resType += ")"
      case Type.TypeLambda(argNames, argBounds, resType) =>
        this += "TypeLambda(" ++= argNames += ", " ++= argBounds += ", " += resType += ")"
      case TypeBounds(lo, hi) =>
        this += "TypeBounds(" += lo += ", " += hi += ")"
      case NoPrefix() =>
        this += "NoPrefix"
    }

    def visitModifier(x: Modifier): Buffer = x match {
      case Modifier.Flags(flags) => this += "Flags(" += flags.toString += ")"
      case Modifier.QualifiedPrivate(tp) => this += "QualifiedPrivate(" += tp += ")"
      case Modifier.QualifiedProtected(tp) => this += "QualifiedProtected(" += tp += ")"
      case Modifier.Annotation(tree) => this += "Annotation(" += tree += ")"
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
