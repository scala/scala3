package scala.tasty.util

import scala.tasty.Tasty

class ShowExtractors[T <: Tasty with Singleton](tasty0: T) extends Show[T](tasty0) {
  import tasty.{rootContext => _, _}

  def showTree(tree: Tree)(implicit ctx: Context): String =
    new Buffer().visitTree(tree).result()
  def showCaseDef(caseDef: CaseDef)(implicit ctx: Context): String =
    new Buffer().visitCaseDef(caseDef).result()

  def showPattern(pattern: tasty.Pattern)(implicit ctx: tasty.Context): String =
    new Buffer().visitPattern(pattern).result()

  def showTypeOrBoundsTree(tpt: TypeOrBoundsTree)(implicit ctx: Context): String =
    new Buffer().visitTypeTree(tpt).result()

  def showTypeOrBounds(tpe: TypeOrBounds)(implicit ctx: Context): String =
    new Buffer().visitType(tpe).result()

  def showConstant(const: Constant)(implicit ctx: Context): String =
    new Buffer().visitConstant(const).result()

  def showSymbol(symbol: Symbol)(implicit ctx: Context): String =
    new Buffer().visitSymbol(symbol).result()

  private class Buffer(implicit ctx: Context) { self =>

    private val sb: StringBuilder = new StringBuilder

    def result(): String = sb.result()

    def visitTree(x: Tree): Buffer = x match {
      case Term.Ident(name) =>
        this += "Term.Ident(\"" += name += "\")"
      case Term.Select(qualifier, name, signature) =>
        this += "Term.Select(" += qualifier += ", \"" += name += "\", " += signature += ")"
      case Term.This(qual) =>
        this += "Term.This(" += qual += ")"
      case Term.Super(qual, mix) =>
        this += "Term.TypeApply(" += qual += ", " += mix += ")"
      case Term.Apply(fun, args) =>
        this += "Term.Apply(" += fun += ", " ++= args += ")"
      case Term.TypeApply(fun, args) =>
        this += "Term.TypeApply(" += fun += ", " ++= args += ")"
      case Term.Literal(const) =>
        this += "Term.Literal(" += const += ")"
      case Term.New(tpt) =>
        this += "Term.New(" += tpt += ")"
      case Term.Typed(expr, tpt) =>
        this += "Term.Typed(" += expr += ", "  += tpt += ")"
      case Term.NamedArg(name, arg) =>
        this += "Term.NamedArg(\"" += name += "\", " += arg += ")"
      case Term.Assign(lhs, rhs) =>
        this += "Term.Assign(" += lhs += ", " += rhs += ")"
      case Term.Block(stats, expr) =>
        this += "Term.Block(" ++= stats += ", " += expr += ")"
      case Term.If(cond, thenp, elsep) =>
        this += "Term.If(" += cond += ", " += thenp += ", " += elsep += ")"
      case Term.Lambda(meth, tpt) =>
        this += "Term.Lambda(" += meth += ", " += tpt += ")"
      case Term.Match(selector, cases) =>
        this += "Term.Match(" += selector += ", " ++= cases += ")"
      case Term.Return(expr) =>
        this += "Term.Return(" += expr += ")"
      case Term.While(cond, body) =>
        this += "Term.While(" += cond += ", " += body += ")"
      case Term.Try(block, handlers, finalizer) =>
        this += "Term.Try(" += block += ", " ++= handlers += ", " += finalizer += ")"
      case Term.Repeated(elems) =>
        this += "Term.Repeated(" ++= elems += ")"
      case Term.Inlined(call, bindings, expansion) =>
        this += "Term.Inlined("
        visitOption(call, visitTermOrTypeTree)
        this += ", " ++= bindings += ", " += expansion += ")"
      case ValDef(name, tpt, rhs) =>
        this += "ValDef(\"" += name += "\", " += tpt += ", " += rhs += ")"
      case DefDef(name, typeParams, paramss, returnTpt, rhs) =>
        this += "DefDef(\"" += name += "\", " ++= typeParams += ", " +++= paramss += ", " += returnTpt += ", " += rhs += ")"
      case TypeDef(name, rhs) =>
        this += "TypeDef(\"" += name += "\", " += rhs += ")"
      case ClassDef(name, constr, parents, self, body) =>
        this += "ClassDef(\"" += name += "\", " += constr += ", "
        visitList[TermOrTypeTree](parents, visitTermOrTypeTree)
        this += ", " += self += ", " ++= body += ")"
      case PackageDef(name, owner) =>
        this += "PackageDef(\"" += name += "\", " += owner += ")"
      case Import(expr, selectors) =>
        this += "Import(" += expr += ", " ++= selectors += ")"
      case PackageClause(pid, stats) =>
        this += "PackageClause(" += pid += ", " ++= stats += ")"
    }

    def visitTypeTree(x: TypeOrBoundsTree): Buffer = x match {
      case TypeTree.Synthetic() =>
        this += "TypeTree.Synthetic()"
      case TypeTree.Ident(name) =>
        this += "TypeTree.Ident(\"" += name += "\")"
      case TypeTree.Select(qualifier, name) =>
        this += "TypeTree.Select(" += qualifier += ", \"" += name += "\")"
      case TypeTree.Project(qualifier, name) =>
        this += "TypeTree.Project(" += qualifier += ", \"" += name += "\")"
      case TypeTree.Singleton(ref) =>
        this += "TypeTree.Singleton(" += ref += ")"
      case TypeTree.And(left, right) =>
        this += "TypeTree.And(" += left += ", " += right += ")"
      case TypeTree.Or(left, right) =>
        this += "TypeTree.Or(" += left += ", " += right += ")"
      case TypeTree.Refined(tpt, refinements) =>
        this += "TypeTree.Refined(" += tpt += ", " ++= refinements += ")"
      case TypeTree.Applied(tpt, args) =>
        this += "TypeTree.Applied(" += tpt += ", " ++= args += ")"
      case TypeTree.ByName(result) =>
        this += "TypeTree.ByName(" += result += ")"
      case TypeTree.Annotated(arg, annot) =>
        this += "TypeTree.Annotated(" += arg += ", " += annot += ")"
      case TypeTree.TypeLambdaTree(tparams, body) =>
        this += "LambdaTypeTree(" ++= tparams += ", " += body += ")"
      case TypeTree.Bind(name, bounds) =>
        this += "Bind(" += name += ", " += bounds += ")"
      case TypeBoundsTree(lo, hi) =>
        this += "TypeBoundsTree(" += lo += ", " += hi += ")"
      case SyntheticBounds() =>
        this += s"SyntheticBounds()"
    }

    def visitCaseDef(x: CaseDef): Buffer = {
      val CaseDef(pat, guard, body) = x
      this += "CaseDef(" += pat += ", " += guard += ", " += body += ")"
    }

    def visitPattern(x: Pattern): Buffer = x match {
      case Pattern.Value(v) =>
        this += "Pattern.Value(" += v += ")"
      case Pattern.Bind(name, body) =>
        this += "Pattern.Bind(\"" += name += "\", " += body += ")"
      case Pattern.Unapply(fun, implicits, patterns) =>
        this += "Pattern.Unapply(" += fun += ", " ++= implicits += ", " ++= patterns += ")"
      case Pattern.Alternative(patterns) =>
        this += "Pattern.Alternative(" ++= patterns += ")"
      case Pattern.TypeTest(tpt) =>
        this += "Pattern.TypeTest(" += tpt += ")"
    }

    def visitTermOrTypeTree(x: TermOrTypeTree): Buffer = x match {
      case IsTerm(termOrTypeTree) => this += termOrTypeTree
      case IsTypeTree(termOrTypeTree) => this += termOrTypeTree
    }

    def visitConstant(x: Constant): Buffer = x match {
      case Constant.Unit() => this += "Constant.Unit()"
      case Constant.Null() => this += "Constant.Null()"
      case Constant.Boolean(value) => this += "Constant.Boolean(" += value += ")"
      case Constant.Byte(value) => this += "Constant.Byte(" += value += ")"
      case Constant.Short(value) => this += "Constant.Short(" += value += ")"
      case Constant.Char(value) => this += "Constant.Char(" += value += ")"
      case Constant.Int(value) => this += "Constant.Int(" += value.toString += ")"
      case Constant.Long(value) => this += "Constant.Long(" += value += ")"
      case Constant.Float(value) => this += "Constant.Float(" += value += ")"
      case Constant.Double(value) => this += "Constant.Double(" += value += ")"
      case Constant.String(value) => this += "Constant.String(\"" += value += "\")"
      case Constant.ClassTag(value) => this += "Constant.ClassTag(" += value += ")"
      case Constant.Symbol(value) => this += "Constant.Symbol('" += value.name += ")"
    }

    def visitType(x: TypeOrBounds): Buffer = x match {
      case Type.ConstantType(value) =>
        this += "Type.ConstantType(" += value += ")"
      case Type.SymRef(sym, qual) =>
        this += "Type.SymRef(" += sym += ", " += qual += ")"
      case Type.TermRef(name, qual) =>
        this += "Type.TermRef(\"" += name += "\", " += qual += ")"
      case Type.TypeRef(name, qual) =>
        this += "Type.TypeRef(\"" += name += "\", " += qual += ")"
      case Type.Refinement(parent, name, info) =>
        this += "Type.Refinement(" += parent += ", " += name += ", " += info += ")"
      case Type.AppliedType(tycon, args) =>
        this += "Type.AppliedType(" += tycon += ", " ++= args += ")"
      case Type.AnnotatedType(underlying, annot) =>
        this += "Type.AnnotatedType(" += underlying += ", " += annot += ")"
      case Type.AndType(left, right) =>
        this += "Type.AndType(" += left += ", " += right += ")"
      case Type.OrType(left, right) =>
        this += "Type.OrType(" += left += ", " += right += ")"
      case Type.ByNameType(underlying) =>
        this += "Type.ByNameType(" += underlying += ")"
      case Type.ParamRef(binder, idx) =>
        this += "Type.ParamRef(" += binder += ", " += idx += ")"
      case Type.ThisType(tp) =>
        this += "Type.ThisType(" += tp += ")"
      case Type.SuperType(thistpe, supertpe) =>
        this += "Type.SuperType(" += thistpe += ", " += supertpe += ")"
      case Type.RecursiveThis(binder) =>
        this += "Type.RecursiveThis(" += binder += ")"
      case Type.RecursiveType(underlying) =>
        this += "Type.RecursiveType(" += underlying += ")"
      case Type.MethodType(argNames, argTypes, resType) =>
        this += "Type.MethodType(" ++= argNames += ", " ++= argTypes += ", " += resType += ")"
      case Type.PolyType(argNames, argBounds, resType) =>
        this += "Type.PolyType(" ++= argNames += ", " ++= argBounds += ", " += resType += ")"
      case Type.TypeLambda(argNames, argBounds, resType) =>
        // resType is not printed to avoid cycles
        this += "Type.TypeLambda(" ++= argNames += ", " ++= argBounds += ", _)"
      case TypeBounds(lo, hi) =>
        this += "TypeBounds(" += lo += ", " += hi += ")"
      case NoPrefix() =>
        this += "NoPrefix()"
    }

    def visitId(x: Id): Buffer = {
      val Id(name) = x
      this += "Id(\"" += name += "\")"
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

    def visitSymbol(x: Symbol): Buffer = x match {
      case IsPackageSymbol(x) => this += "IsPackageSymbol(<" += x.fullName += ">)"
      case IsClassSymbol(x) => this += "IsClassSymbol(<" += x.fullName += ">)"
      case IsDefSymbol(x) => this += "IsDefSymbol(<" += x.fullName += ">)"
      case IsValSymbol(x) => this += "IsValSymbol(<" += x.fullName += ">)"
      case IsTypeSymbol(x) => this += "IsTypeSymbol(<" += x.fullName += ">)"
      case NoSymbol() => this += "NoSymbol()"
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
      def +=(x: Option[TypeOrBoundsTree]): Buffer = { visitOption(x, visitTypeTree); buff }
      def ++=(x: List[TypeOrBoundsTree]): Buffer = { visitList(x, visitTypeTree); buff }
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

    private implicit class SymbolOps(buff: Buffer) {
      def +=(x: Symbol): Buffer = { visitSymbol(x); buff }
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
