package scala.tasty
package reflect

class ExtractorsPrinter[R <: Reflection & Singleton](val tasty: R) extends Printer[R] {
  import tasty.{_, given}

  def showTree(tree: Tree)(given ctx: Context): String =
    new Buffer().visitTree(tree).result()

  def showTypeOrBounds(tpe: TypeOrBounds)(given ctx: Context): String =
    new Buffer().visitType(tpe).result()

  def showConstant(const: Constant)(given ctx: Context): String =
    new Buffer().visitConstant(const).result()

  def showSymbol(symbol: Symbol)(given ctx: Context): String =
    new Buffer().visitSymbol(symbol).result()

  def showFlags(flags: Flags)(given ctx: Context): String = {
    val flagList = List.newBuilder[String]
    if (flags.is(Flags.Private)) flagList += "Flags.Private"
    if (flags.is(Flags.Protected)) flagList += "Flags.Protected"
    if (flags.is(Flags.Abstract)) flagList += "Flags.Abstract"
    if (flags.is(Flags.Final)) flagList += "Flags.Final"
    if (flags.is(Flags.Sealed)) flagList += "Flags.Sealed"
    if (flags.is(Flags.Case)) flagList += "Flags.Case"
    if (flags.is(Flags.Implicit)) flagList += "Flags.Implicit"
    if (flags.is(Flags.Erased)) flagList += "Flags.Erased"
    if (flags.is(Flags.Lazy)) flagList += "Flags.Lazy"
    if (flags.is(Flags.Override)) flagList += "Flags.Override"
    if (flags.is(Flags.Inline)) flagList += "Flags.Inline"
    if (flags.is(Flags.Macro)) flagList += "Flags.Macro"
    if (flags.is(Flags.JavaDefined)) flagList += "Flags.JavaDefined"
    if (flags.is(Flags.Static)) flagList += "Flags.javaStatic"
    if (flags.is(Flags.Object)) flagList += "Flags.Object"
    if (flags.is(Flags.Trait)) flagList += "Flags.Trait"
    if (flags.is(Flags.Local)) flagList += "Flags.Local"
    if (flags.is(Flags.Synthetic)) flagList += "Flags.Synthetic"
    if (flags.is(Flags.Artifact)) flagList += "Flags.Artifact"
    if (flags.is(Flags.Mutable)) flagList += "Flags.Mutable"
    if (flags.is(Flags.FieldAccessor)) flagList += "Flags.FieldAccessor"
    if (flags.is(Flags.CaseAcessor)) flagList += "Flags.CaseAcessor"
    if (flags.is(Flags.Covariant)) flagList += "Flags.Covariant"
    if (flags.is(Flags.Contravariant)) flagList += "Flags.Contravariant"
    if (flags.is(Flags.Scala2X)) flagList += "Flags.Scala2X"
    if (flags.is(Flags.DefaultParameterized)) flagList += "Flags.DefaultParameterized"
    if (flags.is(Flags.StableRealizable)) flagList += "Flags.StableRealizable"
    if (flags.is(Flags.Param)) flagList += "Flags.Param"
    if (flags.is(Flags.ParamAccessor)) flagList += "Flags.ParamAccessor"
    if (flags.is(Flags.Enum)) flagList += "Flags.Enum"
    if (flags.is(Flags.ModuleClass)) flagList += "Flags.ModuleClass"
    if (flags.is(Flags.PrivateLocal)) flagList += "Flags.PrivateLocal"
    if (flags.is(Flags.Package)) flagList += "Flags.Package"
    flagList.result().mkString(" | ")
  }

  private class Buffer(given ctx: Context) { self =>

    private val sb: StringBuilder = new StringBuilder

    def result(): String = sb.result()

    def visitTree(x: Tree): Buffer = x match {
      case Ident(name) =>
        this += "Ident(\"" += name += "\")"
      case Select(qualifier, name) =>
        this += "Select(" += qualifier += ", \"" += name += "\")"
      case This(qual) =>
        this += "This(" += qual += ")"
      case Super(qual, mix) =>
        this += "Super(" += qual += ", " += mix += ")"
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
        this += "NamedArg(\"" += name += "\", " += arg += ")"
      case Assign(lhs, rhs) =>
        this += "Assign(" += lhs += ", " += rhs += ")"
      case Block(stats, expr) =>
        this += "Block(" ++= stats += ", " += expr += ")"
      case If(cond, thenp, elsep) =>
        this += "If(" += cond += ", " += thenp += ", " += elsep += ")"
      case Closure(meth, tpt) =>
        this += "Closure(" += meth += ", " += tpt += ")"
      case Match(selector, cases) =>
        this += "Match(" += selector += ", " ++= cases += ")"
      case ImpliedMatch(cases) =>
        this += "ImpliedMatch(" ++= cases += ")"
      case Return(expr) =>
        this += "Return(" += expr += ")"
      case While(cond, body) =>
        this += "While(" += cond += ", " += body += ")"
      case Try(block, handlers, finalizer) =>
        this += "Try(" += block += ", " ++= handlers += ", " += finalizer += ")"
      case Repeated(elems, elemtpt) =>
        this += "Repeated(" ++= elems += ", " += elemtpt += ")"
      case Inlined(call, bindings, expansion) =>
        this += "Inlined("
        visitOption(call, visitTree)
        this += ", " ++= bindings += ", " += expansion += ")"
      case ValDef(name, tpt, rhs) =>
        this += "ValDef(\"" += name += "\", " += tpt += ", " += rhs += ")"
      case DefDef(name, typeParams, paramss, returnTpt, rhs) =>
        this += "DefDef(\"" += name += "\", " ++= typeParams += ", " +++= paramss += ", " += returnTpt += ", " += rhs += ")"
      case TypeDef(name, rhs) =>
        this += "TypeDef(\"" += name += "\", " += rhs += ")"
      case ClassDef(name, constr, parents, derived, self, body) =>
        this += "ClassDef(\"" += name += "\", " += constr += ", "
        visitList[Tree](parents, visitTree)
        this += ", "
        visitList[TypeTree](derived, visitTree)
        this += ", " += self += ", " ++= body += ")"
      case PackageDef(name, owner) =>
        this += "PackageDef(\"" += name += "\", " += owner += ")"
      case Import(expr, selectors) =>
        this += "Import(" += expr += ", " ++= selectors += ")"
      case PackageClause(pid, stats) =>
        this += "PackageClause(" += pid += ", " ++= stats += ")"
      case Inferred() =>
        this += "Inferred()"
      case TypeIdent(name) =>
        this += "TypeIdent(\"" += name += "\")"
      case TypeSelect(qualifier, name) =>
        this += "TypeSelect(" += qualifier += ", \"" += name += "\")"
      case Projection(qualifier, name) =>
        this += "Projection(" += qualifier += ", \"" += name += "\")"
      case Singleton(ref) =>
        this += "Singleton(" += ref += ")"
      case Refined(tpt, refinements) =>
        this += "Refined(" += tpt += ", " ++= refinements += ")"
      case Applied(tpt, args) =>
        this += "Applied(" += tpt += ", " ++= args += ")"
      case ByName(result) =>
        this += "ByName(" += result += ")"
      case Annotated(arg, annot) =>
        this += "Annotated(" += arg += ", " += annot += ")"
      case LambdaTypeTree(tparams, body) =>
        this += "LambdaTypeTree(" ++= tparams += ", " += body += ")"
      case TypeBind(name, bounds) =>
        this += "TypeBind(" += name += ", " += bounds += ")"
      case TypeBlock(aliases, tpt) =>
        this += "TypeBlock(" ++= aliases += ", " += tpt += ")"
      case TypeBoundsTree(lo, hi) =>
        this += "TypeBoundsTree(" += lo += ", " += hi += ")"
      case WildcardTypeTree() =>
        this += s"WildcardTypeTree()"
      case MatchTypeTree(bound, selector, cases) =>
        this += "MatchTypeTree(" += bound += ", " += selector += ", " ++= cases += ")"
      case CaseDef(pat, guard, body) =>
        this += "CaseDef(" += pat += ", " += guard += ", " += body += ")"
      case TypeCaseDef(pat, body) =>
        this += "TypeCaseDef(" += pat += ", " += body += ")"
      case Bind(name, body) =>
        this += "Bind(\"" += name += "\", " += body += ")"
      case Unapply(fun, implicits, patterns) =>
        this += "Unapply(" += fun += ", " ++= implicits += ", " ++= patterns += ")"
      case Alternatives(patterns) =>
        this += "Alternative(" ++= patterns += ")"
    }

    def visitConstant(x: Constant): Buffer = x match {
      case Constant(()) => this += "Constant(())"
      case Constant(null) => this += "Constant(null)"
      case Constant(value: Boolean) => this += "Constant(" += value += ")"
      case Constant(value: Byte) => this += "Constant(" += value += ": Byte)"
      case Constant(value: Short) => this += "Constant(" += value += ": Short)"
      case Constant(value: Char) => this += "Constant('" += value += "')"
      case Constant(value: Int) => this += "Constant(" += value.toString += ")"
      case Constant(value: Long) => this += "Constant(" += value += "L)"
      case Constant(value: Float) => this += "Constant(" += value += "f)"
      case Constant(value: Double) => this += "Constant(" += value += "d)"
      case Constant(value: String) => this += "Constant(\"" += value += "\")"
      case Constant.ClassTag(value) =>
        this += "Constant.ClassTag("
        visitType(value) += ")"
    }

    def visitType(x: TypeOrBounds): Buffer = x match {
      case ConstantType(value) =>
        this += "ConstantType(" += value += ")"
      case TermRef(qual, name) =>
        this += "TermRef(" += qual+= ", \"" += name += "\")"
      case TypeRef(qual, name) =>
        this += "TypeRef(" += qual += ", \"" += name += "\")"
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
      case MatchType(bound, scrutinee, cases) =>
        this += "MatchType(" += bound += ", " += scrutinee += ", " ++= cases += ")"
      case ByNameType(underlying) =>
        this += "ByNameType(" += underlying += ")"
      case ParamRef(binder, idx) =>
        this += "ParamRef(" += binder += ", " += idx += ")"
      case ThisType(tp) =>
        this += "ThisType(" += tp += ")"
      case SuperType(thistpe, supertpe) =>
        this += "SuperType(" += thistpe += ", " += supertpe += ")"
      case RecursiveThis(binder) =>
        this += "RecursiveThis(" += binder += ")"
      case RecursiveType(underlying) =>
        this += "RecursiveType(" += underlying += ")"
      case MethodType(argNames, argTypes, resType) =>
        this += "MethodType(" ++= argNames += ", " ++= argTypes += ", " += resType += ")"
      case PolyType(argNames, argBounds, resType) =>
        this += "PolyType(" ++= argNames += ", " ++= argBounds += ", " += resType += ")"
      case TypeLambda(argNames, argBounds, resType) =>
        // resType is not printed to avoid cycles
        this += "TypeLambda(" ++= argNames += ", " ++= argBounds += ", _)"
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
      this += "Signature(" ++= params.map(_.toString) += ", " += res += ")"
    }

    def visitImportSelector(sel: ImportSelector): Buffer = sel match {
      case SimpleSelector(id) => this += "SimpleSelector(" += id += ")"
      case RenameSelector(id1, id2) => this += "RenameSelector(" += id1 += ", " += id2 += ")"
      case OmitSelector(id) => this += "OmitSelector(" += id += ")"
    }

    def visitSymbol(x: Symbol): Buffer =
      if x.isPackageDef  then this += "IsPackageDefSymbol(<" += x.fullName += ">)"
      else if x.isClassDef then this += "IsClassDefSymbol(<" += x.fullName += ">)"
      else if x.isDefDef then this += "IsDefDefSymbol(<" += x.fullName += ">)"
      else if x.isValDef then this += "IsValDefSymbol(<" += x.fullName += ">)"
      else if x.isTypeDef then this += "IsTypeDefSymbol(<" += x.fullName += ">)"
      else { assert(x.isNoSymbol); this += "NoSymbol()" }

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

    private implicit class ConstantOps(buff: Buffer) {
      def +=(x: Constant): Buffer = { visitConstant(x); buff }
    }

    private implicit class TypeOps(buff: Buffer) {
      def +=(x: TypeOrBounds): Buffer = { visitType(x); buff }
      def +=(x: Option[TypeOrBounds]): Buffer = { visitOption(x, visitType); buff }
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
