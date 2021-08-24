package scala.quoted
package runtime.impl.printers

import scala.quoted._

object Extractors {

  def showTree(using Quotes)(tree: quotes.reflect.Tree): String =
    new ExtractorsPrinter[quotes.type]().visitTree(tree).result()

  def showType(using Quotes)(tpe: quotes.reflect.TypeRepr): String =
    new ExtractorsPrinter[quotes.type]().visitType(tpe).result()

  def showConstant(using Quotes)(const: quotes.reflect.Constant): String =
    new ExtractorsPrinter[quotes.type]().visitConstant(const).result()

  def showSymbol(using Quotes)(symbol: quotes.reflect.Symbol): String =
    new ExtractorsPrinter[quotes.type]().visitSymbol(symbol).result()

  def showFlags(using Quotes)(flags: quotes.reflect.Flags): String = {
    import quotes.reflect._
    val flagList = List.newBuilder[String]
    if (flags.is(Flags.Abstract)) flagList += "Flags.Abstract"
    if (flags.is(Flags.Artifact)) flagList += "Flags.Artifact"
    if (flags.is(Flags.Case)) flagList += "Flags.Case"
    if (flags.is(Flags.CaseAccessor)) flagList += "Flags.CaseAccessor"
    if (flags.is(Flags.Contravariant)) flagList += "Flags.Contravariant"
    if (flags.is(Flags.Covariant)) flagList += "Flags.Covariant"
    if (flags.is(Flags.Deferred)) flagList += "Flags.Deferred"
    if (flags.is(Flags.Enum)) flagList += "Flags.Enum"
    if (flags.is(Flags.Erased)) flagList += "Flags.Erased"
    if (flags.is(Flags.Exported)) flagList += "Flags.Exported"
    if (flags.is(Flags.ExtensionMethod)) flagList += "Flags.ExtensionMethod"
    if (flags.is(Flags.FieldAccessor)) flagList += "Flags.FieldAccessor"
    if (flags.is(Flags.Final)) flagList += "Flags.Final"
    if (flags.is(Flags.Given)) flagList += "Flags.Given"
    if (flags.is(Flags.HasDefault)) flagList += "Flags.HasDefault"
    if (flags.is(Flags.Implicit)) flagList += "Flags.Implicit"
    if (flags.is(Flags.Infix)) flagList += "Flags.Infix"
    if (flags.is(Flags.Inline)) flagList += "Flags.Inline"
    if (flags.is(Flags.JavaDefined)) flagList += "Flags.JavaDefined"
    if (flags.is(Flags.JavaStatic)) flagList += "Flags.JavaStatic"
    if (flags.is(Flags.Lazy)) flagList += "Flags.Lazy"
    if (flags.is(Flags.Local)) flagList += "Flags.Local"
    if (flags.is(Flags.Macro)) flagList += "Flags.Macro"
    if (flags.is(Flags.Method)) flagList += "Flags.Method"
    if (flags.is(Flags.Module)) flagList += "Flags.Module"
    if (flags.is(Flags.Mutable)) flagList += "Flags.Mutable"
    if (flags.is(Flags.NoInits)) flagList += "Flags.NoInits"
    if (flags.is(Flags.Override)) flagList += "Flags.Override"
    if (flags.is(Flags.Package)) flagList += "Flags.Package"
    if (flags.is(Flags.Param)) flagList += "Flags.Param"
    if (flags.is(Flags.ParamAccessor)) flagList += "Flags.ParamAccessor"
    if (flags.is(Flags.Private)) flagList += "Flags.Private"
    if (flags.is(Flags.PrivateLocal)) flagList += "Flags.PrivateLocal"
    if (flags.is(Flags.Protected)) flagList += "Flags.Protected"
    if (flags.is(Flags.Scala2x)) flagList += "Flags.Scala2x"
    if (flags.is(Flags.Sealed)) flagList += "Flags.Sealed"
    if (flags.is(Flags.StableRealizable)) flagList += "Flags.StableRealizable"
    if (flags.is(Flags.Static)) flagList += "Flags.javaStatic"
    if (flags.is(Flags.Synthetic)) flagList += "Flags.Synthetic"
    if (flags.is(Flags.Trait)) flagList += "Flags.Trait"
    if (flags.is(Flags.Transparent)) flagList += "Flags.Transparent"
    flagList.result().mkString(" | ")
  }

  private class ExtractorsPrinter[Q <: Quotes & Singleton](using val quotes: Q) { self =>
    import quotes.reflect._

    private val sb: StringBuilder = new StringBuilder

    def result(): String = sb.result()

    def visitTree(x: Tree): this.type = x match {
      case tree: Ref =>
        tree match
          case Wildcard() =>
            this += "Wildcard()"
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
      case SummonFrom(cases) =>
        this += "SummonFrom(" ++= cases += ")"
      case Return(expr, from) =>
        this += "Return(" += expr += ", " += from += ")"
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
      case DefDef(name, paramsClauses, returnTpt, rhs) =>
        this += "DefDef(\"" += name += "\", " ++= paramsClauses += ", " += returnTpt += ", " += rhs += ")"
      case TypeDef(name, rhs) =>
        this += "TypeDef(\"" += name += "\", " += rhs += ")"
      case ClassDef(name, constr, parents, self, body) =>
        this += "ClassDef(\"" += name += "\", " += constr += ", "
        visitList[Tree](parents, visitTree)
        this += ", " += self += ", " ++= body += ")"
      case Import(expr, selectors) =>
        this += "Import(" += expr += ", " ++= selectors += ")"
      case Export(expr, selectors) =>
        this += "Export(" += expr += ", " ++= selectors += ")"
      case PackageClause(pid, stats) =>
        this += "PackageClause(" += pid += ", " ++= stats += ")"
      case Inferred() =>
        this += "Inferred()"
      case TypeIdent(name) =>
        this += "TypeIdent(\"" += name += "\")"
      case TypeSelect(qualifier, name) =>
        this += "TypeSelect(" += qualifier += ", \"" += name += "\")"
      case TypeProjection(qualifier, name) =>
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
        this += "Alternatives(" ++= patterns += ")"
    }

    def visitConstant(x: Constant): this.type = x match {
      case UnitConstant() => this += "UnitConstant()"
      case NullConstant() => this += "NullConstant()"
      case BooleanConstant(value) => this += "BooleanConstant(" += value += ")"
      case ByteConstant(value) => this += "ByteConstant(" += value += ")"
      case ShortConstant(value) => this += "ShortConstant(" += value += ")"
      case IntConstant(value) => this += "IntConstant(" += value += ")"
      case LongConstant(value) => this += "LongConstant(" += value += "L)"
      case FloatConstant(value) => this += "FloatConstant(" += value += "f)"
      case DoubleConstant(value) => this += "DoubleConstant(" += value += "d)"
      case CharConstant(value) => this += "CharConstant('" += value += "')"
      case StringConstant(value) => this += "StringConstant(\"" += value += "\")"
      case ClassOfConstant(value) =>
        this += "ClassOfConstant("
        visitType(value) += ")"
    }

    def visitType(x: TypeRepr): this.type = x match {
      case ConstantType(value) =>
        this += "ConstantType(" += value += ")"
      case TermRef(qual, name) =>
        this += "TermRef(" += qual+= ", \"" += name += "\")"
      case TypeRef(qual, name) =>
        this += "TypeRef(" += qual += ", \"" += name += "\")"
      case Refinement(parent, name, info) =>
        this += "Refinement(" += parent += ", \"" += name += "\", " += info += ")"
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
        this += "ParamRef(binder, " += idx += ")"
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
        this += "TypeLambda(" ++= argNames += ", " ++= argBounds += ", " += resType += ")"
      case TypeBounds(lo, hi) =>
        this += "TypeBounds(" += lo += ", " += hi += ")"
      case NoPrefix() =>
        this += "NoPrefix()"
      case MatchCase(pat, rhs) =>
        this += "MatchCase(" += pat += ", " += rhs += ")"
    }

    def visitSignature(sig: Signature): this.type = {
      val Signature(params, res) = sig
      this += "Signature(" ++= params.map(_.toString) += ", " += res += ")"
    }

    def visitSelector(sel: Selector): this.type = sel match {
      case SimpleSelector(id) => this += "SimpleSelector(" += id += ")"
      case RenameSelector(id1, id2) => this += "RenameSelector(" += id1 += ", " += id2 += ")"
      case OmitSelector(id) => this += "OmitSelector(" += id += ")"
      case GivenSelector(bound) => this += "GivenSelector(" += bound += ")"
    }

    def visitSymbol(x: Symbol): this.type =
      if x.isPackageDef  then this += "IsPackageDefSymbol(<" += x.fullName += ">)"
      else if x.isClassDef then this += "IsClassDefSymbol(<" += x.fullName += ">)"
      else if x.isDefDef then this += "IsDefDefSymbol(<" += x.fullName += ">)"
      else if x.isValDef then this += "IsValDefSymbol(<" += x.fullName += ">)"
      else if x.isTypeDef then this += "IsTypeDefSymbol(<" += x.fullName += ">)"
      else { assert(x.isNoSymbol); this += "NoSymbol()" }

    def visitParamClause(x: ParamClause): this.type =
      x match
        case TermParamClause(params) => this += "TermParamClause(" ++= params += ")"
        case TypeParamClause(params) => this += "TypeParamClause(" ++= params += ")"

    def +=(x: Boolean): this.type = { sb.append(x); this }
    def +=(x: Byte): this.type = { sb.append(x); this }
    def +=(x: Short): this.type = { sb.append(x); this }
    def +=(x: Int): this.type = { sb.append(x); this }
    def +=(x: Long): this.type = { sb.append(x); this }
    def +=(x: Float): this.type = { sb.append(x); this }
    def +=(x: Double): this.type = { sb.append(x); this }
    def +=(x: Char): this.type = { sb.append(x); this }
    def +=(x: String): this.type = { sb.append(x); this }

    def ++=(xs: List[String]): this.type = visitList[String](xs, +=)

    private implicit class StringOps(buff: self.type) {
      def +=(x: Option[String]): self.type = { visitOption(x, y => buff += "\"" += y += "\""); buff }
    }

    private implicit class TreeOps(buff: self.type) {
      def +=(x: Tree): self.type = { visitTree(x); buff }
      def +=(x: Option[Tree]): self.type = { visitOption(x, visitTree); buff }
      def ++=(x: List[Tree]): self.type = { visitList(x, visitTree); buff }
      def +++=(x: List[List[Tree]]): self.type = { visitList(x, ++=); buff }
    }

    private implicit class ConstantOps(buff: self.type) {
      def +=(x: Constant): self.type = { visitConstant(x); buff }
    }

    private implicit class TypeOps(buff: self.type) {
      def +=(x: TypeRepr): self.type = { visitType(x); buff }
      def +=(x: Option[TypeRepr]): self.type = { visitOption(x, visitType); buff }
      def ++=(x: List[TypeRepr]): self.type = { visitList(x, visitType); buff }
    }

    private implicit class SignatureOps(buff: self.type) {
      def +=(x: Option[Signature]): self.type = { visitOption(x, visitSignature); buff }
    }

    private implicit class SelectorOps(buff: self.type) {
      def ++=(x: List[Selector]): self.type = { visitList(x, visitSelector); buff }
    }

    private implicit class SymbolOps(buff: self.type) {
      def +=(x: Symbol): self.type = { visitSymbol(x); buff }
    }

    private implicit class ParamClauseOps(buff: self.type) {
      def ++=(x: List[ParamClause]): self.type = { visitList(x, visitParamClause); buff }
    }

    private def visitOption[U](opt: Option[U], visit: U => this.type): this.type = opt match {
      case Some(x) =>
        this += "Some("
        visit(x)
        this += ")"
      case _ =>
        this += "None"
    }

    private def visitList[U](list: List[U], visit: U => this.type): this.type = list match {
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
