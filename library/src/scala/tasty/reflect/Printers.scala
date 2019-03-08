package scala.tasty
package reflect

import scala.annotation.switch

import scala.tasty.util.SyntaxHighlightUtils._
import scala.tasty.util.Chars

trait Printers
  extends Core
  with ConstantOps
  with FlagsOps
  with IdOps
  with ImportSelectorOps
  with PatternOps
  with PositionOps
  with SettingsOps
  with SignatureOps
  with StandardDefinitions
  with SymbolOps
  with TreeOps
  with TypeOrBoundsOps {

  /** Adds `show` as an extension method of a `Tree` */
  implicit class TreeShowDeco(tree: Tree) {
    /** Shows the tree as extractors */
    def show(implicit ctx: Context): String = new ExtractorsPrinter().showTree(tree)
    /** Shows the tree as source code */
    def showCode(implicit ctx: Context): String = new SourceCodePrinter().showTree(tree)
  }

  /** Adds `show` as an extension method of a `TypeOrBounds` */
  implicit class TypeOrBoundsShowDeco(tpe: TypeOrBounds) {
    /** Shows the tree as extractors */
    def show(implicit ctx: Context): String = new ExtractorsPrinter().showTypeOrBounds(tpe)
    /** Shows the tree as source code */
    def showCode(implicit ctx: Context): String = new SourceCodePrinter().showTypeOrBounds(tpe)
  }

  /** Adds `show` as an extension method of a `Pattern` */
  implicit class PatternShowDeco(pattern: Pattern) {
    /** Shows the tree as extractors */
    def show(implicit ctx: Context): String = new ExtractorsPrinter().showPattern(pattern)
    /** Shows the tree as source code */
    def showCode(implicit ctx: Context): String = new SourceCodePrinter().showPattern(pattern)
  }

  /** Adds `show` as an extension method of a `Constant` */
  implicit class ConstantShowDeco(const: Constant) {
    /** Shows the tree as extractors */
    def show(implicit ctx: Context): String = new ExtractorsPrinter().showConstant(const)
    /** Shows the tree as source code */
    def showCode(implicit ctx: Context): String = new SourceCodePrinter().showConstant(const)
  }

  /** Adds `show` as an extension method of a `Symbol` */
  implicit class SymbolShowDeco(symbol: Symbol) {
    /** Shows the tree as extractors */
    def show(implicit ctx: Context): String = new ExtractorsPrinter().showSymbol(symbol)
    /** Shows the tree as source code */
    def showCode(implicit ctx: Context): String = new SourceCodePrinter().showSymbol(symbol)
  }

  /** Adds `show` as an extension method of a `Flags` */
  implicit class FlagsShowDeco(flags: Flags) {
    /** Shows the tree as extractors */
    def show(implicit ctx: Context): String = new ExtractorsPrinter().showFlags(flags)
    /** Shows the tree as source code */
    def showCode(implicit ctx: Context): String = new SourceCodePrinter().showFlags(flags)
  }

  abstract class Printer {

    def showTree(tree: Tree)(implicit ctx: Context): String

    def showPattern(pattern: Pattern)(implicit ctx: Context): String

    def showTypeOrBounds(tpe: TypeOrBounds)(implicit ctx: Context): String

    def showConstant(const: Constant)(implicit ctx: Context): String

    def showSymbol(symbol: Symbol)(implicit ctx: Context): String

    def showFlags(flags: Flags)(implicit ctx: Context): String

  }

  class ExtractorsPrinter extends Printer {

    def showTree(tree: Tree)(implicit ctx: Context): String =
      new Buffer().visitTree(tree).result()

    def showPattern(pattern: Pattern)(implicit ctx: Context): String =
      new Buffer().visitPattern(pattern).result()

    def showTypeOrBounds(tpe: TypeOrBounds)(implicit ctx: Context): String =
      new Buffer().visitType(tpe).result()

    def showConstant(const: Constant)(implicit ctx: Context): String =
      new Buffer().visitConstant(const).result()

    def showSymbol(symbol: Symbol)(implicit ctx: Context): String =
      new Buffer().visitSymbol(symbol).result()

    def showFlags(flags: Flags)(implicit ctx: Context): String = {
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

    private class Buffer(implicit ctx: Context) { self =>

      private val sb: StringBuilder = new StringBuilder

      def result(): String = sb.result()

      def visitTree(x: Tree): Buffer = x match {
        case Term.Ident(name) =>
          this += "Term.Ident(\"" += name += "\")"
        case Term.Select(qualifier, name) =>
          this += "Term.Select(" += qualifier += ", \"" += name += "\")"
        case Term.This(qual) =>
          this += "Term.This(" += qual += ")"
        case Term.Super(qual, mix) =>
          this += "Term.Super(" += qual += ", " += mix += ")"
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
        case Term.Repeated(elems, elemtpt) =>
          this += "Term.Repeated(" ++= elems += ", " += elemtpt += ")"
        case Term.Inlined(call, bindings, expansion) =>
          this += "Term.Inlined("
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
        case Import(importImplied, expr, selectors) =>
          this += "Import(" += importImplied += ", " += expr += ", " ++= selectors += ")"
        case PackageClause(pid, stats) =>
          this += "PackageClause(" += pid += ", " ++= stats += ")"
        case TypeTree.Inferred() =>
          this += "TypeTree.Inferred()"
        case TypeTree.Ident(name) =>
          this += "TypeTree.Ident(\"" += name += "\")"
        case TypeTree.Select(qualifier, name) =>
          this += "TypeTree.Select(" += qualifier += ", \"" += name += "\")"
        case TypeTree.Projection(qualifier, name) =>
          this += "TypeTree.Projection(" += qualifier += ", \"" += name += "\")"
        case TypeTree.Singleton(ref) =>
          this += "TypeTree.Singleton(" += ref += ")"
        case TypeTree.Refined(tpt, refinements) =>
          this += "TypeTree.Refined(" += tpt += ", " ++= refinements += ")"
        case TypeTree.Applied(tpt, args) =>
          this += "TypeTree.Applied(" += tpt += ", " ++= args += ")"
        case TypeTree.ByName(result) =>
          this += "TypeTree.ByName(" += result += ")"
        case TypeTree.Annotated(arg, annot) =>
          this += "TypeTree.Annotated(" += arg += ", " += annot += ")"
        case TypeTree.LambdaTypeTree(tparams, body) =>
          this += "TypeTree.LambdaTypeTree(" ++= tparams += ", " += body += ")"
        case TypeTree.TypeBind(name, bounds) =>
          this += "TypeTree.TypeBind(" += name += ", " += bounds += ")"
        case TypeTree.TypeBlock(aliases, tpt) =>
          this += "TypeTree.TypeBlock(" ++= aliases += ", " += tpt += ")"
        case TypeBoundsTree(lo, hi) =>
          this += "TypeBoundsTree(" += lo += ", " += hi += ")"
        case WildcardTypeTree() =>
          this += s"WildcardTypeTree()"
        case TypeTree.MatchType(bound, selector, cases) =>
          this += "TypeTree.MatchType(" += bound += ", " += selector += ", " ++= cases += ")"
        case CaseDef(pat, guard, body) =>
          this += "CaseDef(" += pat += ", " += guard += ", " += body += ")"
        case TypeCaseDef(pat, body) =>
          this += "TypeCaseDef(" += pat += ", " += body += ")"
      }

      def visitPattern(x: Pattern): Buffer = x match {
        case Pattern.Value(v) =>
          this += "Pattern.Value(" += v += ")"
        case Pattern.Bind(name, body) =>
          this += "Pattern.Bind(\"" += name += "\", " += body += ")"
        case Pattern.Unapply(fun, implicits, patterns) =>
          this += "Pattern.Unapply(" += fun += ", " ++= implicits += ", " ++= patterns += ")"
        case Pattern.Alternatives(patterns) =>
          this += "Pattern.Alternative(" ++= patterns += ")"
        case Pattern.TypeTest(tpt) =>
          this += "Pattern.TypeTest(" += tpt += ")"
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
        case Type.MatchType(bound, scrutinee, cases) =>
          this += "Type.MatchType(" += bound += ", " += scrutinee += ", " ++= cases += ")"
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

      private implicit class PatternOps(buff: Buffer) {
        def +=(x: Pattern): Buffer = { visitPattern(x); buff }
        def ++=(x: List[Pattern]): Buffer = { visitList(x, visitPattern); buff }
      }

      private implicit class ConstantOps(buff: Buffer) {
        def +=(x: Constant): Buffer = { visitConstant(x); buff }
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

  class SourceCodePrinter extends Printer {

    private[this] val color: Boolean = settings.color

    def showTree(tree: Tree)(implicit ctx: Context): String =
      (new Buffer).printTree(tree).result()

    def showPattern(pattern: Pattern)(implicit ctx: Context): String =
      (new Buffer).printPattern(pattern).result()

    def showTypeOrBounds(tpe: TypeOrBounds)(implicit ctx: Context): String =
      (new Buffer).printTypeOrBound(tpe).result()

    def showConstant(const: Constant)(implicit ctx: Context): String =
      (new Buffer).printConstant(const).result()

    def showSymbol(symbol: Symbol)(implicit ctx: Context): String =
      symbol.fullName

    def showFlags(flags: Flags)(implicit ctx: Context): String = {
      val flagList = List.newBuilder[String]
      if (flags.is(Flags.Private)) flagList += "private"
      if (flags.is(Flags.Protected)) flagList += "protected"
      if (flags.is(Flags.Abstract)) flagList += "abstract"
      if (flags.is(Flags.Final)) flagList += "final"
      if (flags.is(Flags.Sealed)) flagList += "sealed"
      if (flags.is(Flags.Case)) flagList += "case"
      if (flags.is(Flags.Implicit)) flagList += "implicit"
      if (flags.is(Flags.Erased)) flagList += "erased"
      if (flags.is(Flags.Lazy)) flagList += "lazy"
      if (flags.is(Flags.Override)) flagList += "override"
      if (flags.is(Flags.Inline)) flagList += "inline"
      if (flags.is(Flags.Macro)) flagList += "macro"
      if (flags.is(Flags.JavaDefined)) flagList += "javaDefined"
      if (flags.is(Flags.Static)) flagList += "javaStatic"
      if (flags.is(Flags.Object)) flagList += "object"
      if (flags.is(Flags.Trait)) flagList += "trait"
      if (flags.is(Flags.Local)) flagList += "local"
      if (flags.is(Flags.Synthetic)) flagList += "synthetic"
      if (flags.is(Flags.Artifact)) flagList += "artifact"
      if (flags.is(Flags.Mutable)) flagList += "mutable"
      if (flags.is(Flags.FieldAccessor)) flagList += "accessor"
      if (flags.is(Flags.CaseAcessor)) flagList += "caseAccessor"
      if (flags.is(Flags.Covariant)) flagList += "covariant"
      if (flags.is(Flags.Contravariant)) flagList += "contravariant"
      if (flags.is(Flags.Scala2X)) flagList += "scala2x"
      if (flags.is(Flags.DefaultParameterized)) flagList += "defaultParameterized"
      if (flags.is(Flags.StableRealizable)) flagList += "stableRealizable"
      if (flags.is(Flags.Param)) flagList += "param"
      if (flags.is(Flags.ParamAccessor)) flagList += "paramAccessor"
      if (flags.is(Flags.Enum)) flagList += "enum"
      if (flags.is(Flags.ModuleClass)) flagList += "moduleClass"
      if (flags.is(Flags.PrivateLocal)) flagList += "private[this]"
      if (flags.is(Flags.Package)) flagList += "package"
      flagList.result().mkString("/*", " ", "*/")
    }

    private class Buffer(implicit ctx: Context) {

      private[this] val sb: StringBuilder = new StringBuilder

      private[this] var indent: Int = 0
      def indented(printIndented: => Unit): Unit = {
        indent += 1
        printIndented
        indent -= 1
      }

      def inParens(body: => Unit): Buffer = {
        this += "("
        body
        this += ")"
      }

      def inSquare(body: => Unit): Buffer = {
        this += "["
        body
        this += "]"
      }

      def inBlock(body: => Unit): Buffer = {
        this += " {"
        indented {
          this += lineBreak()
          body
        }
        this += lineBreak() += "}"
      }

      def result(): String = sb.result()

      def lineBreak(): String = "\n" + ("  " * indent)
      def doubleLineBreak(): String = "\n\n" + ("  " * indent)

      def printTree(tree: Tree): Buffer = tree match {
        case PackageObject(body)=>
          printTree(body) // Print package object

        case PackageClause(Term.Ident(name), (inner @ PackageClause(_, _)) :: Nil) if name != "<empty>" && PackageObject.unapply(inner).isEmpty =>
          // print inner package as `package outer.inner { ... }`
          printTree(inner)

        case tree @ PackageClause(name, stats) =>
          val stats1 = stats.collect {
            case IsPackageClause(stat) => stat
            case IsDefinition(stat) if !(stat.symbol.flags.is(Flags.Object) && stat.symbol.flags.is(Flags.Lazy)) => stat
            case stat @ Import(_, _, _) => stat
          }
          name match {
            case Term.Ident("<empty>") =>
              printTrees(stats1, lineBreak())
            case _ =>
              this += "package "
              printType(name.tpe)
              inBlock(printTrees(stats1, lineBreak()))
          }

        case Import(importImplied, expr, selectors) =>
          this += "import "
          if (importImplied) this += "implied "
          printTree(expr)
          this += "."
          printImportSelectors(selectors)

        case IsClassDef(cdef @ ClassDef(name, DefDef(_, targs, argss, _, _), parents, derived, self, stats)) =>
          printDefAnnotations(cdef)

          val flags = cdef.symbol.flags
          if (flags.is(Flags.Implicit)) this += highlightKeyword("implicit ", color)
          if (flags.is(Flags.Sealed)) this += highlightKeyword("sealed ", color)
          if (flags.is(Flags.Final) && !flags.is(Flags.Object)) this += highlightKeyword("final ", color)
          if (flags.is(Flags.Case)) this += highlightKeyword("case ", color)

          if (name == "package$") {
            this += highlightKeyword("package object ", color) += highlightTypeDef(cdef.symbol.owner.name.stripSuffix("$"), color)
          }
          else if (flags.is(Flags.Object)) this += highlightKeyword("object ", color) += highlightTypeDef(name.stripSuffix("$"), color)
          else if (flags.is(Flags.Trait)) this += highlightKeyword("trait ", color) += highlightTypeDef(name, color)
          else if (flags.is(Flags.Abstract)) this += highlightKeyword("abstract class ", color) += highlightTypeDef(name, color)
          else this += highlightKeyword("class ", color) += highlightTypeDef(name, color)

          val typeParams = stats.collect { case IsTypeDef(targ) => targ  }.filter(_.symbol.isTypeParam).zip(targs)
          if (!flags.is(Flags.Object)) {
            printTargsDefs(typeParams)
            val it = argss.iterator
            while (it.hasNext)
              printArgsDefs(it.next())
          }

          val parents1 = parents.filter {
            case IsTerm(Term.Apply(Term.Select(Term.New(tpt), _), _)) => !Types.JavaLangObject.unapply(tpt.tpe)
            case IsTypeTree(TypeTree.Select(Term.Select(Term.Ident("_root_"), "scala"), "Product")) => false
            case IsTypeTree(TypeTree.Select(Term.Select(Term.Ident("_root_"), "scala"), "Serializable")) => false
            case _ => true
          }
          if (parents1.nonEmpty)
            this += highlightKeyword(" extends ", color)

          def printParent(parent: Tree /* Term | TypeTree */, needEmptyParens: Boolean = false): Unit = parent match {
            case IsTypeTree(parent) =>
              printTypeTree(parent)
            case IsTerm(Term.TypeApply(fun, targs)) =>
              printParent(fun)
            case IsTerm(Term.Apply(fun@Term.Apply(_,_), args)) =>
              printParent(fun, true)
              if (!args.isEmpty || needEmptyParens)
                inParens(printTrees(args, ", "))
            case IsTerm(Term.Apply(fun, args)) =>
              printParent(fun)
              if (!args.isEmpty || needEmptyParens)
                inParens(printTrees(args, ", "))
            case IsTerm(Term.Select(Term.New(tpt), _)) =>
              printTypeTree(tpt)
            case IsTerm(parent) =>
              throw new MatchError(parent.show)
          }

          def printSeparated(list: List[Tree /* Term | TypeTree */]): Unit = list match {
            case Nil =>
            case x :: Nil => printParent(x)
            case x :: xs =>
              printParent(x)
              this += highlightKeyword(" with ", color)
              printSeparated(xs)
          }
          printSeparated(parents1)

          if (derived.nonEmpty) {
            this += highlightKeyword(" derives ", color)
            printTypeTrees(derived, ", ")
          }

          def keepDefinition(d: Definition): Boolean = {
            val flags = d.symbol.flags
            def isUndecompilableCaseClassMethod: Boolean = {
              // Currently the compiler does not allow overriding some of the methods generated for case classes
              d.symbol.flags.is(Flags.Synthetic) &&
              (d match {
                case DefDef("apply" | "unapply" | "writeReplace", _, _, _, _) if d.symbol.owner.flags.is(Flags.Object) => true
                case DefDef(n, _, _, _, _) if d.symbol.owner.flags.is(Flags.Case) =>
                  n == "copy" ||
                  n.matches("copy\\$default\\$[1-9][0-9]*") || // default parameters for the copy method
                  n.matches("_[1-9][0-9]*") || // Getters from Product
                  n == "productElementName"
                case _ => false
              })
            }
            def isInnerModuleObject = d.symbol.flags.is(Flags.Lazy) && d.symbol.flags.is(Flags.Object)
            !flags.is(Flags.Param) && !flags.is(Flags.ParamAccessor) && !flags.is(Flags.FieldAccessor) && !isUndecompilableCaseClassMethod && !isInnerModuleObject
          }
          val stats1 = stats.collect {
            case IsDefinition(stat) if keepDefinition(stat) => stat
            case stat @ Import(_, _, _) => stat
            case IsTerm(stat) => stat
          }

          def printBody(printSelf: Boolean) = {
            this += " {"
            indented {
              if (printSelf) {
                val Some(ValDef(name, tpt, _)) = self
                indented {
                  val name1 = if (name == "_") "this" else name
                  this += " " += highlightValDef(name1, color) += ": "
                  printTypeTree(tpt)
                  this += " =>"
                }
              }
              this += lineBreak()
              printTrees(stats1, lineBreak())
            }
            this += lineBreak() += "}"
          }
          self match {
            case Some(ValDef(_, TypeTree.Singleton(_), _)) =>
              if (stats1.nonEmpty)
                printBody(printSelf = false)
            case Some(ValDef(_, _, _)) =>
              printBody(printSelf = true)
            case _ =>
              if (stats1.nonEmpty)
                printBody(printSelf = false)
          }
          this

        case IsTypeDef(tdef @ TypeDef(name, rhs)) =>
          printDefAnnotations(tdef)
          this += highlightKeyword("type ", color)
          printTargDef((tdef, tdef), isMember = true)

        case IsValDef(vdef @ ValDef(name, tpt, rhs)) =>
          printDefAnnotations(vdef)

          val flags = vdef.symbol.flags
          if (flags.is(Flags.Implicit)) this += highlightKeyword("implicit ", color)
          if (flags.is(Flags.Override)) this += highlightKeyword("override ", color)
          if (flags.is(Flags.Final) && !flags.is(Flags.Object)) this += highlightKeyword("final ", color)

          printProtectedOrPrivate(vdef)

          if (flags.is(Flags.Lazy)) this += highlightKeyword("lazy ", color)
          if (vdef.symbol.flags.is(Flags.Mutable)) this += highlightKeyword("var ", color)
          else this += highlightKeyword("val ", color)

          this += highlightValDef(name, color) += ": "
          printTypeTree(tpt)
          rhs match {
            case Some(tree) =>
              this += " = "
              printTree(tree)
            case None =>
              this
          }

        case Term.While(cond, body) =>
          (cond, body) match {
            case (Term.Block(Term.Block(Nil, body1) :: Nil, Term.Block(Nil, cond1)), Term.Literal(Constant.Unit())) =>
              this += highlightKeyword("do ", color)
              printTree(body1) += highlightKeyword(" while ", color)
              inParens(printTree(cond1))
            case _ =>
              this += highlightKeyword("while ", color)
              inParens(printTree(cond)) += " "
              printTree(body)
          }

        case IsDefDef(ddef @ DefDef(name, targs, argss, _, rhsOpt)) if name.startsWith("$anonfun") =>
          // Decompile lambda definition
          assert(targs.isEmpty)
          val args :: Nil = argss
          val Some(rhs) = rhsOpt
          inParens {
            printArgsDefs(args)
            this += " => "
            printTree(rhs)
          }

        case IsDefDef(ddef @ DefDef(name, targs, argss, tpt, rhs)) =>
          printDefAnnotations(ddef)

          val isConstructor = name == "<init>"

          val flags = ddef.symbol.flags
          if (flags.is(Flags.Implicit)) this += highlightKeyword("implicit ", color)
          if (flags.is(Flags.Inline)) this += highlightKeyword("inline ", color)
          if (flags.is(Flags.Override)) this += highlightKeyword("override ", color)
          if (flags.is(Flags.Final) && !flags.is(Flags.Object)) this += highlightKeyword("final ", color)

          printProtectedOrPrivate(ddef)

          this += highlightKeyword("def ", color) += highlightValDef((if (isConstructor) "this" else name), color)
          printTargsDefs(targs.zip(targs))
          val it = argss.iterator
          while (it.hasNext)
            printArgsDefs(it.next())
          if (!isConstructor) {
            this += ": "
            printTypeTree(tpt)
          }
          rhs match {
            case Some(tree) =>
              this += " = "
              printTree(tree)
            case None =>
          }
          this

        case Term.Ident("_") =>
          this += "_"

        case IsTerm(tree @ Term.Ident(_)) =>
          printType(tree.tpe)

        case Term.Select(qual, name) =>
          printTree(qual)
          if (name != "<init>" && name != "package")
            this += "." += name
          this

        case Term.Literal(const) =>
          printConstant(const)

        case Term.This(id) =>
          id match {
            case Some(x) =>
              this += x.name.stripSuffix("$") += "."
            case None =>
          }
          this += "this"

        case Term.New(tpt) =>
          this += "new "
          printTypeTree(tpt)

        case Term.NamedArg(name, arg) =>
          this += name += " = "
          printTree(arg)

        case SpecialOp("throw", expr :: Nil) =>
          this += "throw "
          printTree(expr)

        case Term.Apply(fn, args) if fn.symbol.fullName == "scala.internal.Quoted$.exprQuote" =>
          args.head match {
            case Term.Block(stats, expr) =>
              this += "'{"
              indented {
                this += lineBreak()
                printFlatBlock(stats, expr)
              }
              this += lineBreak() += "}"
            case _ =>
              this += "'{"
              printTree(args.head)
              this += "}"
          }

        case Term.TypeApply(fn, args) if fn.symbol.fullName == "scala.internal.Quoted$.typeQuote" =>
          this += "'["
          printTypeTree(args.head)
          this += "]"

        case Term.Apply(fn, args) =>
          fn match {
            case Term.Select(Term.This(_), "<init>") => this += "this" // call to constructor inside a constructor
            case _ => printTree(fn)
          }
          val args1 = args match {
            case init :+ Term.Typed(Term.Repeated(Nil, _), _) => init // drop empty var args at the end
            case _ => args
          }

          inParens(printTrees(args1, ", "))

        case Term.TypeApply(fn, args) =>
          printTree(fn)
          fn match {
            case Term.Select(Term.New(TypeTree.Applied(_, _)), "<init>") =>
              // type bounds already printed in `fn`
              this
            case _ =>
              inSquare(printTrees(args, ", "))
          }

        case Term.Super(qual, idOpt) =>
          qual match {
            case Term.This(Some(Id(name))) => this += name += "."
            case Term.This(None) =>
          }
          this += "super"
          for (id <- idOpt)
            inSquare(this += id.name)
          this

        case Term.Typed(term, tpt) =>
          tpt.tpe match {
            case Types.Repeated(_) =>
              term match {
                case Term.Repeated(_, _) =>
                  printTree(term)
                case _ =>
                  printTree(term)
                  this += ": " += highlightTypeDef("_*", color)
              }
            case _ =>
              inParens {
                printTree(term)
                this += (if (Chars.isOperatorPart(sb.last)) " : " else ": ")
                def printTypeOrAnnots(tpe: Type): Unit = tpe match {
                  case Type.AnnotatedType(tp, annot) if tp == term.tpe =>
                    printAnnotation(annot)
                  case Type.AnnotatedType(tp, annot) =>
                    printTypeOrAnnots(tp)
                    this += " "
                    printAnnotation(annot)
                  case tpe =>
                    printType(tpe)
                }
                printTypeOrAnnots(tpt.tpe)
              }
          }

        case Term.Assign(lhs, rhs) =>
          printTree(lhs)
          this += " = "
          printTree(rhs)

        case Term.Block(stats0, expr) =>
          val stats = stats0.filter {
            case IsValDef(tree) => !tree.symbol.flags.is(Flags.Object)
            case _ => true
          }
          printFlatBlock(stats, expr)

        case Term.Inlined(_, bindings, expansion) =>
          printFlatBlock(bindings, expansion)

        case Term.Lambda(meth, tpt) =>
          // Printed in by it's DefDef
          this

        case Term.If(cond, thenp, elsep) =>
          this += highlightKeyword("if ", color)
          inParens(printTree(cond))
          this += " "
          printTree(thenp)
          this+= highlightKeyword(" else ", color)
          printTree(elsep)

        case Term.Match(selector, cases) =>
          printTree(selector)
          this += highlightKeyword(" match", color)
          inBlock(printCases(cases, lineBreak()))

        case Term.Try(body, cases, finallyOpt) =>
          this += highlightKeyword("try ", color)
          printTree(body)
          if (cases.nonEmpty) {
            this += highlightKeyword(" catch", color)
            inBlock(printCases(cases, lineBreak()))
          }
          finallyOpt match {
            case Some(t) =>
              this += highlightKeyword(" finally ", color)
              printTree(t)
            case None =>
              this
          }

        case Term.Return(expr) =>
          this += "return "
          printTree(expr)

        case Term.Repeated(elems, _) =>
          printTrees(elems, ", ")

        case TypeBoundsTree(lo, hi) =>
          this += "_ >: "
          printTypeTree(lo)
          this += " <: "
          printTypeTree(hi)

        case IsWildcardTypeTree(tpt) =>
          printTypeOrBound(tpt.tpe)

        case IsTypeTree(tpt) =>
          printTypeTree(tpt)

        case _ =>
          throw new MatchError(tree.show)

      }

      def flatBlock(stats: List[Statement], expr: Term): (List[Statement], Term) = {
        val flatStats = List.newBuilder[Statement]
        def extractFlatStats(stat: Statement): Unit = stat match {
          case Term.Block(stats1, expr1) =>
            val it = stats1.iterator
            while (it.hasNext)
              extractFlatStats(it.next())
            extractFlatStats(expr1)
          case Term.Inlined(_, bindings, expansion) =>
            val it = bindings.iterator
            while (it.hasNext)
              extractFlatStats(it.next())
            extractFlatStats(expansion)
          case Term.Literal(Constant.Unit()) => // ignore
          case stat => flatStats += stat
        }
        def extractFlatExpr(term: Term): Term = term match {
          case Term.Block(stats1, expr1) =>
            val it = stats1.iterator
            while (it.hasNext)
              extractFlatStats(it.next())
            extractFlatExpr(expr1)
          case Term.Inlined(_, bindings, expansion) =>
            val it = bindings.iterator
            while (it.hasNext)
              extractFlatStats(it.next())
            extractFlatExpr(expansion)
          case term => term
        }
        val it = stats.iterator
        while (it.hasNext)
          extractFlatStats(it.next())
        val flatExpr = extractFlatExpr(expr)
        (flatStats.result(), flatExpr)
      }

      def printFlatBlock(stats: List[Statement], expr: Term): Buffer = {
        val (stats1, expr1) = flatBlock(stats, expr)
        // Remove Term.Lambda nodes, lambdas are printed by their definition
        val stats2 = stats1.filter { case Term.Lambda(_, _) => false; case _ => true }
        val (stats3, expr3) = expr1 match {
          case Term.Lambda(_, _) =>
            val init :+ last  = stats2
            (init, last)
          case _ => (stats2, expr1)
        }
        if (stats3.isEmpty) {
          printTree(expr3)
        } else {
          this += "{"
          indented {
            printStats(stats3, expr3)
          }
          this += lineBreak() += "}"
        }
      }

      def printStats(stats: List[Tree], expr: Tree): Unit = {
        def printSeparator(next: Tree): Unit = {
          // Avoid accidental application of opening `{` on next line with a double break
          def rec(next: Tree): Unit = next match {
            case Term.Block(stats, _) if stats.nonEmpty => this += doubleLineBreak()
            case Term.Inlined(_, bindings, _) if bindings.nonEmpty => this += doubleLineBreak()
            case Term.Select(qual, _) => rec(qual)
            case Term.Apply(fn, _) => rec(fn)
            case Term.TypeApply(fn, _) => rec(fn)
            case Term.Typed(_, _) => this += doubleLineBreak()
            case _ => this += lineBreak()
          }
          next match {
            case IsTerm(term) =>
              flatBlock(Nil, term) match {
                case (next :: _, _) => rec(next)
                case (Nil, next) => rec(next)
              }
            case _ => this += lineBreak()
          }
        }
        def printSeparated(list: List[Tree]): Unit = list match {
          case Nil =>
            printTree(expr)
          case x :: xs =>
            printTree(x)
            printSeparator(if (xs.isEmpty) expr else xs.head)
            printSeparated(xs)
        }

        this += lineBreak()
        printSeparated(stats)
      }

      def printList[T](xs: List[T], sep: String, print: T => Buffer): Buffer = {
        def printSeparated(list: List[T]): Unit = list match {
          case Nil =>
          case x :: Nil => print(x)
          case x :: xs =>
            print(x)
            this += sep
            printSeparated(xs)
        }
        printSeparated(xs)
        this
      }

      def printTrees(trees: List[Tree], sep: String): Buffer =
        printList(trees, sep, printTree)

      def printTypeTrees(trees: List[TypeTree], sep: String): Buffer =
        printList(trees, sep, printTypeTree)

      def printTypes(trees: List[Type], sep: String): Buffer = {
        def printSeparated(list: List[Type]): Unit = list match {
          case Nil =>
          case x :: Nil => printType(x)
          case x :: xs =>
            printType(x)
            this += sep
            printSeparated(xs)
        }
        printSeparated(trees)
        this
      }

      def printImportSelectors(selectors: List[ImportSelector]): Buffer = {
        def printSeparated(list: List[ImportSelector]): Unit = list match {
          case Nil =>
          case x :: Nil => printImportSelector(x)
          case x :: xs =>
            printImportSelector(x)
            this += ", "
            printSeparated(xs)
        }
        this += "{"
        printSeparated(selectors)
        this += "}"
      }

      def printCases(cases: List[CaseDef], sep: String): Buffer = {
        def printSeparated(list: List[CaseDef]): Unit = list match {
          case Nil =>
          case x :: Nil => printCaseDef(x)
          case x :: xs =>
            printCaseDef(x)
            this += sep
            printSeparated(xs)
        }
        printSeparated(cases)
        this
      }

      def printTypeCases(cases: List[TypeCaseDef], sep: String): Buffer = {
        def printSeparated(list: List[TypeCaseDef]): Unit = list match {
          case Nil =>
          case x :: Nil => printTypeCaseDef(x)
          case x :: xs =>
            printTypeCaseDef(x)
            this += sep
            printSeparated(xs)
        }
        printSeparated(cases)
        this
      }

      def printPatterns(cases: List[Pattern], sep: String): Buffer = {
        def printSeparated(list: List[Pattern]): Unit = list match {
          case Nil =>
          case x :: Nil => printPattern(x)
          case x :: xs =>
            printPattern(x)
            this += sep
            printSeparated(xs)
        }
        printSeparated(cases)
        this
      }

      def printTypesOrBounds(types: List[TypeOrBounds], sep: String): Buffer = {
        def printSeparated(list: List[TypeOrBounds]): Unit = list match {
          case Nil =>
          case x :: Nil => printTypeOrBound(x)
          case x :: xs =>
            printTypeOrBound(x)
            this += sep
            printSeparated(xs)
        }
        printSeparated(types)
        this
      }

      def printTargsDefs(targs: List[(TypeDef, TypeDef)], isDef:Boolean = true): Unit = {
        if (!targs.isEmpty) {
          def printSeparated(list: List[(TypeDef, TypeDef)]): Unit = list match {
            case Nil =>
            case x :: Nil => printTargDef(x, isDef = isDef)
            case x :: xs =>
              printTargDef(x, isDef = isDef)
              this += ", "
              printSeparated(xs)
          }

          inSquare(printSeparated(targs))
        }
      }

      def printTargDef(arg: (TypeDef, TypeDef), isMember: Boolean = false, isDef:Boolean = true): Buffer = {
        val (argDef, argCons) = arg

        if (isDef) {
          if (argDef.symbol.flags.is(Flags.Covariant)) {
            this += highlightValDef("+", color)
          } else if (argDef.symbol.flags.is(Flags.Contravariant)) {
            this += highlightValDef("-", color)
          }
        }

        this += argCons.name
        argCons.rhs match {
          case IsTypeBoundsTree(rhs) => printBoundsTree(rhs)
          case IsWildcardTypeTree(rhs) =>
            printTypeOrBound(rhs.tpe)
          case rhs @ TypeTree.LambdaTypeTree(tparams, body) =>
            def printParam(t: Tree /*TypeTree | TypeBoundsTree*/): Unit = t match {
              case IsTypeBoundsTree(t) => printBoundsTree(t)
              case IsTypeTree(t) => printTypeTree(t)
            }
            def printSeparated(list: List[TypeDef]): Unit = list match {
              case Nil =>
              case x :: Nil =>
                this += x.name
                printParam(x.rhs)
              case x :: xs =>
                this += x.name
                printParam(x.rhs)
                this += ", "
                printSeparated(xs)
            }
            inSquare(printSeparated(tparams))
            if (isMember) {
              body match {
                case TypeTree.MatchType(Some(bound), _, _) =>
                  this +=  " <: "
                  printTypeTree(bound)
                case _ =>
              }
              this += " = "
              printTypeOrBoundsTree(body)
            }
            else this
          case IsTypeTree(rhs) =>
            this += " = "
            printTypeTree(rhs)
        }
      }

      def printArgsDefs(args: List[ValDef]): Unit = inParens {
        args match {
          case Nil =>
          case arg :: _ =>
            if (arg.symbol.flags.is(Flags.Erased)) this += "erased "
            if (arg.symbol.flags.is(Flags.Implicit)) this += "implicit "
        }

        def printSeparated(list: List[ValDef]): Unit = list match {
          case Nil =>
          case x :: Nil => printParamDef(x)
          case x :: xs =>
            printParamDef(x)
            this += ", "
            printSeparated(xs)
        }

        printSeparated(args)
      }

      def printAnnotations(trees: List[Term]): Buffer = {
        def printSeparated(list: List[Term]): Unit = list match {
          case Nil =>
          case x :: Nil => printAnnotation(x)
          case x :: xs =>
            printAnnotation(x)
            this += " "
            printSeparated(xs)
        }
        printSeparated(trees)
        this
      }

      def printParamDef(arg: ValDef): Unit = {
        val name = arg.name
        arg.symbol.owner match {
          case IsDefSymbol(sym) if sym.name == "<init>" =>
            val ClassDef(_, _, _, _, _, body) = sym.owner.asClass.tree
            body.collectFirst {
              case IsValDef(vdef @ ValDef(`name`, _, _)) if vdef.symbol.flags.is(Flags.ParamAccessor) =>
                if (!vdef.symbol.flags.is(Flags.Local)) {
                  var printedPrefix = false
                  if (vdef.symbol.flags.is(Flags.Override)) {
                    this += "override "
                    printedPrefix = true
                  }
                  printedPrefix  |= printProtectedOrPrivate(vdef)
                  if (vdef.symbol.flags.is(Flags.Mutable)) this += highlightValDef("var ", color)
                  else if (printedPrefix || !vdef.symbol.flags.is(Flags.CaseAcessor)) this += highlightValDef("val ", color)
                }
            }
          case _ =>
        }

        this += highlightValDef(name, color) += ": "
        printTypeTree(arg.tpt)
      }

      def printCaseDef(caseDef: CaseDef): Buffer = {
        this += highlightValDef("case ", color)
        printPattern(caseDef.pattern)
        caseDef.guard match {
          case Some(t) =>
            this += " if "
            printTree(t)
          case None =>
        }
        this += highlightValDef(" =>", color)
        indented {
          caseDef.rhs match {
            case Term.Block(stats, expr) =>
              printStats(stats, expr)
            case body =>
              this += lineBreak()
              printTree(body)
          }
        }
        this
      }

      def printTypeCaseDef(caseDef: TypeCaseDef): Buffer = {
        this += highlightValDef("case ", color)
        printTypeTree(caseDef.pattern)
        this += highlightValDef(" => ", color)
        printTypeTree(caseDef.rhs)
        this
      }

      def printPattern(pattern: Pattern): Buffer = pattern match {
        case Pattern.Value(v) =>
          v match {
            case Term.Ident("_") => this += "_"
            case _ => printTree(v)
          }

        case Pattern.Bind(name, Pattern.Value(Term.Ident("_"))) =>
          this += name

        case Pattern.Bind(name, Pattern.TypeTest(tpt)) =>
          this += highlightValDef(name, color) += ": "
          printTypeTree(tpt)

        case Pattern.Bind(name, pattern) =>
          this += name += " @ "
          printPattern(pattern)

        case Pattern.Unapply(fun, implicits, patterns) =>
          fun match {
            case Term.Select(extractor, "unapply" | "unapplySeq") => printTree(extractor)
            case Term.TypeApply(Term.Select(extractor, "unapply" | "unapplySeq"), _) => printTree(extractor)
            case _ => throw new MatchError(fun.show)
          }
          inParens(printPatterns(patterns, ", "))

        case Pattern.Alternatives(trees) =>
          inParens(printPatterns(trees, " | "))

        case Pattern.TypeTest(tpt) =>
          this += "_: "
          printTypeOrBoundsTree(tpt)

        case _ =>
          throw new MatchError(pattern.show)

      }

      def printConstant(const: Constant): Buffer = const match {
        case Constant.Unit() => this += highlightLiteral("()", color)
        case Constant.Null() => this += highlightLiteral("null", color)
        case Constant.Boolean(v) => this += highlightLiteral(v.toString, color)
        case Constant.Byte(v) => this += highlightLiteral(v.toString, color)
        case Constant.Short(v) => this += highlightLiteral(v.toString, color)
        case Constant.Int(v) => this += highlightLiteral(v.toString, color)
        case Constant.Long(v) => this += highlightLiteral(v.toString + "L", color)
        case Constant.Float(v) => this += highlightLiteral(v.toString + "f", color)
        case Constant.Double(v) => this += highlightLiteral(v.toString, color)
        case Constant.Char(v) => this += highlightString('\'' + escapedChar(v) + '\'', color)
        case Constant.String(v) => this += highlightString('"' + escapedString(v) + '"', color)
        case Constant.ClassTag(v) =>
          this += "classOf"
          inSquare(printType(v))
        case Constant.Symbol(v) =>
          this += highlightLiteral("'" + v.name, color)
      }

      def printTypeOrBoundsTree(tpt: Tree /*TypeTree | TypeBoundsTree*/): Buffer = tpt match {
        case TypeBoundsTree(lo, hi) =>
          this += "_ >: "
          printTypeTree(lo)
          this += " <: "
          printTypeTree(hi)
        case IsWildcardTypeTree(tpt) =>
          printTypeOrBound(tpt.tpe)
        case IsTypeTree(tpt) =>
          printTypeTree(tpt)
      }

      def printTypeTree(tree: TypeTree): Buffer = tree match {
        case TypeTree.Inferred() =>
          // TODO try to move this logic into `printType`
          def printTypeAndAnnots(tpe: Type): Buffer = tpe match {
            case Type.AnnotatedType(tp, annot) =>
              printTypeAndAnnots(tp)
              this += " "
              printAnnotation(annot)
            case Type.SymRef(IsClassSymbol(sym), _) if sym.fullName == "scala.runtime.Null$" || sym.fullName == "scala.runtime.Nothing$" =>
              // scala.runtime.Null$ and scala.runtime.Nothing$ are not modules, those are their actual names
              printType(tpe)
            case tpe @ Type.SymRef(IsClassSymbol(sym), _) if sym.name.endsWith("$") =>
              printType(tpe)
              this += ".type"
            case tpe => printType(tpe)
          }
          printTypeAndAnnots(tree.tpe)

        case TypeTree.Ident(name) =>
          printType(tree.tpe)

        case TypeTree.Select(qual, name) =>
          printTree(qual) += "." += highlightTypeDef(name, color)

        case TypeTree.Projection(qual, name) =>
          printTypeTree(qual) += "#" += highlightTypeDef(name, color)

        case TypeTree.Singleton(ref) =>
          printTree(ref)
          ref match {
            case Term.Literal(_) => this
            case _ => this += ".type"
          }

        case TypeTree.Refined(tpt, refinements) =>
          printTypeTree(tpt)
          inBlock(printTrees(refinements, "; "))

        case TypeTree.Applied(tpt, args) =>
          printTypeTree(tpt)
          inSquare(printTrees(args, ", "))

        case TypeTree.Annotated(tpt, annot) =>
          val Annotation(ref, args) = annot
          ref.tpe match {
            case Types.RepeatedAnnotation() =>
              val Types.Sequence(tp) = tpt.tpe
              printType(tp)
              this += highlightTypeDef("*", color)
            case _ =>
              printTypeTree(tpt)
              this += " "
              printAnnotation(annot)
          }

        case TypeTree.MatchType(bound, selector, cases) =>
          printTypeTree(selector)
          this += highlightKeyword(" match ", color)
          inBlock(printTypeCases(cases, lineBreak()))

        case TypeTree.ByName(result) =>
          this += highlightTypeDef("=> ", color)
          printTypeTree(result)

        case TypeTree.LambdaTypeTree(tparams, body) =>
          printTargsDefs(tparams.zip(tparams), isDef = false)
          this += highlightTypeDef(" => ", color)
          printTypeOrBoundsTree(body)

        case TypeTree.TypeBind(name, _) =>
          this += highlightTypeDef(name, color)

        case TypeTree.TypeBlock(aliases, tpt) =>
          inBlock {
            printTrees(aliases, lineBreak())
            printTypeTree(tpt)
          }

        case _ =>
          throw new MatchError(tree.show)

      }

      def printTypeOrBound(tpe: TypeOrBounds): Buffer = tpe match {
        case tpe@TypeBounds(lo, hi) =>
          this += "_ >: "
          printType(lo)
          this += " <: "
          printType(hi)
        case IsType(tpe) => printType(tpe)
      }

      def printType(tpe: Type): Buffer = tpe match {
        case Type.ConstantType(const) =>
          printConstant(const)

        case Type.SymRef(sym, prefix) =>
          prefix match {
            case Types.EmptyPrefix() =>
            case IsType(prefix @ Type.SymRef(IsClassSymbol(_), _)) =>
              printType(prefix)
              this += "#"
            case IsType(prefix) =>
              if (!sym.flags.is(Flags.Local)) {
                printType(prefix)
                this += "."
              }
          }
          this += highlightTypeDef(sym.name.stripSuffix("$"), color)

        case Type.TermRef(name, prefix) =>
          prefix match {
            case Type.ThisType(Types.EmptyPackage()) =>
              this += highlightTypeDef(name, color)
            case IsType(prefix) =>
              printType(prefix)
              if (name != "package")
                this += "." += highlightTypeDef(name, color)
              this
            case NoPrefix() =>
              this += highlightTypeDef(name, color)
          }

        case Type.TypeRef(name, prefix) =>
          prefix match {
            case NoPrefix() | Type.ThisType(Types.EmptyPackage()) =>
            case IsType(prefix) => printType(prefix) += "."
          }
          if (name.endsWith("$")) this += highlightTypeDef(name.stripSuffix("$"), color) += ".type"
          else this += highlightTypeDef(name, color)

        case tpe @ Type.Refinement(_, _, _) =>
          printRefinement(tpe)

        case Type.AppliedType(tp, args) =>
          tp match {
            case Type.TypeRef("<repeated>", Types.ScalaPackage()) =>
              this += "_*"
            case _ =>
              printType(tp)
              inSquare(printTypesOrBounds(args, ", "))
          }

        case Type.AnnotatedType(tp, annot) =>
          val Annotation(ref, args) = annot
          printType(tp)
          this += " "
          printAnnotation(annot)

        case Type.AndType(left, right) =>
          printType(left)
          this += highlightTypeDef(" & ", color)
          printType(right)

        case Type.OrType(left, right) =>
          printType(left)
          this += highlightTypeDef(" | ", color)
          printType(right)

        case Type.MatchType(bound, scrutinee, cases) =>
          printType(scrutinee)
          this += highlightKeyword(" match ", color)
          inBlock(printTypes(cases, lineBreak()))

        case Type.ByNameType(tp) =>
          this += highlightTypeDef(" => ", color)
          printType(tp)

        case Type.ThisType(tp) =>
          tp match {
            case Type.SymRef(cdef, _) if !cdef.flags.is(Flags.Object) =>
              printFullClassName(tp)
              this += highlightTypeDef(".this", color)
            case Type.TypeRef(name, prefix) if name.endsWith("$") =>
              prefix match {
                case Types.EmptyPrefix() =>
                case _ =>
                  printTypeOrBound(prefix)
                  this += "."
              }
              this += highlightTypeDef(name.stripSuffix("$"), color)
            case _ =>
              printType(tp)
          }

        case Type.SuperType(thistpe, supertpe) =>
          printType(supertpe)
          this += highlightTypeDef(".super", color)

        case Type.TypeLambda(paramNames, tparams, body) =>
          inSquare(printMethodicTypeParams(paramNames, tparams))
          this += highlightTypeDef(" => ", color)
          printTypeOrBound(body)

        case Type.ParamRef(lambda, idx) =>
          lambda match {
            case Type.MethodType(params, _, _) => this += params(idx)
            case Type.PolyType(params, _, _) => this += params(idx)
            case Type.TypeLambda(params, _, _) => this += params(idx)
          }

        case Type.RecursiveType(tpe) =>
          printType(tpe)

        case Type.RecursiveThis(_) =>
          this += highlightTypeDef("this", color)

        case _ =>
          throw new MatchError(tpe.show)
      }

      def printImportSelector(sel: ImportSelector): Buffer = sel match {
        case SimpleSelector(Id(name)) => this += name
        case OmitSelector(Id(name)) => this += name += " => _"
        case RenameSelector(Id(name), Id(newName)) => this += name += " => " += newName
      }

      def printDefinitionName(sym: Definition): Buffer = sym match {
        case ValDef(name, _, _) => this += highlightValDef(name, color)
        case DefDef(name, _, _, _, _) => this += highlightValDef(name, color)
        case ClassDef(name, _, _, _, _, _) => this += highlightTypeDef(name.stripSuffix("$"), color)
        case TypeDef(name, _) => this += highlightTypeDef(name, color)
        case PackageDef(name, _) => this += highlightTypeDef(name, color)
      }

      def printAnnotation(annot: Term): Buffer = {
        val Annotation(ref, args) = annot
        this += "@"
        printTypeTree(ref)
        if (args.isEmpty)
          this
        else
          inParens(printTrees(args, ", "))
      }

      def printDefAnnotations(definition: Definition): Buffer = {
        val annots = definition.symbol.annots.filter {
          case Annotation(annot, _) =>
            annot.tpe match {
              case Type.TypeRef(_, Type.SymRef(sym, _)) if sym.fullName == "scala.annotation.internal" => false
              case Type.TypeRef("forceInline", Types.ScalaPackage()) => false
              case _ => true
            }
          case x => throw new MatchError(x.show)
        }
        printAnnotations(annots)
        if (annots.nonEmpty) this += " "
        else this
      }

      def printRefinement(tpe: Type): Buffer = {
        def printMethodicType(tp: TypeOrBounds): Unit = tp match {
          case tp @ Type.MethodType(paramNames, params, res) =>
            inParens(printMethodicTypeParams(paramNames, params))
            printMethodicType(res)
          case tp @ Type.TypeLambda(paramNames, params, res) =>
            inSquare(printMethodicTypeParams(paramNames, params))
            printMethodicType(res)
          case Type.ByNameType(t) =>
            this += ": "
            printType(t)
          case IsType(tp) =>
            this += ": "
            printType(tp)
        }
        def rec(tp: Type): Unit = tp match {
          case Type.Refinement(parent, name, info) =>
            rec(parent)
            indented {
              this += lineBreak()
              info match {
                case IsTypeBounds(info) =>
                  this += highlightKeyword("type ", color) += highlightTypeDef(name, color)
                  printBounds(info)
                case Type.ByNameType(_) | Type.MethodType(_, _, _) | Type.TypeLambda(_, _, _) =>
                  this += highlightKeyword("def ", color) += highlightTypeDef(name, color)
                  printMethodicType(info)
                case IsType(info) =>
                  this += highlightKeyword("val ", color) += highlightValDef(name, color)
                  printMethodicType(info)
              }
            }
          case tp =>
            printType(tp)
            this += " {"
        }
        rec(tpe)
        this += lineBreak() += "}"
      }

      def printMethodicTypeParams(paramNames: List[String], params: List[TypeOrBounds]): Unit = {
        def printInfo(info: TypeOrBounds) = info match {
          case IsTypeBounds(info) => printBounds(info)
          case IsType(info) =>
            this += ": "
            printType(info)
        }
        def printSeparated(list: List[(String, TypeOrBounds)]): Unit = list match {
          case Nil =>
          case (name, info) :: Nil =>
            this += name
            printInfo(info)
          case (name, info) :: xs =>
            this += name
            printInfo(info)
            this += ", "
            printSeparated(xs)
        }
        printSeparated(paramNames.zip(params))
      }

      def printBoundsTree(bounds: TypeBoundsTree): Buffer = {
        bounds.low match {
          case TypeTree.Inferred() =>
          case low =>
            this += " >: "
            printTypeTree(low)
        }
        bounds.hi match {
          case TypeTree.Inferred() => this
          case hi =>
            this += " <: "
            printTypeTree(hi)
        }
      }

      def printBounds(bounds: TypeBounds): Buffer = {
        this += " >: "
        printType(bounds.low)
        this += " <: "
        printType(bounds.hi)
      }

      def printProtectedOrPrivate(definition: Definition): Boolean = {
        var prefixWasPrinted = false
        def printWithin(within: Type) = within match {
          case Type.SymRef(sym, _) =>
            this += sym.name
          case _ => printFullClassName(within)
        }
        if (definition.symbol.flags.is(Flags.Protected)) {
          this += highlightKeyword("protected", color)
          definition.symbol.protectedWithin match {
            case Some(within) =>
              inSquare(printWithin(within))
            case _ =>
          }
          prefixWasPrinted = true
        } else {
          definition.symbol.privateWithin match {
            case Some(within) =>
              this += highlightKeyword("private", color)
              inSquare(printWithin(within))
              prefixWasPrinted = true
            case _ =>
          }
        }
        if (prefixWasPrinted)
          this += " "
        prefixWasPrinted
      }

      def printFullClassName(tp: TypeOrBounds): Unit = {
        def printClassPrefix(prefix: TypeOrBounds): Unit = prefix match {
          case Type.SymRef(IsClassSymbol(sym), prefix2) =>
            printClassPrefix(prefix2)
            this += sym.name += "."
          case _ =>
        }
        val Type.SymRef(sym, prefix) = tp
        printClassPrefix(prefix)
        this += sym.name
      }

      def +=(x: Boolean): this.type = { sb.append(x); this }
      def +=(x: Byte): this.type = { sb.append(x); this }
      def +=(x: Short): this.type = { sb.append(x); this }
      def +=(x: Int): this.type = { sb.append(x); this }
      def +=(x: Long): this.type = { sb.append(x); this }
      def +=(x: Float): this.type = { sb.append(x); this }
      def +=(x: Double): this.type = { sb.append(x); this }
      def +=(x: Char): this.type = { sb.append(x); this }
      def +=(x: String): this.type = { sb.append(x); this }

      private def escapedChar(ch: Char): String = (ch: @switch) match {
        case '\b' => "\\b"
        case '\t' => "\\t"
        case '\n' => "\\n"
        case '\f' => "\\f"
        case '\r' => "\\r"
        case '"' => "\\\""
        case '\'' => "\\\'"
        case '\\' => "\\\\"
        case _ => if (ch.isControl) "\\0" + Integer.toOctalString(ch) else String.valueOf(ch)
      }

      private def escapedString(str: String): String = str flatMap escapedChar
    }

    private object SpecialOp {
      def unapply(arg: Tree)(implicit ctx: Context): Option[(String, List[Term])] = arg match {
        case IsTerm(arg @ Term.Apply(fn, args)) =>
          fn.tpe match {
            case Type.SymRef(IsDefSymbol(sym), Type.ThisType(Type.SymRef(sym2, _))) if sym2.name == "<special-ops>" =>
              Some((sym.tree.name, args))
            case _ => None
          }
        case _ => None
      }
    }

    private object Annotation {
      def unapply(arg: Tree)(implicit ctx: Context): Option[(TypeTree, List[Term])] = arg match {
        case Term.New(annot) => Some((annot, Nil))
        case Term.Apply(Term.Select(Term.New(annot), "<init>"), args) => Some((annot, args))
        case Term.Apply(Term.TypeApply(Term.Select(Term.New(annot), "<init>"), targs), args) => Some((annot, args))
        case _ => None
      }
    }

    // TODO Provide some of these in scala.tasty.Reflection.scala and implement them using checks on symbols for performance
    private object Types {

      object JavaLangObject {
        def unapply(tpe: Type)(implicit ctx: Context): Boolean = tpe match {
          case Type.TypeRef("Object", Type.SymRef(sym, _)) if sym.fullName == "java.lang" => true
          case _ => false
        }
      }

      object Sequence {
        def unapply(tpe: Type)(implicit ctx: Context): Option[Type] = tpe match {
          case Type.AppliedType(Type.TypeRef("Seq", Type.SymRef(sym, _)), IsType(tp) :: Nil) if sym.fullName == "scala.collection" => Some(tp)
          case _ => None
        }
      }

      object RepeatedAnnotation {
        def unapply(tpe: Type)(implicit ctx: Context): Boolean = tpe match {
          case Type.TypeRef("Repeated", Type.SymRef(sym, _)) if sym.fullName == "scala.annotation.internal" => true
          case _ => false
        }
      }

      object Repeated {
        def unapply(tpe: Type)(implicit ctx: Context): Option[Type] = tpe match {
          case Type.AppliedType(Type.TypeRef("<repeated>", ScalaPackage()), IsType(tp) :: Nil) => Some(tp)
          case _ => None
        }
      }

      object ScalaPackage {
        def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Boolean = tpe match {
          case Type.SymRef(sym, _) => sym == definitions.ScalaPackage
          case _ => false
        }
      }

      object RootPackage {
        def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Boolean = tpe match {
          case Type.SymRef(sym, _) => sym.fullName == "<root>" // TODO use Symbol.==
          case _ => false
        }
      }

      object EmptyPackage {
        def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Boolean = tpe match {
          case Type.SymRef(sym, _) => sym.fullName == "<empty>"
          case _ => false
        }
      }

      object EmptyPrefix {
        def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Boolean = tpe match {
          case NoPrefix() | Type.ThisType(Types.EmptyPackage() | Types.RootPackage()) => true
          case _ => false
        }
      }
    }

    object PackageObject {
      def unapply(tree: Tree)(implicit ctx: Context): Option[Tree] = tree match {
        case PackageClause(_, ValDef("package", _, _) :: body :: Nil) => Some(body)
        case _ => None
      }
    }

  }

}
