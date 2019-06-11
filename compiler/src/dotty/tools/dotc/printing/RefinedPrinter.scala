package dotty.tools.dotc
package printing

import core._
import Texts._
import Types._
import Flags._
import Names._
import Symbols._
import NameOps._
import TypeErasure.ErasedValueType
import Contexts.Context
import Annotations.Annotation
import Denotations._
import SymDenotations._
import StdNames.{nme, tpnme}
import ast.{Trees, untpd}
import typer.{Implicits, Namer, Applications}
import typer.ProtoTypes._
import Trees._
import TypeApplications._
import Decorators._
import scala.tasty.util.Chars.isOperatorPart
import transform.TypeUtils._

import language.implicitConversions
import dotty.tools.dotc.util.{NameTransformer, SourcePosition}
import dotty.tools.dotc.ast.untpd.{MemberDef, Modifiers, PackageDef, RefTree, Template, TypeDef, ValOrDefDef}

class RefinedPrinter(_ctx: Context) extends PlainPrinter(_ctx) {

  /** A stack of enclosing DefDef, TypeDef, or ClassDef, or ModuleDefs nodes */
  private[this] var enclosingDef: untpd.Tree = untpd.EmptyTree
  private[this] var myCtx: Context = super.ctx
  private[this] var printPos = ctx.settings.YprintPos.value
  private[this] val printLines = ctx.settings.printLines.value

  override protected[this] implicit def ctx: Context = myCtx

  def withEnclosingDef(enclDef: Tree[_ >: Untyped])(op: => Text): Text = {
    val savedCtx = myCtx
    if (enclDef.hasType && enclDef.symbol.exists)
      myCtx = ctx.withOwner(enclDef.symbol)
    val savedDef = enclosingDef
    enclosingDef = enclDef
    try op finally {
      myCtx = savedCtx
      enclosingDef = savedDef
    }
  }

  def inPattern(op: => Text): Text = {
    val savedCtx = myCtx
    myCtx = ctx.addMode(Mode.Pattern)
    try op finally myCtx = savedCtx
  }

  def withoutPos(op: => Text): Text = {
    val savedPrintPos = printPos
    printPos = false
    try op finally printPos = savedPrintPos
  }

  protected def enclDefIsClass: Boolean = enclosingDef match {
    case owner: TypeDef => owner.isClassDef
    case owner: untpd.ModuleDef => true
    case _ => false
  }

  override protected def recursionLimitExceeded(): Unit = {}

  protected def PrintableFlags(isType: Boolean): FlagSet = {
    if (isType) TypeSourceModifierFlags | Module | Local
    else TermSourceModifierFlags | Module | Local
  }.toCommonFlags

  override def nameString(name: Name): String =
    if (ctx.settings.YdebugNames.value) name.debugString else NameTransformer.decodeIllegalChars(name.toString)

  override protected def simpleNameString(sym: Symbol): String =
    nameString(if (ctx.property(XprintMode).isEmpty) sym.initial.name else sym.name)

  override def fullNameString(sym: Symbol): String =
    if (isEmptyPrefix(sym.maybeOwner)) nameString(sym)
    else super.fullNameString(sym)

  override protected def fullNameOwner(sym: Symbol): Symbol = {
    val owner = super.fullNameOwner(sym)
    if (owner is ModuleClass) owner.sourceModule else owner
  }

  override def toTextRef(tp: SingletonType): Text = controlled {
    tp match {
      case tp: ThisType if !printDebug =>
        if (tp.cls.isAnonymousClass) keywordStr("this")
        if (tp.cls.is(ModuleClass)) fullNameString(tp.cls.sourceModule)
        else super.toTextRef(tp)
      case _ =>
        super.toTextRef(tp)
    }
  }

  override def toTextPrefix(tp: Type): Text = controlled {
    def isOmittable(sym: Symbol) =
      if (printDebug) false
      else if (homogenizedView) isEmptyPrefix(sym) // drop <root> and anonymous classes, but not scala, Predef.
      else isOmittablePrefix(sym)
    tp match {
      case tp: ThisType if isOmittable(tp.cls) =>
        ""
      case tp @ TermRef(pre, _) =>
        val sym = tp.symbol
        if (sym.isPackageObject && !homogenizedView) toTextPrefix(pre)
        else if (isOmittable(sym)) ""
        else super.toTextPrefix(tp)
      case _ => super.toTextPrefix(tp)
    }
  }

  override protected def toTextParents(parents: List[Type]): Text =
    Text(parents.map(toTextLocal).map(typeText), keywordStr(" with "))

  override protected def refinementNameString(tp: RefinedType): String =
    if (tp.parent.isInstanceOf[WildcardType] || tp.refinedName == nme.WILDCARD)
      super.refinementNameString(tp)
    else {
      val tsym = tp.parent.member(tp.refinedName).symbol
      if (!tsym.exists) super.refinementNameString(tp)
      else simpleNameString(tsym)
    }

  override def toText(tp: Type): Text = controlled {
    def toTextTuple(args: List[Type]): Text =
      "(" ~ argsText(args) ~ ")"

    def toTextFunction(args: List[Type], isContextual: Boolean, isErased: Boolean): Text =
      changePrec(GlobalPrec) {
        val argStr: Text =
          if (args.length == 2 && !defn.isTupleType(args.head))
            atPrec(InfixPrec) { argText(args.head) }
          else
            toTextTuple(args.init)
        (keywordText("given ") provided isContextual) ~
        (keywordText("erased ") provided isErased) ~
        argStr ~ " => " ~ argText(args.last)
      }

    def toTextDependentFunction(appType: MethodType): Text =
      (keywordText("given ") provided appType.isImplicitMethod) ~
      (keywordText("erased ") provided appType.isErasedMethod) ~
      "(" ~ paramsText(appType) ~ ") => " ~ toText(appType.resultType)

    def isInfixType(tp: Type): Boolean = tp match {
      case AppliedType(tycon, args) =>
        args.length == 2 &&
        tycon.typeSymbol.getAnnotation(defn.ShowAsInfixAnnot).map(_.argumentConstant(0).forall(_.booleanValue))
          .getOrElse(!Character.isUnicodeIdentifierStart(tycon.typeSymbol.name.toString.head))
      case _ => false
    }

    def tyconName(tp: Type): Name = tp.typeSymbol.name
    def checkAssocMismatch(tp: Type, isRightAssoc: Boolean) = tp match {
      case AppliedType(tycon, _) => isInfixType(tp) && tyconName(tycon).endsWith(":") != isRightAssoc
      case AndType(_, _) => isRightAssoc
      case OrType(_, _) => isRightAssoc
      case _ => false
    }

    def toTextInfixType(opName: Name, l: Type, r: Type)(op: => Text): Text = {
      val isRightAssoc = opName.endsWith(":")
      val opPrec = parsing.precedence(opName)

      changePrec(opPrec) {
        val leftPrec = if (isRightAssoc || checkAssocMismatch(l, isRightAssoc)) opPrec + 1 else opPrec
        val rightPrec = if (!isRightAssoc || checkAssocMismatch(r, isRightAssoc)) opPrec + 1 else opPrec

        atPrec(leftPrec) { argText(l) }  ~ " " ~ op ~ " " ~ atPrec(rightPrec) { argText(r) }
      }
    }

    homogenize(tp) match {
      case tp @ AppliedType(tycon, args) =>
        val cls = tycon.typeSymbol
        if (tycon.isRepeatedParam)  toTextLocal(args.head) ~ "*"
        else if (defn.isFunctionClass(cls))  toTextFunction(args, cls.name.isImplicitFunction, cls.name.isErasedFunction)
        else if (tp.tupleArity >= 2 && !printDebug)  toTextTuple(tp.tupleElementTypes)
        else if (isInfixType(tp)) {
          val l :: r :: Nil = args
          val opName = tyconName(tycon)
          toTextInfixType(tyconName(tycon), l, r) { simpleNameString(tycon.typeSymbol) }
        }
        else super.toText(tp)

      // Since RefinedPrinter, unlike PlainPrinter, can output right-associative type-operators, we must override handling
      // of AndType and OrType to account for associativity
      case AndType(tp1, tp2) =>
        toTextInfixType(tpnme.raw.AMP, tp1, tp2) { toText(tpnme.raw.AMP) }
      case OrType(tp1, tp2) =>
        toTextInfixType(tpnme.raw.BAR, tp1, tp2) { toText(tpnme.raw.BAR) }
      case EtaExpansion(tycon) if !printDebug =>
        toText(tycon)
      case tp: RefinedType if defn.isFunctionType(tp) =>
        toTextDependentFunction(tp.refinedInfo.asInstanceOf[MethodType])
      case tp: TypeRef =>
        if (tp.symbol.isAnonymousClass && !ctx.settings.uniqid.value)
          toText(tp.info)
        else if (tp.symbol.is(Param))
          tp.prefix match {
            case pre: ThisType if pre.cls == tp.symbol.owner =>
              nameString(tp.symbol)
            case _ => super.toText(tp)
          }
        else super.toText(tp)
      case tp: ExprType =>
        exprToText(tp)
      case ErasedValueType(tycon, underlying) =>
        "ErasedValueType(" ~ toText(tycon) ~ ", " ~ toText(underlying) ~ ")"
      case tp: ClassInfo =>
        toTextParents(tp.parents) ~~ "{...}"
      case JavaArrayType(elemtp) =>
        toText(elemtp) ~ "[]"
      case tp: AnnotatedType if homogenizedView =>
        // Positions of annotations in types are not serialized
        // (they don't need to because we keep the original type tree with
        //  the original annotation anyway. Therefore, there will always be
        //  one version of the annotation tree that has the correct positions).
        withoutPos(super.toText(tp))
      case tp: SelectionProto =>
        "?{ " ~ toText(tp.name) ~
           (Str(" ") provided !tp.name.toSimpleName.last.isLetterOrDigit) ~
           ": " ~ toText(tp.memberProto) ~ " }"
      case tp: ViewProto =>
        toText(tp.argType) ~ " ?=>? " ~ toText(tp.resultType)
      case tp @ FunProto(args, resultType) =>
        val argsText = args match {
          case dummyTreeOfType(tp) :: Nil if !(tp isRef defn.NullClass) => "null: " ~ toText(tp)
          case _ => toTextGlobal(args, ", ")
        }
        "[applied to " ~ (Str("given ") provided tp.isContextualMethod) ~ (Str("erased ") provided tp.isErasedMethod) ~ "(" ~ argsText ~ ") returning " ~ toText(resultType) ~ "]"
      case IgnoredProto(ignored) =>
        "?" ~ (("(ignored: " ~ toText(ignored) ~ ")") provided printDebug)
      case tp @ PolyProto(targs, resType) =>
        "[applied to [" ~ toTextGlobal(targs, ", ") ~ "] returning " ~ toText(resType)
      case _ =>
        super.toText(tp)
    }
  }

  protected def exprToText(tp: ExprType): Text =
    "=> " ~ toText(tp.resType)

  protected def blockToText[T >: Untyped](block: Block[T]): Text =
    blockText(block.stats :+ block.expr)

  protected def blockText[T >: Untyped](trees: List[Tree[T]]): Text =
    ("{" ~ toText(trees, "\n") ~ "}").close

  protected def typeApplyText[T >: Untyped](tree: TypeApply[T]): Text = {
    val isQuote = !printDebug && tree.fun.hasType && tree.fun.symbol == defn.InternalQuoted_typeQuote
    val (open, close) = if (isQuote) (keywordStr("'["), keywordStr("]")) else ("[", "]")
    val funText = toTextLocal(tree.fun).provided(!isQuote)
    tree.fun match {
      case Select(New(tpt), nme.CONSTRUCTOR) if tpt.typeOpt.dealias.isInstanceOf[AppliedType] =>
        funText  // type was already printed by toText(new)
      case _ =>
        funText ~ open ~ toTextGlobal(tree.args, ", ") ~ close
    }
  }

  protected def toTextCore[T >: Untyped](tree: Tree[T]): Text = {
    import untpd.{modsDeco => _, _}

    def isLocalThis(tree: Tree) = tree.typeOpt match {
      case tp: ThisType => tp.cls == ctx.owner.enclosingClass
      case _ => false
    }

    def optDotPrefix(tree: This) = optText(tree.qual)(_ ~ ".") provided !isLocalThis(tree)

    def caseBlockText(tree: Tree): Text = tree match {
      case Block(stats, expr) => toText(stats :+ expr, "\n")
      case expr => toText(expr)
    }

    // Dotty deviation: called with an untpd.Tree, so cannot be a untpd.Tree[T] (seems to be a Scala2 problem to allow this)
    // More deviations marked below as // DD
    def enumText(tree: untpd.Tree) = tree match { // DD
      case _: untpd.GenFrom | _: untpd.GenAlias => toText(tree)
      case _ => keywordStr("if ") ~ toText(tree)
    }

    def forText(enums: List[untpd.Tree], expr: untpd.Tree, sep: String): Text = // DD
      changePrec(GlobalPrec) { keywordStr("for ") ~ Text(enums map enumText, "; ") ~ sep ~ toText(expr) }

    def cxBoundToText(bound: untpd.Tree): Text = bound match { // DD
      case AppliedTypeTree(tpt, _) => " : " ~ toText(tpt)
      case untpd.Function(_, tpt) => " <% " ~ toText(tpt)
    }

    def varianceText(mods: untpd.Modifiers) =
      if (mods is Covariant) "+"
      else if (mods is Contravariant) "-"
      else ""

    def argText(arg: Tree): Text = arg match {
      case arg: TypeBoundsTree => "_" ~ toTextGlobal(arg)
      case arg: TypeTree =>
        arg.typeOpt match {
          case tp: TypeBounds => "_" ~ toTextGlobal(arg)
          case _ => toTextGlobal(arg)
        }
      case _ => toTextGlobal(arg)
    }

    def dropBlock(tree: Tree): Tree = tree match {
      case Block(Nil, expr) => expr
      case _ => tree
    }

    tree match {
      case id: Trees.BackquotedIdent[_] if !homogenizedView =>
        "`" ~ toText(id.name) ~ "`"
      case id: Trees.SearchFailureIdent[_] =>
        tree.typeOpt match {
          case reason: Implicits.SearchFailureType =>
            toText(id.name) ~ "implicitly[" ~ toText(reason.clarify(reason.expectedType)) ~ "]"
          case _ =>
            toText(id.name)
        }
      case Ident(name) =>
        val txt = tree.typeOpt match {
          case tp: NamedType if name != nme.WILDCARD =>
            val pre = if (tp.symbol is JavaStatic) tp.prefix.widen else tp.prefix
            toTextPrefix(pre) ~ withPos(selectionString(tp), tree.sourcePos)
          case _ =>
            toText(name)
        }
        if (name.isTypeName) typeText(txt)
        else txt
      case tree @ Select(qual, name) =>
        if (!printDebug && tree.hasType && tree.symbol == defn.QuotedType_splice) typeText("${") ~ toTextLocal(qual) ~ typeText("}")
        else if (qual.isType) toTextLocal(qual) ~ "#" ~ typeText(toText(name))
        else toTextLocal(qual) ~ ("." ~ nameIdText(tree) provided (name != nme.CONSTRUCTOR || printDebug))
      case tree: This =>
        optDotPrefix(tree) ~ keywordStr("this") ~ idText(tree)
      case Super(qual: This, mix) =>
        optDotPrefix(qual) ~ keywordStr("super") ~ optText(mix)("[" ~ _ ~ "]")
      case app @ Apply(fun, args) =>
        if (fun.hasType && fun.symbol == defn.throwMethod)
          changePrec (GlobalPrec) {
            keywordStr("throw ") ~ toText(args.head)
          }
        else if (!printDebug && fun.hasType && fun.symbol == defn.InternalQuoted_exprQuote)
          keywordStr("'{") ~ toTextGlobal(args, ", ") ~ keywordStr("}")
        else if (!printDebug && fun.hasType && fun.symbol == defn.InternalQuoted_exprSplice)
          keywordStr("${") ~ toTextGlobal(args, ", ") ~ keywordStr("}")
        else if (app.isGivenApply && !homogenizedView)
          changePrec(InfixPrec) {
            toTextLocal(fun) ~ " given " ~
            (if (args.length == 1) toTextLocal(args.head) else "(" ~ toTextGlobal(args, ", ") ~ ")")
          }
        else
          toTextLocal(fun) ~ "(" ~ toTextGlobal(args, ", ") ~ ")"
      case tree: TypeApply =>
        typeApplyText(tree)
      case Literal(c) =>
        tree.typeOpt match {
          case ConstantType(tc) => withPos(toText(tc), tree.sourcePos)
          case _ => withPos(toText(c), tree.sourcePos)
        }
      case New(tpt) =>
        keywordStr("new ") ~ {
          tpt match {
            case tpt: Template => toTextTemplate(tpt, ofNew = true)
            case _ => toTextLocal(tpt)
          }
        }
      case Typed(expr, tpt) =>
        changePrec(InfixPrec) {
          val exprText = toText(expr)
          val line = exprText.lastLine
          val colon = if (!line.isEmpty && isOperatorPart(line.last)) " :" else ":"
          exprText ~ colon ~ toText(tpt) }
      case NamedArg(name, arg) =>
        toText(name) ~ " = " ~ toText(arg)
      case Assign(lhs, rhs) =>
        changePrec(GlobalPrec) { toTextLocal(lhs) ~ " = " ~ toText(rhs) }
      case block: Block =>
        blockToText(block)
      case If(cond, thenp, elsep) =>
        val isInline = tree.isInstanceOf[Trees.InlineIf[_]]
        changePrec(GlobalPrec) {
          keywordStr(if (isInline) "inline if " else "if ") ~
          toText(cond) ~ (keywordText(" then") provided !cond.isInstanceOf[Parens]) ~~
          toText(thenp) ~ optText(elsep)(keywordStr(" else ") ~ _)
        }
      case Closure(env, ref, target) =>
        "closure(" ~ (toTextGlobal(env, ", ") ~ " | " provided env.nonEmpty) ~
        toTextGlobal(ref) ~ (":" ~ toText(target) provided !target.isEmpty) ~ ")"
      case Match(sel, cases) =>
        val isInline = tree.isInstanceOf[Trees.InlineMatch[_]]
        if (sel.isEmpty && !isInline) blockText(cases)
        else changePrec(GlobalPrec) {
          val selTxt: Text =
            if (isInline)
              if (sel.isEmpty) keywordStr("implicit")
              else keywordStr("inline ") ~ toText(sel)
            else toText(sel)
          selTxt ~ keywordStr(" match ") ~ blockText(cases)
        }
      case CaseDef(pat, guard, body) =>
        keywordStr("case ") ~ inPattern(toText(pat)) ~ optText(guard)(keywordStr(" if ") ~ _) ~ " => " ~ caseBlockText(body)
      case Labeled(bind, expr) =>
        changePrec(GlobalPrec) { toText(bind.name) ~ keywordStr("[") ~ toText(bind.symbol.info) ~ keywordStr("]: ") ~ toText(expr) }
      case Return(expr, from) =>
        val sym = from.symbol
        if (sym.is(Label))
          changePrec(GlobalPrec) { keywordStr("return[") ~ toText(sym.name) ~ keywordStr("]") ~ optText(expr)(" " ~ _) }
        else
          changePrec(GlobalPrec) { keywordStr("return") ~ optText(expr)(" " ~ _) }
      case WhileDo(cond, body) =>
        changePrec(GlobalPrec) { keywordStr("while ") ~ toText(cond) ~ keywordStr(" do ") ~ toText(body) }
      case Try(expr, cases, finalizer) =>
        changePrec(GlobalPrec) {
          keywordStr("try ") ~ toText(expr) ~ optText(cases)(keywordStr(" catch ") ~ _) ~ optText(finalizer)(keywordStr(" finally ") ~ _)
        }
      case Throw(expr) =>
        changePrec(GlobalPrec) {
          keywordStr("throw ") ~ toText(expr)
        }
      case SeqLiteral(elems, elemtpt) =>
        "[" ~ toTextGlobal(elems, ",") ~ " : " ~ toText(elemtpt) ~ "]"
      case tree @ Inlined(call, bindings, body) =>
        (("/* inlined from " ~ (if (call.isEmpty) "outside" else toText(call)) ~ " */ ") `provided`
          !homogenizedView && ctx.settings.XprintInline.value) ~
        blockText(bindings :+ body)
      case tpt: untpd.DerivedTypeTree =>
        "<derived typetree watching " ~ summarized(toText(tpt.watched)) ~ ">"
      case TypeTree() =>
        typeText(toText(tree.typeOpt))
      case SingletonTypeTree(ref) =>
        toTextLocal(ref) ~ "." ~ keywordStr("type")
      case RefinedTypeTree(tpt, refines) =>
        toTextLocal(tpt) ~ " " ~ blockText(refines)
      case AppliedTypeTree(tpt, args) =>
        if (tpt.symbol == defn.orType && args.length == 2)
          changePrec(OrTypePrec) { toText(args(0)) ~ " | " ~ atPrec(OrTypePrec + 1) { toText(args(1)) } }
        else if (tpt.symbol == defn.andType && args.length == 2)
          changePrec(AndTypePrec) { toText(args(0)) ~ " & " ~ atPrec(AndTypePrec + 1) { toText(args(1)) } }
        else
          toTextLocal(tpt) ~ "[" ~ Text(args map argText, ", ") ~ "]"
      case LambdaTypeTree(tparams, body) =>
        changePrec(GlobalPrec) {
          tparamsText(tparams) ~ " =>> " ~ toText(body)
        }
      case MatchTypeTree(bound, sel, cases) =>
        changePrec(GlobalPrec) {
          toText(sel) ~ keywordStr(" match ") ~ blockText(cases) ~
          (" <: " ~ toText(bound) provided !bound.isEmpty)
        }
      case ByNameTypeTree(tpt) =>
        "=> " ~ toTextLocal(tpt)
      case TypeBoundsTree(lo, hi) =>
        if (lo eq hi) optText(lo)(" = " ~ _)
        else optText(lo)(" >: " ~ _) ~ optText(hi)(" <: " ~ _)
      case Bind(name, body) =>
        changePrec(InfixPrec) { toText(name) ~ " @ " ~ toText(body) }
      case Alternative(trees) =>
        changePrec(OrPrec) { toText(trees, " | ") }
      case UnApply(fun, implicits, patterns) =>
        val extractor = fun match {
          case Select(extractor, name) if name.isUnapplyName => extractor
          case _ => fun
        }
        toTextLocal(extractor) ~
        "(" ~ toTextGlobal(patterns, ", ") ~ ")" ~
        ("(" ~ toTextGlobal(implicits, ", ") ~ ")" provided implicits.nonEmpty)
      case tree @ ValDef(_, _, _) =>
        valDefToText(tree)
      case tree @ DefDef(_, _, _, _, _) =>
        defDefToText(tree)
      case tree @ TypeDef(name, rhs) =>
        def typeDefText(tparamsText: => Text, rhsText: => Text) =
          dclTextOr(tree) {
            modText(tree.mods, tree.symbol, keywordStr("type"), isType = true) ~~
            (varianceText(tree.mods) ~ typeText(nameIdText(tree))) ~
            withEnclosingDef(tree) { tparamsText ~ rhsText }
          }
        def recur(rhs: Tree, tparamsTxt: => Text): Text = rhs match {
          case impl: Template =>
            templateText(tree, impl)
          case rhs: TypeBoundsTree =>
            typeDefText(tparamsTxt, toText(rhs))
          case LambdaTypeTree(tparams, body) =>
            recur(body, tparamsText(tparams))
          case rhs: TypeTree if isBounds(rhs.typeOpt) =>
            typeDefText(tparamsTxt, toText(rhs))
          case rhs =>
            typeDefText(tparamsTxt, optText(rhs)(" = " ~ _))
        }
        recur(rhs, "")
      case Import(importImplied, expr, selectors) =>
        def selectorText(sel: Tree): Text = sel match {
          case Thicket(l :: r :: Nil) => toTextGlobal(l) ~ " => " ~ toTextGlobal(r)
          case _: Ident => toTextGlobal(sel)
          case TypeBoundsTree(_, tpt) => "for " ~ toTextGlobal(tpt)
        }
        val selectorsText: Text = selectors match {
          case id :: Nil => toText(id)
          case _ => "{" ~ Text(selectors map selectorText, ", ") ~ "}"
        }
        keywordText("import ") ~ (keywordText("delegate ") provided importImplied) ~
        toTextLocal(expr) ~ "." ~ selectorsText
      case packageDef: PackageDef =>
        packageDefText(packageDef)
      case tree: Template =>
        toTextTemplate(tree)
      case Annotated(arg, annot) =>
        toTextLocal(arg) ~~ annotText(annot)
      case EmptyTree =>
        "<empty>"
      case TypedSplice(t) =>
        if (printDebug) "[" ~ toText(t) ~ "]#TS#"
        else toText(t)
      case tree @ ModuleDef(name, impl) =>
        withEnclosingDef(tree) {
          modText(tree.mods, NoSymbol, keywordStr("object"), isType = false) ~~
          nameIdText(tree) ~ toTextTemplate(impl)
        }
      case SymbolLit(str) =>
        "'" + str
      case InterpolatedString(id, segments) =>
        def strText(str: Literal) = withPos(escapedString(str.const.stringValue), tree.sourcePos)
        def segmentText(segment: Tree) = segment match {
          case Thicket(List(str: Literal, expr)) => strText(str) ~ "{" ~ toTextGlobal(expr) ~ "}"
          case str: Literal => strText(str)
        }
        toText(id) ~ "\"" ~ Text(segments map segmentText, "") ~ "\""
      case Function(args, body) =>
        var implicitSeen: Boolean = false
        var contextual: Boolean = false
        var isErased: Boolean = false
        def argToText(arg: Tree) = arg match {
          case arg @ ValDef(name, tpt, _) =>
            val implicitText =
              if ((arg.mods is Given)) { contextual = true; "" }
              else if ((arg.mods is Erased)) { isErased = true; "" }
              else if ((arg.mods is Implicit) && !implicitSeen) { implicitSeen = true; keywordStr("implicit ") }
              else ""
            implicitText ~ toText(name) ~ optAscription(tpt)
          case _ =>
            toText(arg)
        }
        val argsText = args match {
          case (arg @ ValDef(_, tpt, _)) :: Nil if tpt.isEmpty => argToText(arg)
          case _ => "(" ~ Text(args map argToText, ", ") ~ ")"
        }
        changePrec(GlobalPrec) {
		  (keywordText("given ") provided contextual) ~
		  (keywordText("erased ") provided isErased) ~
          argsText ~ " => " ~ toText(body)
        }
      case PolyFunction(targs, body) =>
        val targsText = "[" ~ Text(targs.map((arg: Tree) => toText(arg)), ", ") ~ "]"
        changePrec(GlobalPrec) {
          targsText ~ " => " ~ toText(body)
        }
      case InfixOp(l, op, r) =>
        val opPrec = parsing.precedence(op.name)
        changePrec(opPrec) { toText(l) ~ " " ~ toText(op) ~ " " ~ toText(r) }
      case PostfixOp(l, op) =>
        changePrec(InfixPrec) { toText(l) ~ " " ~ toText(op) }
      case PrefixOp(op, r) =>
        changePrec(DotPrec) { toText(op) ~ " " ~ toText(r) }
      case Parens(t) =>
        "(" ~ toTextGlobal(t) ~ ")"
      case Tuple(ts) =>
        "(" ~ toTextGlobal(ts, ", ") ~ ")"
      case DoWhile(cond, body) =>
        changePrec(GlobalPrec) { keywordStr("do ") ~ toText(body) ~ keywordStr(" while ") ~ toText(cond) }
      case ForYield(enums, expr) =>
        forText(enums, expr, keywordStr(" yield "))
      case ForDo(enums, expr) =>
        forText(enums, expr, keywordStr(" do "))
      case GenFrom(pat, expr, checkMode) =>
        (Str("case ") provided checkMode == untpd.GenCheckMode.FilterAlways) ~
        toText(pat) ~ " <- " ~ toText(expr)
      case GenAlias(pat, expr) =>
        toText(pat) ~ " = " ~ toText(expr)
      case ContextBounds(bounds, cxBounds) =>
        (toText(bounds) /: cxBounds) {(t, cxb) =>
          t ~ cxBoundToText(cxb)
        }
      case PatDef(mods, pats, tpt, rhs) =>
        modText(mods, NoSymbol, keywordStr("val"), isType = false) ~~
        toText(pats, ", ") ~ optAscription(tpt) ~ optText(rhs)(" = " ~ _)
      case ParsedTry(expr, handler, finalizer) =>
        changePrec(GlobalPrec) {
          keywordStr("try ") ~ toText(expr) ~ " " ~ keywordStr("catch") ~ " {" ~ toText(handler) ~ "}" ~ optText(finalizer)(keywordStr(" finally ") ~ _)
        }
      case Quote(tree) =>
        if (tree.isType) keywordStr("'[") ~ toTextGlobal(dropBlock(tree)) ~ keywordStr("]")
        else keywordStr("'{") ~ toTextGlobal(dropBlock(tree)) ~ keywordStr("}")
      case Splice(tree) =>
        keywordStr("${") ~ toTextGlobal(dropBlock(tree)) ~ keywordStr("}")
      case tree: Applications.IntegratedTypeArgs =>
        toText(tree.app) ~ Str("(with integrated type args)").provided(printDebug)
      case Thicket(trees) =>
        "Thicket {" ~~ toTextGlobal(trees, "\n") ~~ "}"
      case _ =>
        tree.fallbackToText(this)
    }
  }

  override def toText[T >: Untyped](tree: Tree[T]): Text = controlled {
    import untpd.{modsDeco => _, _}

    var txt = toTextCore(tree)

    def suppressTypes =
      tree.isType || tree.isDef || // don't print types of types or defs
      homogenizedView && ctx.mode.is(Mode.Pattern)
        // When comparing pickled info, disregard types of patterns.
        // The reason is that GADT matching can rewrite types of pattern trees
        // without changing the trees themselves. (see Typer.typedCase.indexPatterns.transform).
        // But then pickling and unpickling the original trees will yield trees
        // with the original types before they are rewritten, which causes a discrepancy.

    def suppressPositions = tree match {
      case _: WithoutTypeOrPos[_] | _: TypeTree => true // TypeTrees never have an interesting position
      case _ => false
    }

    if (ctx.settings.XprintTypes.value && tree.hasType) {
      // add type to term nodes; replace type nodes with their types unless -Yprint-pos is also set.
      val tp1 = tree.typeOpt match {
        case tp: TermRef if tree.isInstanceOf[RefTree] && !tp.denot.isOverloaded => tp.underlying
        case tp => tp
      }
      val tp2 = {
        val tp = tp1.tryNormalize
        if (tp != NoType) tp else tp1
      }
      if (!suppressTypes)
        txt = ("<" ~ txt ~ ":" ~ toText(tp2) ~ ">").close
      else if (tree.isType && !homogenizedView)
        txt = toText(tp2)
    }
    if (!suppressPositions) {
      if (printPos) {
        val posStr =
          if (homogenizedView || debugPos)
            if (tree.isInstanceOf[MemberDef]) Str(s"${tree.source}${tree.span}")
            else Str(s"${tree.source}${tree.span.toSynthetic}")
          else
            "<" ~ toText(tree.sourcePos) ~ ">"
        val clsStr = ""//if (tree.isType) tree.getClass.toString else ""
        txt = (txt ~ "@" ~ posStr ~ clsStr).close
      }
      if (ctx.settings.YprintPosSyms.value && tree.isDef)
        txt = (txt ~
          s"@@(${tree.symbol.name}=" ~ tree.symbol.sourcePos.toString ~ ")").close
    }
    if (ctx.settings.YshowTreeIds.value)
      txt = (txt ~ "#" ~ tree.uniqueId.toString).close
    tree match {
      case Block(_, _) | Template(_, _, _, _) => txt
      case _ => txt.close
    }
  }

  /** Print modifiers from symbols if tree has type, overriding the untpd behavior. */
  implicit def modsDeco(mdef: untpd.MemberDef)(implicit ctx: Context): untpd.ModsDecorator =
    new untpd.ModsDecorator {
      def mods = if (mdef.hasType) Modifiers(mdef.symbol) else mdef.rawMods
    }

  def Modifiers(sym: Symbol)(implicit ctx: Context): Modifiers = untpd.Modifiers(
    sym.flags & (if (sym.isType) ModifierFlags | VarianceFlags else ModifierFlags),
    if (sym.privateWithin.exists) sym.privateWithin.asType.name else tpnme.EMPTY,
    sym.annotations.filterNot(ann => dropAnnotForModText(ann.symbol)).map(_.tree))

  protected def dropAnnotForModText(sym: Symbol): Boolean = sym == defn.BodyAnnot

  protected def optAscription[T >: Untyped](tpt: Tree[T]): Text = optText(tpt)(": " ~ _)

  private def idText(tree: untpd.Tree): Text = {
    if ((ctx.settings.uniqid.value || Printer.debugPrintUnique) && tree.hasType && tree.symbol.exists) s"#${tree.symbol.id}" else ""
  }

  private def useSymbol(tree: untpd.Tree) =
    tree.hasType && tree.symbol.exists && ctx.settings.YprintSyms.value

  protected def nameIdText[T >: Untyped](tree: NameTree[T]): Text = {
    if (tree.hasType && tree.symbol.exists) {
      val str: Text = nameString(tree.symbol)
      tree match {
        case tree: RefTree => withPos(str, tree.sourcePos)
        case tree: MemberDef => withPos(str, tree.sourcePos.withSpan(tree.nameSpan))
        case _ => str
      }
    }
    else toText(tree.name) ~ idText(tree)
  }

  private def toTextOwner(tree: Tree[_]) =
    "[owner = " ~ tree.symbol.maybeOwner.show ~ "]" provided ctx.settings.YprintDebugOwners.value

  protected def dclTextOr[T >: Untyped](tree: Tree[T])(treeText: => Text): Text =
    toTextOwner(tree) ~ {
      if (useSymbol(tree)) annotsText(tree.symbol) ~~ dclText(tree.symbol)
      else treeText
    }

  def tparamsText[T >: Untyped](params: List[Tree[T]]): Text =
    "[" ~ toText(params, ", ") ~ "]" provided params.nonEmpty

  def addVparamssText[T >: Untyped](txt: Text, vparamss: List[List[ValDef[T]]], isExtension: Boolean = false): Text = {
    def paramsText(params: List[ValDef[T]]) = "(" ~ toText(params, ", ") ~ ")"
    val (leading, paramss) =
      if (isExtension && vparamss.nonEmpty) (paramsText(vparamss.head) ~ " " ~ txt, vparamss.tail)
      else (txt, vparamss)
    (leading /: paramss)((txt, params) =>
      txt ~
      (Str(" given ") provided params.nonEmpty && params.head.mods.is(Given)) ~
      paramsText(params))
  }
  protected def valDefToText[T >: Untyped](tree: ValDef[T]): Text = {
    import untpd.{modsDeco => _}
    dclTextOr(tree) {
      modText(tree.mods, tree.symbol, keywordStr(if (tree.mods is Mutable) "var" else "val"), isType = false) ~~
        valDefText(nameIdText(tree)) ~ optAscription(tree.tpt) ~
        withEnclosingDef(tree) { optText(tree.rhs)(" = " ~ _) }
    }
  }

  protected def defDefToText[T >: Untyped](tree: DefDef[T]): Text = {
    import untpd.{modsDeco => _}
    dclTextOr(tree) {
      val prefix = modText(tree.mods, tree.symbol, keywordStr("def"), isType = false) ~~ valDefText(nameIdText(tree))
      val isExtension = tree.hasType && tree.symbol.is(Extension)
      withEnclosingDef(tree) {
        addVparamssText(prefix ~ tparamsText(tree.tparams), tree.vparamss, isExtension) ~
          optAscription(tree.tpt) ~
          optText(tree.rhs)(" = " ~ _)
      }
    }
  }

  protected def toTextTemplate(impl: Template, ofNew: Boolean = false): Text = {
    val Template(constr @ DefDef(_, tparams, vparamss, _, _), _, self, _) = impl
    val tparamsTxt = withEnclosingDef(constr) { tparamsText(tparams) }
    val primaryConstrs = if (constr.rhs.isEmpty) Nil else constr :: Nil
    val prefix: Text =
      if (vparamss.isEmpty || primaryConstrs.nonEmpty) tparamsTxt
      else {
        var modsText = modText(constr.mods, constr.symbol, "", isType = false)
        if (!modsText.isEmpty) modsText = " " ~ modsText
        if (constr.mods.hasAnnotations && !constr.mods.hasFlags) modsText = modsText ~~ " this"
        withEnclosingDef(constr) { addVparamssText(tparamsTxt ~~ modsText, vparamss) }
      }
    val parentsText = Text(impl.parents.map(constrText), if (ofNew) keywordStr(" with ") else ", ")
    val derivedText = Text(impl.derived.map(toText(_)), ", ")
    val selfText = {
      val selfName = if (self.name == nme.WILDCARD) keywordStr("this") else self.name.toString
      (selfName ~ optText(self.tpt)(": " ~ _) ~ " =>").close
    }.provided(!self.isEmpty)
    val body = if (ctx.settings.YtestPickler.value) {
      // Pickling/unpickling reorders the body members, so we need to homogenize
      val (params, rest) = impl.body partition {
        case stat: TypeDef => stat.symbol.is(Param)
        case stat: ValOrDefDef =>
          stat.symbol.is(ParamAccessor) && !stat.symbol.isSetter
        case _ => false
      }
      params ::: rest
    } else impl.body

    val bodyText = " {" ~~ selfText ~~ toTextGlobal(primaryConstrs ::: body, "\n") ~ "}"

    prefix ~
    keywordText(" extends").provided(!ofNew && impl.parents.nonEmpty) ~~ parentsText ~
    keywordText(" derives").provided(impl.derived.nonEmpty) ~~ derivedText ~
    bodyText
  }

  protected def templateText(tree: TypeDef, impl: Template): Text = {
    val decl = modText(tree.mods, tree.symbol, keywordStr(if ((tree).mods is Trait) "trait" else "class"), isType = true)
    ( decl ~~ typeText(nameIdText(tree)) ~ withEnclosingDef(tree) { toTextTemplate(impl) }
    // ~ (if (tree.hasType && printDebug) i"[decls = ${tree.symbol.info.decls}]" else "") // uncomment to enable
    )
  }

  protected def toTextPackageId[T >: Untyped](pid: Tree[T]): Text =
    if (homogenizedView && pid.hasType) toTextLocal(pid.tpe.asInstanceOf[Showable])
    else toTextLocal(pid)

  protected def packageDefText(tree: PackageDef): Text = {
    val statsText = tree.stats match {
      case (pdef: PackageDef) :: Nil => toText(pdef)
      case _ => toTextGlobal(tree.stats, "\n")
    }
    val bodyText =
      if (currentPrecedence == TopLevelPrec) "\n" ~ statsText else " {" ~ statsText ~ "}"
    keywordStr("package ") ~ toTextPackageId(tree.pid) ~ bodyText
  }

  protected def constrText(tree: untpd.Tree): Text = toTextLocal(tree).stripPrefix(keywordStr("new ")) // DD

  protected def annotText(tree: untpd.Tree): Text = "@" ~ constrText(tree) // DD

  protected def modText(mods: untpd.Modifiers, sym: Symbol, kw: String, isType: Boolean): Text = { // DD
    val suppressKw = if (enclDefIsClass) mods is ParamAndLocal else mods is Param
    var flagMask =
      if (ctx.settings.YdebugFlags.value) AnyFlags
      else if (suppressKw) PrintableFlags(isType) &~ Private
      else PrintableFlags(isType)
    if (homogenizedView && mods.flags.isTypeFlags) flagMask &~= ImplicitOrImplied // drop implicit/delegate from classes
    val rawFlags = if (sym.exists) sym.flags else mods.flags
    if (rawFlags.is(Param)) flagMask = flagMask &~ Given
    val flags = rawFlags & flagMask
    val flagsText = toTextFlags(sym, flags)
    val annotations =
      if (sym.exists) sym.annotations.filterNot(ann => dropAnnotForModText(ann.symbol)).map(_.tree)
      else mods.annotations.filterNot(tree => dropAnnotForModText(tree.symbol))
    Text(annotations.map(annotText), " ") ~~ flagsText ~~ (Str(kw) provided !suppressKw)
  }

  def optText(name: Name)(encl: Text => Text): Text =
    if (name.isEmpty) "" else encl(toText(name))

  def optText[T >: Untyped](tree: Tree[T])(encl: Text => Text): Text =
    if (tree.isEmpty) "" else encl(toText(tree))

  def optText[T >: Untyped](tree: List[Tree[T]])(encl: Text => Text): Text =
    if (tree.exists(!_.isEmpty)) encl(blockText(tree)) else ""

  override protected def ParamRefNameString(name: Name): String =
    name.invariantName.toString

  override protected def treatAsTypeParam(sym: Symbol): Boolean = sym is TypeParam

  override protected def treatAsTypeArg(sym: Symbol): Boolean =
    sym.isType && (sym is ProtectedLocal) &&
      (sym.allOverriddenSymbols exists (_ is TypeParam))

  override def toText(sym: Symbol): Text = {
    if (sym.isImport)
      sym.infoOrCompleter match {
        case info: Namer#Completer => return info.original.show
        case info: ImportType => return s"import $info.expr.show"
        case _ =>
      }
    def name =
      if (printDebug)
        nameString(sym)
      else if (sym.is(ModuleClass) && sym.isPackageObject && sym.name.stripModuleClassSuffix == tpnme.PACKAGE)
        nameString(sym.owner.name)
      else if (sym.is(ModuleClass))
        nameString(sym.name.stripModuleClassSuffix)
      else if (hasMeaninglessName(sym))
        simpleNameString(sym.owner) + idString(sym)
      else
        nameString(sym)
    (keywordText(kindString(sym)) ~~ {
      if (sym.isAnonymousClass)
        toTextParents(sym.info.parents) ~~ "{...}"
      else
        typeText(name)
    }).close
  }

  /** String representation of symbol's kind. */
  override def kindString(sym: Symbol): String = {
    val flags = sym.flagsUNSAFE
    if (flags is Package) "package"
    else if (sym.isPackageObject) "package object"
    else if (flags is Module) "object"
    else if (sym.isClassConstructor) "constructor"
    else super.kindString(sym)
  }

  /** String representation of symbol's definition keyword */
  override protected def keyString(sym: Symbol): String = {
    val flags = sym.flagsUNSAFE
    if (sym.isType && sym.owner.isTerm) ""
    else if (sym.isPackageObject) "package object"
    else if (flags.is(Module) && flags.is(Case)) "case object"
    else if (sym.isClass && flags.is(Case)) "case class"
    else super.keyString(sym)
  }

  override def toTextFlags(sym: Symbol): Text =
    if (ctx.settings.YdebugFlags.value)
      super.toTextFlags(sym)
    else {
      var flags = sym.flagsUNSAFE
      if (flags is TypeParam) flags = flags &~ Protected
      toTextFlags(sym, flags & PrintableFlags(sym.isType))
    }

  override def toText(denot: Denotation): Text = denot match {
    case denot: MultiDenotation => Text(denot.alternatives.map(dclText), " <and> ")
    case NoDenotation => "NoDenotation"
    case _ =>
      if (denot.symbol.exists) toText(denot.symbol)
      else "some " ~ toText(denot.info)
  }

  override def plain: PlainPrinter = new PlainPrinter(_ctx)

  private def withPos(txt: Text, pos: SourcePosition): Text = {
    if (!printLines || !pos.exists) txt
    else txt match {
      case Str(s, _) => Str(s, LineRange(pos.line, pos.endLine))
      case _ => txt
    }
  }
}
