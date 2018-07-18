package dotty.tools.dotc
package printing

import core._
import Texts._
import Types._
import Flags._
import Names._
import Symbols._
import NameOps._
import Constants._
import TypeErasure.ErasedValueType
import Contexts.Context
import Scopes.Scope
import Denotations._
import SymDenotations._
import Annotations.Annotation
import StdNames.{nme, tpnme}
import ast.{Trees, tpd, untpd}
import typer.{Implicits, Inliner, Namer}
import typer.ProtoTypes._
import Trees._
import TypeApplications._
import Decorators._
import config.Config
import util.Positions._
import dotty.tools.dotc.transform.SymUtils._
import dotty.tools.dotc.transform.FirstTransform

import scala.annotation.switch
import language.implicitConversions
import dotty.tools.dotc.util.SourcePosition
import Highlighting._
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

  protected def enclDefIsClass = enclosingDef match {
    case owner: TypeDef => owner.isClassDef
    case owner: untpd.ModuleDef => true
    case _ => false
  }

  override protected def recursionLimitExceeded() = {}

  protected val PrintableFlags = (SourceModifierFlags | Label | Module | Local).toCommonFlags

  override def nameString(name: Name): String =
    if (ctx.settings.YdebugNames.value) name.debugString else name.toString

  override protected def simpleNameString(sym: Symbol): String =
    nameString(if (ctx.property(XprintMode).isEmpty) sym.originalName else sym.name)

  override def fullNameString(sym: Symbol): String =
    if (isEmptyPrefix(sym.maybeOwner)) nameString(sym)
    else super.fullNameString(sym)

  override protected def fullNameOwner(sym: Symbol) = {
    val owner = super.fullNameOwner(sym)
    if (owner is ModuleClass) owner.sourceModule else owner
  }

  override def toTextRef(tp: SingletonType): Text = controlled {
    tp match {
      case tp: ThisType =>
        if (tp.cls.isAnonymousClass) return keywordStr("this")
        if (tp.cls is ModuleClass) return fullNameString(tp.cls.sourceModule)
      case _ =>
    }
    super.toTextRef(tp)
  }

  override def toTextPrefix(tp: Type): Text = controlled {
    def isOmittable(sym: Symbol) =
      if (ctx.settings.verbose.value) false
      else if (homogenizedView) isEmptyPrefix(sym) // drop <root> and anonymous classes, but not scala, Predef.
      else isOmittablePrefix(sym)
    tp match {
      case tp: ThisType =>
        if (isOmittable(tp.cls)) return ""
      case tp @ TermRef(pre, _) =>
        val sym = tp.symbol
        if (sym.isPackageObject) return toTextPrefix(pre)
        if (isOmittable(sym)) return ""
      case _ =>
    }
    super.toTextPrefix(tp)
  }

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
      "(" ~ Text(args.map(argText), ", ") ~ ")"

    def toTextFunction(args: List[Type], isImplicit: Boolean, isErased: Boolean): Text =
      changePrec(GlobalPrec) {
        val argStr: Text =
          if (args.length == 2 && !defn.isTupleType(args.head))
            atPrec(InfixPrec) { argText(args.head) }
          else
            toTextTuple(args.init)
        (keywordText("erased ") provided isErased) ~ (keywordText("implicit ") provided isImplicit) ~ argStr ~ " => " ~ argText(args.last)
      }

    def toTextDependentFunction(appType: MethodType): Text = {
      (keywordText("implicit ") provided appType.isImplicitMethod) ~
      "(" ~ paramsText(appType) ~ ") => " ~ toText(appType.resultType)
    }

    def isInfixType(tp: Type): Boolean = tp match {
      case AppliedType(tycon, args) =>
        args.length == 2 &&
        tycon.typeSymbol.getAnnotation(defn.ShowAsInfixAnnot).map(_.argumentConstant(0).forall(_.booleanValue))
          .getOrElse(!Character.isUnicodeIdentifierStart(tycon.typeSymbol.name.toString.head))
      case _ => false
    }

    def toTextInfixType(op: Type, args: List[Type]): Text = {
      /* SLS 3.2.8: all infix types have the same precedence.
       * In A op B op' C, op and op' need the same associativity.
       * Therefore, if op is left associative, anything on its right
       * needs to be parenthesized if it's an infix type, and vice versa. */
      val l :: r :: Nil = args
      val isRightAssoc = op.typeSymbol.name.endsWith(":")
      val leftArg = if (isRightAssoc && isInfixType(l)) "(" ~ argText(l) ~ ")" else argText(l)
      val rightArg = if (!isRightAssoc && isInfixType(r)) "(" ~ argText(r) ~ ")" else argText(r)

      leftArg ~ " " ~ toTextLocal(op) ~ " " ~ rightArg
    }

    homogenize(tp) match {
      case AppliedType(tycon, args) =>
        val cls = tycon.typeSymbol
        if (tycon.isRepeatedParam) return toTextLocal(args.head) ~ "*"
        if (defn.isFunctionClass(cls)) return toTextFunction(args, cls.name.isImplicitFunction, cls.name.isErasedFunction)
        if (defn.isTupleClass(cls)) return toTextTuple(args)
        if (isInfixType(tp)) return toTextInfixType(tycon, args)
      case EtaExpansion(tycon) =>
        return toText(tycon)
      case tp: RefinedType if defn.isFunctionType(tp) =>
        return toTextDependentFunction(tp.refinedInfo.asInstanceOf[MethodType])
      case tp: TypeRef =>
        if (tp.symbol.isAnonymousClass && !ctx.settings.uniqid.value)
          return toText(tp.info)
        if (tp.symbol.is(Param))
          tp.prefix match {
            case pre: ThisType if pre.cls == tp.symbol.owner =>
              return nameString(tp.symbol)
            case _ =>
          }
      case tp: ExprType =>
        return exprToText(tp)
      case ErasedValueType(tycon, underlying) =>
        return "ErasedValueType(" ~ toText(tycon) ~ ", " ~ toText(underlying) ~ ")"
      case tp: ClassInfo =>
        return toTextParents(tp.parents) ~ "{...}"
      case JavaArrayType(elemtp) =>
        return toText(elemtp) ~ "[]"
      case tp: AnnotatedType if homogenizedView =>
        // Positions of annotations in types are not serialized
        // (they don't need to because we keep the original type tree with
        //  the original annotation anyway. Therefore, there will always be
        //  one version of the annotation tree that has the correct positions).
        withoutPos(super.toText(tp))
      case tp: SelectionProto =>
        return "?{ " ~ toText(tp.name) ~
           (Str(" ") provided !tp.name.toSimpleName.last.isLetterOrDigit) ~
           ": " ~ toText(tp.memberProto) ~ " }"
      case tp: ViewProto =>
        return toText(tp.argType) ~ " ?=>? " ~ toText(tp.resultType)
      case tp @ FunProto(args, resultType, _) =>
        val argsText = args match {
          case dummyTreeOfType(tp) :: Nil if !(tp isRef defn.NullClass) => "null: " ~ toText(tp)
          case _ => toTextGlobal(args, ", ")
        }
        return "FunProto(" ~ argsText ~ "):" ~ toText(resultType)
      case tp: IgnoredProto =>
        return "?"
      case tp @ PolyProto(targs, resType) =>
        return "PolyProto(" ~ toTextGlobal(targs, ", ") ~ "): " ~ toText(resType)
      case _ =>
    }
    super.toText(tp)
  }

  protected def exprToText(tp: ExprType): Text =
    "=> " ~ toText(tp.resType)

  protected def blockToText[T >: Untyped](block: Block[T]): Text =
    blockText(block.stats :+ block.expr)

  protected def blockText[T >: Untyped](trees: List[Tree[T]]): Text =
    ("{" ~ toText(trees, "\n") ~ "}").close

  protected def typeApplyText[T >: Untyped](tree: TypeApply[T]): Text =
    toTextLocal(tree.fun) ~ "[" ~ toTextGlobal(tree.args, ", ") ~ "]"

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

    tree match {
      case id: Trees.BackquotedIdent[_] if !homogenizedView =>
        "`" ~ toText(id.name) ~ "`"
      case id: Trees.SearchFailureIdent[_] =>
        tree.typeOpt match {
          case reason: Implicits.SearchFailureType =>
            toText(id.name) ~ "implicitly[" ~ toText(reason.expectedType) ~ "]"
          case _ =>
            toText(id.name)
        }
      case Ident(name) =>
        val txt = tree.typeOpt match {
          case tp: NamedType if name != nme.WILDCARD =>
            val pre = if (tp.symbol is JavaStatic) tp.prefix.widen else tp.prefix
            toTextPrefix(pre) ~ withPos(selectionString(tp), tree.pos)
          case _ =>
            toText(name)
        }
        if (name.isTypeName) typeText(txt)
        else txt
      case tree @ Select(qual, name) =>
        if (qual.isType) toTextLocal(qual) ~ "#" ~ typeText(toText(name))
        else toTextLocal(qual) ~ ("." ~ nameIdText(tree) provided name != nme.CONSTRUCTOR)
      case tree: This =>
        optDotPrefix(tree) ~ keywordStr("this") ~ idText(tree)
      case Super(qual: This, mix) =>
        optDotPrefix(qual) ~ keywordStr("super") ~ optText(mix)("[" ~ _ ~ "]")
      case Apply(fun, args) =>
        if (fun.hasType && fun.symbol == defn.throwMethod)
          changePrec (GlobalPrec) {
            keywordStr("throw ") ~ toText(args.head)
          }
        else
          toTextLocal(fun) ~ "(" ~ toTextGlobal(args, ", ") ~ ")"
      case tree: TypeApply =>
        typeApplyText(tree)
      case Literal(c) =>
        tree.typeOpt match {
          case ConstantType(tc) => withPos(toText(tc), tree.pos)
          case _ => withPos(toText(c), tree.pos)
        }
      case New(tpt) =>
        keywordStr("new ") ~ {
          tpt match {
            case tpt: Template => toTextTemplate(tpt, ofNew = true)
            case _ =>
              if (tpt.hasType)
                toTextLocal(tpt.typeOpt.underlyingClassRef(refinementOK = false))
              else
                toTextLocal(tpt)
          }
        }
      case Typed(expr, tpt) =>
        changePrec(InfixPrec) { toText(expr) ~ ": " ~ toText(tpt) }
      case NamedArg(name, arg) =>
        toText(name) ~ " = " ~ toText(arg)
      case Assign(lhs, rhs) =>
        changePrec(GlobalPrec) { toTextLocal(lhs) ~ " = " ~ toText(rhs) }
      case block: Block =>
        blockToText(block)
      case If(cond, thenp, elsep) =>
        changePrec(GlobalPrec) {
          keywordStr("if ") ~ toText(cond) ~ (keywordText(" then") provided !cond.isInstanceOf[Parens]) ~~ toText(thenp) ~ optText(elsep)(keywordStr(" else ") ~ _)
        }
      case Closure(env, ref, target) =>
        "closure(" ~ (toTextGlobal(env, ", ") ~ " | " provided env.nonEmpty) ~
        toTextGlobal(ref) ~ (":" ~ toText(target) provided !target.isEmpty) ~ ")"
      case Match(sel, cases) =>
        if (sel.isEmpty) blockText(cases)
        else changePrec(GlobalPrec) { toText(sel) ~ keywordStr(" match ") ~ blockText(cases) }
      case CaseDef(pat, guard, body) =>
        keywordStr("case ") ~ inPattern(toText(pat)) ~ optText(guard)(keywordStr(" if ") ~ _) ~ " => " ~ caseBlockText(body)
      case Return(expr, from) =>
        changePrec(GlobalPrec) { keywordStr("return") ~ optText(expr)(" " ~ _) }
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
        (("/* inlined from " ~ toText(call) ~ "*/ ") provided !homogenizedView && !ctx.settings.YshowNoInline.value) ~
        blockText(bindings :+ body)
      case tpt: untpd.DerivedTypeTree =>
        "<derived typetree watching " ~ summarized(toText(tpt.watched)) ~ ">"
      case TypeTree() =>
        typeText(toText(tree.typeOpt))
      case SingletonTypeTree(ref) =>
        toTextLocal(ref) ~ "." ~ keywordStr("type")
      case AndTypeTree(l, r) =>
        changePrec(AndPrec) { toText(l) ~ " & " ~ toText(r) }
      case OrTypeTree(l, r) =>
        changePrec(OrPrec) { toText(l) ~ " | " ~ toText(r) }
      case RefinedTypeTree(tpt, refines) =>
        toTextLocal(tpt) ~ " " ~ blockText(refines)
      case AppliedTypeTree(tpt, args) =>
        toTextLocal(tpt) ~ "[" ~ Text(args map argText, ", ") ~ "]"
      case LambdaTypeTree(tparams, body) =>
        changePrec(GlobalPrec) {
          tparamsText(tparams) ~ " -> " ~ toText(body)
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
          case Select(extractor, nme.unapply) => extractor
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
            modText(tree.mods, tree.symbol, keywordStr("type")) ~~ (varianceText(tree.mods) ~ typeText(nameIdText(tree))) ~
            withEnclosingDef(tree) { tparamsText ~ rhsText }
          }
        def recur(rhs: Tree, tparamsTxt: => Text): Text = rhs match {
          case impl: Template =>
            templateText(tree, impl)
          case rhs: TypeBoundsTree =>
            typeDefText(tparamsTxt, toText(rhs))
          case LambdaTypeTree(tparams, body) =>
            recur(body, tparamsText(tparams))
          case rhs: TypeTree if rhs.typeOpt.isInstanceOf[TypeBounds] =>
            typeDefText(tparamsTxt, toText(rhs))
          case rhs =>
            typeDefText(tparamsTxt, optText(rhs)(" = " ~ _))
        }
        recur(rhs, "")
      case Import(expr, selectors) =>
        def selectorText(sel: Tree): Text = sel match {
          case Thicket(l :: r :: Nil) => toTextGlobal(l) ~ " => " ~ toTextGlobal(r)
          case _ => toTextGlobal(sel)
        }
        val selectorsText: Text = selectors match {
          case id :: Nil => toText(id)
          case _ => "{" ~ Text(selectors map selectorText, ", ") ~ "}"
        }
        keywordStr("import ") ~ toTextLocal(expr) ~ "." ~ selectorsText
      case packageDef: PackageDef =>
        packageDefText(packageDef)
      case tree: Template =>
        toTextTemplate(tree)
      case Annotated(arg, annot) =>
        toTextLocal(arg) ~~ annotText(annot)
      case EmptyTree =>
        "<empty>"
      case TypedSplice(t) =>
        if (ctx.settings.YprintDebug.value) "[" ~ toText(t) ~ "]#TS#"
        else toText(t)
      case tpd.UntypedSplice(t) =>
        if (ctx.settings.YprintDebug.value) "[" ~ toText(t) ~ ":" ~ toText(tree.typeOpt) ~ "]#US#"
        else toText(t)
      case tree @ ModuleDef(name, impl) =>
        withEnclosingDef(tree) {
          modText(tree.mods, NoSymbol, keywordStr("object")) ~~ nameIdText(tree) ~ toTextTemplate(impl)
        }
      case SymbolLit(str) =>
        "'" + str
      case InterpolatedString(id, segments) =>
        def strText(str: Literal) = withPos(escapedString(str.const.stringValue), tree.pos)
        def segmentText(segment: Tree) = segment match {
          case Thicket(List(str: Literal, expr)) => strText(str) ~ "{" ~ toTextGlobal(expr) ~ "}"
          case str: Literal => strText(str)
        }
        toText(id) ~ "\"" ~ Text(segments map segmentText, "") ~ "\""
      case Function(args, body) =>
        var implicitSeen: Boolean = false
        def argToText(arg: Tree) = arg match {
          case arg @ ValDef(name, tpt, _) =>
            val implicitText =
              if ((arg.mods is Implicit) && !implicitSeen) { implicitSeen = true; keywordStr("implicit ") }
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
          argsText ~ " => " ~ toText(body)
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
      case WhileDo(cond, body) =>
        changePrec(GlobalPrec) { keywordStr("while ") ~ toText(cond) ~ keywordStr(" do ") ~ toText(body) }
      case DoWhile(cond, body) =>
        changePrec(GlobalPrec) { keywordStr("do ") ~ toText(body) ~ keywordStr(" while ") ~ toText(cond) }
      case ForYield(enums, expr) =>
        forText(enums, expr, keywordStr(" yield "))
      case ForDo(enums, expr) =>
        forText(enums, expr, keywordStr(" do "))
      case GenFrom(pat, expr) =>
        toText(pat) ~ " <- " ~ toText(expr)
      case GenAlias(pat, expr) =>
        toText(pat) ~ " = " ~ toText(expr)
      case ContextBounds(bounds, cxBounds) =>
        (toText(bounds) /: cxBounds) {(t, cxb) =>
          t ~ cxBoundToText(cxb)
        }
      case PatDef(mods, pats, tpt, rhs) =>
        modText(mods, NoSymbol, keywordStr("val")) ~~ toText(pats, ", ") ~ optAscription(tpt) ~
          optText(rhs)(" = " ~ _)
      case ParsedTry(expr, handler, finalizer) =>
        changePrec(GlobalPrec) {
          keywordStr("try ") ~ toText(expr) ~ " " ~ keywordStr("catch") ~ " {" ~ toText(handler) ~ "}" ~ optText(finalizer)(keywordStr(" finally ") ~ _)
        }
      case Quote(tree) =>
        if (tree.isType) "'[" ~ toTextGlobal(tree) ~ "]" else "'(" ~ toTextGlobal(tree) ~ ")"
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
      def tp = tree.typeOpt match {
        case tp: TermRef if tree.isInstanceOf[RefTree] && !tp.denot.isOverloaded => tp.underlying
        case tp: ConstantType if homogenizedView =>
          // constant folded types are forgotten in Tasty, are reconstituted subsequently in FirstTransform.
          // Therefore we have to gloss over this when comparing before/after pickling by widening to
          // underlying type `T`, or, if expression is a unary primitive operation, to `=> T`.
          tree match {
            case Select(qual, _) if qual.typeOpt.widen.typeSymbol.isPrimitiveValueClass =>
              ExprType(tp.widen)
            case _ => tp.widen
          }
        case tp => tp
      }
      if (!suppressTypes)
        txt = ("<" ~ txt ~ ":" ~ toText(tp) ~ ">").close
      else if (tree.isType && !homogenizedView)
        txt = toText(tp)
    }
    if (!suppressPositions) {
      if (printPos) {
        val pos =
          if (homogenizedView && !tree.isInstanceOf[MemberDef]) tree.pos.toSynthetic
          else tree.pos
        val clsStr = ""//if (tree.isType) tree.getClass.toString else ""
        txt = (txt ~ "@" ~ pos.toString ~ clsStr).close
      }
      if (ctx.settings.YprintPosSyms.value && tree.isDef)
        txt = (txt ~
          s"@@(${tree.symbol.name}=" ~ tree.symbol.pos.toString ~ ")").close
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
    sym.annotations map (_.tree))

  protected def optAscription[T >: Untyped](tpt: Tree[T]) = optText(tpt)(": " ~ _)

  private def idText(tree: untpd.Tree): Text = {
    if ((ctx.settings.uniqid.value || Printer.debugPrintUnique) && tree.hasType && tree.symbol.exists) s"#${tree.symbol.id}" else ""
  }

  private def useSymbol(tree: untpd.Tree) =
    tree.hasType && tree.symbol.exists && ctx.settings.YprintSyms.value

  protected def nameIdText[T >: Untyped](tree: NameTree[T]): Text = {
    if (tree.hasType && tree.symbol.exists) {
      val str: Text = nameString(tree.symbol)
      tree match {
        case tree: RefTree => withPos(str, tree.pos)
        case tree: MemberDef => withPos(str, tree.namePos)
        case _ => str
      }
    }
    else toText(tree.name) ~ idText(tree)
  }

  private def toTextOwner(tree: Tree[_]) =
    "[owner = " ~ tree.symbol.maybeOwner.show ~ "]" provided ctx.settings.YprintDebugOwners.value

  protected def dclTextOr[T >: Untyped](tree: Tree[T])(treeText: => Text) =
    toTextOwner(tree) ~ {
      if (useSymbol(tree)) annotsText(tree.symbol) ~~ dclText(tree.symbol)
      else treeText
    }

  def tparamsText[T >: Untyped](params: List[Tree[T]]): Text =
    "[" ~ toText(params, ", ") ~ "]" provided params.nonEmpty

  def addVparamssText[T >: Untyped](txt: Text, vparamss: List[List[ValDef[T]]]): Text =
    (txt /: vparamss)((txt, vparams) => txt ~ "(" ~ toText(vparams, ", ") ~ ")")

  protected def valDefToText[T >: Untyped](tree: ValDef[T]): Text = {
    import untpd.{modsDeco => _, _}
    dclTextOr(tree) {
      modText(tree.mods, tree.symbol, keywordStr(if (tree.mods is Mutable) "var" else "val")) ~~
        valDefText(nameIdText(tree)) ~ optAscription(tree.tpt) ~
        withEnclosingDef(tree) { optText(tree.rhs)(" = " ~ _) }
    }
  }

  protected def defDefToText[T >: Untyped](tree: DefDef[T]): Text = {
    import untpd.{modsDeco => _, _}
    dclTextOr(tree) {
      val prefix = modText(tree.mods, tree.symbol, keywordStr("def")) ~~ valDefText(nameIdText(tree))
      withEnclosingDef(tree) {
          addVparamssText(prefix ~ tparamsText(tree.tparams), tree.vparamss) ~ optAscription(tree.tpt) ~
            optText(tree.rhs)(" = " ~ _)
      }
    }
  }

  protected def toTextTemplate(impl: Template, ofNew: Boolean = false): Text = {
    val Template(constr @ DefDef(_, tparams, vparamss, _, _), parents, self, _) = impl
    val tparamsTxt = withEnclosingDef(constr) { tparamsText(tparams) }
    val primaryConstrs = if (constr.rhs.isEmpty) Nil else constr :: Nil
    val prefix: Text =
      if (vparamss.isEmpty || primaryConstrs.nonEmpty) tparamsTxt
      else {
        var modsText = modText(constr.mods, constr.symbol, "")
        if (!modsText.isEmpty) modsText = " " ~ modsText
        if (constr.mods.hasAnnotations && !constr.mods.hasFlags) modsText = modsText ~~ " this"
        withEnclosingDef(constr) { addVparamssText(tparamsTxt ~~ modsText, vparamss) }
      }
    val parentsText = Text(parents map constrText, keywordStr(" with "))
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

    prefix ~ keywordText(" extends").provided(!ofNew && parents.nonEmpty) ~~ parentsText ~ bodyText
  }

  protected def templateText(tree: TypeDef, impl: Template): Text = {
    val decl = modText(tree.mods, tree.symbol, keywordStr(if ((tree).mods is Trait) "trait" else "class"))
    decl ~~ typeText(nameIdText(tree)) ~ withEnclosingDef(tree) { toTextTemplate(impl) } ~
      (if (tree.hasType && ctx.settings.verbose.value) i"[decls = ${tree.symbol.info.decls}]" else "")
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

  protected def modText(mods: untpd.Modifiers, sym: Symbol, kw: String, suppress: FlagSet = EmptyFlags): Text = { // DD
    val suppressKw = if (enclDefIsClass) mods is ParamAndLocal else mods is Param
    var flagMask =
      if (ctx.settings.YdebugFlags.value) AnyFlags
      else if (suppressKw) PrintableFlags &~ Private &~ suppress
      else PrintableFlags &~ suppress
    if (homogenizedView && mods.flags.isTypeFlags) flagMask &~= Implicit // drop implicit from classes
    val flags = (if (sym.exists) sym.flags else (mods.flags)) & flagMask
    val flagsText = if (flags.isEmpty) "" else keywordStr(flags.toString)
    val annotations = filterModTextAnnots(
      if (sym.exists) sym.annotations.filterNot(_.isInstanceOf[Annotations.BodyAnnotation]).map(_.tree)
      else mods.annotations)
    Text(annotations.map(annotText), " ") ~~ flagsText ~~ (Str(kw) provided !suppressKw)
  }

  protected def filterModTextAnnots(annots: List[untpd.Tree]): List[untpd.Tree] = annots

  def optText(name: Name)(encl: Text => Text): Text =
    if (name.isEmpty) "" else encl(toText(name))

  def optText[T >: Untyped](tree: Tree[T])(encl: Text => Text): Text =
    if (tree.isEmpty) "" else encl(toText(tree))

  def optText[T >: Untyped](tree: List[Tree[T]])(encl: Text => Text): Text =
    if (tree.exists(!_.isEmpty)) encl(blockText(tree)) else ""

  override protected def ParamRefNameString(name: Name): String =
    name.invariantName.toString

  override protected def treatAsTypeParam(sym: Symbol): Boolean = sym is TypeParam

  override protected def treatAsTypeArg(sym: Symbol) =
    sym.isType && (sym is ProtectedLocal) &&
      (sym.allOverriddenSymbols exists (_ is TypeParam))

  override def toText(sym: Symbol): Text = {
    if (sym.isImport)
      sym.infoOrCompleter match {
        case info: Namer#Completer => return info.original.show
        case info: ImportType => return s"import $info.expr.show"
        case _ =>
      }
    if (sym.is(ModuleClass)) {
      val name =
        if (sym.isPackageObject) sym.owner.name
        else sym.name.stripModuleClassSuffix
      kindString(sym) ~~ (nameString(name) + idString(sym))
    }
    else
      super.toText(sym)
  }

  override def kindString(sym: Symbol) = {
    val flags = sym.flagsUNSAFE
    if (flags is Package) "package"
    else if (sym.isPackageObject) "package object"
    else if (flags is Module) "object"
    else if (flags is ImplClass) "class"
    else if (sym.isClassConstructor) "constructor"
    else super.kindString(sym)
  }

  override protected def keyString(sym: Symbol): String = {
    val flags = sym.flagsUNSAFE
    if (sym.isType && sym.owner.isTerm) ""
    else if (sym.isPackageObject) "package object"
    else if (flags.is(Module) && flags.is(Case)) "case object"
    else if (sym.isClass && flags.is(Case)) "case class"
    else if (flags is Module) "object"
    else if (sym.isTerm && !flags.is(Param) && flags.is(Implicit)) "implicit val"
    else if (sym.isTerm && !flags.is(Param) && flags.is(Erased)) "erased val"
    else super.keyString(sym)
  }

  override def toTextFlags(sym: Symbol) =
    if (ctx.settings.YdebugFlags.value)
      super.toTextFlags(sym)
    else {
      var flags = sym.flagsUNSAFE
      if (flags is TypeParam) flags = flags &~ Protected
      Text((flags & PrintableFlags).flagStrings map (flag => stringToText(keywordStr(flag))), " ")
    }

  override def toText(denot: Denotation): Text = denot match {
    case denot: MultiDenotation => Text(denot.alternatives.map(dclText), " <and> ")
    case NoDenotation => "NoDenotation"
    case _ =>
      if (denot.symbol.exists) toText(denot.symbol)
      else "some " ~ toText(denot.info)
  }

  override def plain = new PlainPrinter(_ctx)

  private def withPos(txt: Text, pos: SourcePosition): Text = {
    if (!printLines || !pos.exists) txt
    else txt match {
      case Str(s, _) => Str(s, LineRange(pos.line, pos.endLine))
      case _ => txt
    }
  }
}
