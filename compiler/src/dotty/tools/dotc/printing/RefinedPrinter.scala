package dotty.tools.dotc
package printing

import core._
import Texts._, Types._, Flags._, Names._, Symbols._, NameOps._, Constants._
import TypeErasure.ErasedValueType
import Contexts.Context, Scopes.Scope, Denotations._, SymDenotations._, Annotations.Annotation
import StdNames.{nme, tpnme}
import ast.{Trees, untpd, tpd}
import typer.{Namer, Inliner}
import typer.ProtoTypes.{SelectionProto, ViewProto, FunProto, IgnoredProto, dummyTreeOfType}
import Trees._
import TypeApplications._
import Decorators._
import config.Config
import transform.SymUtils._
import scala.annotation.switch
import language.implicitConversions

class RefinedPrinter(_ctx: Context) extends PlainPrinter(_ctx) {

  /** A stack of enclosing DefDef, TypeDef, or ClassDef, or ModuleDefs nodes */
  private var enclosingDef: untpd.Tree = untpd.EmptyTree
  private var myCtx: Context = _ctx
  private var printPos = ctx.settings.Yprintpos.value
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

  private def enclDefIsClass = enclosingDef match {
    case owner: TypeDef[_] => owner.isClassDef
    case owner: untpd.ModuleDef => true
    case _ => false
  }

  override protected def recursionLimitExceeded() = {}

  protected val PrintableFlags = (SourceModifierFlags | Label | Module | Local).toCommonFlags

  override def nameString(name: Name): String =
    if (ctx.settings.debugNames.value) name.debugString else name.toString

  override protected def simpleNameString(sym: Symbol): String = {
    val name = if (ctx.property(XprintMode).isEmpty) sym.originalName else sym.name
    nameString(if (sym.is(TypeParam)) name.asTypeName.unexpandedName else name)
  }

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
        if (tp.cls.isAnonymousClass) return "this"
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
    def toTextFunction(args: List[Type], isImplicit: Boolean): Text =
      changePrec(GlobalPrec) {
        val argStr: Text =
          if (args.length == 2 && !defn.isTupleType(args.head))
            atPrec(InfixPrec) { argText(args.head) }
          else
            toTextTuple(args.init)
        ("implicit " provided isImplicit) ~ argStr ~ " => " ~ argText(args.last)
      }
    homogenize(tp) match {
      case x: ConstantType if homogenizedView =>
        return toText(x.widen)
      case AppliedType(tycon, args) =>
        val cls = tycon.typeSymbol
        if (tycon.isRepeatedParam) return toTextLocal(args.head) ~ "*"
        if (defn.isFunctionClass(cls)) return toTextFunction(args, cls.name.isImplicitFunction)
        if (defn.isTupleClass(cls)) return toTextTuple(args)
        return (toTextLocal(tycon) ~ "[" ~ Text(args map argText, ", ") ~ "]").close
      case tp: TypeRef =>
        val hideType = !ctx.settings.debugAlias.value && (tp.symbol.isAliasPreferred)
        if (hideType && !ctx.phase.erasedTypes && !tp.symbol.isCompleting) {
          tp.info match {
            case TypeAlias(alias) => return toText(alias)
            case _ => if (tp.prefix.isInstanceOf[ThisType]) return nameString(tp.symbol)
          }
        }
        else if (tp.symbol.isAnonymousClass && !ctx.settings.uniqid.value)
          return toText(tp.info)
      case ExprType(result) =>
        return "=> " ~ toText(result)
      case ErasedValueType(tycon, underlying) =>
        return "ErasedValueType(" ~ toText(tycon) ~ ", " ~ toText(underlying) ~ ")"
      case tp: ClassInfo =>
        return toTextParents(tp.parentsWithArgs) ~ "{...}"
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
           (" " provided !tp.name.toSimpleName.last.isLetterOrDigit) ~
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
      case _ =>
    }
    super.toText(tp)
  }

  def blockText[T >: Untyped](trees: List[Tree[T]]): Text =
    ("{" ~ toText(trees, "\n") ~ "}").close

  override def toText[T >: Untyped](tree: Tree[T]): Text = controlled {

    import untpd.{modsDeco => _, _}

    /** Print modifiers from symbols if tree has type, overriding the untpd behavior. */
    implicit def modsDeco(mdef: untpd.MemberDef)(implicit ctx: Context): untpd.ModsDecorator =
      new untpd.ModsDecorator {
        def mods = if (mdef.hasType) Modifiers(mdef.symbol) else mdef.rawMods
      }

    def Modifiers(sym: Symbol)(implicit ctx: Context): Modifiers = untpd.Modifiers(
      sym.flags & (if (sym.isType) ModifierFlags | VarianceFlags else ModifierFlags),
      if (sym.privateWithin.exists) sym.privateWithin.asType.name else tpnme.EMPTY,
      sym.annotations map (_.tree))

    def isLocalThis(tree: Tree) = tree.typeOpt match {
      case tp: ThisType => tp.cls == ctx.owner.enclosingClass
      case _ => false
    }

    def optDotPrefix(tree: This) = optText(tree.qual)(_ ~ ".") provided !isLocalThis(tree)

    def optAscription(tpt: untpd.Tree) = optText(tpt)(": " ~ _)
      // Dotty deviation: called with an untpd.Tree, so cannot be a untpd.Tree[T] (seems to be a Scala2 problem to allow this)
      // More deviations marked below as // DD

    def tparamsText[T >: Untyped](params: List[Tree]): Text =
      "[" ~ toText(params, ", ") ~ "]" provided params.nonEmpty

    def addVparamssText(txt: Text, vparamss: List[List[ValDef]]): Text =
      (txt /: vparamss)((txt, vparams) => txt ~ "(" ~ toText(vparams, ", ") ~ ")")

    def caseBlockText(tree: Tree): Text = tree match {
      case Block(stats, expr) => toText(stats :+ expr, "\n")
      case expr => toText(expr)
    }

    def enumText(tree: untpd.Tree) = tree match { // DD
      case _: untpd.GenFrom | _: untpd.GenAlias => toText(tree)
      case _ => "if " ~ toText(tree)
    }

    def forText(enums: List[untpd.Tree], expr: untpd.Tree, sep: String): Text = // DD
      changePrec(GlobalPrec) { "for " ~ Text(enums map enumText, "; ") ~ sep ~ toText(expr) }

    def cxBoundToText(bound: untpd.Tree): Text = bound match { // DD
      case AppliedTypeTree(tpt, _) => " : " ~ toText(tpt)
      case untpd.Function(_, tpt) => " <% " ~ toText(tpt)
    }

    def constrText(tree: untpd.Tree): Text = toTextLocal(tree).stripPrefix("new ") // DD

    def annotText(tree: untpd.Tree): Text = "@" ~ constrText(tree) // DD

    def useSymbol =
      tree.hasType && tree.symbol.exists && ctx.settings.YprintSyms.value

    def modText(mods: untpd.Modifiers, kw: String): Text = { // DD
      val suppressKw = if (enclDefIsClass) mods is ParamAndLocal else mods is Param
      var flagMask =
        if (ctx.settings.debugFlags.value) AnyFlags
        else if (suppressKw) PrintableFlags &~ Private
        else PrintableFlags
      if (homogenizedView && mods.flags.isTypeFlags) flagMask &~= Implicit // drop implicit from classes
      val flagsText = (mods.flags & flagMask).toString
      Text(mods.annotations.map(annotText), " ") ~~ flagsText ~~ (kw provided !suppressKw)
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

    def dclTextOr(treeText: => Text) =
      if (useSymbol)
        annotsText(tree.symbol) ~~ dclText(tree.symbol) ~
        ( " <in " ~ toText(tree.symbol.owner) ~ ">" provided ctx.settings.debugOwners.value)
      else treeText

    def idText(tree: untpd.Tree): Text = {
      if (ctx.settings.uniqid.value && tree.hasType && tree.symbol.exists) s"#${tree.symbol.id}" else ""
    }

    def nameIdText(tree: untpd.NameTree): Text =
      if (tree.hasType && tree.symbol.exists) nameString(tree.symbol)
      else toText(tree.name) ~ idText(tree)

    def toTextTemplate(impl: Template, ofNew: Boolean = false): Text = {
      val Template(constr @ DefDef(_, tparams, vparamss, _, _), parents, self, _) = impl
      val tparamsTxt = withEnclosingDef(constr) { tparamsText(tparams) }
      val primaryConstrs = if (constr.rhs.isEmpty) Nil else constr :: Nil
      val prefix: Text =
        if (vparamss.isEmpty || primaryConstrs.nonEmpty) tparamsTxt
        else {
          var modsText = modText(constr.mods, "")
          if (!modsText.isEmpty) modsText = " " ~ modsText
          if (constr.mods.hasAnnotations && !constr.mods.hasFlags) modsText = modsText ~~ " this"
          withEnclosingDef(constr) { addVparamssText(tparamsTxt ~~ modsText, vparamss) }
        }
      val parentsText = Text(parents map constrText, " with ")
      val selfText = {
        val selfName = if (self.name == nme.WILDCARD) "this" else self.name.toString
        (selfName ~ optText(self.tpt)(": " ~ _) ~ " =>").close
      } provided !self.isEmpty

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

      val bodyText = "{" ~~ selfText ~~ toTextGlobal(primaryConstrs ::: body, "\n") ~ "}"

      prefix ~ (" extends" provided !ofNew) ~~ parentsText ~~ bodyText
    }

    def toTextPackageId(pid: Tree): Text =
      if (homogenizedView && pid.hasType) toTextLocal(pid.tpe)
      else toTextLocal(pid)

    def toTextCore(tree: Tree): Text = tree match {
      case id: Trees.BackquotedIdent[_] if !homogenizedView =>
        "`" ~ toText(id.name) ~ "`"
      case Ident(name) =>
        tree.typeOpt match {
          case tp: NamedType if name != nme.WILDCARD =>
            val pre = if (tp.symbol is JavaStatic) tp.prefix.widen else tp.prefix
            toTextPrefix(pre) ~ selectionString(tp)
          case _ =>
            toText(name)
        }
      case tree @ Select(qual, name) =>
        if (qual.isType) toTextLocal(qual) ~ "#" ~ toText(name)
        else toTextLocal(qual) ~ ("." ~ nameIdText(tree) provided name != nme.CONSTRUCTOR)
      case tree: This =>
        optDotPrefix(tree) ~ "this" ~ idText(tree)
      case Super(qual: This, mix) =>
        optDotPrefix(qual) ~ "super" ~ optText(mix)("[" ~ _ ~ "]")
      case Apply(fun, args) =>
        if (fun.hasType && fun.symbol == defn.throwMethod)
          changePrec (GlobalPrec) {
            "throw " ~ toText(args.head)
          }
        else
          toTextLocal(fun) ~ "(" ~ toTextGlobal(args, ", ") ~ ")"
      case TypeApply(fun, args) =>
        toTextLocal(fun) ~ "[" ~ toTextGlobal(args, ", ") ~ "]"
      case Literal(c) =>
        tree.typeOpt match {
          case ConstantType(tc) => toText(tc)
          case _ => toText(c)
        }
      case New(tpt) =>
        "new " ~ {
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
      case Block(stats, expr) =>
        blockText(stats :+ expr)
      case If(cond, thenp, elsep) =>
        changePrec(GlobalPrec) {
          "if " ~ toText(cond) ~ (" then" provided !cond.isInstanceOf[Parens]) ~~ toText(thenp) ~ optText(elsep)(" else " ~ _)
        }
      case Closure(env, ref, target) =>
        "closure(" ~ (toTextGlobal(env, ", ") ~ " | " provided env.nonEmpty) ~
        toTextGlobal(ref) ~ (":" ~ toText(target) provided !target.isEmpty) ~ ")"
      case Match(sel, cases) =>
        if (sel.isEmpty) blockText(cases)
        else changePrec(GlobalPrec) { toText(sel) ~ " match " ~ blockText(cases) }
      case CaseDef(pat, guard, body) =>
        "case " ~ inPattern(toText(pat)) ~ optText(guard)(" if " ~ _) ~ " => " ~ caseBlockText(body)
      case Return(expr, from) =>
        changePrec(GlobalPrec) { "return" ~ optText(expr)(" " ~ _) }
      case Try(expr, cases, finalizer) =>
        changePrec(GlobalPrec) {
          "try " ~ toText(expr) ~ optText(cases)(" catch " ~ _) ~ optText(finalizer)(" finally " ~ _)
        }
      case Throw(expr) =>
        changePrec(GlobalPrec) {
          "throw " ~ toText(expr)
        }
      case SeqLiteral(elems, elemtpt) =>
        "[" ~ toTextGlobal(elems, ",") ~ " : " ~ toText(elemtpt) ~ "]"
      case tree @ Inlined(call, bindings, body) =>
        (("/* inlined from " ~ toText(call) ~ "*/ ") provided !homogenizedView) ~
        blockText(bindings :+ body)
      case tpt: untpd.DerivedTypeTree =>
        "<derived typetree watching " ~ summarized(toText(tpt.watched)) ~ ">"
      case TypeTree() =>
        toText(tree.typeOpt)
      case SingletonTypeTree(ref) =>
        toTextLocal(ref) ~ ".type"
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
        optText(lo)(" >: " ~ _) ~ optText(hi)(" <: " ~ _)
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
      case tree @ ValDef(name, tpt, _) =>
        dclTextOr {
          modText(tree.mods, if (tree.mods is Mutable) "var" else "val") ~~
          nameIdText(tree) ~ optAscription(tpt) ~
          withEnclosingDef(tree) { optText(tree.rhs)(" = " ~ _) }
        }
      case tree @ DefDef(name, tparams, vparamss, tpt, _) =>
        dclTextOr {
          val prefix = modText(tree.mods, "def") ~~ nameIdText(tree)
          withEnclosingDef(tree) {
            addVparamssText(prefix ~ tparamsText(tparams), vparamss) ~ optAscription(tpt) ~
            optText(tree.rhs)(" = " ~ _)
          }
        }
      case tree @ TypeDef(name, rhs) =>
        def typeDefText(tparamsText: => Text, rhsText: => Text) =
          dclTextOr {
            modText(tree.mods, "type") ~~ (varianceText(tree.mods) ~ nameIdText(tree)) ~
            withEnclosingDef(tree) {
              if (tree.hasType) toText(tree.symbol.info) // TODO: always print RHS, once we pickle/unpickle type trees
              else tparamsText ~ rhsText
            }
          }
        def recur(rhs: Tree, tparamsTxt: => Text): Text = rhs match {
          case impl: Template =>
            modText(tree.mods, if ((tree).mods is Trait) "trait" else "class") ~~
            nameIdText(tree) ~ withEnclosingDef(tree) { toTextTemplate(impl) } ~
            (if (tree.hasType && ctx.settings.verbose.value) i"[decls = ${tree.symbol.info.decls}]" else "")
          case rhs: TypeBoundsTree =>
            typeDefText(tparamsTxt, toText(rhs))
          case LambdaTypeTree(tparams, body) =>
            recur(body, tparamsText(tparams))
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
        "import " ~ toTextLocal(expr) ~ "." ~ selectorsText
      case PackageDef(pid, stats) =>
        val statsText = stats match {
          case (pdef: PackageDef) :: Nil => toText(pdef)
          case _ => toTextGlobal(stats, "\n")
        }
        val bodyText =
          if (currentPrecedence == TopLevelPrec) "\n" ~ statsText else " {" ~ statsText ~ "}"
        "package " ~ toTextPackageId(pid) ~ bodyText
      case tree: Template =>
        toTextTemplate(tree)
      case Annotated(arg, annot) =>
        toTextLocal(arg) ~~ annotText(annot)
      case EmptyTree =>
        "<empty>"
      case TypedSplice(t) =>
        toText(t)
      case tree @ ModuleDef(name, impl) =>
        withEnclosingDef(tree) {
          modText(tree.mods, "object") ~~ nameIdText(tree) ~ toTextTemplate(impl)
        }
      case SymbolLit(str) =>
        "'" + str
      case InterpolatedString(id, segments) =>
        def strText(str: Literal) = Str(escapedString(str.const.stringValue))
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
              if ((arg.mods is Implicit) && !implicitSeen) { implicitSeen = true; "implicit " }
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
        changePrec(GlobalPrec) { "while " ~ toText(cond) ~ " do " ~ toText(body) }
      case DoWhile(cond, body) =>
        changePrec(GlobalPrec) { "do " ~ toText(body) ~ " while " ~ toText(cond) }
      case ForYield(enums, expr) =>
        forText(enums, expr, " yield ")
      case ForDo(enums, expr) =>
        forText(enums, expr, " do ")
      case GenFrom(pat, expr) =>
        toText(pat) ~ " <- " ~ toText(expr)
      case GenAlias(pat, expr) =>
        toText(pat) ~ " = " ~ toText(expr)
      case ContextBounds(bounds, cxBounds) =>
        (toText(bounds) /: cxBounds) {(t, cxb) =>
          t ~ cxBoundToText(cxb)
        }
      case PatDef(mods, pats, tpt, rhs) =>
        modText(mods, "val") ~~ toText(pats, ", ") ~ optAscription(tpt) ~
          optText(rhs)(" = " ~ _)
      case ParsedTry(expr, handler, finalizer) =>
        changePrec(GlobalPrec) {
          "try " ~ toText(expr) ~ " catch {" ~ toText(handler) ~ "}" ~ optText(finalizer)(" finally " ~ _)
        }
      case Thicket(trees) =>
        "Thicket {" ~~ toTextGlobal(trees, "\n") ~~ "}"
      case _ =>
        tree.fallbackToText(this)
    }

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

    if (ctx.settings.printtypes.value && tree.hasType) {
      // add type to term nodes; replace type nodes with their types unless -Yprintpos is also set.
      def tp = tree.typeOpt match {
        case tp: TermRef if tree.isInstanceOf[RefTree] && !tp.denot.isOverloaded => tp.underlying
        case tp => tp
      }
      if (!suppressTypes)
        txt = ("<" ~ txt ~ ":" ~ toText(tp) ~ ">").close
      else if (tree.isType && !homogenizedView)
        txt = toText(tp)
    }
    if (printPos && !suppressPositions) {
      // add positions
      val pos =
        if (homogenizedView && !tree.isInstanceOf[MemberDef]) tree.pos.toSynthetic
        else tree.pos
      val clsStr = ""//if (tree.isType) tree.getClass.toString else ""
      txt = (txt ~ "@" ~ pos.toString ~ clsStr).close
    }
    tree match {
      case Block(_, _) | Template(_, _, _, _) => txt
      case _ => txt.close
    }
  }

  def optText(name: Name)(encl: Text => Text): Text =
    if (name.isEmpty) "" else encl(toText(name))

  def optText[T >: Untyped](tree: Tree[T])(encl: Text => Text): Text =
    if (tree.isEmpty) "" else encl(toText(tree))

  def optText[T >: Untyped](tree: List[Tree[T]])(encl: Text => Text): Text =
    if (tree.exists(!_.isEmpty)) encl(blockText(tree)) else ""

  override protected def ParamRefNameString(name: Name): String =
    name.unexpandedName.toString

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
    if (sym.is(ModuleClass))
      kindString(sym) ~~ (nameString(sym.name.stripModuleClassSuffix) + idString(sym))
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
    else super.keyString(sym)
  }

  override def toTextFlags(sym: Symbol) =
    if (ctx.settings.debugFlags.value)
      super.toTextFlags(sym)
    else {
      var flags = sym.flagsUNSAFE
      if (flags is TypeParam) flags = flags &~ Protected
      Text((flags & PrintableFlags).flagStrings map stringToText, " ")
    }

  override def toText(denot: Denotation): Text = denot match {
    case denot: MultiDenotation => Text(denot.alternatives.map(dclText), " <and> ")
    case NoDenotation => "NoDenotation"
    case _ =>
      if (denot.symbol.exists) toText(denot.symbol)
      else "some " ~ toText(denot.info)
  }

  override def plain = new PlainPrinter(_ctx)
}
