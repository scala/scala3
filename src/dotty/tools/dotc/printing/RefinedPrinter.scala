package dotty.tools.dotc
package printing

import core._
import Texts._, Types._, Flags._, Names._, Symbols._, NameOps._, Constants._
import Contexts.Context, Scopes.Scope, Denotations.Denotation, Annotations.Annotation
import StdNames.nme
import ast.{Trees, untpd}
import Trees._
import scala.annotation.switch

class RefinedPrinter(_ctx: Context) extends PlainPrinter(_ctx) {

  override protected def recursionLimitExceeeded() = {}

  protected val PrintableFlags = (ModifierFlags | Label | Module).toCommonFlags

  /** The closest enclosing DefDef, TypeDef, or ClassDef node */
  private var currentOwner: untpd.Tree = untpd.EmptyTree

  def atOwner(owner: Tree[_ >: Untyped])(op: => Text): Text = {
    val saved = currentOwner
    currentOwner = owner
    try op
    finally { currentOwner = saved }
  }

  private def ownerIsClass = currentOwner match {
    case owner: TypeDef[_] => owner.isClassDef
    case owner: untpd.ModuleDef => true
    case _ => false
  }

  override def nameString(name: Name): String = name.decode.toString

  override protected def simpleNameString(sym: Symbol): String = {
    var name = sym.originalName
    if (sym is ModuleClass) name = name.stripModuleClassSuffix
    name.decode.toString
  }

  override def toTextRef(tp: SingletonType): Text = controlled {
    tp match {
      case ThisType(cls) =>
        if (cls.isAnonymousClass) return "this"
        if (cls is ModuleClass) return fullNameString(cls)
      case _ =>
    }
    super.toTextRef(tp)
  }

  override def toTextPrefix(tp: Type): Text = controlled {
    tp match {
      case ThisType(cls) =>
        if (isOmittablePrefix(cls)) return ""
      case tp @ TermRef(pre, name) =>
        val sym = tp.symbol
        if (sym.isPackageObject) return toTextPrefix(pre)
        if (isOmittablePrefix(sym)) return ""
      case _ =>
    }
    super.toTextPrefix(tp)
  }

  override protected def refinementNameString(tp: RefinedType): String =
    if (tp.parent.isInstanceOf[WildcardType] || tp.refinedName == nme.WILDCARD)
      super.refinementNameString(tp)
    else {
      val tsym = tp.member(tp.refinedName).symbol
      if (!tsym.exists) super.refinementNameString(tp)
      else {
        val name = tsym.originalName
        nameString(if (tsym is ExpandedTypeParam) name.asTypeName.unexpandedName() else name)
      }
    }

  override def toText(tp: Type): Text = controlled {
    def toTextTuple(args: List[Type]): Text =
      "(" ~ toTextGlobal(args, ", ") ~ ")"
    def toTextFunction(args: List[Type]): Text =
      changePrec(GlobalPrec) {
        val argStr: Text =
          if (args.length == 2 && !defn.isTupleType(args.head))
            atPrec(InfixPrec) { toText(args.head) }
          else
            toTextTuple(args.init)
        argStr ~ " => " ~ toText(args.last)
      }
    tp match {
      case tp: RefinedType =>
        val args = tp.typeArgs
        if (args.nonEmpty) {
          val tycon = tp.unrefine
          val cls = tycon.typeSymbol
          if (cls.typeParams.length == args.length) {
            if (tycon.isRepeatedParam) return toTextLocal(args.head) ~ "*"
            if (cls == defn.ByNameParamClass) return "=> " ~ toText(args.head)
            if (defn.FunctionClasses contains cls) return toTextFunction(args)
            if (defn.TupleClasses contains cls) return toTextTuple(args)
          }
          return (toTextLocal(tycon) ~ "[" ~ toTextGlobal(args, ", ") ~ "]").close
        }
      case tp @ TypeRef(pre, name) =>
        if (tp.symbol is TypeParam | TypeArgument) {
          return tp.info match {
            case TypeAlias(hi) => toText(hi)
            case _ => nameString(tp.symbol)
          }
        }
      case typer.Inferencing.FunProto(args, resultType, _) =>
        "funproto(" ~ toTextGlobal(args, ", ") ~ "):" ~ toText(resultType)
      case _ =>
    }
    super.toText(tp)
  }

  override def toText[T >: Untyped](tree: Tree[T]): Text = {

    def optDotPrefix(name: Name) = optText(name)(_ ~ ".")

    def optAscription(tpt: Tree[T]) = optText(tpt)(": " ~ _)

    def tparamsText[T >: Untyped](params: List[Tree[T]]): Text =
      "[" ~ toText(params, ", ") ~ "]" provided params.nonEmpty

    def addVparamssText(txt: Text, vparamss: List[List[ValDef[T]]]): Text =
      (txt /: vparamss)((txt, vparams) => txt ~ "(" ~ toText(vparams, ", ") ~ ")")

    def blockText(trees: List[Tree[T]]): Text =
      "{" ~ toText(trees, "\n") ~ "}"

    def caseBlockText(tree: Tree[T]): Text = tree match {
      case Block(stats, expr) => toText(stats :+ expr, "\n")
      case expr => toText(expr)
    }

    def forText(enums: List[Tree[T]], expr: Tree[T], sep: String): Text =
      changePrec(GlobalPrec) { "for " ~ toText(enums, "; ") ~ sep ~ toText(expr) }

    def cxBoundToText(bound: Tree[T]): Text = bound match {
      case AppliedTypeTree(tpt, _) => " : " ~ toText(tpt)
      case untpd.Function(_, tpt) => " <% " ~ toText(tpt)
    }

    def constrText(tree: Tree[T]): Text = toTextLocal(tree).stripPrefix("new ")

    def annotText(tree: Tree[T]): Text = "@" ~ constrText(tree)

    def modText(mods: Modifiers[T], kw: String): Text = {
      val suppressKw = if (ownerIsClass) mods is ParamAndLocal else mods is Param
      val flagMask = if (suppressKw) PrintableFlags &~ Private else PrintableFlags
      val modsText: Text = (mods.flags & flagMask).toString
      Text(mods.annotations.map(annotText), " ") ~~ modsText ~~ (kw provided !suppressKw)
    }

    import untpd._

    var txt: Text = tree match {
      case id: Trees.BackquotedIdent[_] =>
        "`" ~ toText(id.name) ~ "`"
      case Ident(name) =>
        tree.typeOpt match {
          case tp: NamedType if name != nme.WILDCARD => toTextPrefix(tp.prefix) ~ selectionString(tp)
          case _ => toText(name)
        }
      case Select(qual, name) =>
        toTextLocal(qual) ~ ("." ~ toText(name) provided name != nme.CONSTRUCTOR)
      case This(name) =>
        optDotPrefix(name) ~ "this"
      case Super(This(name), mix) =>
        optDotPrefix(name) ~ "super" ~ optText(mix)("[" ~ _ ~ "]")
      case Apply(fun, args) =>
        toTextLocal(fun) ~ "(" ~ toTextGlobal(args, ", ") ~ ")"
      case TypeApply(fun, args) =>
        toTextLocal(fun) ~ "[" ~ toTextGlobal(args, ", ") ~ "]"
      case Literal(c) =>
        toText(c)
      case New(tpt) =>
        "new " ~ toTextLocal(tpt)
      case Pair(l, r) =>
        "(" ~ toTextGlobal(l) ~ ", " ~ toTextGlobal(r) ~ ")"
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
      case Closure(env, ref, _) =>
        "closure(" ~ (toTextGlobal(env, ", ") ~ " | " provided env.nonEmpty) ~
        toTextGlobal(ref) ~ ")"
      case Match(sel, cases) =>
        if (sel.isEmpty) blockText(cases)
        else changePrec(GlobalPrec) { toText(sel) ~ " match " ~ blockText(cases) }
      case CaseDef(pat, guard, body) =>
        "case " ~ toText(pat) ~ optText(guard)("if " ~ _) ~ " => " ~ caseBlockText(body)
      case Return(expr, from) =>
        changePrec(GlobalPrec) { "return " ~ optText(expr)(" " ~ _) }
      case Try(expr, handler, finalizer) =>
        changePrec(GlobalPrec) {
          "try " ~ toText(expr) ~ optText(handler)(" catch " ~ _) ~ optText(finalizer)(" finally " ~ _)
        }
      case Throw(expr) =>
        changePrec(GlobalPrec) {
          "throw " ~ toText(expr)
        }
      case SeqLiteral(elems) =>
        "[" ~ toTextGlobal(elems, ",") ~ "]"
      case TypeTree(orig) =>
        if (tree.hasType) toText(tree.typeOpt) else toText(orig)
      case SingletonTypeTree(ref) =>
        toTextLocal(ref) ~ ".type"
      case SelectFromTypeTree(qual, name) =>
        toTextLocal(qual) ~ "#" ~ toText(name)
      case AndTypeTree(l, r) =>
        changePrec(AndPrec) { toText(l) ~ " & " ~ toText(r) }
      case OrTypeTree(l, r) =>
        changePrec(OrPrec) { toText(l) ~ " | " ~ toText(r) }
      case RefinedTypeTree(tpt, refines) =>
        toTextLocal(tpt) ~ " " ~ blockText(refines)
      case AppliedTypeTree(tpt, args) =>
        toTextLocal(tpt) ~ "[" ~ toTextGlobal(args, ", ") ~ "]"
      case TypeBoundsTree(lo, hi) =>
        optText(lo)(" >: " ~ _) ~ optText(hi)(" <: " ~ _)
      case Bind(name, body) =>
        changePrec(InfixPrec) { toText(name) ~ " @ " ~ toText(body) }
      case Alternative(trees) =>
        changePrec(OrPrec) { toText(trees, " | ") }
      case UnApply(fun, args) =>
        toTextLocal(fun) ~ "(" ~ toTextGlobal(args, ", ") ~ ")"
      case ValDef(mods, name, tpt, rhs) =>
        modText(mods, if (mods is Mutable) "var" else "val") ~~ toText(name) ~
          optAscription(tpt) ~ optText(rhs)(" = " ~ _)
      case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
        atOwner(tree) {
          val first = modText(mods, "def") ~~ toText(name) ~ tparamsText(tparams)
          addVparamssText(first, vparamss) ~ optAscription(tpt) ~ optText(rhs)(" = " ~ _)
        }
      case tree @ TypeDef(mods, name, rhs) =>
        atOwner(tree) {
          def typeDefText(rhsText: Text) =
            modText(mods, "type") ~~ toText(name) ~ tparamsText(tree.tparams) ~ rhsText
          rhs match {
            case impl: Template =>
              modText(mods, if (mods is Trait) "trait" else "class") ~~ toText(name) ~ toText(impl)
            case rhs: TypeBoundsTree =>
              typeDefText(toText(rhs))
            case _ =>
              typeDefText(optText(rhs)(" = " ~ _))
          }
        }
      case Template(DefDef(mods, _, tparams, vparamss, _, _), parents, self, stats) =>
        val tparamsTxt = tparamsText(tparams)
        val prefix: Text =
          if (vparamss.isEmpty) tparamsTxt
          else {
            var modsText = modText(mods, "")
            if (mods.hasAnnotations && !mods.hasFlags) modsText = modsText ~~ " this"
            addVparamssText(tparamsTxt ~~ modsText, vparamss)
          }
        val parentsText = Text(parents map constrText, " with ")
        val selfText = {
          val selfName = if (self.name == nme.WILDCARD) "this" else self.name.toString
          (selfName ~ optText(self.tpt)(": " ~ _) ~ " =>").close
        } provided !self.isEmpty
        val bodyText = "{" ~~ selfText ~~ toTextGlobal(stats, "\n") ~ "}"
        prefix ~~ (" extends" provided ownerIsClass) ~~ parentsText ~~ bodyText
      case Import(expr, selectors) =>
        def selectorText(sel: Tree): Text = sel match {
          case Pair(l, r) => toTextGlobal(l) ~ " => " ~ toTextGlobal(r)
          case _ => toTextGlobal(sel)
        }
        val selectorsText: Text = selectors match {
          case Ident(name) :: Nil => toText(name)
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
        "package " ~ toTextLocal(pid) ~ bodyText
      case Annotated(annot, arg) =>
        toTextLocal(arg) ~~ annotText(annot)
      case EmptyTree =>
        "<empty>"
      case SharedTree(shared) =>
        toText(shared)
      case TypedSplice(t) =>
        toText(t)
      case ModuleDef(mods, name, impl) =>
        atOwner(tree) {
          modText(mods, "object") ~~ toText(name) ~ toText(impl)
        }
      case SymbolLit(str) =>
        "'" + str
      case InterpolatedString(id, strings, elems) =>
        def interleave(strs: List[Text], elems: List[Text]): Text = ((strs, elems): @unchecked) match {
          case (Nil, Nil) => ""
          case (str :: Nil, Nil) => str
          case (str :: strs1, elem :: elems1) => str ~ elem ~ interleave(strs1, elems1)
        }
        val strTexts = strings map (str => Str(escapedString(str.const.stringValue)))
        val elemsTexts = elems map (elem => "{" ~ toTextGlobal(elem) ~ "}")
        toText(id) ~ "\"" ~ interleave(strTexts, elemsTexts) ~ "\""
      case Function(args, body) =>
        var implicitSeen: Boolean = false
        def argToText(arg: Tree) = arg match {
          case ValDef(mods, name, tpt, _) =>
            val implicitText =
              if ((mods is Implicit) && !implicitSeen) { implicitSeen = true; "implicit " }
              else ""
            implicitText ~ toText(name) ~ optAscription(tpt)
          case _ =>
            toText(arg)
        }
        val argsText = args match {
          case (arg @ ValDef(_, _, tpt, _)) :: Nil if tpt.isEmpty => argToText(arg)
          case _ => "(" ~ Text(args map argToText, ", ") ~ ")"
        }
        changePrec(GlobalPrec) {
          argsText ~ " => " ~ toText(body)
        }
      case InfixOp(l, op, r) =>
        val opPrec = parsing.precedence(op)
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
      case Thicket(trees) =>
        "Thicket {" ~~ toTextGlobal(trees, "\n") ~~ "}"
      case _ =>
        tree.fallbackToText(this)
    }
    if (ctx.settings.printtypes.value && tree.hasType) {
      val tp = tree.typeOpt match {
        case tp: TermRef if tree.isInstanceOf[RefTree] && !tp.denot.isOverloaded => tp.underlying
        case tp => tp
      }
      if (tree.isType) txt = toText(tp)
      else if (!tree.isDef) txt = "<" ~ txt ~ ":" ~ toText(tp) ~ ">"
    }
    if (ctx.settings.Yprintpos.value && !tree.isInstanceOf[WithoutTypeOrPos[_]])
      txt = txt ~ "@" ~ tree.pos.toString
    tree match {
      case Block(_, _) | Template(_, _, _, _) => txt
      case _ => txt.close
    }
  }

  def optText(name: Name)(encl: Text => Text): Text =
    if (name.isEmpty) "" else encl(toText(name))

  def optText[T >: Untyped](tree: Tree[T])(encl: Text => Text): Text =
    if (tree.isEmpty) "" else encl(toText(tree))

  override protected def polyParamName(name: TypeName): TypeName =
    name.unexpandedName()

  override protected def treatAsTypeParam(sym: Symbol): Boolean = sym is TypeParam

  override protected def treatAsTypeArg(sym: Symbol) =
    sym.isType && (sym is ProtectedLocal) &&
      (sym.allOverriddenSymbols exists (_ is TypeParam))

  override protected def reconstituteParent(cls: ClassSymbol, parent: Type): Type =
    (parent /: parent.classSymbol.typeParams) { (parent, tparam) =>
      val targSym = cls.decls.lookup(tparam.name)
      if (targSym.exists) RefinedType(parent, targSym.name, targSym.info)
      else parent
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
    if (sym.isType && (flags is ExpandedTypeParam)) ""
    else super.keyString(sym)
  }

  override def toTextFlags(sym: Symbol) = {
    var flags = sym.flags
    if (flags is TypeParam) flags = flags &~ Protected
    Text(flags.flagStrings.filterNot(_.startsWith("<")) map stringToText, " ")
  }

  override def toText(denot: Denotation): Text = toText(denot.symbol)

  override def plain = new PlainPrinter(_ctx)
}
