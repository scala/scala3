package dotty.tools.dotc
package printing

import core._
import Texts._, Types._, Flags._, Names._, Symbols._, NameOps._, Constants._
import Contexts.Context, Scopes.Scope, Denotations.Denotation, Annotations.Annotation
import StdNames.nme
import ast.Trees._
import ast.untpd
import scala.annotation.switch

class RefinedPrinter(_ctx: Context) extends PlainPrinter(_ctx) {
  override protected def recursionLimitExceeeded() = {}

  /** The closest enclosing DefDef, TypeDef, or ClassDef node */
  private var currentOwner: Tree[_ >: Untyped] = EmptyTree()

  def atOwner(owner: Tree[_ >: Untyped])(op: => Text): Text = {
    val saved = owner
    currentOwner = owner
    try op
    finally { currentOwner = saved }
  }

  override def nameString(name: Name): String = name.toString

  override protected def simpleNameString(sym: Symbol): String = {
    var name = sym.originalName
    if (sym is ModuleClass) name = name.stripModuleClassSuffix
    name.decode.toString
  }

  override def toTextPrefix(tp: Type): Text = controlled {
    tp match {
      case ThisType(cls) =>
        if (cls.isAnonymousClass) return "this."
        if (isOmittablePrefix(cls)) return ""
        if (cls is ModuleClass) return fullNameString(cls) + "."
      case tp @ TermRef(pre, name) =>
        val sym = tp.symbol
        if (sym.isPackageObject) return toTextPrefix(pre)
        if (isOmittablePrefix(sym)) return ""
      case _ =>
    }
    super.toTextPrefix(tp)
  }

  override protected def refinementNameString(tp: RefinedType): String = {
    val tsym = tp.member(tp.refinedName).symbol
    val name = tsym.originalName
    nameString(if (tsym is ExpandedTypeParam) name.asTypeName.unexpandedName() else name)
  }

  override def toText(tp: Type): Text = controlled {
    def toTextTuple(args: List[Type]): Text =
      "(" ~ toTextGlobal(args, ", ") ~ ")"
    def toTextFunction(args: List[Type]): Text =
      changePrec(GlobalPrec) {
        val argStr: Text =
          if (args.length == 2 && !(defn.TupleClasses contains args.head.typeSymbol))
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
            if (cls == defn.RepeatedParamClass) return toTextLocal(args.head) ~ "*"
            if (cls == defn.ByNameParamClass) return "=> " ~ toText(args.head)
            if (defn.FunctionClasses contains cls) return toTextFunction(args)
            if (defn.TupleClasses contains cls) return toTextTuple(args)
          }
          return (toTextLocal(tycon) ~ "[" ~ toTextGlobal(args, ", ") ~ "]").close
        }
      case tp @ TypeRef(pre, name) =>
        if (tp.symbol is TypeParam) return nameString(tp.symbol)
      case _ =>
    }
    super.toText(tp)
  }

  override def toText[T >: Untyped](tree: Tree[T]): Text = {

    def optDotPrefix(name: Name) = optText(name)(_ ~ ".")

    def tparamsText(params: List[Tree[T]]): Text =
      "[" ~ toText(params, ", ") ~ "]" provided params.nonEmpty

    def addVparamssText[T >: Untyped](txt: Text, vparamss: List[List[ValDef[T]]]): Text =
      (txt /: vparamss)((txt, vparams) => txt ~ "(" ~ toText(vparams, ", ") ~ ")")

    def blockText[T >: Untyped](trees: List[Tree[T]]): Text =
      "{" ~ toText(trees, "\n") ~ "}"

    def caseBlockText[T >: Untyped](tree: Tree[T]): Text = tree match {
      case Block(stats, expr) => toText(stats :+ expr, "\n")
      case expr => toText(expr)
    }

    def forText(enums: List[Tree[T]], expr: Tree[T], sep: String): Text =
      changePrec(GlobalPrec) { "for " ~ toText(enums, "; ") ~ sep ~ toText(expr) }

    def cxBoundToText(bound: Tree[T]): Text = bound match {
      case AppliedTypeTree(tpt, _) => " : " ~ toText(tpt)
      case untpd.Function(_, tpt) => " <% " ~ toText(tpt)
    }

    val txt: Text = tree match {
      case id: BackquotedIdent[_] =>
        "`" ~ toText(id.name) ~ "`"
      case Ident(name) =>
        toText(name)
      case Select(qual, name) =>
        toTextLocal(qual) ~ "." ~ toText(name)
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
      case Typed(l, tpt) =>
        changePrec(InfixPrec) { toText(l) ~ ": " ~ toText(tpt) }
      case NamedArg(name, arg) =>
        toText(name) ~ " = " ~ toText(arg)
      case Assign(lhs, rhs) =>
        changePrec(GlobalPrec) { toTextLocal(lhs) ~ " = " ~ toText(rhs) }
      case Block(stats, expr) =>
        blockText(stats :+ expr)
      case If(cond, thenp, elsep) =>
        changePrec(GlobalPrec) {
          "if " ~ toText(cond) ~ (" then" provided !cond.isInstanceOf[untpd.Parens]) ~~ toText(thenp) ~ optText(elsep)(" else " ~ _)
        }
      case Match(sel, cases) =>
        if (sel.isEmpty) blockText(cases)
        else changePrec(GlobalPrec) { toText(sel) ~ " match " ~ blockText(cases) }
      case CaseDef(pat, guard, body) =>
        "case " ~ toText(pat) ~ optText(guard)("if " ~ _) ~ " => " ~ caseBlockText(body)
      case Return(expr, from) =>
        changePrec(GlobalPrec) { "return " ~ toText(expr) }
      case Try(expr, handler, finalizer) =>
        changePrec(GlobalPrec) {
          "try " ~ toText(expr) ~ optText(handler)(" catch " ~ _) ~ optText(finalizer)(" finally " ~ _)
        }
      case Throw(expr) =>
        changePrec(GlobalPrec) {
          "throw " ~ toText(expr)
        }
      case SeqLiteral(elempt, elems) =>
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
        toTextLocal(tpt) ~ "(" ~ toTextGlobal(args, ", ") ~ ")"
      case TypeBoundsTree(lo, hi) =>
        optText(lo)(" >: " ~ _) ~ optText(hi)(" <: " ~ _)
      case Bind(name, body) =>
        changePrec(InfixPrec) { toText(name) ~ " @ " ~ toText(body) }
      case Alternative(trees) =>
        changePrec(OrPrec) { toText(trees, " | ") }
      case UnApply(fun, args) =>
        toTextLocal(fun) ~ "(" ~ toTextGlobal(args, ", ") ~ ")"
      case ValDef(mods, name, tpt, rhs) =>
        toText(mods, if (mods is Mutable) "var" else "val") ~~ toText(name) ~
          optText(tpt)(": " ~ _) ~ optText(rhs)(" = " ~ _)
      case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
        atOwner(tree) {
          val first = toText(mods, "def") ~~ toText(name) ~ tparamsText(tparams)
          addVparamssText(first, vparamss) ~ optText(tpt)(": " ~ _) ~ optText(rhs)(" = " ~ _)
        }
      case TypeDef(mods, name, tparams, rhs) =>
        atOwner(tree) {
          val rhsText = rhs match {
            case TypeBoundsTree(_, _) => toText(rhs)
            case _ => optText(rhs)(" = " ~ _)
          }
          toText(mods, "type") ~~ toText(name) ~ tparamsText(tparams) ~ rhsText
        }
      case Template(DefDef(mods, _, _, vparamss, _, _), parents, self, stats) =>
        val prefix: Text =
          if (vparamss.isEmpty) ""
          else {
            var modsText = toText(mods, "")
            if (mods.hasAnnotations && !mods.hasFlags) modsText = modsText ~~ " this"
            addVparamssText(modsText, vparamss) ~ " extends"
          }
        val parentsText = toTextLocal(parents, "with")
        val selfText = optText(self)(" " ~ _ ~ " => ")
        val bodyText: Text = stats match {
          case Nil =>
            assert(self.isEmpty)
            ""
          case EmptyTree() :: Nil =>
            "{" ~ selfText ~ "}"
          case stats =>
            "{" ~ selfText ~ toTextGlobal(stats, "\n") ~ "}"
        }
        prefix ~~ parentsText ~~ bodyText
      case ClassDef(mods, name, tparams, impl) =>
        atOwner(tree) {
          toText(mods, if (mods is Trait) "trait" else "class") ~~
            toText(name) ~ tparamsText(tparams) ~ toText(impl)
        }
      case Import(expr, selectors) =>
        def selectorText(sel: UntypedTree): Text = sel match {
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
          case (pdef: PackageDef[_]) :: Nil => toText(pdef)
          case _ => toTextGlobal(stats, "\n")
        }
        val bodyText =
          if (currentPrecedence == TopLevelPrec) "\n" ~ statsText else " {" ~ statsText ~ "}"
        "package " ~ toTextLocal(pid) ~ bodyText
      case Annotated(annot, arg) =>
        toTextLocal(arg) ~ " @" ~ toTextLocal(annot)
      case EmptyTree() =>
        "<empty>"
      case SharedTree(shared) =>
        toText(shared)
      case untpd.TypedSplice(t) =>
        toText(t)
      case untpd.ModuleDef(mods, name, impl) =>
        toText(mods, "object") ~~ toText(name) ~ " extends " ~ toText(impl)
      case untpd.SymbolLit(str) =>
        "'" + str
      case untpd.InterpolatedString(id, strings, elems) =>
        def interleave(strs: List[Text], elems: List[Text]): Text = ((strs, elems): @unchecked) match {
          case (Nil, Nil) => ""
          case (str :: Nil, Nil) => str
          case (str :: strs1, elem :: elems1) => str ~ elem ~ interleave(strs1, elems1)
        }
        val strTexts = strings map (str => Str(escapedString(str.const.stringValue)))
        val elemsTexts = elems map (elem => "{" ~ toTextGlobal(elem) ~ "}")
        toText(id) ~ "\"" ~ interleave(strTexts, elemsTexts) ~ "\""
      case untpd.Function(args, body) =>
        var implicitSeen: Boolean = false
        def argToText(arg: untpd.Tree) = arg match {
          case ValDef(mods, name, tpt, _) =>
            val implicitText =
              if ((mods is Implicit) && !implicitSeen) { implicitSeen = true; "implicit " }
              else ""
            implicitText ~ toText(name) ~ optText(tpt)(": " ~ _)
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
      case untpd.InfixOp(l, op, r) =>
        val opPrec = parsing.precedence(op)
        changePrec(opPrec) { toText(l) ~ " " ~ toText(op) ~ " " ~ toText(r) }
      case untpd.PostfixOp(l, op) =>
        changePrec(InfixPrec) { toText(l) ~ " " ~ toText(op) }
      case untpd.PrefixOp(op, r) =>
        changePrec(DotPrec) { toText(op) ~ " " ~ toText(r) }
      case untpd.Parens(t) =>
        "(" ~ toTextGlobal(t) ~ ")"
      case untpd.Tuple(ts) =>
        "(" ~ toTextGlobal(ts, ", ") ~ ")"
      case untpd.WhileDo(cond, body) =>
        changePrec(GlobalPrec) { "while " ~ toText(cond) ~ " do " ~ toText(body) }
      case untpd.DoWhile(cond, body) =>
        changePrec(GlobalPrec) { "do " ~ toText(body) ~ " while " ~ toText(cond) }
      case untpd.ForYield(enums, expr) =>
        forText(enums, expr, " yield ")
      case untpd.ForDo(enums, expr) =>
        forText(enums, expr, " do ")
      case untpd.GenFrom(pat, expr) =>
        toText(pat) ~ " <- " ~ toText(expr)
      case untpd.GenAlias(pat, expr) =>
        toText(pat) ~ " = " ~ toText(expr)
      case untpd.ContextBounds(bounds, cxBounds) =>
        (toText(bounds) /: cxBounds) {(t, cxb) =>
          t ~ cxBoundToText(cxb)
        }
      case untpd.PatDef(mods, pats, tpt, rhs) =>
        toText(mods, "val") ~~ toText(pats, ", ") ~ optText(tpt)(": " ~ _) ~
          optText(rhs)(" = " ~ _)
      case _ =>
        tree.fallbackToText(this)
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

  def toText[T >: Untyped](mods: Modifiers[T], kw: String): Text = {
    def annotText(annot: Tree[T]) = "@" ~ toTextLocal(annot)
    def suppressKw = currentOwner match {
      case _: ClassDef[_] => mods is ParamAndLocal
      case _ => mods is Param
    }
    val flagMask = if (suppressKw) ModifierFlags &~ Private else ModifierFlags
    val modsText: Text = (mods.flags & flagMask).toString
    Text(mods.annotations.map(annotText), " ") ~~ modsText ~~ (kw provided !suppressKw)
  }

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
}
