package scala.tasty
package util

import scala.tasty.constants._
import scala.tasty.names._
import scala.tasty.modifiers._
import scala.tasty.trees._
import scala.tasty.types._

object TastyPrinter {

  def stringOf(tree: Tree)(implicit ctx: Context): String = {
    implicit val buff: StringBuilder = new StringBuilder
    visit(tree)
    buff.toString()
  }

  def stringOf(tpe: MaybeType)(implicit ctx: Context): String = {
    implicit val buff: StringBuilder = new StringBuilder
    visit(tpe)
    buff.toString()
  }

  def stringOf(name: Name)(implicit ctx: Context): String = {
    implicit val buff: StringBuilder = new StringBuilder
    visit(name)
    buff.toString()
  }

  def stringOf(name: PossiblySignedName)(implicit ctx: Context): String = {
    implicit val buff: StringBuilder = new StringBuilder
    visit(name)
    buff.toString()
  }

  def stringOf(mod: Modifier)(implicit ctx: Context): String = {
    implicit val buff: StringBuilder = new StringBuilder
    visit(mod)
    buff.toString()
  }

  def stringOf(mod: Constant)(implicit ctx: Context): String = {
    implicit val buff: StringBuilder = new StringBuilder
    visit(mod)
    buff.toString()
  }

  private def visit(tree: Tree)(implicit buff: StringBuilder, ctx: Context): Unit = tree match {
    case Ident(name) =>
      buff append "Ident("
      visit(name: Name)
      buff append ")"
    case Select(qualifier, name) =>
      buff append "Select("
      visit(qualifier)
      buff append ", "
      visit(name)
      buff append ")"
    case This(qual) =>
      buff append "This(" append qual append ")"
    case Super(qual, mix) =>
      buff append "TypeApply("
      visit(qual)
      buff append ", "
      buff append mix
      buff append ")"
    case Apply(fun, args) =>
      buff append "Apply("
      visit(fun)
      buff append ", "
      visitAny(args)
      buff append ")"
    case TypeApply(fun, args) =>
      buff append "TypeApply("
      visit(fun)
      buff append ", "
      visitAny(args)
      buff append ")"
    case Literal(const) =>
      buff append "Literal("
      visit(const)
      buff append ")"
    case New(tpt) =>
      buff append "New("
      visit(tpt)
      buff append ")"
    case Typed(expr, tpt) =>
      buff append "Typed("
      visit(expr)
      buff append ", "
      visit(tpt)
      buff append ")"
    case NamedArg(name, arg) =>
      buff append "NamedArg("
      visit(name: Name)
      buff append ", "
      visit(arg)
      buff append ")"
    case Assign(lhs, rhs) =>
      buff append "Assign("
      visit(lhs)
      buff append ", "
      visit(rhs)
      buff append ")"
    case Block(stats, expr) =>
      buff append "Block("
      visitAny(stats)
      buff append ", "
      visit(expr)
      buff append ")"
    case If(cond, thenp, elsep) =>
      buff append "If("
      visit(cond)
      buff append ", "
      visit(thenp)
      buff append ", "
      visit(elsep)
      buff append ")"
    case Lambda(meth, tpt) =>
      buff append "Lambda("
      visit(meth)
      buff append ", "
      visitAny(tpt)
      buff append ")"
    case Match(selector, cases) =>
      buff append "Match("
      visit(selector)
      buff append ", "
      visitAny(cases)
      buff append ")"
    case CaseDef(pat, guard, body) =>
      buff append "CaseDef("
      visit(pat)
      buff append ", "
      visitAny(guard)
      buff append ", "
      visit(body)
      buff append ")"
    case Return(expr) =>
      buff append "Return("
      visit(expr)
      buff append ")"
    case Try(block, handlers, finalizer) =>
      buff append "Try("
      visit(block)
      buff append ", "
      visitAny(handlers)
      buff append ", "
      visitAny(finalizer)
      buff append ")"
    case Repeated(elems) =>
      buff append "Repeated("
      visitAny(elems)
      buff append ")"
    case Inlined(call, bindings, expansion) =>
      buff append "Inlined("
      visit(call)
      buff append ", "
      visitAny(bindings)
      buff append ", "
      visit(expansion)
      buff append ")"
    case Synthetic() =>
      buff append "Synthetic()"
    case TypeIdent(name) =>
      buff append "Ident(" // FIXME print as TypeIdent
      visit(name)
      buff append ")"
    case TypeSelect(qualifier, name) =>
      buff append "TypeSelect("
      visit(qualifier)
      buff append ", "
      visit(name)
      buff append ")"
    case Singleton(ref) =>
      buff append "Singleton("
      visit(ref)
      buff append ")"
    case And(left, right) =>
      buff append "And("
      visit(left)
      buff append ", "
      visit(right)
      buff append ")"
    case Or(left, right) =>
      buff append "Or("
      visit(left)
      buff append ", "
      visit(right)
      buff append ")"
    case Refined(tpt, refinements) =>
      buff append "Refined("
      visit(tpt)
      buff append ", "
      visitAny(refinements)
      buff append ")"
    case Applied(tpt, args) =>
      buff append "Applied("
      visit(tpt)
      buff append ", "
      visitAny(args)
      buff append ")"
    case ByName(result) =>
      buff append "ByName("
      visit(result)
      buff append ")"
    case TypeBoundsTree(lo, hi) =>
      buff append "TypeBoundsTree("
      visit(lo)
      buff append ", "
      visit(hi)
      buff append ")"
    case Annotated(arg, annot) =>
      buff append "Annotated("
      visit(arg)
      buff append ", "
      visit(annot)
      buff append ")"
    case Value(v) =>
      buff append "Value("
      visit(v)
      buff append ")"
    case Bind(name, body) =>
      buff append "Bind("
      visit(name: Name)
      buff append ", "
      visit(body)
      buff append ")"
    case Unapply(fun, implicits, patterns) =>
      buff append "Unapply("
      visit(fun)
      buff append ", "
      visitAny(implicits)
      buff append ", "
      visitAny(patterns)
      buff append ")"
    case Alternative(patterns) =>
      buff append "Alternative("
      visitAny(patterns)
      buff append ")"
    case TypeTest(tpt) =>
      buff append "TypeTest("
      visit(tpt)
      buff append ")"
    case ValDef(name, tpt, rhs) =>
      buff append "ValDef("
      visit(name: Name)
      buff append ", "
      visit(tpt)
      buff append ", "
      visitAny(rhs)
      buff append ")"
    case DefDef(name, typeParams, paramss, returnTpt, rhs) =>
      buff append "DefDef("
      visit(name: Name)
      buff append ", "
      visitAny(typeParams)
      buff append ", "
      visitAny(paramss)
      buff append ", "
      visit(returnTpt)
      buff append ", "
      visitAny(rhs)
      buff append ")"
    case TypeDef(name, rhs) =>
      buff append "TypeDef("
      visit(name: Name)
      buff append ", "
      visit(rhs)
      buff append ")"
    case ClassDef(name, constr, parents, self, body) =>
      buff append "ClassDef("
      visit(name: Name)
      buff append ", "
      visit(constr)
      buff append ", "
      visitAny(parents)
      buff append ", "
      visitAny(self)
      buff append ", "
      visitAny(body)
      buff append ")"
    case PackageDef(name, members) =>
      buff append "PackageDef("
      visit(name: Name)
      buff append ", "
      visitAny(members)
      buff append ")"
    case Import(expr, selectors) =>
      buff append "Import("
      visit(expr)
      buff append ", "
      visitAny(selectors)
      buff append ")"
    case PackageClause(pid, stats) =>
      buff append "PackageClause("
      visit(pid)
      buff append ", "
      visitAny(stats)
      buff append ")"
  }

  private def visitAny(x: Any)(implicit buff: StringBuilder, ctx: Context): Unit = x match {
    case x: Tree => visit(x)
    case x: MaybeType => visit(x)
    case x: Name => visit(x)
    case x: PossiblySignedName => visit(x)
    case x: Constant => visit(x)
    case x: Modifier => visit(x)
    case x: ImportSelector => visit(x)
    case x0 :: xs =>
      buff append "List("
      visitAny(x0)
      def visitNext(xs: List[_]): Unit = xs match {
        case y :: ys =>
          buff append ", "
          visitAny(y)
          visitNext(ys)
        case Nil =>
      }
      visitNext(xs)
      buff append ")"
    case Nil => buff append "Nil"
    case Some(y) =>
      buff append "Some("
      visitAny(y)
      buff append ")"
    case None => buff append "None"
  }

  private def visit(const: Constant)(implicit buff: StringBuilder, ctx: Context): Unit = const match {
    case constants.Unit() => buff append "Unit()"
    case constants.Null() => buff append "Null()"
    case constants.Boolean(value) => buff append "Boolean(" append value append ")"
    case constants.Byte(value) => buff append "Byte(" append value append ")"
    case constants.Short(value) => buff append "Short(" append value append ")"
    case constants.Char(value) => buff append "Char('" append value append "')"
    case constants.Int(value) => buff append "Int(" append value append ")"
    case constants.Long(value) => buff append "Long(" append value append ")"
    case constants.Float(value) => buff append "Float(" append value append ")"
    case constants.Double(value) => buff append "Double(" append value append ")"
    case constants.String(value) => buff append "String(\"" append value append "\")" // TODO escape string characters?
  }

  private def visit(name: PossiblySignedName)(implicit buff: StringBuilder, ctx: Context): Unit = name match {
    case SignedName(underlying, resSig, paramsSigs) =>
      buff append "SignedName("
      visit(underlying: Name)
      buff append ", "
      visit(resSig)
      buff append ", "
      visitAny(paramsSigs)
      buff append ")"
    case name: TermName => visit(name: Name)
  }

  private def visit(name: Name)(implicit buff: StringBuilder, ctx: Context): Unit = name match {
    case TypeName(underlying) =>
      buff append "TypeName("
      visit(underlying: Name)
      buff append ")"
    case Simple(strName) =>
      buff append "Simple(" append strName append ")"
    case Qualified(qual, strName) =>
      buff append "Qualified("
      visit(qual: Name)
      buff append ","
      buff append strName
      buff append ")"
    case DefaultGetter(underlying, strName) =>
      buff append "DefaultGetter("
      visit(underlying: Name)
      buff append ","
      buff append strName
      buff append ")"
    case Variant(underlying, variance) =>
      buff append "Variant("
      visit(underlying: Name)
      buff append ","
      buff append variance
      buff append ")"
    case SuperAccessor(underlying) =>
      buff append "SuperAccessor("
      visit(underlying: Name)
      buff append ")"
    case ProtectedAccessor(underlying) =>
      buff append "ProtectedAccessor("
      visit(underlying: Name)
      buff append ")"
    case ProtectedSetter(underlying) =>
      buff append "ProtectedSetter("
      visit(underlying: Name)
      buff append ")"
    case ObjectClass(underlying) =>
      buff append "ObjectClass("
      visit(underlying: Name)
      buff append ")"
  }

  private def visit(selector: ImportSelector)(implicit buff: StringBuilder, ctx: Context): Unit = selector match {
    case SimpleSelector(id) => buff append "SimpleSelector(" append id append ")"
    case OmitSelector(id) => buff append "OmitSelector(" append id append ")"
    case RenameSelector(id1, id2) =>
      buff append "RenameSelector(" append id1 append ", " append id2 append ")"
  }

  private def visit(tpe: MaybeType)(implicit buff: StringBuilder, ctx: Context): Unit = {
    def visitName(sym: Definition): Unit = sym match {
      case ValDef(name, _, _) => visit(name: Name)
      case DefDef(name, _, _, _, _) => visit(name: Name)
      case TypeDef(name, _) => visit(name: Name)
      case ClassDef(name, _, _, _, _) => visit(name: Name)
      case PackageDef(name, _) => visit(name: Name)
      case _ => buff append "NoDefinition"
    }

    tpe match {
      case types.ConstantType(value) =>
        buff append "ConstantType("
        visit(value)
        buff append ")"
      case types.SymRef(sym, qual) =>
        buff append "SymRef(<"
        visitName(sym)
        buff append ">, "
        visit(qual)
        buff append ")"
      case types.NameRef(name, qual) =>
        buff append "NameRef("
        visit(name)
        buff append ", "
        visit(qual)
        buff append ")"
      case types.Refinement(parent, name, info) =>
        buff append "Refinement("
        visit(parent)
        buff append ", "
        visit(name)
        buff append ", "
        visit(info)
        buff append ")"
      case types.AppliedType(tycon, args) =>
        buff append "AppliedType("
        visit(tycon)
        buff append ", "
        visitAny(args)
        buff append ")"
      case types.AnnotatedType(underlying, annot) =>
        buff append "AnnotatedType("
        visit(underlying)
        buff append ", "
        visit(annot)
        buff append ")"
      case types.AndType(left, right) =>
        buff append "AndType("
        visit(left)
        buff append ", "
        visit(right)
        buff append ")"
      case types.OrType(left, right) =>
        buff append "OrType("
        visit(left)
        buff append ", "
        visit(right)
        buff append ")"
      case types.ByNameType(underlying) =>
        buff append "ByNameType("
        visit(underlying)
        buff append ")"
      case types.ParamRef(binder, idx) =>
        buff append "ParamRef("
        visit(binder)
        buff append ", " append idx append ")"
      case types.ThisType(tp) =>
        buff append "ThisType("
        visit(tp)
        buff append ")"
      case types.RecursiveThis(binder) =>
        buff append "RecursiveThis("
        visit(binder)
        buff append ")"
      case types.MethodType(argNames, argTypes, resType) =>
        buff append "MethodType("
        visitAny(argNames)
        buff append ", "
        visitAny(argTypes)
        buff append ", "
        visitAny(resType)
        buff append ")"
      case types.PolyType(argNames, argBounds, resType) =>
        buff append "PolyType("
        visitAny(argNames)
        buff append ", "
        visitAny(argBounds)
        buff append ", "
        visitAny(resType)
        buff append ")"
      case types.TypeLambda(argNames, argBounds, resType) =>
        buff append "TypeLambda("
        visitAny(argNames)
        buff append ", "
        visitAny(argBounds)
        buff append ", "
        visitAny(resType)
        buff append ")"
      case TypeBounds(lo, hi) =>
        buff append "TypeBounds("
        visit(lo)
        buff append ", "
        visit(hi)
        buff append ")"
      case tp: NoPrefix =>
        buff append "NoPrefix"
    }
  }

  private def visit(mod: Modifier)(implicit buff: StringBuilder, ctx: Context): Unit = mod match {
    case Flags(flags) =>
      buff append "Flags(" append flags append ")"
    case QualifiedPrivate(tp) =>
      buff append "QualifiedPrivate("
      visit(tp)
      buff append ")"
    case QualifiedProtected(tp) =>
      buff append "QualifiedProtected("
      visit(tp)
      buff append ")"
    case Annotation(tree) =>
      buff append "Annotation("
      visit(tree)
      buff append ")"
  }
}
