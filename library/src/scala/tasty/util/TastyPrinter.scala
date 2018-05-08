package scala.tasty.util

import scala.tasty.Tasty

class TastyPrinter[T <: Tasty with Singleton](val tasty: T) {
  import tasty._

  def stringOfTree(tree: Tree)(implicit ctx: Context): String = {
    implicit val buff: StringBuilder = new StringBuilder
    visitTree(tree)
    buff.toString()
  }

  def stringOfTypeTree(tree: MaybeTypeTree)(implicit ctx: Context): String = {
    implicit val buff: StringBuilder = new StringBuilder
    visitTypeTree(tree)
    buff.toString()
  }

  def stringOfType(tpe: MaybeType)(implicit ctx: Context): String = {
    implicit val buff: StringBuilder = new StringBuilder
    visitType(tpe)
    buff.toString()
  }

  def stringOfModifier(mod: Modifier)(implicit ctx: Context): String = {
    implicit val buff: StringBuilder = new StringBuilder
    visitModifier(mod)
    buff.toString()
  }

  def stringOfConstant(mod: Constant)(implicit ctx: Context): String = {
    implicit val buff: StringBuilder = new StringBuilder
    visitConstant(mod)
    buff.toString()
  }

  private def visitTree(x: Tree)(implicit buff: StringBuilder, ctx: Context): Unit = x match {
    case Ident(name) =>
      buff append "Ident(" append name append ")"
    case Select(qualifier, name, signature) =>
      buff append "Select("
      visitTree(qualifier)
      buff append ", " append name append ", "
      signature match {
        case Some(sig) =>
          val Signature(params, res) = sig
          buff append "Some(Signature("
          if (params.isEmpty) buff append "Nil"
          else buff append params
          buff append ", " append res append "))"
        case None => buff append "None"
      }
      buff append ")"
    case This(qual) =>
      buff append "This("
      qual match {
        case Some(id) =>
          buff append "Some("
          visitId(id)
          buff append ")"
        case None => buff append "None"
      }
      buff append ")"
    case Super(qual, mix) =>
      buff append "TypeApply("
      visitTree(qual)
      buff append ", "
      mix match {
        case Some(id) =>
          buff append "Some("
          visitId(id)
          buff append ")"
        case None => buff append "None"
      }
      buff append ")"
    case Apply(fun, args) =>
      buff append "Apply("
      visitTree(fun)
      buff append ", "
      visitTrees(args)
      buff append ")"
    case TypeApply(fun, args) =>
      buff append "TypeApply("
      visitTree(fun)
      buff append ", "
      visitTypeTrees(args)
      buff append ")"
    case Literal(const) =>
      buff append "Literal("
      visitConstant(const)
      buff append ")"
    case New(tpt) =>
      buff append "New("
      visitTypeTree(tpt)
      buff append ")"
    case Typed(expr, tpt) =>
      buff append "Typed("
      visitTree(expr)
      buff append ", "
      visitTypeTree(tpt)
      buff append ")"
    case NamedArg(name, arg) =>
      buff append "NamedArg(" append name append ", "
      visitTree(arg)
      buff append ")"
    case Assign(lhs, rhs) =>
      buff append "Assign("
      visitTree(lhs)
      buff append ", "
      visitTree(rhs)
      buff append ")"
    case Block(stats, expr) =>
      buff append "Block("
      visitTrees(stats)
      buff append ", "
      visitTree(expr)
      buff append ")"
    case If(cond, thenp, elsep) =>
      buff append "If("
      visitTree(cond)
      buff append ", "
      visitTree(thenp)
      buff append ", "
      visitTree(elsep)
      buff append ")"
    case Lambda(meth, tpt) =>
      buff append "Lambda("
      visitTree(meth)
      buff append ", "
      tpt match {
        case Some(tree) =>
          buff append "Some("
          visitTypeTree(tree)
          buff append ")"
        case _ => buff append "None"
      }
      buff append ")"
    case Match(selector, cases) =>
      buff append "Match("
      visitTree(selector)
      buff append ", "
      visitCaseDefs(cases)
      buff append ")"
    case Return(expr) =>
      buff append "Return("
      visitTree(expr)
      buff append ")"
    case Try(block, handlers, finalizer) =>
      buff append "Try("
      visitTree(block)
      buff append ", "
      visitCaseDefs(handlers)
      buff append ", "
      finalizer match {
        case Some(tree) =>
          buff append "Some("
          visitTree(tree)
          buff append ")"
        case _ => buff append "None"
      }
      buff append ")"
    case Repeated(elems) =>
      buff append "Repeated("
      visitTrees(elems)
      buff append ")"
    case Inlined(call, bindings, expansion) =>
      buff append "Inlined("
      visitTree(call)
      buff append ", "
      visitTrees(bindings)
      buff append ", "
      visitTree(expansion)
      buff append ")"
    case ValDef(name, tpt, rhs) =>
      buff append "ValDef(" append name append ", "
      visitTypeTree(tpt)
      buff append ", "
      rhs match {
        case Some(tree) =>
          buff append "Some("
          visitTree(tree)
          buff append ")"
        case _ => buff append "None"
      }
      buff append ")"
    case DefDef(name, typeParams, paramss, returnTpt, rhs) =>
      buff append "DefDef(" append name append ", "
      visitTrees(typeParams)
      buff append ", "
      paramss match {
        case x0 :: xs =>
          buff append "List("
          visitTrees(x0)
          def visitNext(xs: List[List[ValDef]]): Unit = xs match {
            case y :: ys =>
              buff append ", "
              visitTrees(y)
              visitNext(ys)
            case Nil =>
          }
          visitNext(xs)
          buff append ")"
        case Nil => buff append "Nil"
      }
      buff append ", "
      visitTypeTree(returnTpt)
      buff append ", "
      rhs match {
        case Some(tree) =>
          buff append "Some("
          visitTree(tree)
          buff append ")"
        case None => buff append "None"
      }
      buff append ")"
    case TypeDef(name, rhs) =>
      buff append "TypeDef(" append name append ", "
      visitTypeTree(rhs)
      buff append ")"
    case ClassDef(name, constr, parents, self, body) =>
      buff append "ClassDef(" append name append ", "
      visitTree(constr)
      buff append ", "
      visitParents(parents)
      buff append ", "
      self match {
        case Some(tree) =>
          buff append "Some("
          visitTree(tree)
          buff append ")"
        case _ => buff append "None"
      }
      buff append ", "
      visitTrees(body)
      buff append ")"
    case PackageDef(name, members) =>
      buff append "PackageDef("
      buff append name
      buff append ", "
      visitTrees(members)
      buff append ")"
    case Import(expr, selectors) =>
      buff append "Import("
      visitTree(expr)
      buff append ", "

      buff append "List("
      selectors.foreach {
        case SimpleSelector(id) =>
          buff append "SimpleSelector("
          visitId(id)
          buff append ")"
        case RenameSelector(id1, id2) =>
          buff append "RenameSelector("
          visitId(id1)
          buff append ", "
          visitId(id2)
          buff append ")"
        case OmitSelector(id) =>
          buff append "OmitSelector("
          visitId(id)
          buff append ")"
      }
      buff append ")"
      buff append ")"
    case PackageClause(pid, stats) =>
      buff append "PackageClause("
      visitTree(pid)
      buff append ", "
      visitTrees(stats)
      buff append ")"
  }

  private def visitTrees(list: List[Tree])(implicit buff: StringBuilder, ctx: Context): Unit = {
    list match {
      case x0 :: xs =>
        buff append "List("
        visitTree(x0)
        def visitNext(xs: List[Tree]): Unit = xs match {
          case y :: ys =>
            buff append ", "
            visitTree(y)
            visitNext(ys)
          case Nil =>
        }
        visitNext(xs)
        buff append ")"
      case Nil => buff append "Nil"
    }
  }

  private def visitTypeTree(x: MaybeTypeTree)(implicit buff: StringBuilder, ctx: Context): Unit = {
    x match {
      case Synthetic() =>
        buff append "Synthetic()"
      case TypeIdent(name) =>
        buff append "TypeIdent(" append name append ")"
      case TypeSelect(qualifier, name) =>
        buff append "TypeSelect("
        visitTree(qualifier)
        buff append ", " append name append ")"
      case Singleton(ref) =>
        buff append "Singleton("
        visitTree(ref)
        buff append ")"
      case And(left, right) =>
        buff append "And("
        visitTypeTree(left)
        buff append ", "
        visitTypeTree(right)
        buff append ")"
      case Or(left, right) =>
        buff append "Or("
        visitTypeTree(left)
        buff append ", "
        visitTypeTree(right)
        buff append ")"
      case Refined(tpt, refinements) =>
        buff append "Refined("
        visitTypeTree(tpt)
        buff append ", "
        visitTrees(refinements)
        buff append ")"
      case Applied(tpt, args) =>
        buff append "Applied("
        visitTypeTree(tpt)
        buff append ", "
        visitTypeTrees(args)
        buff append ")"
      case ByName(result) =>
        buff append "ByName("
        visitTypeTree(result)
        buff append ")"
      case Annotated(arg, annot) =>
        buff append "Annotated("
        visitTypeTree(arg)
        buff append ", "
        visitTree(annot)
        buff append ")"
      case TypeBoundsTree(lo, hi) =>
        buff append "TypeBoundsTree("
        visitTypeTree(lo)
        buff append ", "
        visitTypeTree(hi)
        buff append ")"
    }
  }

  private def visitTypeTrees(list: List[TypeTree])(implicit buff: StringBuilder, ctx: Context): Unit = {
    list match {
      case x0 :: xs =>
        buff append "List("
        visitTypeTree(x0)
        def visitNext(xs: List[TypeTree]): Unit = xs match {
          case y :: ys =>
            buff append ", "
            visitTypeTree(y)
            visitNext(ys)
          case Nil =>
        }
        visitNext(xs)
        buff append ")"
      case Nil => buff append "Nil"
    }
  }

  private def visitCaseDef(x: CaseDef)(implicit buff: StringBuilder, ctx: Context): Unit = {
    x match {
      case CaseDef(pat, guard, body) =>
        buff append "CaseDef("
        visitPattern(pat)
        buff append ", "
        guard match {
          case Some(tree) =>
            buff append "Some("
            visitTree(guard.get)
            buff append ")"
          case None =>
            buff append "None"
        }
        buff append ", "
        visitTree(body)
        buff append ")"
    }
  }

  private def visitCaseDefs(list: List[CaseDef])(implicit buff: StringBuilder, ctx: Context): Unit = {
    list match {
      case x0 :: xs =>
        buff append "List("
        visitCaseDef(x0)
        def visitNext(xs: List[CaseDef]): Unit = xs match {
          case y :: ys =>
            buff append ", "
            visitCaseDef(y)
            visitNext(ys)
          case Nil =>
        }
        visitNext(xs)
        buff append ")"
      case Nil => buff append "Nil"
    }
  }

  private def visitPattern(x: Pattern)(implicit buff: StringBuilder, ctx: Context): Unit = x match {
    case Value(v) =>
      buff append "Value("
      visitTree(v)
      buff append ")"
    case Bind(name, body) =>
      buff append "Bind(" append name append ", "
      visitPattern(body)
      buff append ")"
    case Unapply(fun, implicits, patterns) =>
      buff append "Unapply("
      visitTree(fun)
      buff append ", "
      visitTrees(implicits)
      buff append ", "
      visitPatterns(patterns)
      buff append ")"
    case Alternative(patterns) =>
      buff append "Alternative("
      visitPatterns(patterns)
      buff append ")"
    case TypeTest(tpt) =>
      buff append "TypeTest("
      visitTypeTree(tpt)
      buff append ")"
  }

  private def visitPatterns(list: List[Pattern])(implicit buff: StringBuilder, ctx: Context): Unit = {
    list match {
      case x0 :: xs =>
        buff append "List("
        visitPattern(x0)
        def visitNext(xs: List[Pattern]): Unit = xs match {
          case y :: ys =>
            buff append ", "
            visitPattern(y)
            visitNext(ys)
          case Nil =>
        }
        visitNext(xs)
        buff append ")"
      case Nil => buff append "Nil"
    }
  }

  private def visitConstant(x: Constant)(implicit buff: StringBuilder, ctx: Context): Unit = x match {
    case UnitConstant() => buff append "Unit()"
    case NullConstant() => buff append "Null()"
    case BooleanConstant(value) => buff append "Boolean(" append value append ")"
    case ByteConstant(value) => buff append "Byte(" append value append ")"
    case ShortConstant(value) => buff append "Short(" append value append ")"
    case CharConstant(value) => buff append "Char(" append value append ")"
    case IntConstant(value) => buff append "Int(" append value append ")"
    case LongConstant(value) => buff append "Long(" append value append ")"
    case FloatConstant(value) => buff append "Float(" append value append ")"
    case DoubleConstant(value) => buff append "Double(" append value append ")"
    case StringConstant(value) => buff append "String(" append value append ")"
  }

  private def visitType(x: MaybeType)(implicit buff: StringBuilder, ctx: Context): Unit = x match {
    case ConstantType(value) =>
      buff append "ConstantType("
      visitConstant(value)
      buff append ")"
    case SymRef(sym, qual) =>
      def visitName(sym: Definition): Unit = sym match {
        case ValDef(name, _, _) => buff append name
        case DefDef(name, _, _, _, _) => buff append name
        case TypeDef(name, _) => buff append name
        case ClassDef(name, _, _, _, _) => buff append name
        case PackageDef(name, _) => buff append name
        case _ => buff append "#"
      }
      buff append "SymRef("
      visitName(sym)
      buff append ", "
      visitType(qual)
      buff append ")"
    case NameRef(name, qual) =>
      buff append "NameRef(" append name append ", "
      visitType(qual)
      buff append ")"
    case Refinement(parent, name, info) =>
      buff append "Refinement("
      visitType(parent)
      buff append ", " append name append ", "
      visitType(info)
      buff append ")"
    case AppliedType(tycon, args) =>
      buff append "AppliedType("
      visitType(tycon)
      buff append ", "
      visitTypes(args)
      buff append ")"
    case AnnotatedType(underlying, annot) =>
      buff append "AnnotatedType("
      visitType(underlying)
      buff append ", "
      visitTree(annot)
      buff append ")"
    case AndType(left, right) =>
      buff append "AndType("
      visitType(left)
      buff append ", "
      visitType(right)
      buff append ")"
    case OrType(left, right) =>
      buff append "OrType("
      visitType(left)
      buff append ", "
      visitType(right)
      buff append ")"
    case ByNameType(underlying) =>
      buff append "ByNameType("
      visitType(underlying)
      buff append ")"
    case ParamRef(binder, idx) =>
      buff append "ParamRef("
      visitType(binder)
      buff append ", " append idx append ")"
    case ThisType(tp) =>
      buff append "ThisType("
      visitType(tp)
      buff append ")"
    case RecursiveThis(binder) =>
      buff append "RecursiveThis("
      visitType(binder)
      buff append ")"
    case MethodType(argNames, argTypes, resType) =>
      buff append "MethodType("
      if (argNames.isEmpty) buff append "Nil"
      else buff append argNames
      buff append ", "
      visitTypes(argTypes)
      buff append ", "
      visitType(resType)
      buff append ")"
    case PolyType(argNames, argBounds, resType) =>
      buff append "PolyType("
      if (argNames.isEmpty) buff append "Nil"
      else buff append argNames
      buff append ", "
      visitTypes(argBounds)
      buff append ", "
      visitType(resType)
      buff append ")"
    case TypeLambda(argNames, argBounds, resType) =>
      buff append "TypeLambda("
      if (argNames.isEmpty) buff append "Nil"
      else buff append argNames
      buff append ", "
      visitTypes(argBounds)
      buff append ", "
      visitType(resType)
      buff append ")"
    case TypeBounds(lo, hi) =>
      buff append "TypeBounds("
      visitType(lo)
      buff append ", "
      visitType(hi)
      buff append ")"
    case NoPrefix() =>
      buff append "NoPrefix"
  }

  private def visitTypes(list: List[MaybeType])(implicit buff: StringBuilder, ctx: Context): Unit = {
    list match {
      case x0 :: xs =>
        buff append "List("
        visitType(x0)
        def visitNext(xs: List[MaybeType]): Unit = xs match {
          case y :: ys =>
            buff append ", "
            visitType(y)
            visitNext(ys)
          case Nil =>
        }
        visitNext(xs)
        buff append ")"
      case Nil => buff append "Nil"
    }
  }

  private def visitModifier(x: Modifier)(implicit buff: StringBuilder, ctx: Context): Unit = x match {
    case Flags(flags) =>
      buff append "Flags(" append flags append ")"
    case QualifiedPrivate(tp) =>
      buff append "QualifiedPrivate("
      visitType(tp)
      buff append ")"
    case QualifiedProtected(tp) =>
      buff append "QualifiedProtected("
      visitType(tp)
      buff append ")"
    case Annotation(tree) =>
      buff append "Annotation("
      visitTree(tree)
      buff append ")"
  }

  private def visitParent(x: Parent)(implicit buff: StringBuilder, ctx: Context): Unit = x match {
    case TermParent(term) =>
      buff append "TermParent("
      visitTree(term)
      buff append ")"
    case TypeParent(typeTree) =>
      buff append "TypeParent("
      visitTypeTree(typeTree)
      buff append ")"
  }

  private def visitParents(list: List[Parent])(implicit buff: StringBuilder, ctx: Context): Unit = {
    list match {
      case x0 :: xs =>
        buff append "List("
        visitParent(x0)
        def visitNext(xs: List[Parent]): Unit = xs match {
          case y :: ys =>
            buff append ", "
            visitParent(y)
            visitNext(ys)
          case Nil =>
        }
        visitNext(xs)
        buff append ")"
      case Nil => buff append "Nil"
    }
  }

  private def visitId(x: Id)(implicit buff: StringBuilder, ctx: Context): Unit = {
    import tasty._
    x match {
      case Id(name) => buff append "Id(" append name append ")"
    }
  }
}
