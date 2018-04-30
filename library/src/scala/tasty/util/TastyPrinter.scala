package scala.tasty.util

import scala.tasty.Tasty

object TastyPrinter {

  def stringOfTree(tasty: Tasty)(tree: tasty.TopLevelStatement)(implicit ctx: tasty.Context): String = {
    implicit val buff: StringBuilder = new StringBuilder
    visitTree(tasty)(tree)
    buff.toString()
  }

  def stringOfTypeTree(tasty: Tasty)(tree: tasty.MaybeTypeTree)(implicit ctx: tasty.Context): String = {
    implicit val buff: StringBuilder = new StringBuilder
    visitTypeTree(tasty)(tree)
    buff.toString()
  }

  def stringOfType(tasty: Tasty)(tpe: tasty.MaybeType)(implicit ctx: tasty.Context): String = {
    implicit val buff: StringBuilder = new StringBuilder
    visitType(tasty)(tpe)
    buff.toString()
  }

  def stringOfModifier(tasty: Tasty)(mod: tasty.Modifier)(implicit ctx: tasty.Context): String = {
    implicit val buff: StringBuilder = new StringBuilder
    visitModifier(tasty)(mod)
    buff.toString()
  }

  def stringOfConstant(tasty: Tasty)(mod: tasty.Constant)(implicit ctx: tasty.Context): String = {
    implicit val buff: StringBuilder = new StringBuilder
    visitConstant(tasty)(mod)
    buff.toString()
  }

  private def visitTree(tasty: Tasty)(x: tasty.TopLevelStatement)(implicit buff: StringBuilder, ctx: tasty.Context): Unit = {
    import tasty._
    x match {
      case Ident(name) =>
        buff append "Ident(" append name append ")"
      case Select(qualifier, name, signature) =>
        buff append "Select("
        visitTree(tasty)(qualifier)
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
            visitId(tasty)(id)
            buff append ")"
          case None => buff append "None"
        }
        buff append ")"
      case Super(qual, mix) =>
        buff append "TypeApply("
        visitTree(tasty)(qual)
        buff append ", "
        mix match {
          case Some(id) =>
            buff append "Some("
            visitId(tasty)(id)
            buff append ")"
          case None => buff append "None"
        }
        buff append ")"
      case Apply(fun, args) =>
        buff append "Apply("
        visitTree(tasty)(fun)
        buff append ", "
        visitTrees(tasty)(args)
        buff append ")"
      case TypeApply(fun, args) =>
        buff append "TypeApply("
        visitTree(tasty)(fun)
        buff append ", "
        visitTypeTrees(tasty)(args)
        buff append ")"
      case Literal(const) =>
        buff append "Literal("
        visitConstant(tasty)(const)
        buff append ")"
      case New(tpt) =>
        buff append "New("
        visitTypeTree(tasty)(tpt)
        buff append ")"
      case Typed(expr, tpt) =>
        buff append "Typed("
        visitTree(tasty)(expr)
        buff append ", "
        visitTypeTree(tasty)(tpt)
        buff append ")"
      case NamedArg(name, arg) =>
        buff append "NamedArg(" append name append ", "
        visitTree(tasty)(arg)
        buff append ")"
      case Assign(lhs, rhs) =>
        buff append "Assign("
        visitTree(tasty)(lhs)
        buff append ", "
        visitTree(tasty)(rhs)
        buff append ")"
      case Block(stats, expr) =>
        buff append "Block("
        visitTrees(tasty)(stats)
        buff append ", "
        visitTree(tasty)(expr)
        buff append ")"
      case If(cond, thenp, elsep) =>
        buff append "If("
        visitTree(tasty)(cond)
        buff append ", "
        visitTree(tasty)(thenp)
        buff append ", "
        visitTree(tasty)(elsep)
        buff append ")"
      case Lambda(meth, tpt) =>
        buff append "Lambda("
        visitTree(tasty)(meth)
        buff append ", "
        tpt match {
          case Some(tree) =>
            buff append "Some("
            visitTypeTree(tasty)(tree)
            buff append ")"
          case _ => buff append "None"
        }
        buff append ")"
      case Match(selector, cases) =>
        buff append "Match("
        visitTree(tasty)(selector)
        buff append ", "
        visitCaseDefs(tasty)(cases)
        buff append ")"
      case Return(expr) =>
        buff append "Return("
        visitTree(tasty)(expr)
        buff append ")"
      case Try(block, handlers, finalizer) =>
        buff append "Try("
        visitTree(tasty)(block)
        buff append ", "
        visitCaseDefs(tasty)(handlers)
        buff append ", "
        finalizer match {
          case Some(tree) =>
            buff append "Some("
            visitTree(tasty)(tree)
            buff append ")"
          case _ => buff append "None"
        }
        buff append ")"
      case Repeated(elems) =>
        buff append "Repeated("
        visitTrees(tasty)(elems)
        buff append ")"
      case Inlined(call, bindings, expansion) =>
        buff append "Inlined("
        visitTree(tasty)(call)
        buff append ", "
        visitTrees(tasty)(bindings)
        buff append ", "
        visitTree(tasty)(expansion)
        buff append ")"
      case ValDef(name, tpt, rhs) =>
        buff append "ValDef(" append name append ", "
        visitTypeTree(tasty)(tpt)
        buff append ", "
        rhs match {
          case Some(tree) =>
            buff append "Some("
            visitTree(tasty)(tree)
            buff append ")"
          case _ => buff append "None"
        }
        buff append ")"
      case DefDef(name, typeParams, paramss, returnTpt, rhs) =>
        buff append "DefDef(" append name append ", "
        visitTrees(tasty)(typeParams)
        buff append ", "
        paramss match {
          case x0 :: xs =>
            buff append "List("
            visitTrees(tasty)(x0)
            def visitNext(xs: List[List[tasty.ValDef]]): Unit = xs match {
              case y :: ys =>
                buff append ", "
                visitTrees(tasty)(y)
                visitNext(ys)
              case Nil =>
            }
            visitNext(xs)
            buff append ")"
          case Nil => buff append "Nil"
        }
        buff append ", "
        visitTypeTree(tasty)(returnTpt)
        buff append ", "
        rhs match {
          case Some(tree) =>
            buff append "Some("
            visitTree(tasty)(tree)
            buff append ")"
          case None => buff append "None"
        }
        buff append ")"
      case TypeDef(name, rhs) =>
        buff append "TypeDef(" append name append ", "
        visitTypeTree(tasty)(rhs)
        buff append ")"
      case ClassDef(name, constr, parents, self, body) =>
        buff append "ClassDef(" append name append ", "
        visitTree(tasty)(constr)
        buff append ", "
        visitParents(tasty)(parents)
        buff append ", "
        self match {
          case Some(tree) =>
            buff append "Some("
            visitTree(tasty)(tree)
            buff append ")"
          case _ => buff append "None"
        }
        buff append ", "
        visitTrees(tasty)(body)
        buff append ")"
      case PackageDef(name, members) =>
        buff append "PackageDef("
        buff append name
        buff append ", "
        visitTrees(tasty)(members)
        buff append ")"
      case Import(expr, selectors) =>
        buff append "Import("
        visitTree(tasty)(expr)
        buff append ", "

        buff append "List("
        selectors.foreach {
          case SimpleSelector(id) =>
            buff append "SimpleSelector("
            visitId(tasty)(id)
            buff append ")"
          case RenameSelector(id1, id2) =>
            buff append "RenameSelector("
            visitId(tasty)(id1)
            buff append ", "
            visitId(tasty)(id2)
            buff append ")"
          case OmitSelector(id) =>
            buff append "OmitSelector("
            visitId(tasty)(id)
            buff append ")"
        }
        buff append ")"
        buff append ")"
      case PackageClause(pid, stats) =>
        buff append "PackageClause("
        visitTree(tasty)(pid)
        buff append ", "
        visitTrees(tasty)(stats)
        buff append ")"
    }
  }

  private def visitTrees(tasty: Tasty)(list: List[tasty.TopLevelStatement])(implicit buff: StringBuilder, ctx: tasty.Context): Unit = {
    list match {
      case x0 :: xs =>
        buff append "List("
        visitTree(tasty)(x0)
        def visitNext(xs: List[tasty.TopLevelStatement]): Unit = xs match {
          case y :: ys =>
            buff append ", "
            visitTree(tasty)(y)
            visitNext(ys)
          case Nil =>
        }
        visitNext(xs)
        buff append ")"
      case Nil => buff append "Nil"
    }
  }

  private def visitTypeTree(tasty: Tasty)(x: tasty.MaybeTypeTree)(implicit buff: StringBuilder, ctx: tasty.Context): Unit = {
    import tasty._
    x match {
      case Synthetic() =>
        buff append "Synthetic()"
      case TypeIdent(name) =>
        buff append "TypeIdent(" append name append ")"
      case TypeSelect(qualifier, name) =>
        buff append "TypeSelect("
        visitTree(tasty)(qualifier)
        buff append ", " append name append ")"
      case Singleton(ref) =>
        buff append "Singleton("
        visitTree(tasty)(ref)
        buff append ")"
      case And(left, right) =>
        buff append "And("
        visitTypeTree(tasty)(left)
        buff append ", "
        visitTypeTree(tasty)(right)
        buff append ")"
      case Or(left, right) =>
        buff append "Or("
        visitTypeTree(tasty)(left)
        buff append ", "
        visitTypeTree(tasty)(right)
        buff append ")"
      case Refined(tpt, refinements) =>
        buff append "Refined("
        visitTypeTree(tasty)(tpt)
        buff append ", "
        visitTrees(tasty)(refinements)
        buff append ")"
      case Applied(tpt, args) =>
        buff append "Applied("
        visitTypeTree(tasty)(tpt)
        buff append ", "
        visitTypeTrees(tasty)(args)
        buff append ")"
      case ByName(result) =>
        buff append "ByName("
        visitTypeTree(tasty)(result)
        buff append ")"
      case Annotated(arg, annot) =>
        buff append "Annotated("
        visitTypeTree(tasty)(arg)
        buff append ", "
        visitTree(tasty)(annot)
        buff append ")"
      case TypeBoundsTree(lo, hi) =>
        buff append "TypeBoundsTree("
        visitTypeTree(tasty)(lo)
        buff append ", "
        visitTypeTree(tasty)(hi)
        buff append ")"
    }
  }

  private def visitTypeTrees(tasty: Tasty)(list: List[tasty.TypeTree])(implicit buff: StringBuilder, ctx: tasty.Context): Unit = {
    list match {
      case x0 :: xs =>
        buff append "List("
        visitTypeTree(tasty)(x0)
        def visitNext(xs: List[tasty.TypeTree]): Unit = xs match {
          case y :: ys =>
            buff append ", "
            visitTypeTree(tasty)(y)
            visitNext(ys)
          case Nil =>
        }
        visitNext(xs)
        buff append ")"
      case Nil => buff append "Nil"
    }
  }

  private def visitCaseDef(tasty: Tasty)(x: tasty.CaseDef)(implicit buff: StringBuilder, ctx: tasty.Context): Unit = {
    import tasty._
    x match {
      case CaseDef(pat, guard, body) =>
        buff append "CaseDef("
        visitPattern(tasty)(pat)
        buff append ", "
        guard match {
          case Some(tree) =>
            buff append "Some("
            visitTree(tasty)(guard.get)
            buff append ")"
          case None =>
            buff append "None"
        }
        buff append ", "
        visitTree(tasty)(body)
        buff append ")"
    }
  }

  private def visitCaseDefs(tasty: Tasty)(list: List[tasty.CaseDef])(implicit buff: StringBuilder, ctx: tasty.Context): Unit = {
    list match {
      case x0 :: xs =>
        buff append "List("
        visitCaseDef(tasty)(x0)
        def visitNext(xs: List[tasty.CaseDef]): Unit = xs match {
          case y :: ys =>
            buff append ", "
            visitCaseDef(tasty)(y)
            visitNext(ys)
          case Nil =>
        }
        visitNext(xs)
        buff append ")"
      case Nil => buff append "Nil"
    }
  }

  private def visitPattern(tasty: Tasty)(x: tasty.Pattern)(implicit buff: StringBuilder, ctx: tasty.Context): Unit = {
    import tasty._
    x match {
      case Value(v) =>
        buff append "Value("
        visitTree(tasty)(v)
        buff append ")"
      case Bind(name, body) =>
        buff append "Bind(" append name append ", "
        visitPattern(tasty)(body)
        buff append ")"
      case Unapply(fun, implicits, patterns) =>
        buff append "Unapply("
        visitTree(tasty)(fun)
        buff append ", "
        visitTrees(tasty)(implicits)
        buff append ", "
        visitPatterns(tasty)(patterns)
        buff append ")"
      case Alternative(patterns) =>
        buff append "Alternative("
        visitPatterns(tasty)(patterns)
        buff append ")"
      case TypeTest(tpt) =>
        buff append "TypeTest("
        visitTypeTree(tasty)(tpt)
        buff append ")"
    }
  }

  private def visitPatterns(tasty: Tasty)(list: List[tasty.Pattern])(implicit buff: StringBuilder, ctx: tasty.Context): Unit = {
    list match {
      case x0 :: xs =>
        buff append "List("
        visitPattern(tasty)(x0)
        def visitNext(xs: List[tasty.Pattern]): Unit = xs match {
          case y :: ys =>
            buff append ", "
            visitPattern(tasty)(y)
            visitNext(ys)
          case Nil =>
        }
        visitNext(xs)
        buff append ")"
      case Nil => buff append "Nil"
    }
  }

  private def visitConstant(tasty: Tasty)(x: tasty.Constant)(implicit buff: StringBuilder, ctx: tasty.Context): Unit = {
    import tasty._
    x match {
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
  }

  private def visitType(tasty: Tasty)(x: tasty.MaybeType)(implicit buff: StringBuilder, ctx: tasty.Context): Unit = {
    import tasty._
    x match {
      case ConstantType(value) =>
        buff append "ConstantType("
        visitConstant(tasty)(value)
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
        visitType(tasty)(qual)
        buff append ")"
      case NameRef(name, qual) =>
        buff append "NameRef(" append name append ", "
        visitType(tasty)(qual)
        buff append ")"
      case Refinement(parent, name, info) =>
        buff append "Refinement("
        visitType(tasty)(parent)
        buff append ", " append name append ", "
        visitType(tasty)(info)
        buff append ")"
      case AppliedType(tycon, args) =>
        buff append "AppliedType("
        visitType(tasty)(tycon)
        buff append ", "
        visitTypes(tasty)(args)
        buff append ")"
      case AnnotatedType(underlying, annot) =>
        buff append "AnnotatedType("
        visitType(tasty)(underlying)
        buff append ", "
        visitTree(tasty)(annot)
        buff append ")"
      case AndType(left, right) =>
        buff append "AndType("
        visitType(tasty)(left)
        buff append ", "
        visitType(tasty)(right)
        buff append ")"
      case OrType(left, right) =>
        buff append "OrType("
        visitType(tasty)(left)
        buff append ", "
        visitType(tasty)(right)
        buff append ")"
      case ByNameType(underlying) =>
        buff append "ByNameType("
        visitType(tasty)(underlying)
        buff append ")"
      case ParamRef(binder, idx) =>
        buff append "ParamRef("
        visitType(tasty)(binder)
        buff append ", " append idx append ")"
      case ThisType(tp) =>
        buff append "ThisType("
        visitType(tasty)(tp)
        buff append ")"
      case RecursiveThis(binder) =>
        buff append "RecursiveThis("
        visitType(tasty)(binder)
        buff append ")"
      case MethodType(argNames, argTypes, resType) =>
        buff append "MethodType("
        if (argNames.isEmpty) buff append "Nil"
        else buff append argNames
        buff append ", "
        visitTypes(tasty)(argTypes)
        buff append ", "
        visitType(tasty)(resType)
        buff append ")"
      case PolyType(argNames, argBounds, resType) =>
        buff append "PolyType("
        if (argNames.isEmpty) buff append "Nil"
        else buff append argNames
        buff append ", "
        visitTypes(tasty)(argBounds)
        buff append ", "
        visitType(tasty)(resType)
        buff append ")"
      case TypeLambda(argNames, argBounds, resType) =>
        buff append "TypeLambda("
        if (argNames.isEmpty) buff append "Nil"
        else buff append argNames
        buff append ", "
        visitTypes(tasty)(argBounds)
        buff append ", "
        visitType(tasty)(resType)
        buff append ")"
      case TypeBounds(lo, hi) =>
        buff append "TypeBounds("
        visitType(tasty)(lo)
        buff append ", "
        visitType(tasty)(hi)
        buff append ")"
      case NoPrefix() =>
        buff append "NoPrefix"
    }
  }

  private def visitTypes(tasty: Tasty)(list: List[tasty.MaybeType])(implicit buff: StringBuilder, ctx: tasty.Context): Unit = {
    list match {
      case x0 :: xs =>
        buff append "List("
        visitType(tasty)(x0)
        def visitNext(xs: List[tasty.MaybeType]): Unit = xs match {
          case y :: ys =>
            buff append ", "
            visitType(tasty)(y)
            visitNext(ys)
          case Nil =>
        }
        visitNext(xs)
        buff append ")"
      case Nil => buff append "Nil"
    }
  }

  private def visitModifier(tasty: Tasty)(x: tasty.Modifier)(implicit buff: StringBuilder, ctx: tasty.Context): Unit = {
    import tasty._
    x match {
      case Flags(flags) =>
        buff append "Flags(" append flags append ")"
      case QualifiedPrivate(tp) =>
        buff append "QualifiedPrivate("
        visitType(tasty)(tp)
        buff append ")"
      case QualifiedProtected(tp) =>
        buff append "QualifiedProtected("
        visitType(tasty)(tp)
        buff append ")"
      case Annotation(tree) =>
        buff append "Annotation("
        visitTree(tasty)(tree)
        buff append ")"
    }
  }

  private def visitParent(tasty: Tasty)(x: tasty.Parent)(implicit buff: StringBuilder, ctx: tasty.Context): Unit = {
    import tasty._
    x match {
      case TermParent(term) =>
        buff append "TermParent("
        visitTree(tasty)(term)
        buff append ")"
      case TypeParent(typeTree) =>
        buff append "TypeParent("
        visitTypeTree(tasty)(typeTree)
        buff append ")"
    }
  }

  private def visitParents(tasty: Tasty)(list: List[tasty.Parent])(implicit buff: StringBuilder, ctx: tasty.Context): Unit = {
    list match {
      case x0 :: xs =>
        buff append "List("
        visitParent(tasty)(x0)
        def visitNext(xs: List[tasty.Parent]): Unit = xs match {
          case y :: ys =>
            buff append ", "
            visitParent(tasty)(y)
            visitNext(ys)
          case Nil =>
        }
        visitNext(xs)
        buff append ")"
      case Nil => buff append "Nil"
    }
  }

  private def visitId(tasty: Tasty)(x: tasty.Id)(implicit buff: StringBuilder, ctx: tasty.Context): Unit = {
    import tasty._
    x match {
      case Id(name) => buff append "Id(" append name append ")"
    }
  }
}
