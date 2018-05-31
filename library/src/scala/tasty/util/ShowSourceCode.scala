package scala.tasty
package util

class ShowSourceCode[T <: Tasty with Singleton](tasty0: T) extends Show[T](tasty0) {
  import tasty._

  def showTree(tree: Tree)(implicit ctx: Context): String =
    (new Buffer).printTree(tree).result()

  def showTypeOrBoundsTree(tpt: TypeOrBoundsTree)(implicit ctx: Context): String =
    (new Buffer).printTypeOrBoundsTree(tpt).result()

  def showTypeOrBounds(tpe: TypeOrBounds)(implicit ctx: Context): String =
    (new Buffer).printTypeOrBound(tpe).result()

  def showConstant(const: Constant)(implicit ctx: Context): String =
    (new Buffer).printConstant(const).result()

  private class Buffer(implicit ctx: Context) {

    private[this] val sb: StringBuilder = new StringBuilder

    private[this] var indent: Int = 0
    def indented(printIndented: => Unit): Unit = {
      indent += 1
      printIndented
      indent -= 1
    }

    def result(): String = sb.result()

    def lineBreak(): String = "\n" + ("  " * indent)

    def printTree(tree: Tree): Buffer = tree match {
      case tree @ PackageClause(Term.Ident(name), stats) =>
        val stats1 = stats.collect {
          case stat @ PackageClause(_, _) => stat
          case stat @ Definition() if !(stat.flags.isObject && stat.flags.isLazy) => stat
          case stat @ Import(_, _) => stat
        }

        if (name == "<empty>") {
          printTrees(stats1, lineBreak())
        } else {
          this += "package " += name += " {"
          indented {
            this += lineBreak()
            printTrees(stats1, lineBreak())
          }
          this += lineBreak() += "}" += lineBreak()
        }

      case Import(expr, selectors) =>
        this += "import "
        printTree(expr)
        this += "."
        printImportSelectors(selectors)

      case cdef @ ClassDef(name, DefDef(_, targs, argss, _, _), parents, self, stats) =>
        val flags = cdef.flags
        if (flags.isFinal && !flags.isObject) this += "final "
        if (flags.isCase) this += "case "

        if (flags.isObject) this += "object " += name.stripSuffix("$")
        else this += "class " += name

        if (!flags.isObject) {
          printTargsDefs(targs)
          val it = argss.iterator
          while (it.hasNext)
            printArgsDefs(it.next())
        }

        val parents1 = parents.filter {
          case Term.Apply(Term.Select(Term.New(tpt), _, _), _) => !Types.JavaLangObject.unapply(tpt.tpe)
          case TypeTree.TypeSelect(Term.Select(Term.Ident("_root_"), "scala", _), "Product") => false
          case _ => true
        }
        if (parents1.nonEmpty) {
          sb.append(" extends")
          parents1.foreach {
            case parent@Term.Apply(Term.TypeApply(Term.Select(Term.New(tpt), _, _), targs), args) =>
              this += " "
              printTypeTree(tpt)
              this += "["
              printTypeTrees(targs, ", ")
              this += "]"
              if (args.nonEmpty) {
                this += "("
                printTrees(args, ", ")
                this += ")"
              }

            case parent@Term.Apply(Term.Select(Term.New(tpt), _, _), args) =>
              this += " "
              printTypeTree(tpt)
              if (args.nonEmpty) {
                this += "("
                printTrees(args, ", ")
                this += ")"
              }

            case parent@TypeTree() =>
              sb.append(" ")
              printTypeTree(parent)
          }
        }

        val stats1 = stats.collect {
          case stat@Definition() if !stat.flags.isParam && !stat.flags.isParamAccessor => stat
          case stat@Import(_, _) => stat
          case stat@Term() => stat
        }
        if (stats1.nonEmpty) {
          this += " {"
          indented {
            this += lineBreak()
            printTrees(stats1, lineBreak())
          }
          this += lineBreak() += "}"
        }
        this

      case tdef@TypeDef(name, rhs) =>
        this += "type "
        printTargDef(tdef)

      case vdef@ValDef(name, tpt, rhs) =>
        val flags = vdef.flags
        if (flags.isOverride) this += "override "

        if (flags.isLazy) this += "lazy "
        if (vdef.flags.isMutable) this += "var "
        else this += "val "

        this += name += ": "
        printTypeTree(tpt)
        rhs match {
          case Some(tree) =>
            this += " = "
            printTree(tree)
          case None =>
            this
        }

      case ddef@DefDef(name, targs, argss, tpt, rhs) =>
        val flags = ddef.flags
        if (flags.isOverride) sb.append("override ")

        this += "def " += name
        printTargsDefs(targs)
        val it = argss.iterator
        while (it.hasNext)
          printArgsDefs(it.next())
        this += ": "
        printTypeTree(tpt)
        rhs match {
          case Some(tree) =>
            this += " = "
            printTree(tree)
          case None =>
        }
        this

      case tree@Term.Ident(name) =>
        printType(tree.tpe)

      case Term.Select(qual, name, sig) =>
        printTree(qual)
        if (name != "<init>")
          this += "." += name
        this

      case Term.Literal(const) =>
        printConstant(const)

      case Term.This(id) =>
        this += "this" // TODO add id

      case Term.New(tpt) =>
        this += "new "
        printTypeTree(tpt)

      case Term.NamedArg(name, arg) =>
        this += name += " = "
        printTree(arg)

      case SpecialOp("throw", expr :: Nil) =>
        this += "throw "
        printTree(expr)

      case Term.Apply(fn, args) =>
        printTree(fn)
        this += "("
        printTrees(args, ", ")
        this += ")"

      case Term.TypeApply(fn, args) =>
        printTree(fn)
        this += "["
        printTypeTrees(args, ", ")
        this += "]"

      case Term.Super(qual, tptOpt) =>
        printTree(qual)
        this += ".super"
        // TODO use tptOpt?

      case Term.Typed(term, tpt) =>
        tpt.tpe match {
          case Types.Repeated(_) =>
            printTree(term)
          case _ =>
            this += "("
            printTree(term)
            this += ": "
            printTypeTree(tpt)
            this += ")"
        }

      case Term.Assign(lhs, rhs) =>
        printTree(lhs)
        this += " = "
        printTree(rhs)

      case Term.Block(stats, expr) =>
        expr match {
          case Term.Lambda(_, _) =>
            // Decompile lambda from { def annon$(...) = ...; closure(annon$, ...)}
            val DefDef(_, _, args :: Nil, _, Some(rhs)) :: Nil = stats
            this += "("
            printArgsDefs(args)
            this += " => "
            printTree(rhs)
            this += ")"

          case Term.Apply(Term.Ident("while$"), _) =>
            val DefDef("while$", _, _, _, Some(Term.If(cond, Term.Block(body :: Nil, _), _))) = stats.head
            this += "while ("
            printTree(cond)
            this += ") "
            printTree(body)

          case Term.Apply(Term.Ident("doWhile$"), _) =>
            val DefDef("doWhile$", _, _, _, Some(Term.Block(List(body), Term.If(cond, _, _)))) = stats.head
            this += "do "
            printTree(body)
            this += " while ("
            printTree(cond)
            this += ")"

          case _ =>
            this += "{"
            indented {
              if (!stats.isEmpty) {
                this += lineBreak()
                printTrees(stats, lineBreak())
              }
              this += lineBreak()
              printTree(expr)
            }
            this += lineBreak() += "}"
        }

      case Term.Inlined(call, bindings, expansion) =>
        sb.append("{ // inlined")
        indented {
          if (!bindings.isEmpty) {
            this += lineBreak()
            printTrees(bindings, lineBreak())
          }
          this += lineBreak()
          printTree(expansion)
        }
        this += lineBreak() += "}"

      case Term.Lambda(meth, tpt) =>
        // Printed in Term.Block branch
        this

      case Term.If(cond, thenp, elsep) =>
        this += "if ("
        printTree(cond)
        this += ") "
        printTree(thenp)
        this+= " else "
        printTree(elsep)

      case Term.Match(selector, cases) =>
        printTree(selector)
        this += " match {"
        indented {
          this += lineBreak()
          printCases(cases, lineBreak())
        }
        this += lineBreak() += "}"

      case Term.Try(body, cases, finallyOpt) =>
        this += "try "
        printTree(body)
        if (cases.nonEmpty) {
          this += " catch "
          printCases(cases, lineBreak())
        }
        finallyOpt match {
          case Some(t) =>
            this += " finally "
            printTree(t)
          case None =>
            this
        }

      case Term.Return(expr) =>
        this += "}"
        printTree(expr)

      case Term.Repeated(elems) =>
        printTrees(elems, ", ")

      case _ =>
        throw new MatchError(tree.show)

    }

    def printTrees(trees: List[Tree], sep: String): Buffer = {
      def printSeparated(list: List[Tree]): Unit = list match {
        case Nil =>
        case x :: Nil => printTree(x)
        case x :: xs =>
          printTree(x)
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
        case x :: Nil => printCase(x)
        case x :: xs =>
          printCase(x)
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

    def printTypeTrees(typesTrees: List[TypeTree], sep: String): Buffer = {
      def printSeparated(list: List[TypeTree]): Unit = list match {
        case Nil =>
        case x :: Nil => printTypeTree(x)
        case x :: xs =>
          printTypeTree(x)
          this += sep
          printSeparated(xs)
      }
      printSeparated(typesTrees)
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

    def printTargsDefs(targs: List[TypeDef]): Unit = {
      if (!targs.isEmpty) {
        def printSeparated(list: List[TypeDef]): Unit = list match {
          case Nil =>
          case x :: Nil => printTargDef(x)
          case x :: xs =>
            printTargDef(x)
            this += ", "
            printSeparated(xs)
        }

        this += "["
        printSeparated(targs)
        this += "]"
      }
    }

    def printTargDef(arg: TypeDef): Buffer = {
      val TypeDef(name, rhs) = arg
      this += name
      rhs match {
        case TypeBoundsTree(lo, hi) =>
          lo match {
            case TypeTree.Synthetic() => this
            case _ =>
              this += " >: "
              printTypeTree(lo)
          }
          hi match {
            case TypeTree.Synthetic() => this
            case _ =>
              this += " <: "
              printTypeTree(hi)
          }
        case tpt@TypeTree() =>
          this += " = "
          printTypeTree(tpt)
      }
    }

    def printArgsDefs(args: List[ValDef]): Unit = {
      this += "("
      args match {
        case Nil =>
        case arg :: _ =>
          if (arg.flags.isErased) this += "erased "
          if (arg.flags.isImplicit) this += "implicit "
      }

      def printSeparated(list: List[ValDef]): Unit = list match {
        case Nil =>
        case x :: Nil => printArgDef(x)
        case x :: xs =>
          printArgDef(x)
          this += ", "
          printSeparated(xs)
      }

      printSeparated(args)
      this += ")"
    }

    def printArgDef(arg: ValDef): Unit = {
      val ValDef(name, tpt, rhs) = arg
      this += name += ": "
      printTypeTree(tpt)
    }

    def printCase(caseDef: CaseDef): Buffer = {
      val CaseDef(pat, guard, body) = caseDef
      this += "case "
      printPattern(pat)
      guard match {
        case Some(t) =>
          this += " if "
          printTree(t)
        case None =>
      }
      this += " =>"
      indented {
        this += lineBreak()
        printTree(body)
      }
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
        this += name += ": "
        printTypeTree(tpt)

      case Pattern.Bind(name, pattern) =>
        this += name += "@ "
        printPattern(pattern)

      case Pattern.Unapply(fun, implicits, patterns) =>
        printTree(fun)
        this += "("
        printPatterns(patterns, ", ")
        this += ")"

      case Pattern.Alternative(trees) =>
        printPatterns(trees, " | ")

      case Pattern.TypeTest(tpt) =>
        this

    }

    def printConstant(const: Constant): Buffer = const match {
      case Constant.Unit() => this += "()"
      case Constant.Null() => this += "null"
      case Constant.Boolean(v) => this += v.toString
      case Constant.Byte(v) => this += v
      case Constant.Short(v) => this += v
      case Constant.Int(v) => this += v
      case Constant.Long(v) => this += v += "L"
      case Constant.Float(v) => this += v
      case Constant.Double(v) => this += v
      case Constant.Char(v) => this += '\'' += v.toString += '\'' // TODO escape char
      case Constant.String(v) => this += '"' += v.toString += '"' // TODO escape string
    }

    def printTypeOrBoundsTree(tpt: TypeOrBoundsTree): Buffer = tpt match {
      case TypeBoundsTree(lo, hi) =>
        this += " >: "
        printTypeTree(lo)
        this += " <: "
        printTypeTree(hi)
      case tpt@Type() =>
        printType(tpt)
    }

    def printTypeTree(tree: TypeTree): Buffer = tree match {
      case TypeTree.Synthetic() =>
        printType(tree.tpe)

      case TypeTree.TypeIdent(name) =>
        printType(tree.tpe)

      case TypeTree.TypeSelect(qual, name) =>
        (qual: Any) match {
          case qual @ TypeTree.TypeIdent(_) => printTypeTree(qual) // FIXME: qual is of type Tree buy we are getting a TypeTree qualifier
          case _ => printTree(qual)
        }
        this += "." += name

      case TypeTree.Singleton(ref) =>
        printTree(ref)

      case TypeTree.Refined(tpt, refinements) =>
        printTypeTree(tpt)
        this += " {"
        indented {
          this += lineBreak()
          printTrees(refinements, "; ")
        }
        this += lineBreak() += "}"

      case TypeTree.Applied(tpt, args) =>
        printTypeTree(tpt)
        this += "["
        printTypeTrees(args, ", ")
        this += "]"

      case TypeTree.Annotated(tpt, annots) =>
        printTypeTree(tpt)
        // TODO print annots

      case TypeTree.And(left, right) =>
        printTypeTree(left)
        this += " & "
        printTypeTree(right)

      case TypeTree.Or(left, right) =>
        printTypeTree(left)
        this += " | "
        printTypeTree(right)

      case TypeTree.ByName(result) =>
        this += "=> "
        printTypeTree(result)

      case _ =>
        throw new MatchError(tree.show)

    }

    def printTypeOrBound(tpe: TypeOrBounds): Buffer = tpe match {
      case tpe@TypeBounds(lo, hi) =>
        this += " >: "
        printType(lo)
        this += " <: "
        printType(hi)
      case tpe@Type() => printType(tpe)
    }

    def printType(tpe: Type): Buffer = tpe match {
      case Type.ConstantType(const) =>
        printConstant(const)

      case Type.SymRef(sym, prefix) =>
        prefix match {
          case Type.ThisType(Types.EmptyPackage() | Types.RootPackage()) =>
          case prefix@Type.SymRef(ClassDef(_, _, _, _, _), _) =>
            printType(prefix)
            this += "#"
          case prefix@Type() =>
            printType(prefix)
            this += "."
          case prefix@NoPrefix() =>
        }
        printDefinitionName(sym)

      case Type.TermRef(name, prefix) =>
        prefix match {
          case prefix@Type() =>
            printType(prefix)
            if (name != "package")
              this += "." += name
            this
          case NoPrefix() =>
            this += name
        }

      case Type.TypeRef(name, prefix) =>
        prefix match {
          case NoPrefix() =>
          case prefix@Type() =>
            printType(prefix)
            this += "."
        }
        this += name.stripSuffix("$")

      case Type.Refinement(parent, name, info) =>
        printType(parent)
        // TODO add refinements

      case Type.AppliedType(tp, args) =>
        printType(tp)
        this += "["
        printTypesOrBounds(args, ", ")
        this += "]"

      case Type.AnnotatedType(tp, annot) =>
        printType(tp)

      case Type.AndType(left, right) =>
        printType(left)
        this += " & "
        printType(right)

      case Type.OrType(left, right) =>
        printType(left)
        this += " | "
        printType(right)

      case Type.ByNameType(tp) =>
        this += " => "
        printType(tp)

      case Type.ThisType(tp) =>
        printType(tp)

      case _ =>
        throw new MatchError(tpe.show)
    }

    def printImportSelector(sel: ImportSelector): Buffer = sel match {
      case SimpleSelector(Id(name)) => this += name
      case OmitSelector(Id(name)) => this += name += " => _"
      case RenameSelector(Id(name), Id(newName)) => this += name += " => " += newName
    }

    def printDefinitionName(sym: Definition): Buffer = sym match {
      case ValDef(name, _, _) => this += name
      case DefDef(name, _, _, _, _) => this += name
      case ClassDef(name, _, _, _, _) => this += name.stripSuffix("$")
      case TypeDef(name, _) => this += name
      case PackageDef(name, _) => this += name
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

  }


  private object SpecialOp {
    def unapply(arg: Term)(implicit ctx: Context): Option[(String, List[Term])] = arg match {
      case arg@Term.Apply(fn, args) =>
        fn.tpe match {
          case Type.SymRef(DefDef(op, _, _, _, _), Type.ThisType(Type.SymRef(PackageDef("<special-ops>", _), NoPrefix()))) =>
            Some((op, args))
          case _ => None
        }
      case _ => None
    }
  }

  // TODO Provide some of these in scala.tasty.Tasty.scala and implement them using checks on symbols for performance
  private object Types {

    object JavaLangObject {
      def unapply(tpe: Type)(implicit ctx: Context): Boolean = tpe match {
        case Type.TypeRef("Object", Type.SymRef(PackageDef("lang", _), Type.ThisType(Type.SymRef(PackageDef("java", _), NoPrefix())))) => true
        case _ => false
      }
    }

    object Repeated {
      def unapply(tpe: Type)(implicit ctx: Context): Option[Type] = tpe match {
        case Type.AppliedType(Type.TypeRef("<repeated>", ScalaPackage()), (tp@Type()) :: Nil) => Some(tp)
        case _ => None
      }
    }

    object ScalaPackage {
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Boolean = tpe match {
        case Type.SymRef(PackageDef("scala", _), Type.ThisType(RootPackage())) => true
        case _ => false
      }
    }

    object RootPackage {
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Boolean = tpe match {
        case Type.SymRef(PackageDef("<root>", _), NoPrefix()) => true
        case _ => false
      }
    }

    object EmptyPackage {
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Boolean = tpe match {
        case Type.SymRef(PackageDef("<empty>", _), NoPrefix() | Type.ThisType(RootPackage())) => true
        case _ => false
      }
    }
  }


}
