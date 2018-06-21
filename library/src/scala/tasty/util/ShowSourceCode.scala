package scala.tasty
package util

import scala.annotation.switch

class ShowSourceCode[T <: Tasty with Singleton](tasty0: T) extends Show[T](tasty0) {
  import tasty.{rootContext => _, _}

  def showTree(tree: Tree)(implicit ctx: Context): String =
    (new Buffer).printTree(tree).result()

  def showCaseDef(caseDef: CaseDef)(implicit ctx: Context): String =
    (new Buffer).printCaseDef(caseDef).result()

  def showPattern(pattern: Pattern)(implicit ctx: Context): String =
    (new Buffer).printPattern(pattern).result()

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
    def doubleLineBreak(): String = "\n\n" + ("  " * indent)

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
          this += lineBreak() += "}"
        }

      case Import(expr, selectors) =>
        this += "import "
        printTree(expr)
        this += "."
        printImportSelectors(selectors)

      case cdef @ ClassDef(name, DefDef(_, targs, argss, _, _), parents, self, stats) =>
        printDefAnnotations(cdef)

        val flags = cdef.flags
        if (flags.isImplicit) this += "implicit "
        if (flags.isFinal && !flags.isObject) this += "final "
        if (flags.isCase) this += "case "

        if (flags.isObject) this += "object " += name.stripSuffix("$")
        else if (flags.isTrait) this += "trait " += name
        else this += "class " += name

        if (!flags.isObject) {
          printTargsDefs(targs)
          val it = argss.iterator
          while (it.hasNext)
            printArgsDefs(it.next())
        }

        val parents1 = parents.filter {
          case Term.Apply(Term.Select(Term.New(tpt), _, _), _) => !Types.JavaLangObject.unapply(tpt.tpe)
          case TypeTree.TermSelect(Term.Select(Term.Ident("_root_"), "scala", _), "Product") => false
          case _ => true
        }
        if (parents1.nonEmpty)
          this += " extends "

        def printParent(parent: Parent): Unit = parent match {
          case parent @ Term.TypeApply(fun, targs) =>
            printParent(fun)
            this += "["
            printTypeOrBoundsTrees(targs, ", ")
            this += "]"

          case parent @ Term.Apply(fun, args) =>
            printParent(fun)
            this += "("
            printTrees(args, ", ")
            this += ")"

          case parent @ Term.Select(Term.New(tpt), _, _) =>
            printTypeTree(tpt)

          case parent @ TypeTree() =>
            printTypeTree(parent)

          case parent @ Term() => throw new MatchError(parent.show)
        }

        def printSeparated(list: List[Parent]): Unit = list match {
          case Nil =>
          case x :: Nil => printParent(x)
          case x :: xs =>
            printParent(x)
            this += " with "
            printSeparated(xs)
        }
        printSeparated(parents1)

        def keepDefinition(d: Definition): Boolean = {
          val flags = d.flags
          def isCaseClassUnOverridableMethod: Boolean = {
            // Currently the compiler does not allow overriding some of the methods generated for case classes
            d.flags.isSynthetic &&
            (d match {
              case DefDef("apply" | "unapply", _, _, _, _) if d.owner.flags.isObject => true
              case DefDef(n, _, _, _, _) if d.owner.flags.isCase =>
                n == "copy" ||
                n.matches("copy\\$default\\$[1-9][0-9]*") || // default parameters for the copy method
                n.matches("_[1-9][0-9]*") // Getters from Product
              case _ => false
            })
          }
          def isInnerModuleObject = d.flags.isLazy && d.flags.isObject
          !flags.isParam && !flags.isParamAccessor && !isCaseClassUnOverridableMethod && !isInnerModuleObject
        }
        val stats1 = stats.collect {
          case stat@Definition() if keepDefinition(stat) => stat
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

      case tdef @ TypeDef(name, rhs) =>
        printDefAnnotations(tdef)
        this += "type "
        printTargDef(tdef, isMember = true)

      case vdef @ ValDef(name, tpt, rhs) =>
        printDefAnnotations(vdef)

        val flags = vdef.flags
        if (flags.isImplicit) this += "implicit "
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

      case While(cond, stats) =>
        this += "while ("
        printTree(cond)
        this += ") "
        stats match {
          case stat :: Nil =>
            printTree(stat)
          case stats =>
            this += "{"
            indented {
              printStats(stats.init, stats.last)
            }
            this += lineBreak() += "}"
        }

      case DoWhile(stats, cond) =>
        this += "do "
        stats match {
          case stat :: Nil =>
            printTree(stat)
          case stats =>
            this += "{"
            indented {
              printStats(stats.init, stats.last)
            }
            this += lineBreak() += "}"
        }
        this += " while ("
        printTree(cond)
        this += ")"

      case ddef @ DefDef(name, targs, argss, tpt, rhs) =>
        printDefAnnotations(ddef)

        val flags = ddef.flags
        if (flags.isImplicit) this += "implicit "
        if (flags.isInline) this += "inline "
        if (flags.isOverride) this += "override "

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

      case tree @ Term.Ident(_) =>
        printType(tree.tpe)

      case Term.Select(qual, name, sig) =>
        printTree(qual)
        if (name != "<init>" && name != "package")
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
        fn match {
          case Term.Select(Term.New(TypeTree.Applied(_, _)), "<init>", _) =>
            // type bounds already printed in `fn`
            this
          case _ =>
            this += "["
            printTypeOrBoundsTrees(args, ", ")
            this += "]"
        }

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
            this += ")"
        }

      case Term.Assign(lhs, rhs) =>
        printTree(lhs)
        this += " = "
        printTree(rhs)

      case Term.Block(stats0, expr) =>
        def isLoopEntryPoint(tree: Tree): Boolean = tree match {
          case Term.Apply(Term.Ident("while$" | "doWhile$"), _) => true
          case _ => false
        }

        val stats = stats0.filterNot(isLoopEntryPoint)

        expr match {
          case Term.Lambda(_, _) =>
            // Decompile lambda from { def annon$(...) = ...; closure(annon$, ...)}
            val DefDef(_, _, args :: Nil, _, Some(rhs)) :: Nil = stats
            this += "("
            printArgsDefs(args)
            this += " => "
            printTree(rhs)
            this += ")"
          case _ =>
            this += "{"
            val (stats1, expr1) =
              if (isLoopEntryPoint(expr)) (stats.init, stats.last)
              else (stats, expr)
            indented {
              printStats(stats1, expr1)
            }
            this += lineBreak() += "}"
        }

      case Term.Inlined(call, bindings, expansion) =>
        this += "{ // inlined"
        indented {
          printStats(bindings, expansion)
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
        this += "return "
        printTree(expr)

      case Term.Repeated(elems) =>
        printTrees(elems, ", ")

      case _ =>
        throw new MatchError(tree.show)

    }

    def printStats(stats: List[Tree], expr: Tree): Unit = {
      def printSeparator(nextStats: List[Tree]) = {
        // Avoid accidental application of opening `{` on next line with a double break
        val next = if (nextStats.isEmpty) expr else nextStats.head
        next match {
          case Term.Block(DefDef("while$" | "doWhile$", _, _, _, _) :: Nil, _) => this += lineBreak()
          case Term.Block(_, _) => this += doubleLineBreak()
          case Term.Inlined(_, _, _) => this += doubleLineBreak()
          case _ => this += lineBreak()
        }
      }
      def printSeparated(list: List[Tree]): Unit = list match {
        case Nil =>
          printTree(expr)
        case x :: xs =>
          printTree(x)
          printSeparator(xs)
          printSeparated(xs)
      }

      this += lineBreak()
      printSeparated(stats)
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
        case x :: Nil => printCaseDef(x)
        case x :: xs =>
          printCaseDef(x)
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

    def printTypeOrBoundsTrees(typesTrees: List[TypeOrBoundsTree], sep: String): Buffer = {
      def printSeparated(list: List[TypeOrBoundsTree]): Unit = list match {
        case Nil =>
        case x :: Nil => printTypeOrBoundsTree(x)
        case x :: xs =>
          printTypeOrBoundsTree(x)
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

    def printTargDef(arg: TypeDef, isMember: Boolean = false): Buffer = {
      val TypeDef(name, rhs) = arg
      def printBounds(bounds: TypeBoundsTree): Buffer = {
        val TypeBoundsTree(lo, hi) = bounds
        lo match {
          case TypeTree.Synthetic() =>
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
      }
      this += name
      rhs match {
        case rhs @ TypeBoundsTree(lo, hi) => printBounds(rhs)
        case rhs @ SyntheticBounds() =>
          printTypeOrBound(rhs.tpe)
        case rhs @ TypeTree.TypeLambdaTree(tparams, body) =>
          def printParam(t: TypeOrBoundsTree): Unit = t match {
            case t @ TypeBoundsTree(_, _) => printBounds(t)
            case t @ TypeTree() => printTypeTree(t)
          }
          def printSeparated(list: List[TypeDef]): Unit = list match {
            case Nil =>
            case x :: Nil =>
              val TypeDef(name, trhs) = x
              this += name
              printParam(trhs)
            case x :: xs =>
              val TypeDef(name, trhs) = x
              this += name
              printParam(trhs)
              this += ", "
              printSeparated(xs)
          }
          this += "["
          printSeparated(tparams)
          this += "]"
          if (isMember) {
            this += " = "
            printTypeOrBoundsTree(body)
          }
          else this
        case rhs @ TypeTree() =>
          this += " = "
          printTypeTree(rhs)
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

    def printArgDef(arg: ValDef): Unit = {
      val ValDef(name, tpt, rhs) = arg
      this += name += ": "
      printTypeTree(tpt)
    }

    def printCaseDef(caseDef: CaseDef): Buffer = {
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
        body match {
          case Term.Block(stats, expr) =>
            printStats(stats, expr)
          case body =>
            this += lineBreak()
            printTree(body)
        }
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
        this += name += " @ "
        printPattern(pattern)

      case Pattern.Unapply(fun, implicits, patterns) =>
        fun match {
          case Term.Select(extractor, "unapply" | "unapplySeq", _) => printTree(extractor)
          case Term.TypeApply(Term.Select(extractor, "unapply" | "unapplySeq", _), _) => printTree(extractor)
          case _ => throw new MatchError(fun.show)
        }
        this += "("
        printPatterns(patterns, ", ")
        this += ")"

      case Pattern.Alternative(trees) =>
        printPatterns(trees, " | ")

      case Pattern.TypeTest(tpt) =>
        this += "_: "
        printTypeOrBoundsTree(tpt)

      case _ =>
        throw new MatchError(pattern.show)

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
      case Constant.Char(v) => this += '\'' += escapedChar(v) += '\''
      case Constant.String(v) => this += '"' += escapedString(v) += '"'
      case Constant.ClassTag(v) =>
        this += "classOf["
        printType(v)
        this += "]"
    }

    def printTypeOrBoundsTree(tpt: TypeOrBoundsTree): Buffer = tpt match {
      case TypeBoundsTree(lo, hi) =>
        this += "_ >: "
        printTypeTree(lo)
        this += " <: "
        printTypeTree(hi)
      case tpt @ SyntheticBounds() =>
        printTypeOrBound(tpt.tpe)
      case tpt @ TypeTree() =>
        printTypeTree(tpt)
    }

    def printTypeTree(tree: TypeTree): Buffer = tree match {
      case TypeTree.Synthetic() =>
        def printTypeAndAnnots(tpe: Type): Buffer = tpe match {
          case Type.AnnotatedType(tp, annot) =>
            printTypeAndAnnots(tp)
            this += " "
            printAnnotation(annot)
          case tpe @ Type.TypeRef(name, _) if name.endsWith("$") =>
            printType(tpe)
            this += ".type"
          case Type.SymRef(ClassDef("Null$" | "Nothing$", _, _, _, _), Type.ThisType(Type.SymRef(PackageDef("runtime", _), NoPrefix()))) =>
            // scala.runtime.Null$ and scala.runtime.Nothing$ are not modules, those are their actual names
            printType(tpe)
          case tpe @ Type.SymRef(ClassDef(name, _, _, _, _), _) if name.endsWith("$") =>
            printType(tpe)
            this += ".type"
          case tpe => printType(tpe)
        }
        printTypeAndAnnots(tree.tpe)

      case TypeTree.TypeIdent(name) =>
        printType(tree.tpe)

      case TypeTree.TermSelect(qual, name) =>
        printTree(qual) += "." += name

      case TypeTree.TypeSelect(qual, name) =>
        printTypeTree(qual) += "#" += name

      case TypeTree.Singleton(ref) =>
        printTree(ref)
        ref match {
          case Term.Literal(_) => this
          case _ => this += ".type"
        }

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
        printTypeOrBoundsTrees(args, ", ")
        this += "]"

      case TypeTree.Annotated(tpt, annot) =>
        val Annotation(ref, args) = annot
        printTypeTree(tpt)
        this += " "
        printAnnotation(annot)

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

      case TypeTree.TypeLambdaTree(tparams, body) =>
        printTargsDefs(tparams)
        this += " => "
        printTypeOrBoundsTree(body)

      case TypeTree.Bind(name, _) =>
        this += name

      case _ =>
        throw new MatchError(tree.show)

    }

    def printTypeOrBound(tpe: TypeOrBounds): Buffer = tpe match {
      case tpe@TypeBounds(lo, hi) =>
        this += "_ >: "
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
          case Types.EmptyPrefix() =>
          case prefix@Type.SymRef(ClassDef(_, _, _, _, _), _) =>
            printType(prefix)
            this += "#"
          case prefix@Type() =>
            printType(prefix)
            this += "."
        }
        printDefinitionName(sym)

      case Type.TermRef(name, prefix) =>
        prefix match {
          case Type.ThisType(Types.EmptyPackage()) =>
            this += name
          case prefix @ Type() =>
            printType(prefix)
            if (name != "package")
              this += "." += name
            this
          case NoPrefix() =>
            this += name
        }

      case Type.TypeRef(name, prefix) =>
        prefix match {
          case NoPrefix() | Type.ThisType(Types.EmptyPackage()) =>
          case prefix@Type() => printType(prefix) += "."
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
        val Annotation(ref, args) = annot
        printType(tp)
        this += " "
        printAnnotation(annot)

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
        tp match {
          case Type.SymRef(cdef @ ClassDef(_, _, _, _, _), _) if !cdef.flags.isObject => this += ".this"
          case _ => this
        }

      case Type.TypeLambda(paramNames, tparams, body) =>
        this += "["
        def printBounds(bounds: TypeBounds): Buffer = {
          val TypeBounds(lo, hi) = bounds
          this += " >: "
          printType(lo)
          this += " <: "
          printType(hi)
        }
        def printSeparated(list: List[(String, TypeBounds)]): Unit = list match {
          case Nil =>
          case (name, bounds) :: Nil =>
            this += name
            printBounds(bounds)
          case (name, bounds) :: xs =>
            this += name
            printBounds(bounds)
            this += ", "
            printSeparated(xs)
        }
        printSeparated(paramNames.zip(tparams))
        this += "] => "
        printTypeOrBound(body)

      case Type.ParamRef(lambda, idx) =>
        lambda match {
          case Type.MethodType(params, _, _) => this += params(idx)
          case Type.PolyType(params, _, _) => this += params(idx)
          case Type.TypeLambda(params, _, _) => this += params(idx)
        }

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

    def printAnnotation(annot: Term): Buffer = {
      val Annotation(ref, args) = annot
      this += "@"
      printTypeTree(ref)
      this += "("
      printTrees(args, ", ")
      this += ")"
    }

    def printDefAnnotations(definition: Definition): Buffer = {
      val annots = definition.annots.filter {
        case Annotation(annot, _) =>
          annot.tpe match {
            case Type.TypeRef(_, Type.SymRef(PackageDef("internal", _), Type.ThisType(Type.SymRef(PackageDef("annotation", _), NoPrefix())))) => false
            case Type.TypeRef("inline", Types.ScalaPackage()) => false
            case _ => true
          }
        case x => throw new MatchError(x.show)
      }
      printAnnotations(annots)
      if (annots.nonEmpty) this += " "
      else this
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

  private object While {
    def unapply(arg: Tree)(implicit ctx: Context): Option[(Term, List[Statement])] = arg match {
      case DefDef("while$", _, _, _, Some(Term.If(cond, Term.Block(bodyStats, _), _))) => Some((cond, bodyStats))
      case Term.Block(List(tree), _) => unapply(tree)
      case _ => None
    }
  }

  private object DoWhile {
    def unapply(arg: Tree)(implicit ctx: Context): Option[(List[Statement], Term)] = arg match {
      case DefDef("doWhile$", _, _, _, Some(Term.Block(body, Term.If(cond, _, _)))) => Some((body, cond))
      case Term.Block(List(tree), _) => unapply(tree)
      case _ => None
    }
  }

  private object Annotation {
    def unapply(arg: Tree)(implicit ctx: Context): Option[(TypeTree, List[Term])] = arg match {
      case Term.New(annot) => Some((annot, Nil))
      case Term.Apply(Term.Select(Term.New(annot), "<init>", _), args) => Some((annot, args))
      case Term.Apply(Term.TypeApply(Term.Select(Term.New(annot), "<init>", _), targs), args) => Some((annot, args))
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

    object EmptyPrefix {
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Boolean = tpe match {
        case NoPrefix() | Type.ThisType(Types.EmptyPackage() | Types.RootPackage()) => true
        case _ => false
      }
    }
  }


}
