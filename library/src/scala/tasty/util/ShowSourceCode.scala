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

    def inParens(body: => Unit): Buffer = {
      this += "("
      body
      this += ")"
    }

    def inSquare(body: => Unit): Buffer = {
      this += "["
      body
      this += "]"
    }

    def inBlock(body: => Unit): Buffer = {
      this += " {"
      indented {
        this += lineBreak()
        body
      }
      this += lineBreak() += "}"
    }

    def result(): String = sb.result()

    def lineBreak(): String = "\n" + ("  " * indent)
    def doubleLineBreak(): String = "\n\n" + ("  " * indent)

    def printTree(tree: Tree): Buffer = tree match {
      case PackageObject(body)=>
        printTree(body) // Print package object

      case PackageClause(Term.Ident(name), (inner @ PackageClause(_, _)) :: Nil) if name != "<empty>" && PackageObject.unapply(inner).isEmpty =>
        // print inner package as `package outer.inner { ... }`
        printTree(inner)

      case tree @ PackageClause(name, stats) =>
        val stats1 = stats.collect {
          case IsPackageClause(stat) => stat
          case IsDefinition(stat) if !(stat.symbol.flags.isObject && stat.symbol.flags.isLazy) => stat
          case stat @ Import(_, _) => stat
        }
        name match {
          case Term.Ident("<empty>") =>
            printTrees(stats1, lineBreak())
          case _ =>
            this += "package "
            printType(name.tpe)
            inBlock(printTrees(stats1, lineBreak()))
        }

      case Import(expr, selectors) =>
        this += "import "
        printTree(expr)
        this += "."
        printImportSelectors(selectors)

      case IsClassDef(cdef @ ClassDef(name, DefDef(_, targs, argss, _, _), parents, self, stats)) =>
        printDefAnnotations(cdef)

        val flags = cdef.symbol.flags
        if (flags.isImplicit) this += "implicit "
        if (flags.isSealed) this += "sealed "
        if (flags.isFinal && !flags.isObject) this += "final "
        if (flags.isCase) this += "case "

        if (name == "package$") {
          this += "package object " += cdef.symbol.owner.name.stripSuffix("$")
        }
        else if (flags.isObject) this += "object " += name.stripSuffix("$")
        else if (flags.isTrait) this += "trait " += name
        else if (flags.isAbstract) this += "abstract class " += name
        else this += "class " += name

        if (!flags.isObject) {
          printTargsDefs(targs)
          val it = argss.iterator
          while (it.hasNext)
            printArgsDefs(it.next())
        }

        val parents1 = parents.filter {
          case IsTerm(Term.Apply(Term.Select(Term.New(tpt), _, _), _)) => !Types.JavaLangObject.unapply(tpt.tpe)
          case IsTypeTree(TypeTree.TermSelect(Term.Select(Term.Ident("_root_"), "scala", _), "Product")) => false
          case _ => true
        }
        if (parents1.nonEmpty)
          this += " extends "

        def printParent(parent: Parent): Unit = parent match {
          case IsTypeTree(parent) =>
            printTypeTree(parent)
          case IsTerm(Term.TypeApply(fun, targs)) =>
            printParent(fun)
            inSquare(printTypeOrBoundsTrees(targs, ", "))
          case IsTerm(Term.Apply(fun, args)) =>
            printParent(fun)
            inParens(printTrees(args, ", "))
          case IsTerm(Term.Select(Term.New(tpt), _, _)) =>
            printTypeTree(tpt)
          case IsTerm(parent) =>
            throw new MatchError(parent.show)
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
          val flags = d.symbol.flags
          def isCaseClassUnOverridableMethod: Boolean = {
            // Currently the compiler does not allow overriding some of the methods generated for case classes
            d.symbol.flags.isSynthetic &&
            (d match {
              case DefDef("apply" | "unapply", _, _, _, _) if d.symbol.owner.flags.isObject => true
              case DefDef(n, _, _, _, _) if d.symbol.owner.flags.isCase =>
                n == "copy" ||
                n.matches("copy\\$default\\$[1-9][0-9]*") || // default parameters for the copy method
                n.matches("_[1-9][0-9]*") // Getters from Product
              case _ => false
            })
          }
          def isInnerModuleObject = d.symbol.flags.isLazy && d.symbol.flags.isObject
          !flags.isParam && !flags.isParamAccessor && !flags.isFieldAccessor && !isCaseClassUnOverridableMethod && !isInnerModuleObject
        }
        val stats1 = stats.collect {
          case IsDefinition(stat) if keepDefinition(stat) => stat
          case stat @ Import(_, _) => stat
          case IsTerm(stat) => stat
        }

        def printBody(printSelf: Boolean) = {
          this += " {"
          indented {
            if (printSelf) {
              val Some(ValDef(name, tpt, _)) = self
              indented {
                val name1 = if (name == "_") "this" else name
                this += " " += name1 += ": "
                printTypeTree(tpt)
                this += " =>"
              }
            }
            this += lineBreak()
            printTrees(stats1, lineBreak())
          }
          this += lineBreak() += "}"
        }
        self match {
          case Some(ValDef(_, TypeTree.Singleton(_), _)) =>
            if (stats1.nonEmpty)
              printBody(printSelf = false)
          case Some(ValDef(_, _, _)) =>
            printBody(printSelf = true)
          case _ =>
            if (stats1.nonEmpty)
              printBody(printSelf = false)
        }
        this

      case IsTypeDef(tdef @ TypeDef(name, rhs)) =>
        printDefAnnotations(tdef)
        this += "type "
        printTargDef(tdef, isMember = true)

      case IsValDef(vdef @ ValDef(name, tpt, rhs)) =>
        printDefAnnotations(vdef)

        val flags = vdef.symbol.flags
        if (flags.isImplicit) this += "implicit "
        if (flags.isOverride) this += "override "

        printProtectedOrPrivate(vdef)

        if (flags.isLazy) this += "lazy "
        if (vdef.symbol.flags.isMutable) this += "var "
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

      case Term.While(cond, body) =>
        (cond, body) match {
          case (Term.Block(Term.Block(Nil, body1) :: Nil, Term.Block(Nil, cond1)), Term.Literal(Constant.Unit())) =>
            this += "do "
            printTree(body1) += " while "
            inParens(printTree(cond1))
          case _ =>
            this += "while "
            inParens(printTree(cond)) += " "
            printTree(body)
        }

      case IsDefDef(ddef @ DefDef(name, targs, argss, tpt, rhs)) =>
        printDefAnnotations(ddef)

        val isConstructor = name == "<init>"

        val flags = ddef.symbol.flags
        if (flags.isImplicit) this += "implicit "
        if (flags.isInline) this += "inline "
        if (flags.isOverride) this += "override "

        printProtectedOrPrivate(ddef)

        this += "def " += (if (isConstructor) "this" else name)
        printTargsDefs(targs)
        val it = argss.iterator
        while (it.hasNext)
          printArgsDefs(it.next())
        if (!isConstructor) {
          this += ": "
          printTypeTree(tpt)
        }
        rhs match {
          case Some(tree) =>
            this += " = "
            printTree(tree)
          case None =>
        }
        this

      case IsTerm(tree @ Term.Ident(_)) =>
        printType(tree.tpe)

      case Term.Select(qual, name, sig) =>
        printTree(qual)
        if (name != "<init>" && name != "package")
          this += "." += name
        this

      case Term.Literal(const) =>
        printConstant(const)

      case Term.This(id) =>
        id match {
          case Some(x) =>
            this += x.name.stripSuffix("$") += "."
          case None =>
        }
        this += "this"

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
        fn match {
          case Term.Select(Term.This(_), "<init>", _) => this += "this" // call to constructor inside a constructor
          case _ => printTree(fn)
        }
        val args1 = args match {
          case init :+ Term.Typed(Term.Repeated(Nil), _) => init // drop empty var args at the end
          case _ => args
        }

        inParens(printTrees(args1, ", "))

      case Term.TypeApply(fn, args) =>
        printTree(fn)
        fn match {
          case Term.Select(Term.New(TypeTree.Applied(_, _)), "<init>", _) =>
            // type bounds already printed in `fn`
            this
          case _ =>
            inSquare(printTypeOrBoundsTrees(args, ", "))
        }

      case Term.Super(qual, idOpt) =>
        qual match {
          case Term.This(Some(Id(name))) => this += name += "."
          case Term.This(None) =>
        }
        this += "super"
        for (id <- idOpt)
          inSquare(this += id.name)
        this

      case Term.Typed(term, tpt) =>
        tpt.tpe match {
          case Types.Repeated(_) =>
            printTree(term)
          case _ =>
            inParens {
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
            }
        }

      case Term.Assign(lhs, rhs) =>
        printTree(lhs)
        this += " = "
        printTree(rhs)

      case Term.Block(stats0, expr) =>
        val stats = stats0.filter {
          case IsValDef(tree) => !tree.symbol.flags.isObject
          case _ => true
        }

        expr match {
          case Term.Lambda(_, _) =>
            // Decompile lambda from { def annon$(...) = ...; closure(annon$, ...)}
            assert(stats.size == 1)
            val DefDef(_, _, args :: Nil, _, Some(rhs)) :: Nil = stats
            inParens {
              printArgsDefs(args)
              this += " => "
              printTree(rhs)
            }
          case _ =>
            this += "{"
            indented {
              printStats(stats, expr)
            }
            this += lineBreak() += "}"
        }

      case Term.Inlined(call, bindings, expansion) => // FIXME: Don't print Inlined with empty calls?
        this += "{ // inlined"
        indented {
          printStats(bindings, expansion)
        }
        this += lineBreak() += "}"

      case Term.Lambda(meth, tpt) =>
        // Printed in Term.Block branch
        this

      case Term.If(cond, thenp, elsep) =>
        this += "if "
        inParens(printTree(cond))
        this += " "
        printTree(thenp)
        this+= " else "
        printTree(elsep)

      case Term.Match(selector, cases) =>
        printTree(selector)
        this += " match"
        inBlock(printCases(cases, lineBreak()))

      case Term.Try(body, cases, finallyOpt) =>
        this += "try "
        printTree(body)
        if (cases.nonEmpty) {
          this += " catch"
          inBlock(printCases(cases, lineBreak()))
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
      def printSeparator(next: Tree): Unit = {
        // Avoid accidental application of opening `{` on next line with a double break
        next match {
          case Term.Block(_, _) => this += doubleLineBreak()
          case Term.Inlined(_, _, _) => this += doubleLineBreak()
          case Term.Select(qual, _, _) => printSeparator(qual)
          case Term.Apply(fn, _) => printSeparator(fn)
          case Term.TypeApply(fn, _) => printSeparator(fn)
          case _ => this += lineBreak()
        }
      }
      def printSeparated(list: List[Tree]): Unit = list match {
        case Nil =>
          printTree(expr)
        case x :: xs =>
          printTree(x)
          printSeparator(if (xs.isEmpty) expr else xs.head)
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

        inSquare(printSeparated(targs))
      }
    }

    def printTargDef(arg: TypeDef, isMember: Boolean = false): Buffer = {
      this += arg.name
      arg.rhs match {
        case IsTypeBoundsTree(rhs) => printBoundsTree(rhs)
        case rhs @ SyntheticBounds() =>
          printTypeOrBound(rhs.tpe)
        case rhs @ TypeTree.TypeLambdaTree(tparams, body) =>
          def printParam(t: TypeOrBoundsTree): Unit = t match {
            case IsTypeBoundsTree(t) => printBoundsTree(t)
            case IsTypeTree(t) => printTypeTree(t)
          }
          def printSeparated(list: List[TypeDef]): Unit = list match {
            case Nil =>
            case x :: Nil =>
              this += x.name
              printParam(x.rhs)
            case x :: xs =>
              this += x.name
              printParam(x.rhs)
              this += ", "
              printSeparated(xs)
          }
          inSquare(printSeparated(tparams))
          if (isMember) {
            this += " = "
            printTypeOrBoundsTree(body)
          }
          else this
        case IsTypeTree(rhs) =>
          this += " = "
          printTypeTree(rhs)
      }
    }

    def printArgsDefs(args: List[ValDef]): Unit = inParens {
      args match {
        case Nil =>
        case arg :: _ =>
          if (arg.symbol.flags.isErased) this += "erased "
          if (arg.symbol.flags.isImplicit) this += "implicit "
      }

      def printSeparated(list: List[ValDef]): Unit = list match {
        case Nil =>
        case x :: Nil => printParamDef(x)
        case x :: xs =>
          printParamDef(x)
          this += ", "
          printSeparated(xs)
      }

      printSeparated(args)
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

    def printParamDef(arg: ValDef): Unit = {
      val name = arg.name
      arg.symbol.owner.tree match {
        case Some(DefDef("<init>", _, _, _, _)) =>
          val Some(ClassDef(_, _, _, _, body)) = arg.symbol.owner.owner.tree
          body.collectFirst {
            case IsValDef(vdef @ ValDef(`name`, _, _)) if vdef.symbol.flags.isParamAccessor =>
              if (!vdef.symbol.flags.isLocal) {
                var printedPrefix = false
                if (vdef.symbol.flags.isOverride) {
                  this += "override "
                  printedPrefix = true
                }
                printedPrefix  |= printProtectedOrPrivate(vdef)
                if (vdef.symbol.flags.isMutable) this += "var "
                else if (printedPrefix || !vdef.symbol.flags.isCaseAcessor) this += "val "
                else this // val not explicitly needed
              }
          }
        case _ =>
      }

      this += name += ": "
      printTypeTree(arg.tpt)
    }

    def printCaseDef(caseDef: CaseDef): Buffer = {
      this += "case "
      printPattern(caseDef.pattern)
      caseDef.guard match {
        case Some(t) =>
          this += " if "
          printTree(t)
        case None =>
      }
      this += " =>"
      indented {
        caseDef.rhs match {
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
        inParens(printPatterns(patterns, ", "))

      case Pattern.Alternative(trees) =>
        inParens(printPatterns(trees, " | "))

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
      case Constant.Float(v) => this += v += "f"
      case Constant.Double(v) => this += v
      case Constant.Char(v) => this += '\'' += escapedChar(v) += '\''
      case Constant.String(v) => this += '"' += escapedString(v) += '"'
      case Constant.ClassTag(v) =>
        this += "classOf"
        inSquare(printType(v))
      case Constant.Symbol(v) =>
        this += "'" += v.name
    }

    def printTypeOrBoundsTree(tpt: TypeOrBoundsTree): Buffer = tpt match {
      case TypeBoundsTree(lo, hi) =>
        this += "_ >: "
        printTypeTree(lo)
        this += " <: "
        printTypeTree(hi)
      case tpt @ SyntheticBounds() =>
        printTypeOrBound(tpt.tpe)
      case IsTypeTree(tpt) =>
        printTypeTree(tpt)
    }

    def printTypeTree(tree: TypeTree): Buffer = tree match {
      case TypeTree.Synthetic() =>
        // TODO try to move this logic into `printType`
        def printTypeAndAnnots(tpe: Type): Buffer = tpe match {
          case Type.AnnotatedType(tp, annot) =>
            printTypeAndAnnots(tp)
            this += " "
            printAnnotation(annot)
          case Type.SymRef(sym, _) if sym.isClass && (sym.fullName == "scala.runtime.Null$" || sym.fullName == "scala.runtime.Nothing$") =>
            // scala.runtime.Null$ and scala.runtime.Nothing$ are not modules, those are their actual names
            printType(tpe)
          case tpe @ Type.SymRef(sym, _) if sym.isClass && sym.name.endsWith("$") =>
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
        inBlock(printTrees(refinements, "; "))

      case TypeTree.Applied(tpt, args) =>
        printTypeTree(tpt)
        inSquare(printTypeOrBoundsTrees(args, ", "))

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
      case IsType(tpe) => printType(tpe)
    }

    def printType(tpe: Type): Buffer = tpe match {
      case Type.ConstantType(const) =>
        printConstant(const)

      case Type.SymRef(sym, prefix) =>
        prefix match {
          case Types.EmptyPrefix() =>
          case IsType(prefix @ Type.SymRef(sym, _)) if sym.isClass =>
            printType(prefix)
            this += "#"
          case IsType(prefix) =>
            if (!sym.flags.isLocal) {
              printType(prefix)
              this += "."
            }
        }
        this += sym.name.stripSuffix("$")

      case Type.TermRef(name, prefix) =>
        prefix match {
          case Type.ThisType(Types.EmptyPackage()) =>
            this += name
          case IsType(prefix) =>
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
          case IsType(prefix) => printType(prefix) += "."
        }
        if (name.endsWith("$")) this += name.stripSuffix("$") += ".type"
        else this += name

      case tpe @ Type.Refinement(_, _, _) =>
        printRefinement(tpe)

      case Type.AppliedType(tp, args) =>
        tp match {
          case Type.TypeRef("<repeated>", Types.ScalaPackage()) =>
            this += "_*"
          case _ =>
            printType(tp)
            inSquare(printTypesOrBounds(args, ", "))
        }

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
        tp match {
          case Type.SymRef(cdef, _) if !cdef.flags.isObject =>
            printFullClassName(tp)
            this += ".this"
          case Type.TypeRef(name, prefix) if name.endsWith("$") =>
            prefix match {
              case Types.EmptyPrefix() =>
              case _ =>
                printTypeOrBound(prefix)
                this += "."
            }
            this += name.stripSuffix("$")
          case _ =>
            printType(tp)
        }

      case Type.SuperType(thistpe, supertpe) =>
        printType(supertpe)
        this += ".super"

      case Type.TypeLambda(paramNames, tparams, body) =>
        inSquare(printMethodicTypeParams(paramNames, tparams))
        this += " => "
        printTypeOrBound(body)

      case Type.ParamRef(lambda, idx) =>
        lambda match {
          case Type.MethodType(params, _, _) => this += params(idx)
          case Type.PolyType(params, _, _) => this += params(idx)
          case Type.TypeLambda(params, _, _) => this += params(idx)
        }

      case Type.RecursiveType(tpe) =>
        printType(tpe)

      case Type.RecursiveThis(_) =>
        this += "this"

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
      inParens(printTrees(args, ", "))
    }

    def printDefAnnotations(definition: Definition): Buffer = {
      val annots = definition.symbol.annots.filter {
        case Annotation(annot, _) =>
          annot.tpe match {
            case Type.TypeRef(_, Type.SymRef(sym, _)) if sym.fullName == "scala.annotation.internal" => false
            case Type.TypeRef("forceInline", Types.ScalaPackage()) => false
            case _ => true
          }
        case x => throw new MatchError(x.show)
      }
      printAnnotations(annots)
      if (annots.nonEmpty) this += " "
      else this
    }

    def printRefinement(tpe: Type): Buffer = {
      def printMethodicType(tp: TypeOrBounds): Unit = tp match {
        case tp @ Type.MethodType(paramNames, params, res) =>
          inParens(printMethodicTypeParams(paramNames, params))
          printMethodicType(res)
        case tp @ Type.TypeLambda(paramNames, params, res) =>
          inSquare(printMethodicTypeParams(paramNames, params))
          printMethodicType(res)
        case Type.ByNameType(t) =>
          this += ": "
          printType(t)
        case IsType(tp) =>
          this += ": "
          printType(tp)
      }
      def rec(tp: Type): Unit = tp match {
        case Type.Refinement(parent, name, info) =>
          rec(parent)
          indented {
            this += lineBreak()
            info match {
              case IsTypeBounds(info) =>
                this += "type " += name
                printBounds(info)
              case Type.ByNameType(_) | Type.MethodType(_, _, _) | Type.TypeLambda(_, _, _) =>
                this += "def " += name
                printMethodicType(info)
              case IsType(info) =>
                this += "val " += name
                printMethodicType(info)
            }
          }
        case tp =>
          printType(tp)
          this += " {"
      }
      rec(tpe)
      this += lineBreak() += "}"
    }

    def printMethodicTypeParams(paramNames: List[String], params: List[TypeOrBounds]): Unit = {
      def printInfo(info: TypeOrBounds) = info match {
        case IsTypeBounds(info) => printBounds(info)
        case IsType(info) =>
          this += ": "
          printType(info)
      }
      def printSeparated(list: List[(String, TypeOrBounds)]): Unit = list match {
        case Nil =>
        case (name, info) :: Nil =>
          this += name
          printInfo(info)
        case (name, info) :: xs =>
          this += name
          printInfo(info)
          this += ", "
          printSeparated(xs)
      }
      printSeparated(paramNames.zip(params))
    }

    def printBoundsTree(bounds: TypeBoundsTree): Buffer = {
      bounds.low match {
        case TypeTree.Synthetic() =>
        case low =>
          this += " >: "
          printTypeTree(low)
      }
      bounds.hi match {
        case TypeTree.Synthetic() => this
        case hi =>
          this += " <: "
          printTypeTree(hi)
      }
    }

    def printBounds(bounds: TypeBounds): Buffer = {
      this += " >: "
      printType(bounds.low)
      this += " <: "
      printType(bounds.hi)
    }

    def printProtectedOrPrivate(definition: Definition): Boolean = {
      var prefixWasPrinted = false
      def printWithin(within: Type) = within match {
        case Type.SymRef(sym, _) =>
          this += sym.name
        case _ => printFullClassName(within)
      }
      if (definition.symbol.flags.isProtected) {
        this += "protected"
        definition.symbol.protectedWithin match {
          case Some(within) =>
            inSquare(printWithin(within))
          case _ =>
        }
        prefixWasPrinted = true
      } else {
        definition.symbol.privateWithin match {
          case Some(within) =>
            this += "private"
            inSquare(printWithin(within))
            prefixWasPrinted = true
          case _ =>
        }
      }
      if (prefixWasPrinted)
        this += " "
      prefixWasPrinted
    }

    def printFullClassName(tp: TypeOrBounds): Unit = {
      def printClassPrefix(prefix: TypeOrBounds): Unit = prefix match {
        case Type.SymRef(sym, prefix2) if sym.isClass =>
          printClassPrefix(prefix2)
          this += sym.name += "."
        case _ =>
      }
      val Type.SymRef(sym, prefix) = tp
      printClassPrefix(prefix)
      this += sym.name
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
    def unapply(arg: Tree)(implicit ctx: Context): Option[(String, List[Term])] = arg match {
      case IsTerm(arg @ Term.Apply(fn, args)) =>
        fn.tpe match {
          case Type.SymRef(sym, Type.ThisType(Type.SymRef(sym2, _))) if sym2.name == "<special-ops>" =>
            sym.tree match {
              case Some(DefDef(op, _, _, _, _)) => Some((op, args))
              case _ => None
            }
          case _ => None
        }
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
        case Type.TypeRef("Object", Type.SymRef(sym, _)) if sym.fullName == "java.lang" => true
        case _ => false
      }
    }

    object Repeated {
      def unapply(tpe: Type)(implicit ctx: Context): Option[Type] = tpe match {
        case Type.AppliedType(Type.TypeRef("<repeated>", ScalaPackage()), IsType(tp) :: Nil) => Some(tp)
        case _ => None
      }
    }

    object ScalaPackage {
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Boolean = tpe match {
        case Type.SymRef(sym, _) => sym == definitions.ScalaPackage
        case _ => false
      }
    }

    object RootPackage {
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Boolean = tpe match {
        case Type.SymRef(sym, _) => sym.fullName == "<root>" // TODO use Symbol.==
        case _ => false
      }
    }

    object EmptyPackage {
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Boolean = tpe match {
        case Type.SymRef(sym, _) => sym.fullName == "<empty>"
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

  object PackageObject {
    def unapply(tree: Tree)(implicit ctx: Context): Option[Tree] = tree match {
      case PackageClause(_, ValDef("package", _, _) :: body :: Nil) => Some(body)
      case _ => None
    }
  }

}
