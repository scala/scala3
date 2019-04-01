package dotty.tools
package dotc
package typer

import core._
import ast._
import ast.Trees._
import StdNames._
import Contexts._, Symbols._, Types._, SymDenotations._, Names._, NameOps._, Flags._, Decorators._
import ProtoTypes._
import util.Spans._
import util.SourcePosition
import collection.mutable
import Constants.Constant
import config.Printers.derive
import Inferencing._
import transform.TypeUtils._
import transform.SymUtils._
import ErrorReporting.errorTree

/** A typer mixin that implements typeclass derivation functionality */
trait Deriving { this: Typer =>

  /** A helper class to derive type class instances for one class or object
   *  @param  cls      The class symbol of the class or object with a `derives` clause
   *  @param  codePos  The default position that should be given to generic
   *                   synthesized infrastructure code that is not connected with a
   *                   `derives` instance.
   */
  class Deriver(cls: ClassSymbol, codePos: SourcePosition)(implicit ctx: Context) {

    /** A buffer for synthesized symbols */
    private var synthetics = new mutable.ListBuffer[Symbol]

    private var derivesGeneric = false

    /** the children of `cls` ordered by textual occurrence */
    lazy val children: List[Symbol] = cls.children

    private def shapeError(explanation: => String): Unit =
      ctx.error(i"cannot take shape of $cls\n$explanation", codePos)

    /** The shape (of type Shape.Case) of a case given by `sym`. `sym` is either `cls`
     *  itself, or a subclass of `cls`, or an instance of `cls`.
     */
    private def caseShape(sym: Symbol): Type = {
      val (constr, elems) =
        sym match {
          case caseClass: ClassSymbol =>
            if (!caseClass.is(Case)) {
              shapeError(
                if (caseClass == cls) "it has anonymous or inaccessible subclasses"
                else i"its subclass $caseClass is not a case class")
              return NoType
            }
            else if (caseClass.is(Module))
              (caseClass.sourceModule.termRef, Nil)
            else caseClass.primaryConstructor.info match {
              case info: PolyType =>
                def instantiate(implicit ctx: Context) = {
                  val poly = constrained(info, untpd.EmptyTree)._1
                  val mono @ MethodType(_) = poly.resultType
                  val resType = mono.finalResultType
                  resType <:< cls.appliedRef
                  val tparams = poly.paramRefs
                  val variances = caseClass.typeParams.map(_.paramVariance)
                  val instanceTypes = (tparams, variances).zipped.map((tparam, variance) =>
                    ctx.typeComparer.instanceType(tparam, fromBelow = variance < 0))
                  (resType.substParams(poly, instanceTypes),
                   mono.paramInfos.map(_.substParams(poly, instanceTypes)))
                }
                instantiate(ctx.fresh.setExploreTyperState().setOwner(caseClass))
              case info: MethodType =>
                (caseClass.typeRef, info.paramInfos)
              case _ =>
                (caseClass.typeRef, Nil)
            }
          case _ =>
            (sym.termRef, Nil)
        }
      val elemShape = TypeOps.nestedPairs(elems)
      defn.ShapeCaseType.appliedTo(constr, elemShape)
    }

    /** The shape of `cls` if `cls` is sealed */
    private def sealedShape: Type = {
      val cases = children.map(caseShape).filter(_.exists)
      val casesShape = TypeOps.nestedPairs(cases)
      defn.ShapeCasesType.appliedTo(casesShape)
    }

    /** The shape of `cls`, referring directly to the type parameters of `cls` instead
     *  of abstracting over them.
     *  Returns NoType if `cls` is neither sealed nor a case class or object.
     */
    lazy val shapeWithClassParams: Type = {
      if (cls.is(Case)) caseShape(cls)
      else if (cls.is(Sealed)) sealedShape
      else {
        shapeError(i"it is neither sealed nor a case class")
        defn.ShapeCasesType.appliedTo(defn.UnitType)
      }
    }.reporting(res => i"shape of $cls = $res", derive)

    private def shapeOfType(tp: Type) = {
      val shape0 = shapeWithClassParams
      val clsType = tp.baseType(cls)
      if (clsType.exists) shape0.subst(cls.typeParams, clsType.argInfos)
      else clsType
    }

    private def add(sym: Symbol): sym.type = {
      ctx.enter(sym)
      synthetics += sym
      sym
    }

    /** Create a synthetic symbol owned by current owner */
    private def newSymbol(name: Name, info: Type,
                          span: Span = ctx.owner.span,
                          flags: FlagSet = EmptyFlags)(implicit ctx: Context): Symbol =
      ctx.newSymbol(ctx.owner, name, flags | Synthetic, info, coord = span)

    /** Create a synthetic method owned by current owner */
    private def newMethod(name: TermName, info: Type,
                          span: Span = ctx.owner.span,
                          flags: FlagSet = EmptyFlags)(implicit ctx: Context): TermSymbol =
      newSymbol(name, info, span, flags | Method).asTerm

    /** A version of Type#underlyingClassRef that works also for higher-kinded types */
    private def underlyingClassRef(tp: Type): Type = tp match {
      case tp: TypeRef if tp.symbol.isClass => tp
      case tp: TypeRef if tp.symbol.isAbstractType => NoType
      case tp: TermRef => NoType
      case tp: TypeProxy => underlyingClassRef(tp.underlying)
      case _ => NoType
    }

    /** Enter type class instance with given name and info in current scope, provided
     *  an instance with the same name does not exist already.
     *  @param  reportErrors  Report an error if an instance with the same name exists already
     */
    private def addDerivedInstance(clsName: Name, info: Type, pos: SourcePosition, reportErrors: Boolean) = {
      val instanceName = s"derived$$$clsName".toTermName
      if (ctx.denotNamed(instanceName).exists) {
        if (reportErrors) ctx.error(i"duplicate typeclass derivation for $clsName", pos)
      }
      else add(newMethod(instanceName, info, pos.span, Implied))
    }

    /** Check derived type tree `derived` for the following well-formedness conditions:
     *  (1) It must be a class type with a stable prefix (@see checkClassTypeWithStablePrefix)
     *  (2) It must have exactly one type parameter
     *  If it passes the checks, enter a typeclass instance for it in the current scope.
     *  Given
     *
     *     class C[Ts] .... derives ... D ...
     *
     *  where `T_1, ..., T_n` are the first-kinded type parameters in `Ts`,
     *  the typeclass instance has the form
     *
     *      implicit def derived$D(implicit ev_1: D[T_1], ..., ev_n: D[T_n]): D[C[Ts]] = D.derived
     *
     *  See the body of this method for how to generalize this to typeclasses with more
     *  or less than one type parameter.
     *
     *  See test run/typeclass-derivation2 and run/derive-multi
     *  for examples that spell out what would be generated.
     *
     *  Note that the name of the derived method contains the name in the derives clause, not
     *  the underlying class name. This allows one to disambiguate derivations of type classes
     *  that have the same name but different prefixes through selective aliasing.
     */
    private def processDerivedInstance(derived: untpd.Tree): Unit = {
      val originalType = typedAheadType(derived, AnyTypeConstructorProto).tpe
      val underlyingType = underlyingClassRef(originalType)
      val derivedType = checkClassType(underlyingType, derived.sourcePos, traitReq = false, stablePrefixReq = true)
      val typeClass = derivedType.classSymbol
      val nparams = typeClass.typeParams.length
      if (derivedType.isRef(defn.GenericClass))
        derivesGeneric = true
      else {
        // A matrix of all parameter combinations of current class parameters
        // and derived typeclass parameters.
        // Rows: parameters of current class
        // Columns: parameters of typeclass

        // Running example: typeclass: class TC[X, Y, Z], deriving class: class A[T, U]
        // clsParamss =
        //     T_X  T_Y  T_Z
        //     U_X  U_Y  U_Z
        val clsParamss: List[List[TypeSymbol]] = cls.typeParams.map { tparam =>
          if (nparams == 0) Nil
          else if (nparams == 1) tparam :: Nil
          else typeClass.typeParams.map(tcparam =>
            tparam.copy(name = s"${tparam.name}_$$_${tcparam.name}".toTypeName)
              .asInstanceOf[TypeSymbol])
        }
        val firstKindedParamss = clsParamss.filter {
          case param :: _ => !param.info.isLambdaSub
          case nil => false
        }

        // The types of the required evidence parameters. In the running example:
        // TC[T_X, T_Y, T_Z], TC[U_X, U_Y, U_Z]
        val evidenceParamInfos =
          for (row <- firstKindedParamss)
          yield derivedType.appliedTo(row.map(_.typeRef))

        // The class instances in the result type. Running example:
        //   A[T_X, U_X], A[T_Y, U_Y], A[T_Z, U_Z]
        val resultInstances =
          for (n <- List.range(0, nparams))
          yield cls.typeRef.appliedTo(clsParamss.map(row => row(n).typeRef))

        // TC[A[T_X, U_X], A[T_Y, U_Y], A[T_Z, U_Z]]
        val resultType = derivedType.appliedTo(resultInstances)

        val clsParams: List[TypeSymbol] = clsParamss.flatten
        val instanceInfo =
          if (clsParams.isEmpty) ExprType(resultType)
          else PolyType.fromParams(clsParams, ImplicitMethodType(evidenceParamInfos, resultType))
        addDerivedInstance(originalType.typeSymbol.name, instanceInfo, derived.sourcePos, reportErrors = true)
      }
    }

    /** Add value corresponding to `val genericClass = new GenericClass(...)`
     *  to `synthetics`, unless a definition of `genericClass` exists already.
     */
    private def addGenericClass(): Unit =
      if (!ctx.denotNamed(nme.genericClass).exists) {
        add(newSymbol(nme.genericClass, defn.GenericClassType, codePos.span))
      }

    private def addGeneric(): Unit = {
      val genericCompleter = new LazyType {
        def complete(denot: SymDenotation)(implicit ctx: Context) = {
          val resultType =
            RefinedType(
              defn.GenericType.appliedTo(cls.appliedRef),
              tpnme.Shape,
              TypeAlias(shapeWithClassParams))
          denot.info = PolyType.fromParams(cls.typeParams, resultType).ensureMethodic
        }
      }
      addDerivedInstance(defn.GenericType.name, genericCompleter, codePos, reportErrors = false)
    }

    /** If any of the instances has a companion with a `derived` member
     *  that refers to `scala.reflect.Generic`, add an implied instance
     *  of `Generic`. Note: this is just an optimization to avoid possible
     *  code duplication. Generic instances are created on the fly if they
     *  are missing from the companion.
     */
    private def maybeAddGeneric(): Unit = {
      val genericCls = defn.GenericClass
      def refersToGeneric(sym: Symbol): Boolean = {
        val companion = sym.info.finalResultType.classSymbol.companionModule
        val derivd = companion.info.member(nme.derived)
        derivd.hasAltWith(sd => sd.info.existsPart(p => p.typeSymbol == genericCls))
      }
      if (derivesGeneric || synthetics.exists(refersToGeneric)) {
        derive.println(i"add generic infrastructure for $cls")
        addGeneric()
        addGenericClass()
      }
    }

    /** Create symbols for derived instances and infrastructure,
     *  append them to `synthetics` buffer, and enter them into class scope.
     *  Also, add generic instances if needed.
     */
    def enterDerived(derived: List[untpd.Tree]) = {
      derived.foreach(processDerivedInstance(_))
      maybeAddGeneric()
    }

    private def tupleElems(tp: Type): List[Type] = tp match {
      case AppliedType(fn, hd :: tl :: Nil) if fn.classSymbol == defn.PairClass =>
        hd :: tupleElems(tl)
      case _ =>
        Nil
    }

    /** Extractor for the `cases` in a `Shaped.Cases(cases)` shape */
    private object ShapeCases {
      def unapply(shape: Type): Option[List[Type]] = shape match {
        case AppliedType(fn, cases :: Nil) if fn.classSymbol == defn.ShapeCasesClass =>
          Some(tupleElems(cases))
        case _ =>
          None
      }
    }

    /** Extractor for the `pattern` and `elements` in a `Shaped.Case(pattern, elements)` shape */
    private object ShapeCase {
      def unapply(shape: Type): Option[(Type, List[Type])] = shape match {
        case AppliedType(fn, pat :: elems :: Nil) if fn.classSymbol == defn.ShapeCaseClass =>
          Some((pat, tupleElems(elems)))
        case _ =>
          None
      }
    }

    /** A helper class to create definition trees for `synthetics` */
    class Finalizer {
      import tpd._

      /** The previously synthetsized `genericClass` symbol */
      private def genericClass =
        synthetics.find(sym => !sym.is(Method) && sym.name == nme.genericClass).get.asTerm

      /** The string to pass to `GenericClass` for initializing case and element labels.
       *  See documentation of `GenericClass.label` for what needs to be passed.
       */
      private def labelString(sh: Type): String = sh match {
        case ShapeCases(cases) =>
          cases.map(labelString).mkString("\u0001")
        case ShapeCase(pat: TermRef, _) =>
          pat.symbol.name.toString
        case ShapeCase(pat, elems) =>
          val patCls = pat.widen.classSymbol
          val patLabel = patCls.name.stripModuleClassSuffix.toString
          val elemLabels = patCls.caseAccessors.filterNot(_.is(PrivateLocal)).map(_.name.toString)
          (patLabel :: elemLabels).mkString("\u0000")
      }

      /** The RHS of the `genericClass` value definition */
      def genericClassRHS =
        New(defn.GenericClassType,
          List(Literal(Constant(cls.typeRef)),
               Literal(Constant(labelString(shapeWithClassParams)))))

      /** The RHS of the `derived$Generic` typeclass instance.
       *  Example: For the class definition
       *
       *    enum Lst[+T] derives ... { case Cons(hd: T, tl: Lst[T]); case Nil }
       *
       *  the following typeclass instance is generated, where
       *    <shape> = Cases[(Case[Cons[T], (T, Lst[T])], Case[Nil.type, Unit])]:
       *
       *    implicit def derived$Generic[T]: Generic[Lst[T]] { type Shape = <shape> } =
       *      new Generic[Lst[T]] {
       *        type Shape = <shape>
       *        def reflect(x$0: Lst[T]): Mirror = x$0 match {
       *          case x$0: Cons[T]  => genericClass.mirror(0, x$0)
       *          case x$0: Nil.type => genericClass.mirror(1)
       *        }
       *        def reify(c: Mirror): Lst[T] = c.ordinal match {
       *          case 0 => Cons[T](c(0).asInstanceOf[T], c(1).asInstanceOf[Lst[T]])
       *          case 1 => Nil
       *        }
       *        def common = genericClass
       *      }
       */
      def genericRHS(genericType: Type, genericClassRef: Tree)(implicit ctx: Context) = {
        val RefinedType(
          genericInstance @ AppliedType(_, clsArg :: Nil),
          tpnme.Shape,
          TypeAlias(shapeArg)) = genericType
        val shape = shapeArg.dealias

        val implClassSym = ctx.newNormalizedClassSymbol(
          ctx.owner, tpnme.ANON_CLASS, EmptyFlags, genericInstance :: Nil, coord = codePos.span)
        val implClassCtx = ctx.withOwner(implClassSym)
        val implClassConstr =
          newMethod(nme.CONSTRUCTOR, MethodType(Nil, implClassSym.typeRef))(implClassCtx).entered

        def implClassStats(implicit ctx: Context): List[Tree] = {
          val shapeType: TypeDef = {
            val shapeAlias = newSymbol(tpnme.Shape, TypeAlias(shape)).entered.asType
            TypeDef(shapeAlias)
          }
          val reflectMethod: DefDef = {
            val meth = newMethod(nme.reflect, MethodType(clsArg :: Nil, defn.MirrorType)).entered
            def rhs(paramRef: Tree)(implicit ctx: Context): Tree = {
              def reflectCase(scrut: Tree, idx: Int, elems: List[Type]): Tree = {
                val ordinal = Literal(Constant(idx))
                val args = if (elems.isEmpty) List(ordinal) else List(ordinal, scrut)
                val mirror = defn.GenericClassType
                  .member(nme.mirror)
                  .suchThat(sym => args.tpes.corresponds(sym.info.firstParamTypes)(_ <:< _))
                genericClassRef.select(mirror.symbol).appliedToArgs(args)
              }
              shape match {
                case ShapeCases(cases) =>
                  val clauses = cases.zipWithIndex.map {
                    case (ShapeCase(pat, elems), idx) =>
                      val patVar = newSymbol(nme.syntheticParamName(0), pat, meth.span)
                      CaseDef(
                        Bind(patVar, Typed(untpd.Ident(nme.WILDCARD).withType(pat), TypeTree(pat))),
                        EmptyTree,
                        reflectCase(ref(patVar), idx, elems))
                  }
                  Match(paramRef, clauses)
                case ShapeCase(pat, elems) =>
                  reflectCase(paramRef, 0, elems)
              }
            }
            tpd.DefDef(meth, paramss => rhs(paramss.head.head)(ctx.fresh.setOwner(meth).setNewScope))
          }

          val reifyMethod: DefDef = {
            val meth = newMethod(nme.reify, MethodType(defn.MirrorType :: Nil, clsArg)).entered
            def rhs(paramRef: Tree)(implicit ctx: Context): Tree = {
              def reifyCase(caseType: Type, elems: List[Type]): Tree = caseType match {
                case caseType: TermRef =>
                  ref(caseType)
                case caseType =>
                  val args =
                    for ((elemTp, idx) <- elems.zipWithIndex)
                    yield paramRef.select(nme.apply).appliedTo(Literal(Constant(idx))).cast(elemTp)
                  New(caseType, args)
              }
              shape match {
                case ShapeCases(cases) =>
                  val clauses =
                    for ((ShapeCase(pat, elems), idx) <- cases.zipWithIndex)
                    yield CaseDef(Literal(Constant(idx)), EmptyTree, reifyCase(pat, elems))
                  Match(paramRef.select(nme.ordinal), clauses)
                case ShapeCase(pat, elems) =>
                  reifyCase(pat, elems)
              }
            }

            tpd.DefDef(meth, paramss => rhs(paramss.head.head)(ctx.withOwner(meth)))
          }

          val commonMethod: DefDef = {
            val meth = newMethod(nme.common, ExprType(defn.GenericClassType)).entered
            tpd.DefDef(meth, genericClassRef)
          }

          List(shapeType, reflectMethod, reifyMethod, commonMethod)
        }

        val implClassDef = ClassDef(implClassSym, DefDef(implClassConstr), implClassStats(implClassCtx))
        Block(implClassDef :: Nil, New(implClassSym.typeRef, Nil))
      }

      /** The type class instance definition with symbol `sym` */
      private def typeclassInstance(sym: Symbol)(implicit ctx: Context): List[Type] => (List[List[tpd.Tree]] => tpd.Tree) =
        (tparamRefs: List[Type]) => (paramRefss: List[List[tpd.Tree]]) => {
          val tparams = tparamRefs.map(_.typeSymbol.asType)
          val params = if (paramRefss.isEmpty) Nil else paramRefss.head.map(_.symbol.asTerm)
          tparams.foreach(ctx.enter)
          params.foreach(ctx.enter)
          def instantiated(info: Type): Type = info match {
            case info: PolyType => instantiated(info.instantiate(tparamRefs))
            case info: MethodType => info.instantiate(params.map(_.termRef))
            case info => info.widenExpr
          }
          def classAndCompanionRef(tp: Type): (ClassSymbol, TermRef) = tp match {
            case tp @ TypeRef(prefix, _) if tp.symbol.isClass =>
              (tp.symbol.asClass, prefix.select(tp.symbol.companionModule).asInstanceOf[TermRef])
            case tp: TypeProxy =>
              classAndCompanionRef(tp.underlying)
          }
          val resultType = instantiated(sym.info)
          val (typeCls, companionRef) = classAndCompanionRef(resultType)
          if (typeCls == defn.GenericClass)
            genericRHS(resultType, ref(genericClass))
          else {
            val module = untpd.ref(companionRef).withSpan(sym.span)
            val rhs = untpd.Select(module, nme.derived)
            typed(rhs, resultType)
          }
        }

      def syntheticDef(sym: Symbol): Tree =
        if (sym.isType)
          tpd.TypeDef(sym.asType)
        else if (sym.is(Method))
          tpd.polyDefDef(sym.asTerm, typeclassInstance(sym)(ctx.fresh.setOwner(sym).setNewScope))
        else
          tpd.ValDef(sym.asTerm, genericClassRHS)

      def syntheticDefs: List[Tree] = synthetics.map(syntheticDef).toList
    }

    def finalize(stat: tpd.TypeDef): tpd.Tree = {
      val templ @ Template(_, _, _, _) = stat.rhs
      tpd.cpy.TypeDef(stat)(
        rhs = tpd.cpy.Template(templ)(body = templ.body ++ new Finalizer().syntheticDefs))
    }

    /** Synthesized instance for `Generic[<clsType>]` */
    def genericInstance(clsType: Type): tpd.Tree = {
      val shape = shapeOfType(clsType)
      val genericType = RefinedType(defn.GenericType.appliedTo(clsType), tpnme.Shape, TypeAlias(shape))
      val finalizer = new Finalizer
      finalizer.genericRHS(genericType, finalizer.genericClassRHS)
    }
  }
}
