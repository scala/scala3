package dotty.tools
package dotc
package core

import TypeErasure.ErasedValueType
import Types.*, Contexts.*, Symbols.*, Flags.*, Decorators.*, SymDenotations.*
import Names.{Name, TermName}
import Constants.Constant

import Names.Name
import StdNames.nme
import config.Feature

class TypeUtils:
  /** A decorator that provides methods on types
   *  that are needed in the transformer pipeline.
   */
  extension (self: Type)

    def isErasedValueType(using Context): Boolean =
      self.isInstanceOf[ErasedValueType]

    def isPrimitiveValueType(using Context): Boolean =
      self.classSymbol.isPrimitiveValueClass

    def isErasedClass(using Context): Boolean =
      val cls = self.underlyingClassRef(refinementOK = true).typeSymbol
      cls.is(Flags.Erased)
       && (cls != defn.SingletonClass || Feature.enabled(Feature.modularity))
         // Singleton counts as an erased class only under x.modularity


    /** Is this type a checked exception? This is the case if the type
     *  derives from Exception but not from RuntimeException. According to
     *  that definition Throwable is unchecked. That makes sense since you should
     *  neither throw nor catch `Throwable` anyway, so we should not define
     *  a capability to do so.
     */
    def isCheckedException(using Context): Boolean =
      self.derivesFrom(defn.ExceptionClass)
      && !self.derivesFrom(defn.RuntimeExceptionClass)

    def isByName: Boolean =
      self.isInstanceOf[ExprType]

    def ensureMethodic(using Context): Type = self match {
      case self: MethodicType => self
      case _ => if (ctx.erasedTypes) MethodType(Nil, self) else ExprType(self)
    }

    def widenToParents(using Context): Type = self.parents match {
      case Nil => self
      case ps => ps.reduceLeft(AndType(_, _))
    }

    /** The element types of this tuple type, which can be made up of EmptyTuple, TupleX and `*:` pairs
     */
    def tupleElementTypes(using Context): Option[List[Type]] =
      tupleElementTypesUpTo(Int.MaxValue)

    /** The element types of this tuple type, which can be made up of EmptyTuple, TupleX and `*:` pairs
     *  @param bound     The maximum number of elements that needs generating minus 1
     *                   The generation will stop once more than bound elems have been generated
     *  @param normalize If true, normalize and dealias at each step.
     *                   If false, never normalize and dealias only to find *:
     *                   and EmptyTuple types. This is useful for printing.
     */
    def tupleElementTypesUpTo(bound: Int, normalize: Boolean = true)(using Context): Option[List[Type]] =
      def recur(tp: Type, bound: Int): Option[List[Type]] =
        if bound < 0 then Some(Nil)
        else (if normalize then tp.normalized else tp).dealias match
          case AppliedType(tycon, hd :: tl :: Nil) if tycon.isRef(defn.PairClass) =>
            recur(tl, bound - 1).map(hd :: _)
          case tp: AppliedType if defn.isTupleNType(tp) && normalize =>
            Some(tp.args)  // if normalize is set, use the dealiased tuple
                           // otherwise rely on the default case below to print unaliased tuples.
          case tp: SkolemType =>
            recur(tp.underlying, bound)
          case tp: SingletonType =>
            if tp.termSymbol == defn.EmptyTupleModule then Some(Nil)
            else if normalize then recur(tp.widen, bound)
            else None
          case _ =>
            if defn.isTupleClass(tp.typeSymbol) && !normalize then Some(tp.dealias.argInfos)
            else None
      recur(self.stripTypeVar, bound)

    /** Is this a generic tuple but not already an instance of one of Tuple1..22? */
    def isGenericTuple(using Context): Boolean =
      self.derivesFrom(defn.PairClass)
      && !defn.isTupleNType(self.widenDealias)

    /** Is this a generic tuple that would fit into the range 1..22?
     *  In this case we need to cast it to make the TupleN members accessible.
     *  This works only for generic tuples of known size up to 22.
     */
    def isSmallGenericTuple(using Context): Boolean = genericTupleArityCompare < 0

    /** Is this a generic tuple with an arity above 22? */
    def isLargeGenericTuple(using Context): Boolean = genericTupleArityCompare > 0

    /** If this is a generic tuple with element types, compare the arity and return:
     *    * -1, if the generic tuple is small (<= MaxTupleArity)
     *    * 1, if the generic tuple is large (> MaxTupleArity)
     *    * 0 if this isn't a generic tuple with element types
     */
    def genericTupleArityCompare(using Context): Int =
      if self.isGenericTuple then
        self.widenTermRefExpr.tupleElementTypesUpTo(Definitions.MaxTupleArity).match
          case Some(elems) => if elems.length <= Definitions.MaxTupleArity then -1 else 1
          case _ => 0
      else 0

    /** Is this a large generic tuple and is `pat` TupleXXL?
     *  TupleXXL.unapplySeq extracts values of type TupleXXL
     *  but large scrutinee terms are typed as large generic tuples.
     *  This allows them to hold on to their precise element types,
     *  but it means type-wise, the terms don't conform to the
     *  extractor's parameter type, so this method identifies case.
     */
    def isTupleXXLExtract(pat: Type)(using Context): Boolean =
      pat.typeSymbol == defn.TupleXXLClass && self.isLargeGenericTuple

    /** The `*:` equivalent of an instance of a Tuple class */
    def toNestedPairs(using Context): Type =
      tupleElementTypes match
        case Some(types) => TypeOps.nestedPairs(types)
        case None => throw new AssertionError("not a tuple")

    def namedTupleElementTypesUpTo(bound: Int, normalize: Boolean = true)(using Context): List[(TermName, Type)] =
      (if normalize then self.normalized else self).dealias match
        case defn.NamedTuple(nmes, vals) =>
          val names = nmes.tupleElementTypesUpTo(bound, normalize).getOrElse(Nil).map(_.dealias).map:
            case ConstantType(Constant(str: String)) => str.toTermName
            case t => throw TypeError(em"Malformed NamedTuple: names must be string types, but $t was found.")
          val values = vals.tupleElementTypesUpTo(bound, normalize).getOrElse(Nil)
          names.zip(values)
        case t =>
          Nil

    def namedTupleElementTypes(using Context): List[(TermName, Type)] =
      namedTupleElementTypesUpTo(Int.MaxValue)

    def isNamedTupleType(using Context): Boolean = self match
      case defn.NamedTuple(_, _) => true
      case _ => false

    def derivesFromNamedTuple(using Context): Boolean = self match
      case defn.NamedTuple(_, _) => true
      case tp: MatchType =>
        tp.bound.derivesFromNamedTuple || tp.reduced.derivesFromNamedTuple
      case tp: TypeProxy => tp.superType.derivesFromNamedTuple
      case tp: AndType => tp.tp1.derivesFromNamedTuple || tp.tp2.derivesFromNamedTuple
      case tp: OrType => tp.tp1.derivesFromNamedTuple && tp.tp2.derivesFromNamedTuple
      case _ => false

    /** Drop all named elements in tuple type */
    def stripNamedTuple(using Context): Type = self.normalized.dealias match
      case defn.NamedTuple(_, vals) =>
        vals
      case self @ AnnotatedType(tp, annot) =>
        val tp1 = tp.stripNamedTuple
        if tp1 ne tp then AnnotatedType(tp1, annot) else self
      case _ =>
        self

    def refinedWith(name: Name, info: Type)(using Context) = RefinedType(self, name, info)

    /** Is this type a methodic type that takes at least one parameter? */
    def takesParams(using Context): Boolean = self.stripPoly match
      case mt: MethodType => mt.paramNames.nonEmpty || mt.resType.takesParams
      case _ => false

    /** Is this type a methodic type that takes implicit parameters (both old and new) at some point? */
    def takesImplicitParams(using Context): Boolean = self.stripPoly match
      case mt: MethodType => mt.isImplicitMethod || mt.resType.takesImplicitParams
      case _ => false

    /** Is this a type deriving only from transparent classes?
     *  @param traitOnly  if true, all class symbols must be transparent traits
     */
    def isTransparent(traitOnly: Boolean = false)(using Context): Boolean = self match
      case AndType(tp1, tp2) =>
        tp1.isTransparent(traitOnly) && tp2.isTransparent(traitOnly)
      case _ =>
        val cls = self.underlyingClassRef(refinementOK = false).typeSymbol
        cls.isTransparentClass && (!traitOnly || cls.is(Trait))

    /** Is this type the ThisType of class `cls?`. Note we can't use `self eq cls.thisType` for this,
     *  since ThisTypes take TermRef parameters and semantically equal TermRefs could have different
     *  forms (for instance one could use as a prefix the ThisType of an enclosing static module or package,
     *  and the other could select it from something further out)
     */
    def isThisTypeOf(cls: Symbol)(using Context) = self match
      case self: Types.ThisType => self.cls == cls
      case _ => false

    /** If `self` is of the form `p.x` where `p` refers to a package
     *  but `x` is not owned by a package, expand it to
     *
     *      p.package.x
     */
    def makePackageObjPrefixExplicit(using Context): Type =
      def tryInsert(tpe: NamedType, pkgClass: SymDenotation): Type = pkgClass match
        case pkg: PackageClassDenotation =>
          var sym = tpe.symbol
          if !sym.exists && tpe.denot.isOverloaded then
            // we know that all alternatives must come from the same package object, since
            // otherwise we would get "is already defined" errors. So we can take the first
            // symbol we see.
            sym = tpe.denot.alternatives.head.symbol
          val pobj = pkg.packageObjFor(sym)
          if pobj.exists then tpe.derivedSelect(pobj.termRef)
          else tpe
        case _ =>
          tpe
      self match
      case tpe: NamedType =>
        if tpe.symbol.isRoot then
          tpe
        else
          tpe.prefix match
            case pre: ThisType if pre.cls.is(Package) => tryInsert(tpe, pre.cls)
            case pre: TermRef if pre.symbol.is(Package) => tryInsert(tpe, pre.symbol.moduleClass)
            case _ => tpe
      case tpe => tpe

    /** Strip all outer refinements off this type */
    def stripRefinement: Type = self match
      case self: RefinedOrRecType => self.parent.stripRefinement
      case seld => self

    /** The constructors of this type that are applicable to `argTypes`, without needing
     *  an implicit conversion. Curried constructors are always excluded.
     *  @param adaptVarargs   if true, allow a constructor with just a varargs argument to
     *                        match an empty argument list.
     */
    def applicableConstructors(argTypes: List[Type], adaptVarargs: Boolean)(using Context): List[Symbol] =
      def isApplicable(constr: Symbol): Boolean =
        def recur(ctpe: Type): Boolean = ctpe match
          case ctpe: PolyType =>
            if argTypes.isEmpty then recur(ctpe.resultType) // no need to know instances
            else recur(ctpe.instantiate(self.argTypes))
          case ctpe: MethodType =>
            var paramInfos = ctpe.paramInfos
            if adaptVarargs && paramInfos.length == argTypes.length + 1
              && atPhaseNoLater(Phases.elimRepeatedPhase)(constr.info.isVarArgsMethod)
            then // accept missing argument for varargs parameter
              paramInfos = paramInfos.init
            argTypes.corresponds(paramInfos)(_ <:< _) && !ctpe.resultType.isInstanceOf[MethodType]
          case _ =>
            false
        recur(constr.info)

      self.decl(nme.CONSTRUCTOR).altsWith(isApplicable).map(_.symbol)

end TypeUtils

