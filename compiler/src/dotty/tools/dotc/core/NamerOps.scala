package dotty.tools
package dotc
package core

import Contexts.*, Symbols.*, Types.*, Flags.*, Scopes.*, Decorators.*, Names.*, NameOps.*
import SymDenotations.{LazyType, SymDenotation}, StdNames.nme
import ContextOps.enter
import TypeApplications.EtaExpansion
import collection.mutable
import config.Printers.typr

/** Operations that are shared between Namer and TreeUnpickler */
object NamerOps:

  /** The type of the constructed instance is returned
   *
   *  @param ctor the constructor
   */
  def effectiveResultType(ctor: Symbol, paramss: List[List[Symbol]])(using Context): Type =
    paramss match
      case TypeSymbols(tparams) :: rest =>
        addParamRefinements(ctor.owner.typeRef.appliedTo(tparams.map(_.typeRef)), rest)
      case _ =>
        addParamRefinements(ctor.owner.typeRef, paramss)

  /** Given a method with tracked term-parameters `p1, ..., pn`, and result type `R`, add the
   *  refinements R { p1 = p1' } ... { pn = pn' }, where pi' is the TermParamRef
   *  of the parameter and pi is its name. This matters only under experimental.modularity,
   *  since without it there are no tracked parameters. Parameter refinements are added for
   *  constructors and given companion methods.
   */
  def addParamRefinements(resType: Type, paramss: List[List[Symbol]])(using Context): Type =
    paramss.flatten.foldLeft(resType): (rt, param) =>
      if param.is(Tracked) then RefinedType(rt, param.name, param.termRef)
      else rt

  /** Split dependent class refinements off parent type. Add them to `refinements`,
   *  unless it is null.
   */
  extension (tp: Type)
    def separateRefinements(cls: ClassSymbol, refinements: mutable.LinkedHashMap[Name, Type] | Null)(using Context): Type =
      val widenSkolemsMap = new TypeMap:
        def apply(tp: Type) = mapOver(tp.widenSkolem)
      tp match
        case RefinedType(tp1, rname, rinfo) =>
          try tp1.separateRefinements(cls, refinements)
          finally
            if refinements != null then
              val rinfo1 = widenSkolemsMap(rinfo)
              refinements(rname) = refinements.get(rname) match
                case Some(tp) => tp & rinfo1
                case None => rinfo1
        case tp @ AnnotatedType(tp1, ann) =>
          tp.derivedAnnotatedType(tp1.separateRefinements(cls, refinements), ann)
        case tp: RecType =>
          tp.parent.substRecThis(tp, cls.thisType).separateRefinements(cls, refinements)
        case tp =>
          tp

  /** If isConstructor, make sure it has at least one non-implicit parameter list
   *  This is done by adding a () in front of a leading old style implicit parameter,
   *  or by adding a () as last -- or only -- parameter list if the constructor has
   *  only using clauses as parameters.
   */
  def normalizeIfConstructor(paramss: List[List[Symbol]], isConstructor: Boolean)(using Context): List[List[Symbol]] =
    if !isConstructor then paramss
    else paramss match
      case TypeSymbols(tparams) :: paramss1 => tparams :: normalizeIfConstructor(paramss1, isConstructor)
      case TermSymbols(vparam :: _) :: _ if vparam.is(Implicit) => Nil :: paramss
      case _ =>
        if paramss.forall {
          case TermSymbols(vparams) => vparams.nonEmpty && vparams.head.is(Given)
          case _ => true
        }
        then paramss :+ Nil
        else paramss

  /** The method type corresponding to given parameters and result type */
  def methodType(paramss: List[List[Symbol]], resultType: Type, isJava: Boolean = false)(using Context): Type =
    def recur(paramss: List[List[Symbol]]): Type = (paramss: @unchecked) match
      case Nil =>
        resultType
      case TermSymbols(params) :: paramss1 =>
        val (isContextual, isImplicit) =
          if params.isEmpty then (false, false)
          else (params.head.is(Given), params.head.is(Implicit))
        val make = MethodType.companion(isContextual = isContextual, isImplicit = isImplicit)
        if isJava then
          for param <- params do
            if param.info.isDirectRef(defn.ObjectClass) then param.info = defn.AnyType
        make.fromSymbols(params, recur(paramss1))
      case TypeSymbols(tparams) :: paramss1 =>
        PolyType.fromParams(tparams, recur(paramss1))

    if paramss.isEmpty then ExprType(resultType) else recur(paramss)
  end methodType

  /** Add moduleClass or sourceModule functionality to completer
   *  for a module or module class
   */
  def adjustModuleCompleter(completer: LazyType, name: Name)(using Context): LazyType =
    val scope = ctx.effectiveScope
    if name.isTermName then
      completer.withModuleClass(findModuleBuddy(name.moduleClassName, scope))
    else
      completer.withSourceModule(findModuleBuddy(name.sourceModuleName, scope))

  /** Find moduleClass/sourceModule in effective scope */
  def findModuleBuddy(name: Name, scope: Scope, alternate: Name = EmptyTermName)(using Context): Symbol =
    var it = scope.lookupAll(name).filter(_.is(Module))
    if !alternate.isEmpty then it ++= scope.lookupAll(alternate).filter(_.is(Module))
    if it.hasNext then it.next()
    else NoSymbol.assertingErrorsReported(em"no companion $name in $scope")

  /** If a class has one of these flags, it does not get a constructor companion */
  private val NoConstructorProxyNeededFlags = Abstract | Trait | Case | Synthetic | Module | Invisible

  /** The flags of a constructor companion */
  private val ConstructorCompanionFlags = Synthetic | ConstructorProxy

  /** The flags of an `apply` method that serves as a constructor proxy */
  val ApplyProxyFlags = Synthetic | ConstructorProxy | Inline | Method

  /** If this is a reference to a class and the reference has a stable prefix, the reference
   *  otherwise NoType
   */
  private def underlyingStableClassRef(tp: Type)(using Context): TypeRef | NoType.type = tp match
    case EtaExpansion(tp1) => underlyingStableClassRef(tp1)
    case _ => tp.underlyingClassRef(refinementOK = false) match
      case ref: TypeRef if ref.prefix.isStable => ref
      case _ => NoType

  /** Does symbol `sym` need constructor proxies to be generated? */
  def needsConstructorProxies(sym: Symbol)(using Context): Boolean =
    sym.isClass
    && !sym.flagsUNSAFE.isOneOf(NoConstructorProxyNeededFlags)
    && !sym.isAnonymousClass
    ||
    sym.isType && sym.is(Exported)
    && underlyingStableClassRef(sym.info.loBound).exists

  /** The completer of a constructor proxy apply method */
  class ApplyProxyCompleter(constr: Symbol)(using Context) extends LazyType:
    def complete(denot: SymDenotation)(using Context): Unit =
      denot.info = constr.info

  /** Add constructor proxy apply methods to `scope`. Proxies are for constructors
   *  in `cls` and they reside in `modcls`.
   */
  def addConstructorApplies(scope: MutableScope, cls: ClassSymbol, modcls: ClassSymbol)(using Context): scope.type =
    def proxy(constr: Symbol): Symbol =
      var flags = ApplyProxyFlags | (constr.flagsUNSAFE & AccessFlags)
      if cls.is(Protected) && !modcls.is(Protected) then flags |= Protected
      newSymbol(
        modcls, nme.apply,
        flags,
        ApplyProxyCompleter(constr),
        cls.privateWithin,
        constr.coord)
    for dcl <- cls.info.decls do
      if dcl.isConstructor then scope.enter(proxy(dcl))
    scope
  end addConstructorApplies

  /** The completer of a constructor companion for class `cls`, where
   *  `modul` is the companion symbol and `modcls` is its class.
   */
  def constructorCompanionCompleter(cls: ClassSymbol)(
      modul: TermSymbol, modcls: ClassSymbol)(using Context): LazyType =
    new LazyType with SymbolLoaders.SecondCompleter {
      def complete(denot: SymDenotation)(using Context): Unit =
        val prefix = modcls.owner.thisType
        denot.info = ClassInfo(
          prefix, modcls, defn.AnyType :: Nil,
          addConstructorApplies(newScope, cls, modcls), TermRef(prefix, modul))
    }.withSourceModule(modul)

  /** A new symbol that is the constructor companion for class `cls` */
  def classConstructorCompanion(cls: ClassSymbol)(using Context): TermSymbol =
    var flags = ConstructorCompanionFlags
    if cls.is(Protected) then flags |= Protected
    val companion = newModuleSymbol(
        cls.owner, cls.name.toTermName,
        flags, flags,
        constructorCompanionCompleter(cls),
        cls.privateWithin,
        cls.coord,
        cls.compUnitInfo)
    companion.moduleClass.registerCompanion(cls)
    cls.registerCompanion(companion.moduleClass)
    companion

  def typeConstructorCompanion(tsym: Symbol, prefix: Type, proxy: Symbol)(using Context): TermSymbol =
    newSymbol(tsym.owner, tsym.name.toTermName,
        ConstructorCompanionFlags | StableRealizable | Method, ExprType(prefix.select(proxy)), coord = tsym.coord)

  /** Add all necessary constructor proxy symbols for members of class `cls`. This means:
   *
   *   - if a member is a class, or type alias, that needs a constructor companion, add one,
   *     provided no member with the same name exists.
   *   - if `cls` is a companion object of a class that needs a constructor companion,
   *     and `cls` does not already define or inherit an `apply` method,
   *     add `apply` methods for all constructors of the companion class.
   */
  def addConstructorProxies(cls: ClassSymbol)(using Context): Unit =

    def memberExists(cls: ClassSymbol, name: TermName): Boolean =
      cls.baseClasses.exists(_.info.decls.lookupEntry(name) != null)

    for mbr <- cls.info.decls do
      if needsConstructorProxies(mbr) then
        mbr match
          case mbr: ClassSymbol =>
            if !mbr.unforcedRegisteredCompanion.exists
              && !memberExists(cls, mbr.name.toTermName)
            then
              classConstructorCompanion(mbr).entered
          case _ =>
            underlyingStableClassRef(mbr.info.loBound): @unchecked match
              case ref: TypeRef =>
                val proxy = ref.symbol.registeredCompanion
                if proxy.is(ConstructorProxy) && !memberExists(cls, mbr.name.toTermName) then
                  typeConstructorCompanion(mbr, ref.prefix, proxy).entered

    if cls.is(Module)
       && needsConstructorProxies(cls.linkedClass)
       && !memberExists(cls, nme.apply)
    then
      addConstructorApplies(cls.info.decls.openForMutations, cls.linkedClass.asClass, cls)
  end addConstructorProxies

  /** Turn `modul` into a constructor companion for class `cls` */
  def makeConstructorCompanion(modul: TermSymbol, cls: ClassSymbol)(using Context): Unit =
    val modcls = modul.moduleClass.asClass
    modul.setFlag(ConstructorCompanionFlags)
    modcls.setFlag(ConstructorCompanionFlags)
    modcls.info = constructorCompanionCompleter(cls)(modul, modcls)
    cls.registeredCompanion = modcls
    modcls.registeredCompanion = cls

  /** For secondary constructors, make it known in the context that their type parameters
   *  are aliases of the class type parameters.
   *  @return  if `sym` is a secondary constructor, a fresh context that
   *           contains GADT constraints linking the type parameters.
   */
  def linkConstructorParams(sym: Symbol)(using Context): Context =
    if sym.isConstructor && !sym.isPrimaryConstructor then
      sym.rawParamss match
        case (tparams @ (tparam :: _)) :: _ if tparam.isType =>
          val rhsCtx = ctx.fresh.setFreshGADTBounds
          linkConstructorParams(sym, tparams, rhsCtx)
          rhsCtx
        case _ =>
          ctx
    else ctx

  /** For secondary constructor `sym`, make it known in the given context `rhsCtx`
   *  that their type parameters are aliases of the class type parameters. This is done
   *  by (ab?)-using GADT constraints. See pos/i941.scala.
   */
  def linkConstructorParams(sym: Symbol, tparams: List[Symbol], rhsCtx: Context)(using Context): Unit =
    rhsCtx.gadtState.addToConstraint(tparams)
    tparams.lazyZip(sym.owner.typeParams).foreach { (psym, tparam) =>
      val tr = tparam.typeRef
      rhsCtx.gadtState.addBound(psym, tr, isUpper = false)
      rhsCtx.gadtState.addBound(psym, tr, isUpper = true)
    }

  /** Create a context-bound companion for type symbol `tsym`, which has a context
   *  bound that defines a set of witnesses with names `witnessNames`.
   *
   *  @param params  If `tsym` is a type parameter, a list of parameter symbols
   *                 that includes all witnesses, otherwise the empty list.
   *
   *  The context-bound companion has as name the name of `tsym` translated to
   *  a term name. We create a synthetic val of the form
   *
   *    val A: `<context-bound-companion>`[witnessRef1 | ... | witnessRefN]
   *
   *  where
   *
   *      <context-bound-companion> is the CBCompanion type created in Definitions
   *      withnessRefK is a reference to the K'th witness.
   *
   *  The companion has the same access flags as the original type.
   */
  def addContextBoundCompanionFor(tsym: Symbol, witnessNames: List[TermName], params: List[Symbol])(using Context): Unit =
    val prefix = ctx.owner.thisType
    val companionName = tsym.name.toTermName
    val witnessRefs =
      if params.nonEmpty then
        witnessNames.map: witnessName =>
            prefix.select(params.find(_.name == witnessName).get)
      else
        witnessNames.map(TermRef(prefix, _))
    val cbtype = defn.CBCompanion.typeRef.appliedTo:
      witnessRefs.reduce[Type](OrType(_, _, soft = false))
    val cbc = newSymbol(
        ctx.owner, companionName,
        (tsym.flagsUNSAFE & (AccessFlags)).toTermFlags | Synthetic,
        cbtype)
    typr.println(s"context bound companion created $cbc for $witnessNames in ${ctx.owner}")
    ctx.enter(cbc)
  end addContextBoundCompanionFor

  /** Add context bound companions to all context-bound types declared in
   *  this class. This assumes that these types already have their
   *  WitnessNames annotation set even before they are completed. This is
   *  the case for unpickling but currently not for Namer. So the method
   *  is only called during unpickling.
   */
  def addContextBoundCompanions(cls: ClassSymbol)(using Context): Unit =
    for sym <- cls.info.decls do
      if sym.isType && !sym.isClass then
        for ann <- sym.annotationsUNSAFE do
          if ann.symbol == defn.WitnessNamesAnnot then
            ann.tree match
              case ast.tpd.WitnessNamesAnnot(witnessNames) =>
                addContextBoundCompanionFor(sym, witnessNames, Nil)
end NamerOps
