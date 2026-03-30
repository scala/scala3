package dotty.tools.dotc.transform

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.transform.MegaPhase.MiniPhase
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Decorators.i
import dotty.tools.dotc.core.Decorators.className
import dotty.tools.dotc.core.Symbols.{Symbol, ClassSymbol, newNormalizedClassSymbol}
import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.core.StdNames.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.newClassSymbol
import scala.Function.const
import dotty.tools.dotc.core.Names.TypeName
import dotty.tools.dotc.core.Symbols.TypeSymbol
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.Flags.EmptyFlags
import dotty.tools.dotc.ast.TreeTypeMap
import dotty.tools.dotc.core.Scopes.EmptyScope
import dotty.tools.dotc.core.StdNames.str.SPECIALIZED_TRAIT_SUFFIX
import dotty.tools.dotc.core.Names.Name
import tpd._
import scala.collection.mutable
import scala.annotation.unspecialized
import dotty.tools.dotc.typer.Synthesizer
import dotty.tools.dotc.typer.Typer
import dotty.tools.dotc.core.NameKinds
import dotty.tools.dotc.core.Flags.GivenOrImplicit
import dotty.tools.dotc.core.NameKinds.ContextBoundParamName
import dotty.tools.dotc.inlines.Inlines
import dotty.tools.dotc.util.Spans.Span

class DesugarSpecializedTraits extends MacroTransform:

  override def phaseName: String = DesugarSpecializedTraits.name
  override def description: String = DesugarSpecializedTraits.description
  override def changesMembers: Boolean = false
  override def changesParents: Boolean = true 
  override def runsAfter: Set[String] =  Set("specializeInlineTraits")
  override def allowsImplicitSearch: Boolean = true

  override def run(using Context): Unit =
    try super.run
    catch case _: CompilationUnit.SuspendException => ()

  
  override def newTransformer(using Context): Transformer = new Transformer {

    private def newInterfaceTrait(specialization: Specialization, specializations: SpecializedTraitCache): (ClassSymbol, SpecializedTraitCache) = {
      val tm = new TypeMap: // TODO: Can we get this into the specialization ideally.
        def apply(t: Type) = specialization.specializedTypeParamsToTypeArgumentsMap.view.mapValues(_.tpe).applyOrElse(t, mapOver) // TODO: IF we can do just types we can get rid fo this 
    
      val inheritedParents = specialization.traitSymbol.denot.info.parents.filterNot(_ eq defn.ObjectType).map(tm(_))
      // Parents may be specializable and so we need to specialize them as well
      // See ArrayIterator extends Iterator in specialized-trait-collections-example.scala
      val specializations1 = inheritedParents.foldLeft(specializations)((specializations, parent) => 
          parent match {
            case Specialization(spec) if spec.isSpecialized => (specializations.addInterface(spec)) 
            case _ => specializations
          }
      )

      // Create new trait
      val parents = defn.ObjectType
                    :: AppliedTypeTree(Ident(specialization.traitSymbol.typeRef), specialization.specialization).tpe // original trait, specialized to Foo[Int]
                    :: inheritedParents.map(replaceSpecializedSymbolsMap(specializations1).typeMap(_))                                                                // parents of the original trait, specialized to Foo$sp$Int

      val traitSymbol = newNormalizedClassSymbol(
        specialization.traitSymbol.owner,
        DesugarSpecializedTraits.newSpecializedTraitName(specialization),
        Flags.Synthetic | Flags.Trait | Flags.Inline,
        parents,
        NoType, // TODO: What happens if the creator of the specialized inline trait provides a self type? 
        specialization.traitSymbol.privateWithin,
        // TODO: Do we need a compUnit info?
      )

      // Create type parameters for new trait
      val tps = newTypeParams(traitSymbol,
                    specialization.unspecializedTypeParams.map(_.typeSymbol.name.asTypeName),
                    EmptyFlags,
                    targets => targets.map(t => specialization.traitSymbol.typeParams.find(_.name == t.name).get.info.bounds)
                )
      tps.foreach(traitSymbol.enter(_, EmptyScope))


      // Replace old type parameters that were copied from original trait with new ones
      // inside the parents of the new trait 
      val tpMap: Map[Type, Type] = specialization.unspecializedTypeParams.zip(tps.map(_.typeRef)).toMap
      val freshTypeVarMap = new TypeMap:
        def apply(t: Type) = tpMap.applyOrElse(t, mapOver)
      traitSymbol.info = ClassInfo(traitSymbol.owner.thisType, traitSymbol, traitSymbol.info.parents.map(freshTypeVarMap(_)), traitSymbol.info.decls) // TODO: What happens if the creator of the specialized inline trait provides a self type?
      (traitSymbol.entered, specializations1)
    }

    private def buildInterfaceTraitTree(interfaceSymbol: ClassSymbol)(using Context) = {
      val init = newDefaultConstructor(interfaceSymbol)
      
      // Fix constructor so that it:
      //    1) Has correct generic type parameters
      //    2) Returns the correct type corresponding to those type parameters applied to this trait
      val rt = interfaceSymbol.typeRef.appliedTo(interfaceSymbol.typeParams.map(_.typeRef))
      def resultType(tpe: Type): Type = tpe match {
          case mt @ MethodType(paramNames) => mt.derivedLambdaType(paramNames, mt.paramInfos, rt)
          case pt : PolyType => pt.derivedLambdaType(pt.paramNames, pt.paramInfos, resultType(pt.resType))
      }
      init.info = resultType(init.info)
      init.info = PolyType.fromParams(init.owner.typeParams, init.info)

      // TODO: Confirm that we don't need to worry about copying the evidence parameters over from the old constructor
      // These should be dealt with when we instantiate the original trait as a parent of this one. Otherwise we should be
      // able to copy them over, apply the specialization (keeping e.g. Numeric[Int] that arises from this) and 
      // pruning any that belong to Specialized.

      ClassDef(interfaceSymbol, DefDef(init.entered), Nil)
    }

    private def generateImplementationClassParents(specialization: Specialization, interfaceSymbol: ClassSymbol) = 
      val objectParent = defn.ObjectType
      val traitSpParent = interfaceSymbol.typeRef.appliedTo(specialization.unspecializedTypeArgs.map(_.tpe))
      val originalTraitSpecializedParent = AppliedTypeTree(Ident(specialization.traitSymbol.typeRef), specialization.typeArguments).tpe
      (objectParent, traitSpParent, originalTraitSpecializedParent)

    private def newImplementationClass(specialization: Specialization, interfaceSymbol: ClassSymbol) =
      val (objectParent, traitSpParent, originalTraitSpecializedParent) = generateImplementationClassParents(specialization, interfaceSymbol)
      val parents = List(objectParent, traitSpParent, originalTraitSpecializedParent)

      newNormalizedClassSymbol(
        specialization.traitSymbol.owner,
        DesugarSpecializedTraits.newImplementationClassName(specialization),
        Flags.Synthetic,
        parents,
        NoType, // TODO: What happens if the creator of the specialized inline trait provides a self type? 
        specialization.traitSymbol.privateWithin,
        // TODO: Do we need a compUnit info?
      ).entered

    // TODO: Do we want to share some code with the newSpecializedInterfaceTrait and buildInterfaceTraitTree?
    // TODO: Standardise a bit so that we either generate the symbols and later the classes or not.
    private def buildImplementationClassTree(specialization: Specialization, interfaceSymbol: ClassSymbol, classSymbol: ClassSymbol)(using Context) = {
      val (objectParent, traitSpParent, originalTraitSpecializedParent) = generateImplementationClassParents(specialization, interfaceSymbol)
      val init = newDefaultConstructor(classSymbol)
      
      val tm = new TypeMap: // TODO: Can we get this into the specialization ideally.
        def apply(t: Type) = specialization.constructorParamToArgumentTypeMap.view.applyOrElse(t, mapOver) // TODO: IF we can do just types we can get rid fo this 
      
      val tm2 = new TypeMap:
          def apply(t: Type) = t match {
            case Specialization(spec) if spec.traitSymbol eq specialization.traitSymbol =>
              classSymbol.typeRef
            case _ => mapOver(t)
          }

      val nonTypeParams = specialization.traitSymbol.primaryConstructor.rawParamss.tail
      val valueParams = nonTypeParams.map(_.map(param => param.copy(owner = init, info = tm(param.info)))) // .map(_.filterNot(isSyntheticEvidence)
     
      init.setParamss(valueParams)

      val paramAccessorss = valueParams.map(params => params.map(_.copy(owner = classSymbol, flags= Flags.LocalParamAccessor))) 
      paramAccessorss.foreach(_.foreach(classSymbol.enter(_)))

      init.info = tm2(specialization.traitSymbol.primaryConstructor.info.appliedTo(specialization.typeArguments.map(_.tpe)))
      
      val typer = Typer(ctx.nestingLevel + 1) // TODO: actually get these from the user.
      
      val newParamss = 
        specialization.traitSymbol.primaryConstructor.paramSymss.tail.zip(paramAccessorss.map(_.map(ref))) // skip the type params
        .map((paramSyms, paramAccessors) =>
          paramSyms.zip(paramAccessors).map(
            (paramSym, accessor) =>
               if paramSym.name.asTermName.is(ContextBoundParamName) 
               then typer.implicitArgTree(tm(paramSym.info), paramSym.span) // TODO: Fix spans throughout
               else accessor
          )
        )

      val newParams1 = if (newParamss.length == 1) then newParamss ++ List(List()) else newParamss

      // TODO: Clean adn robust
      val classDef = ClassDefWithParents(
        classSymbol,
        DefDef(init.asTerm.entered), 
        List(
          New(objectParent, objectParent.classSymbol.primaryConstructor.asTerm, Nil),
          New(traitSpParent, traitSpParent.classSymbol.primaryConstructor.asTerm, Nil),
          New(originalTraitSpecializedParent.typeConstructor)
            .select(TermRef(originalTraitSpecializedParent.typeConstructor, specialization.traitSymbol.primaryConstructor.asTerm)) // TODO: Check for other constructors
            .appliedToTypes(originalTraitSpecializedParent.argTypes)
            // .appliedToArgss(paramAccessors.map(_.map(ref)))
            .appliedToArgss(newParams1)
            
            // TODO: What about potential custom typeclass instances? How do we balance that with generating another version of the class every time? Probably just generate the basic version and then let them apply their own version want (based on some kind of hashing). Then we generate a whole new impl class / or anon class which is still specialised to their instances that they provided, at the time that we see it?
            // To be honest if our assumption is that we aren't very often going to do anything weird we can just always generate the class at the point of use, with the evidences specialized (but only if we don't ahve that one already - i.e. effectively consider the evidences as part of the name) 
          ),
          // Put into body of class
          paramAccessorss.flatMap(syms => syms.map(sym => tpd.ValDef(sym.asTerm))) // .withFlags(Flags.LocalParamAccessor).withType(sym.info)
        )
      classDef
    }

    private def replaceSpecializedSymbolsMap(specializations: SpecializedTraitCache) =
      // Use the TreeTypeMap to replace instances (can we do this without accidentally replacing the definitions? I think it should be ok)
      val typeMap = new TypeMap:
        def apply(t: Type) = t match {
          case Specialization(spec) => 
            {
              for (specializedSymbol <- specializations.getInterfaceSymbol(spec))
              yield specializedSymbol.typeRef.appliedTo(spec.unspecializedTypeArgs.map(_.tpe))
            }.getOrElse(mapOver(t))
          case _ => mapOver(t)
        }

      def treeMap(tree: Tree): Tree = tree match {
        // Replace (anonymous class version of) new Foo[Int] {} with new Foo$impl$Int.asInstanceOf[Foo$sp$Int] 
        case Block(List(TypeDef(anon, Template(_, parentCalls: List[Tree], _, _))),  
                  Typed(Apply(Select(New(anon1),ctor), _), t: TypeTree)) if anon1.symbol.isAnonymousClass =>
          parentCalls match {
            case _ :+ Apply(Apply(tpe, ctorArgs), ev) => // extends Object, parents of spec trait, spec trait
              val spec = Specialization.unapply(t.tpe).get
              { // We don't replace non-specialized anonymous class instantiations e.g. new Foo[T] where T is defined in the enclosing scope.
                for (specializedSymbol <- specializations.getImplementationSymbol(spec))
                yield Typed(Apply(Apply(Select(New(ref(specializedSymbol)),ctor), ctorArgs), ev), t)
              }.getOrElse(tree)
            case _ => tree
          }

        // Replace class Bar extends Foo[Int](params) with class Bar extends Foo$sp$Int(params)
        // Note: We always drop the evidence params when creating these new specialized traits so we know that there are none, but we may need to revisit this if we decide we do want to copy the evidence parameters over
        case Apply(TypeApply(fun@Select(New(tpt), init), args), ev) if fun.symbol.isConstructor => 
          val spec = Specialization(fun.symbol.owner, args)
          {
            for (specializedSymbol <- specializations.getInterfaceSymbol(spec))
            yield New(ref(specializedSymbol)).select(init).appliedToTypeTrees(spec.unspecializedTypeArgs)
          }.getOrElse(tree)

        // Replace AppliedTypeTree instances in code
        case Specialization(spec) => {
          for (specializedSymbol <- specializations.getInterfaceSymbol(spec))
          yield AppliedTypeTree(Ident(specializedSymbol.typeRef), spec.unspecializedTypeArgs) // TODO: Matching on a Specialization and then outputting ATT is weird - maybe have a method on specialization to convert to ATT .toAppliedTypeTree?
        }.getOrElse(tree)

        case tree => tree
      }
      
      new TreeTypeMap(typeMap, treeMap) {
        override def transform(tree: Tree)(using Context): Tree = tree match { // HACK: This seems to do what we want but I don't understand why we don't do this by default? Surely we should apply transformDefs over template body?
          case dd@DefDef(name, paramss, tpt, preRhs) => 
            val transformedDef = super.transform(dd)
            transformedDef.symbol.info = mapType(transformedDef.symbol.info)
            transformedDef

          // TODO: Fix the Bar extends Foo case. Avoid updating the parents that we want to keep the same.
          case impl@Template(constr, preParentsOrDerived, self, _) => 
            cpy.Template(impl)(body = impl.body.map(transform(_)))
          case tree => super.transform(tree)
        }
      }
    end replaceSpecializedSymbolsMap

    // Returns (new stmts including original, new symbols including original)
    private def transformStatements(stats: List[Tree], span: Span, specializations: SpecializedTraitCache): (List[Tree], SpecializedTraitCache) = {
      val specializations1 = collectReferencedSpecializations(stats, specializations)
      val generatedTraitStats = specializations1.getNewInterfaceSymbols.toList.map(buildInterfaceTraitTree)
      val generatedClassStats = specializations1.getNewImplementationSymbols.toList.map(buildImplementationClassTree)

      val specializations2 = specializations1.installNewInterfaceSymbols.installNewImplementationSymbols

      // TODO: How do we calculate the spans correctly?
      val generatedTraitStats1 = generatedTraitStats.map(trtDef => Inlines.inlineParentInlineTraits(Inlines.transformInlineTrait(trtDef.withSpan(span))))
      val generatedClassStats1 = generatedClassStats.map(clsDef => Inlines.inlineParentInlineTraits(clsDef.withSpan(span)))
      
      if (generatedTraitStats1.isEmpty && generatedClassStats1.isEmpty)
        (stats.map(replaceSpecializedSymbolsMap(specializations2)(_)), specializations2)
      else 
        val (generatedTraitStats2, specializations3) = transformStatements(generatedTraitStats1, span, specializations2)
        val (generatedClassStats2, specializations4) = transformStatements(generatedClassStats1, span, specializations3)
        (generatedTraitStats2 ++ generatedClassStats2 ++ stats.map(replaceSpecializedSymbolsMap(specializations4)(_)), specializations4)
    }

    override def transform(tree: Tree)(using Context): Tree = tree
       match { // TODO: Is Package level processing really what we want? Given we are not going to output the classes somewhere else do we not really want either to deepFold the whole tree directly or do a more direct transform?
        case pkg@PackageDef(pid, stats) => // TODO: If we do everything ourselves and match only on the package then we can get rid of the MacroTransform aspect and just have a Phase with the transformPackageDef method.
          val (stats1, _) = transformStatements(stats, tree.span, SpecializedTraitCache(genInterfaceSymbol = newInterfaceTrait, genImplementationSymbol = newImplementationClass)) // TODO: Fix span
          cpy.PackageDef(pkg)(pid, stats1)
      }

    // TODO: There is a case where recursive expansion causes something to need an implementation where it didn't before.


    // TODO: Try with just generating new Foo(100) with no function to pass it to and no other references to Foo. this may not work because we might not
    // correctly detect it. 

    // TODO : Is it not better to just delete the Specialized?

    private def collectReferencedSpecializations(stats: List[Tree], specializations: SpecializedTraitCache)(using Context): SpecializedTraitCache =
      stats.foldLeft(specializations)((specializations, tree) => {
        tree.deepFold(specializations)((specializations, tree) => tree match
          case Typed(Apply(Select(New(anon),ctor),List()), t: TypeTree) if anon.symbol.isAnonymousClass =>
            t.tpe match {
              case Specialization(spec) if spec.isSpecialized => specializations.addInterfaceAndImplementation(spec)
              case _ => specializations
            }
          case Specialization(spec) if (spec.isSpecialized) => 
            specializations.addInterface(spec)
          case _ => specializations
        )
      })
  }
end DesugarSpecializedTraits
                  // TODO: Need to think carefully about the behaviour when we are integrating libraries - should the library generate the implementation classes or the user?
                  // In any case we need to read back in either the $sp$ classes or the $impl$ traits to be able to work with them.              

object DesugarSpecializedTraits:
  val name: String = "desugarSpecializedTraits"
  val description: String = "Replaces traits having type parameters that have the Specialized annotation with specialized versions"

  // TODO: What happens with this name generation if we have Vec[Vec[T]] for example? We potentially don't have an Ident
  // TODO: Check what happens here when we have a case where the types being specialized into are user defined instead of primitives or type vars.
  private def generateName(specialization: Specialization, suffix: String)(using Context) = // TODO: Probably don't use show
    specialization.specializedTypeArgs.collect(t => t.tpe.show ++ str.SPECIALIZED_TRAIT_TYPE_SEP).foldLeft((specialization.traitSymbol.name ++ suffix).asTypeName)((n1, n2) => n1 ++ n2)

  private[transform] def newSpecializedTraitName(specialization: Specialization)(using Context): TypeName = 
    generateName(specialization, str.SPECIALIZED_TRAIT_SUFFIX)

  private[transform] def newImplementationClassName(specialization: Specialization)(using Context): TypeName = 
    generateName(specialization, str.SPECIALIZED_TRAIT_IMPL_SUFFIX)

  // TODO: Put this somewhere else; consider if we want to do it like this?
  def isSpecializationOf(type1: Type, type2: Type)(using Context) = 
    type2 match {
      case Specialization(spec) => type1 match {
        case AppliedType(tp, args) => 
          tp.typeSymbol.name == newSpecializedTraitName(spec)
        case _ => false
      }
      case _ => false
    }
end DesugarSpecializedTraits
/*
  Stores the specializations we have found in the program and the symbols for the interface traits and implementation classes
  that will replace them. We generate these symbols when we enter the specializations into the cache, via the functions
  we store in genInterfaceSymbol and genImplementationSymbol. 

  Model: Contains two levels:
           - interface/implementation symbols we have found since the last installNewInterface/ImplementationSymbols call
            (i.e. typically on this iteration) ("new")
           - Those we found prior to that call

  Invariant: (newImplementationSymbols \cup implementationSymbols) \subseteq (interfaceSymbols \cup newInterfaceSymbols).
    This is enforced by only providing addInterface and addInterfaceAndImplementation, and allows the unchecked get in 
    getNeWImplementationSymbols.

*/

object SpecializedTraitCache:
  type SymbolMap = Map[Specialization, ClassSymbol]
  type GenInterfaceSymbol = (Specialization, SpecializedTraitCache) => (ClassSymbol, SpecializedTraitCache)
  type GenImplementationSymbol = (Specialization, ClassSymbol) => ClassSymbol


class SpecializedTraitCache(
  private val newInterfaceSymbols: SpecializedTraitCache.SymbolMap = Map.empty,
  private val newImplementationSymbols: SpecializedTraitCache.SymbolMap = Map.empty,
  private val interfaceSymbols: SpecializedTraitCache.SymbolMap = Map.empty,
  private val implementationSymbols: SpecializedTraitCache.SymbolMap = Map.empty,
  private val genInterfaceSymbol: SpecializedTraitCache.GenInterfaceSymbol,
  private val genImplementationSymbol: SpecializedTraitCache.GenImplementationSymbol
):

  def copy(
    newInterfaceSymbols: SpecializedTraitCache.SymbolMap = this.newInterfaceSymbols,
    newImplementationSymbols: SpecializedTraitCache.SymbolMap = this.newImplementationSymbols,
    interfaceSymbols: SpecializedTraitCache.SymbolMap = this.interfaceSymbols,
    implementationSymbols: SpecializedTraitCache.SymbolMap = this.implementationSymbols,
    genInterfaceSymbol: SpecializedTraitCache.GenInterfaceSymbol = this.genInterfaceSymbol,
    genImplementationSymbol: SpecializedTraitCache.GenImplementationSymbol = this.genImplementationSymbol)
      = SpecializedTraitCache(newInterfaceSymbols, newImplementationSymbols, interfaceSymbols, implementationSymbols, genInterfaceSymbol, genImplementationSymbol)

  def getInterfaceSymbol(spec: Specialization): Option[ClassSymbol] = newInterfaceSymbols.orElse(interfaceSymbols).lift(spec)
  def getImplementationSymbol(spec: Specialization): Option[ClassSymbol] = newImplementationSymbols.orElse(implementationSymbols).lift(spec)

  def getNewInterfaceSymbols = newInterfaceSymbols.values 
  def getNewImplementationSymbols: List[(Specialization, ClassSymbol, ClassSymbol)] = newImplementationSymbols.map((k, v) => (k, getInterfaceSymbol(k).get, v)).toList

  def addInterface(spec: Specialization): SpecializedTraitCache = 
    if (newInterfaceSymbols.contains(spec) || interfaceSymbols.contains(spec)) then
      this
    else
      val (targetSymbol, resultingCache) = genInterfaceSymbol(spec, this)
      resultingCache.copy(newInterfaceSymbols = resultingCache.newInterfaceSymbols + (spec -> targetSymbol))
  def addInterfaceAndImplementation(spec: Specialization): SpecializedTraitCache = 
    if (newImplementationSymbols.contains(spec) || implementationSymbols.contains(spec)) then
      this
    else
      val withInterface = addInterface(spec)
      withInterface.copy(newImplementationSymbols = withInterface.newImplementationSymbols + (spec -> genImplementationSymbol(spec, withInterface.getInterfaceSymbol(spec).get)))

  def installNewInterfaceSymbols =
    this.copy(
      newInterfaceSymbols = Map.empty,
      interfaceSymbols = interfaceSymbols ++ newInterfaceSymbols)

  def installNewImplementationSymbols =
    this.copy(
      newImplementationSymbols = Map.empty,
      implementationSymbols = implementationSymbols ++ newImplementationSymbols)

end SpecializedTraitCache

/* Represents an application traitSymbol[typeArguments] */
class Specialization(val traitSymbol: Symbol, val typeArguments: List[Tree])(using Context): // TODO: Can we get away with List[Type]
  object SpecializedEvidence {
    def unapply(tpe: Type)(using Context): Option[Type] = tpe match {
      case AppliedType(tycon, List(tpeArg)) if tycon =:= ctx.definitions.SpecializedClass.typeRef => Some(tpeArg)
      case _ => None
    }
  }

  val specializedTypeParams: List[Type] = traitSymbol.unforcedDecls.implicitDecls.collect(_.info match { case SpecializedEvidence(typeVar) => typeVar })
  
  private val specializedTypeParamsSet = specializedTypeParams.toSet
  private val paramToArgList = traitSymbol.typeParams.map(_.typeRef.asInstanceOf[Type]).zip(typeArguments)

  val unspecializedTypeParams: List[Type] = paramToArgList.filterNot((tParam, tArg) => specializedTypeParamsSet(tParam)).map(_._1)
  val specializedTypeArgs: List[Tree] = paramToArgList.filter((tParam, tArg) => specializedTypeParamsSet(tParam)).map(_._2)
  val unspecializedTypeArgs: List[Tree] = paramToArgList.filterNot((tParam, tArg) => specializedTypeParamsSet(tParam)).map(_._2)

  val specializedTypeParamsToTypeArgumentsMap: Map[Type, Tree] = paramToArgList.toMap.filter((k, v) => specializedTypeParamsSet(k))
  val specialization: List[Tree] = traitSymbol.typeParams.map(_.typeRef).map(specializedTypeParamsToTypeArgumentsMap.applyOrElse(_, TypeTree(_))) // TODO: Don't really like this name
  // val constructorParamToArgumentTypeMap: Map[Type, Type] = traitSymbol.primaryConstructor.typeParams.zip(paramToArgList).filter((constrParam, paramArg) => specializedTypeParamsSet(paramArg._1)).map((constrParam, paramArg) => (constrParam.typeRef, paramArg._1)).toMap

  // TODO: Potentially can get this out of the specialization.specialization directly given we make the same assumption about one primary constructor and param ordering. 
  def constructorParamToArgumentTypeMap: Map[Type, Type] = 
    traitSymbol.primaryConstructor.rawParamss.head.map(_.typeRef).zip(typeArguments.map(_.tpe)).toMap

  def hasSpecializedParams: Boolean = specializedTypeParams.nonEmpty

  // If inline trait Foo[T] has a method taking another Foo[T] there's no point specializing the reference
  // since the resulting sp$T$ would be the same as the starting trait.
  def isSpecialized: Boolean = 
    hasSpecializedParams && typeArguments.exists(tpt => !tpt.symbol.isTypeParam) //  .zip(traitSymbol.typeParams).forall((t, s) => t.tpe =:= s.typeRef))

  // Note: We only care about the specialized arguments for equality; a specialization of Vec[A: Specialized, B] with B = Int and one
  // with B = String can be considered to be the same as they use the same specialized trait
  // TODO: I don't really like this logic being in Specialization because they are really different
  // We should really put that logic in the SpecializedTraitCache because it's at that point that we treat them as the same.
  override def equals(obj: Any): Boolean = 
    obj.isInstanceOf[Specialization] && obj.asInstanceOf[Specialization].traitSymbol == traitSymbol
    && specializedTypeArgs.zip(obj.asInstanceOf[Specialization].specializedTypeArgs).forall((a1, a2) => a1.tpe =:= a2.tpe)

  override def hashCode(): Int = 
    (traitSymbol, specializedTypeArgs.map(_.tpe.widen.dealias.show)).hashCode() // TODO: Consider not using show for this for performance reasons (correctness also?)
end Specialization

object Specialization:
  def unapply(tpt: Tree)(using Context): Option[Specialization] = tpt match {
    case AppliedTypeTree(specializedTrait: Ident, concreteTypeTrees: List[Tree]) => Some(Specialization(specializedTrait.denot.symbol, concreteTypeTrees))
    case t: TypeTree => Specialization.unapply(t.tpe)
    case _ => None
  }
  
  def unapply(tpe: Type)(using Context): Option[Specialization] = tpe match {
    case AppliedType(tycon: Type, args: List[Type]) => Some(Specialization(tycon.typeSymbol, args.map(TypeTree(_))))
    case _ => None
  }
end Specialization

// Would be nice to define a Specialization class I think
//   -> Map the specialized type params to Int etc
//   -> Map the non-specialized type params to new type params
//   -> Be a canonical representation so we can store that in a set
//   -> Generate a name / string representation for use in new traits
//   -> Get the specialized list to apply


// TODO: Fix name generation which doesn't work if the tpye isn't provided explicitly




// Generate impl instead of generating anonymous classes every time to avoid insane code bloat
  // Do we really want the method definitions to live in the implementation classes or in the trait?|
  // I think in the trait is fine but note that this only actually saves any space if we don't use anonymous classes (because those copy parent members automatically it seems)
// Need to make sure all my examples are up to date, consistent with what we do and what we want to do so that they are actually useful for the future.
// Need to somehow make my naming a lot more consistent as well.
// Correctly generate names
// generate classes as well
// do we actually want to generate Iteratorsp$Int
// should we be worried about the results that we generate causing more stuff to be generated?
// figure out why we generate the T version.
// Try to see if we can do with only types and not trees
// Synthesise Specialized instances so that people can't do stupid stuff like Specialized[Array[T]]. type x = Specialized[Array[Array[Int]]]
// Set the Synthetic flags somewhere
// Cache / only generate once instead of multiple times.
// Ideally standardise on either specialization or specializationMap

// TODO: Think carefully about use of primaryConstructor and the other appropriateConstructors call or whatever it was.

// Probably (tree)typemap

// Would it be better to just copy rather than creating everything from scratch? I think this is right

// 1. Figure out which specializations we need to generate
// 2. Generate ArrayIterator$sp$Int and ArrayIterator$impl$Int wherever they live
// 3. Replace ArrayIterator[Int] with ArrayIterator$sp$Int
// 4. Replace new ArrayIterator[Int](xs) {} with new ArrayIterator$impl$Int(xs) {}
// 5. Somehow figure out the caching
// 6. Delete references to Specialized I guess

// Synthesize Specialized[T] instances.
// TODO: Need to try with a bigger project with multiple packages later on to see if we get the behaviour that we are expecting to get in terms of the classes that we generate.

// Need to ban all of these but we will do that earlier I guess?
// Vec[Vec[Int]] hehe <- fine
// Vec[S, S[T]: Specialized] <- banned
// Vec[S, T[T]: Specialized] <- banned
// Vec[Array[T]: Specialized] <- banned


// TODO: Prune the generated anonymous classes.


// need to test with explicit evidence / our own custom type classes
// TODO: Make sure name encoding is fully qualified - e.g. potential for conflicts if we define our own class Int.
  // // TODO: check that we have a single type var only

// trait Vec$Sp[S] extends Vec[S, Int, Int, Int, Int]
// inline trait Two[S: Specialized] extends Vec$sp[S]
// does mean that any methods in the original trait lose their specialization - maybe we /should/ make the generated traits inline?
// hmm but we can't do that because we need the methods called on the Vec$Sp trait to be the specialized ones - that is really important. 
// Could potentially copy over the inline based on whether Two is inline or not? Needs some thought.


// In the case of foo[S](a: Vec[S, Int, Int, Int, Int]) I think we ideally do want this because we should be able to get speed gains by accessing the specialized members 

// Should we allow these? I think they are all fine
// inline trait Two[S: Specialized] extends Vec[S, Int, Int, Int, Int]
// inline trait Two[S] extends Vec[S, Int, Int, Int, Int] // Maybe worth warning? Perhaps behind an extra flag
// trait Two[S] extends Vec[S, Int, Int, Int, Int]
// TODO: We want a self reference case where Vec[T] has some method that takes a Vec[Int] for example.
// TODO: Fix broken "inline" tests 
// TODO: Only specialize if there is some material increase in specialization - I think only if at least one new parameter gets fully specialized
// Maybe it is better to not allow partial specializations --  we can think about that.
// TODO: Need to add the rule that we need to directly extend inline traits to pass parameters in the same way as for normal traits
// See inline-trait-param-no-shadow.scala

// could ban normal traits from mixing in inline traits on the basis that 
// it's a very unlikely usecase and would fix the mixin problem - although
// we could just try and fix the mixin problem properly.

// TOOD: probably need to do this from Timothée:
  // - private members are not renamed, only private parameter accessors are;
// this needs to be changed so that all overridable private members are renamed
// Implement postphase checks for inline traits
// TODO: Don't synthesize specialized instances for random generic types probably - as Hamza said we want to be able to control the specialization
// TODO: If we are to ban trait extends inline trait then need to fix some tests.
// TODO: Check that when we extend parents we actually do so in the specialized forms.
// TODO: Fix specialized-trait-collections-example.scala
// TODO: Test extensively with inline methods and inline traits.

// TODO:
  // Need  to enforce these:
//  - can extend only a single specialized trait,
//  - cannot mix in further classes or traits, and
//  - cannot contain member definitions.
// I'm not sure we strictly need them though.

// TODO: Put classes onto the classpath as desired.

// Concerns:
//  - The superclass of `C` is a top class, or `C` itself is a top class.
// Drop all specialized trait parameters of A

// If we can manage to get rid of the inheritance there that could be helpful in terms of avoiding multiple values
// BUT: generate a version which is with just inline traits that has this problem as well.
// Need to deal with the caching at some point
// These implementation classes are type correct as long as we inject the knowledge that a specialization trait
// like `Seq$sp$Int` is equal to its parameterized version `Seq[Int]`

// Warning for dropping Specialized qualifier or it doesn't compile?
