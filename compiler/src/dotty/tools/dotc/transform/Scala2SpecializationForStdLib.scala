package dotty.tools.dotc.transform

import dotty.tools.dotc.ast.{TreeTypeMap, untpd}
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Decorators.{toTermName, toTypeName}
import dotty.tools.dotc.core.DenotTransformers.InfoTransformer
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.Names.*
import dotty.tools.dotc.core.NameOps.setterName
import dotty.tools.dotc.core.Phases.*
import dotty.tools.dotc.core.Scopes.*
import dotty.tools.dotc.core.StdNames.{nme, tpnme}
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.SymDenotations.*
import dotty.tools.dotc.core.Types.*

import scala.collection
import scala.collection.immutable
import scala.collection.mutable

/**
 * Implements just enough Scala2-style specialization for the standard library and only the standard library
 *
 * THIS IS NOT TO BE USED ELSEWHERE. DO NOT TRY TO USE IT FOR YOUR OWN CODE. DO NOT ASK FOR IT TO BE SUPPORTED.
 */
object Scala2SpecializationForStdLib:
  /* The stdlib only has specialized types remaining, i.e., there are no `def`s with `@specialized` type params.
   * Per chapter 4 in Iulian Dragos's thesis (available at https://doi.org/10.5075/epfl-thesis-4820), type specialization means:
   * 1. Generate specialized versions of all methods whose signature contain a specialized type or an array of a specialized type.
   *    In the base class/trait, these delegate to the unspecialized method by boxing arguments and unboxing return values as needed.
   * 2. Generate a `def specInstance$: Boolean` method that returns `false` in the base class/trait.
   * 3. Wrap the primary constructor in `if !specInstance$ then ...`
   *    (This must come after the Constructors phase, which creates primary constructors)
   * 4. Generate specialized subclasses:
   *    a. handle 'final' classes:
   *      i. the original class must be 'final' when pickled if it was declared as such
   *      ii. the original class must not be 'final' when emitted
   *    b. subclass methods work in the opposite way:
   *      i. the unspecialized versions delegates via boxing/unboxing to their specialized form
   *         (this must be done after the Getters phase, otherwise we cannot delegate `val`s)
   *      ii. the specialized form is the original body but specialized
   *    c. subclass `specInstance$` methods return `true`
   *    d. subclass constructors are a specialized form of the super one (it calls the super one, which will have no effect due to the `specInstance$` check)
   * 5. Ensure that every creation of a class with specializations for which the type arguments match a specialization
   *    is replaced with a creation of the specialized form (e.g., for `C[@specialized(Int) T, U]`, `C[Int, String]` is replaced, but `C[String, Int]` isn't)
   * 6. Ensure that every method call on a class with specializations for which the type arguments of the class match a specialization
   *    are replaced with a call to the specialized method (e.g., for `def foo(n: T): Unit` on `C[@specialized(Int) T]`, any call to `foo` for which `T = Int` is replaced).
   *    (These last two must come after the typer, since we may have methods like `def x: T` that would be overloaded only on the return type,
   *     which isn't permitted, so we can't simply use `@targetName`)
   *
   * Because this is only for the standard library, steps 2, 3, 5 and 6 are not needed.
   * The compiler already has SpecializeFunctions and SpecializeTuples for programs that use the stdlib.
   */

  // TODO: Get rid of this once #27515 is in -- it's caused by CC doing rechecking very late
  def ignoreTypeBoundsError()(using Context): Boolean =
    ctx.owner.name.endsWith(ourNames.specializationSuffix)

  // Avoid polluting nme.*
  private object ourNames:
    val annotation: TermName = "annotation".toTermName
    val unspecialized: TypeName = "unspecialized".toTypeName
    val specialized: TypeName = "specialized".toTypeName
    val specializable: TermName = "Specializable".toTermName
    val specializationPrefix = "$mc"
    val specializationSuffix = "$sp"

  /** Name + specialized types -> specialized name */
  private def specializedName[T <: Name](name: T, specialization: Specialization, memberType: Type | Null)(using Context): name.ThisName =
    // Match what Scala 2 does: filter type parameters to those used and sort them by name, regardless of the order in which they're declared
    val usedSpec = memberType match
      case null | NoType => specialization
      // for specializing constructors, the constructor itself has type parameters, so we match on names
      case tpr: TypeParamRef => specialization.filter((t, _) => t.existsPart {
        case n: NamedType => n.name == tpr.paramName
        case _ => false
      })
      // (which can include arrays...)
      case AppliedType(tycon, List(tpr: TypeParamRef)) if tycon =:= defn.ArrayType => specialization.filter((t, _) => t.existsPart {
        case n: NamedType => n.name == tpr.paramName
        case _ => false
      })
      case tpe => specialization.filter((t, _) => tpe.existsPart(_ == t))
    val sortedSpec = usedSpec.toList.sortBy(_._1.asInstanceOf[NamedType].name.toString)
    val specString = sortedSpec.map(e => ctx.definitions.typeTag(e._2)).mkString("")
    // We have to go through String because ++ on DerivedName does the append before the derivation (i.e., `original$appended$derived`)
    name.likeSpaced((name.toString + ourNames.specializationPrefix + specString + ourNames.specializationSuffix).toTermName)

  /** Specialized name -> unspecialized form (assumes the name is specialized) */
  private def unspecializedName[T <: Name](name: T): name.ThisName =
    val str = name.toString
    val idx = str.indexOf(ourNames.specializationPrefix)
    val unspecStr = str.substring(0, idx)
    name.likeSpaced(unspecStr.toTermName)

  private type Specialization = immutable.Map[Type, NamedType]
  private val emptySpecialization: Specialization = immutable.Map.empty
  private type ParameterSpecializations = collection.Map[Type, collection.Seq[NamedType]]

  private final class Specializer(specialization: Specialization, specializationsByType: collection.Map[Name, ParameterSpecializations],
                                  superTypeSymbol: Symbol, subTypeSymbol: Symbol = NoSymbol,
                                  unspecializedSymbols: List[Symbol] = Nil, specializedSymbols: List[Symbol] = Nil) {
    def specialize(tree: Tree)(using Context): Tree =
      specializeCore(tree).asInstanceOf[Tree]

    def specialize(tp: Type, topLevelPolyType: Boolean = false, scopeForTypeMembers: Option[MutableScope] = None, onlyArrays: Boolean = false)(using Context): Type =
      specializeCore(tp, topLevelPolyType, scopeForTypeMembers, onlyArrays).asInstanceOf[Type]

    def withExtraMemberSubst(unspecialized: Symbol, specialized: Symbol): Specializer =
      Specializer(specialization, specializationsByType, superTypeSymbol, subTypeSymbol, unspecialized :: unspecializedSymbols, specialized :: specializedSymbols)

    private def specializeCore(item: Tree | Type, topLevelPolyType: Boolean = false, scopeForTypeMembers: Option[MutableScope] = None, onlyArrays: Boolean = false)(using Context): Tree | Type =
      val paramRefsSpecialization = mutable.Map.empty[TypeParamRef, Type]
      val paramRefsReplacement = mutable.Map.empty[TypeParamRef, Type]
      val typeMap = new TypeMap:
        override def apply(tp: Type): Type =
          tp match
            // Basic case: specialized type
            case tr: TypeRef =>
              specialization.get(tr) match
                case Some(replacement) => replacement
                case None =>
                  scopeForTypeMembers match
                    case Some(decls) if tr.prefix =:= superTypeSymbol.thisType =>
                      val sym = newSymbol(subTypeSymbol, tr.name, tr.symbol.flags | Synthetic, tr.info)
                      decls.enter(sym)
                      sym.typeRef
                    case _ => mapOver(tp)
            // Specialized type parameter if we are specializing a top-level polytype
            case tpr: TypeParamRef =>
              paramRefsSpecialization.get(tpr).orElse(paramRefsReplacement.get(tpr)).getOrElse(mapOver(tpr))
            // Method type, whose parameter names must be mangled for specialization if their types are specialized
            case meth: MethodType =>
              val resultParamTypes = meth.paramInfos.map(apply)
              val resultParamNames = meth.paramNames.zip(meth.paramInfos).zip(resultParamTypes).map { case ((n, i), specializedI) =>
                if i =:= specializedI then n else specializedName(n, specialization, i)
              }
              meth.companion(resultParamNames, resultParamTypes, apply(meth.resType))
            // Applied type, which may be...
            case AppliedType(tycon: NamedType, args) =>
              if tycon == superTypeSymbol.typeRef then
                // ... the type being specialized, or...
                val (specializedArgs, unspecializedArgs) = args.partition {
                  case tpr: TypeParamRef => paramRefsSpecialization.contains(tpr)
                  case _ => false
                }
                if paramRefsSpecialization.nonEmpty && specializedArgs.size == paramRefsSpecialization.size then
                  val specialized = tycon.typeSymbol.owner.info.decls.lookup(specializedName(tycon.name, specialization, null)).typeRef
                  specialized.appliedTo(unspecializedArgs.map(apply))
                else mapOver(tp)
              else
                specializationsByType.get(tycon.name) match
                  case Some(specsByParam) =>
                    // ...another type that may have the same specialization.
                    val specializedArgs = tycon.typeParamSymbols.zip(args).flatMap((tp, ta) => specialization.get(ta).map(sa => (tp, sa)))
                    if specializedArgs.isEmpty then
                      mapOver(tp)
                    else
                      // (but, e.g., `C[T]` may be specialized for `Int, Double` but extend `B[T]` that is only specialized for `Int`!)
                      val tyconHasSpecialization = specializedArgs.forall((tp, sa) => specsByParam(tp.typeRef).contains(sa))
                      if tyconHasSpecialization then
                        val unspecializedArgs = args.filterNot(specialization.contains)
                        val specialized = tycon.typeSymbol.owner.info.decls.lookup(specializedName(tycon.name, specialization, null)).typeRef
                        specialized.appliedTo(unspecializedArgs.map(apply))
                      else mapOver(tp)
                  case None =>
                    if !onlyArrays || tycon =:= defn.ArrayType then mapOver(tp)
                    else tp
            case _ =>
              mapOver(tp)

      def replaceSelectionOnThis(qual: Name): Option[Tree] =
        val onSup = superTypeSymbol.info.member(qual)
        val specQual = specializedName(qual, specialization, onSup.info)
        val onSub = subTypeSymbol.info.member(specQual)
        if onSub.exists then
          Some(ref(onSub.symbol))
        else if onSup.exists then
          Some(Super(This(subTypeSymbol.asClass), tpnme.EMPTY).select(qual))
        else
          None

      val map = new TreeTypeMap(
        typeMap = typeMap,
        treeMap = {
          case tree@Select(This(Ident(n)), qual) if n == superTypeSymbol.name || n.isEmpty => replaceSelectionOnThis(qual).getOrElse(tree)
          case tree@Ident(qual) => replaceSelectionOnThis(qual).getOrElse(tree)
          case tree => tree
        },
        oldOwners = unspecializedSymbols,
        newOwners = specializedSymbols,
        substFrom = unspecializedSymbols,
        substTo = specializedSymbols
      )
      item match
        case poly: PolyType if topLevelPolyType =>
          def matchesByName(tp: Type, name: TypeName): Boolean = tp match
            case nt: NamedType => nt.name == name
            case _ => false

          val retainedIndices = poly.paramRefs.zipWithIndex.flatMap((pr, idx) =>
            specialization.find((k, _) => matchesByName(k, pr.paramName)) match
              case Some((k, v)) =>
                paramRefsSpecialization(pr) = v
                None
              case None =>
                Some(idx)
          )
          if retainedIndices.isEmpty then
            map.mapType(poly.resType)
          else
            PolyType(retainedIndices.map(poly.paramNames(_)))(
              p =>
                for (oldTpr, newTpr) <- retainedIndices.map(poly.paramRefs(_)).zip(p.paramRefs) do
                  paramRefsReplacement(oldTpr) = newTpr
                retainedIndices.map(poly.paramInfos(_)).map(pi => TypeBounds(map.mapType(pi.lo), map.mapType(pi.hi))),
              _ => map.mapType(poly.resType)
            )
        case tp: Type => map.mapType(tp)
        case t: Tree => map.apply(t)
  }

  /** Generates specialized subclasses (everything except what the phase below does) */
  final class PrePickler extends Phase with InfoTransformer:
    private val _specializationsByType: mutable.Map[Name, ParameterSpecializations] = mutable.Map.empty
    /** Type name -> [type parameter -> specializations]. Only contains type names that have specializations, i.e., value lengths != 0 */
    def specializationsByType: collection.Map[Name, ParameterSpecializations] = _specializationsByType

    override def phaseName: String =
      "scala2SpecializationForStdLib.PrePickler"

    override def isEnabled(using Context): Boolean =
      ctx.settings.YcompilingStdLib.value

    override def changesMembers: Boolean = true

    override def changesParents: Boolean = true

    override protected def infoMayChange(sym: Symbol)(using Context): Boolean =
      sym.isClass && !sym.name.endsWith(ourNames.specializationSuffix)

    override protected def run(using Context): Unit =
      ctx.compilationUnit.tpdTree = handleStat(ctx.compilationUnit.tpdTree)

    override def transformInfo(tp: Type, sym: Symbol)(using Context): Type = tp match
      case ci: ClassInfo =>
        specializedBaseClasses(tp) match
          case Nil => ci
          // Replace the parent class if there is a more specialized one
          case hd :: tl if !hd.typeSymbol.is(Trait) => ci.derivedClassInfo(declaredParents = hd :: ci.parents.tail ++ tl)
          case traits => ci.derivedClassInfo(declaredParents = ci.parents ++ traits)
      case _ =>
        tp

    private def handleStat(stat: Tree)(using Context): Tree = stat match
      case p @ PackageDef(pid, stats) =>
        var changed = false
        val specializedStats = stats.flatMap {
          case td: TypeDef =>
            val specialized = specializeTypeDef(td)
            if specialized.isEmpty then List(td)
            else
              changed = true
              specialized
          case s =>
            val handled = handleStat(s)
            if handled `ne` s then changed = true
            List(handled)
        }
        if changed then cpy.PackageDef(p)(pid = pid, stats = specializedStats)
        else p
      case s =>
        s

    /** Returns Nil if there is nothing to do, or a list to replace the original typedef with. */
    private def specializeTypeDef(td: TypeDef)(using Context): List[TypeDef] = td.rhs match
      case t: Template =>
        val superTypeSymbol = td.symbol
        // First, collect specialized params (from the body, where their annotations have been preserved, not from the template constr),
        val typeParams = t.body.collect { case td2: TypeDef => td2 }
        // and find which types the params are specialized for.
        val specializedTypeParams =
          typeParams.flatMap(tp =>
            val types = tp.mods.annotations.flatMap {
              case Apply(Select(New(Ident(ourNames.specialized)), nme.CONSTRUCTOR), args) => specializationsForTypeParameter(args)
              case _ => Nil
            }
            if types.isEmpty then None
            else Some((tp.tpe, types))
          ).toMap
        // The current type may also have specialized parents
        val newSuperTypeParents = specializedBaseClasses(superTypeSymbol.info) match {
          case Nil => t.parents
          case hd :: tl if !hd.typeSymbol.is(Trait) =>
            // Replace the parent class if there is a more specialized one
            val originalNewCls = t.parents.head.asInstanceOf[Apply]
            val hdCtor = hd match {
              case AppliedType(hdTycon, hdArgs) => New(hdTycon).select(nme.CONSTRUCTOR).appliedToTypes(hdArgs)
              case _ => New(hd).select(nme.CONSTRUCTOR)
            }
            hdCtor.appliedToArgs(originalNewCls.args) :: t.parents.tail ++ tl.map(TypeTree(_))
          case traits => t.parents ++ traits.map(TypeTree(_))
        }
        // Then, transform that information to one [type param => specialized type] map per specialization
        // (e.g., [@specialized(Int, Double) X, @specialized(Short) Y, Z] => [[X => Int, Y => Short], [X => Double, Y => Short]]
        val specializations = specializedTypeParams.foldLeft(List(emptySpecialization)) {
          case (acc, (tp, types)) =>
            for
              typ <- types
              rest <- acc
            yield rest + (tp -> typ)
        }.filter(_.nonEmpty)
        if specializedTypeParams.nonEmpty then
          _specializationsByType(td.name) = specializedTypeParams
        // Collect the members that need a specialized version, which will exist in both the supertype and the specialized subtypes, but with different bodies.
        val membersNeedingSpecialization =
          if specializations.isEmpty then Nil
          else
            val testSpecializer = Specializer(specializations.head, specializationsByType, superTypeSymbol)
            t.body.collect { case m: ValOrDefDef if needsSpecializing(m, testSpecializer) => m }
        // Use the specialization maps to create the specialized methods in the supertype,
        val superTypeSpecializedMethods = membersNeedingSpecialization.flatMap {
          case dd: DefDef => specializations.flatMap(sp => specializedForSuperType(dd, sp))
          case _ => Nil
        }
        // which should be the only change we need to make to that type.
        // Except we also need to make private nonspecialized vals protected,
        // so that specialized bodies in subtypes can use them.
        // e.g., in `class C[@specialized(Int) T](arr: Array[T]) { var pos = 0; def get = arr(pos) }`,
        // `get` will be specialized as it returns `T` , but `pos` won't, yet `get$mcI$sp`'s body is `this.arr$mcI$sp(super.pos)`.
        // We must also create the setter now, so `Getters` doesn't run into trouble trying to create it later.
        val extraMembers =
          if specializations.isEmpty then Nil
          else t.body.flatMap {
            case vd: ValDef if !membersNeedingSpecialization.contains(vd) =>
              val vdSym = vd.symbol.asTerm
              if vdSym.is(Private) then
                vdSym.resetFlag(Private)
                vdSym.setFlag(Protected)
                val newSetter = vdSym.copy(
                  name = vdSym.name.setterName,
                  flags = (vdSym.flags | Method | Accessor) &~ Mutable,
                  info = MethodType(vdSym.info.widenExpr :: Nil, defn.UnitType)
                ).entered
                Some(DefDef(newSetter.asTerm, unitLiteral))
              else
                None
            case _ => None
          }
        val specializedSuperType = cpy.TypeDef(td)(rhs = cpy.Template(t)(
          parents = newSuperTypeParents,
          body = t.body ++ superTypeSpecializedMethods ++ extraMembers
        ))
        // Then use the specialization maps to create specialized subtypes,
        val superTypeConstrSymbol = t.constr.symbol
        val specializedSubTypes = specializations.map(spec =>
          val subTypeName = specializedName(td.name, spec, null)
          // case-to-case inheritance is prohibited so never create a case class
          val subTypeFlags = ((superTypeSymbol.flags &~ CaseClass) &~ Touched) | Synthetic
          val subTypeCompleter = new LazyType {
            def complete(denot: SymDenotation)(using Context): Unit = {
              val cls = denot.asClass.classSymbol
              val decls = newScope(superTypeSymbol.owner.nestingLevel + 1)
              val specializer = Specializer(spec, specializationsByType, superTypeSymbol, cls)
              val directParent = superTypeSymbol.typeRef.appliedTo(typeParams.map(tp => specializer.specialize(tp.tpe, scopeForTypeMembers = Some(decls))))
              val indirectParents = specializedBaseClasses(directParent)
              // for traits, Object must be their first supertype;
              // for classes, the supertype we're specializing must be first so that constructor arguments are forwarded properly,
              // and we don't want to inherit from their own class supertype
              val parents =
                if superTypeSymbol.is(Trait)
                then defn.ObjectType :: directParent :: indirectParents
                else directParent :: indirectParents
              denot.info = ClassInfo(superTypeSymbol.owner.thisType, cls, parents, decls)
            }
          }
          val subTypeSymbol = newClassSymbol(
            superTypeSymbol.owner,
            subTypeName,
            // specialized traits never have an $init$
            if superTypeSymbol.is(Trait) then subTypeFlags | NoInitsTrait else subTypeFlags,
            subTypeCompleter
          ).entered
          // Create members with the opposite forwarding direction as the supertype,
          val members =
            // with the caveat that `trait`s get no new bodies (presumably because Scala 2 originally targeted JDKs < 8 without default interface methods?)
            if td.symbol.is(Trait) then Nil
            else
              // Transform the vals first so they can be used when specializing the def bodies
              val memberVals = membersNeedingSpecialization.collect { case vd: ValDef => vd }
              val specializedMemberVals = memberVals.map(vd => specializedForSubType(vd, spec, superTypeSymbol, subTypeSymbol))
              val specializedMemberDefs = membersNeedingSpecialization.flatMap {
                case dd: DefDef => specializedForSubType(dd, spec, memberVals, specializedMemberVals, superTypeSymbol, subTypeSymbol)
                case _ => Nil
              }
              specializedMemberVals ++ specializedMemberDefs
          val specializer = Specializer(spec, specializationsByType, superTypeSymbol, subTypeSymbol)
          val constrInfo = specializer.specialize(superTypeConstrSymbol.info, topLevelPolyType = true)
          val constrSymbol = newSymbol(subTypeSymbol, nme.CONSTRUCTOR, superTypeConstrSymbol.flags | Synthetic, constrInfo).entered
          val parentArgs = members.map(_.symbol).filter(_.is(ParamAccessor)).map(ref)
          val constrDef = DefDef(constrSymbol)
          ClassDef(subTypeSymbol, constrDef, members, parentArgs)
        )
        specializedSuperType :: specializedSubTypes
      case _ =>
        Nil

    /** Returns (type, type arguments) tuples */
    private def specializedBaseClasses(tp: Type)(using Context): List[Type] =
      tp.parents.flatMap {
        case app@AppliedType(tycon: NamedType, args) =>
          _specializationsByType.get(tycon.name) match
            case Some(specializations) =>
              val thisSpecialization = tycon.typeParamSymbols.zip(args).flatMap((tpSym, a) =>
                specializations.get(tpSym.typeRef) match
                  case Some(specializedTypes) =>
                    if specializedTypes.exists(_ =:= a) then Some(tpSym.typeRef -> a.asInstanceOf[NamedType])
                    else None
                  case None =>
                    None
              ).toMap[Type, NamedType]
              if thisSpecialization.size == specializations.size then
                val specName = specializedName(tycon.name, thisSpecialization, null)
                val specialized = tycon.typeSymbol.owner.info.decls.lookup(specName)
                val remainingTypeArgs = tycon.typeParamSymbols.zip(args).filterNot((tpSym, _) => thisSpecialization.contains(tpSym.typeRef)).map(_._2)
                List(specialized.typeRef.appliedTo(remainingTypeArgs))
              else
                Nil
            case None =>
              specializedBaseClasses(app)
        case other =>
          specializedBaseClasses(other)
      }

    private def specializationsForTypeParameter(annots: List[untpd.Tree])(using Context): List[NamedType] = annots match
      case untpd.Select(untpd.Ident(ourNames.specializable), name) :: Nil =>
        name.toString match
          case "Primitives" =>  List(defn.ByteType, defn.ShortType, defn.IntType, defn.LongType, defn.CharType, defn.FloatType, defn.DoubleType, defn.BooleanType, defn.UnitType)
          case "Everything" =>  List(defn.ByteType, defn.ShortType, defn.IntType, defn.LongType, defn.CharType, defn.FloatType, defn.DoubleType, defn.BooleanType, defn.UnitType /* not actually used: defn.AnyRefType */)
          // unused: case "Bits32AndUp" => List(defn.IntType, defn.LongType, defn.FloatType, defn.DoubleType)
          // unused: case "Integral" =>    List(defn.ByteType, defn.ShortType, defn.IntType, defn.LongType, defn.CharType)
          // unused: case "AllNumeric" =>  List(defn.ByteType, defn.ShortType, defn.IntType, defn.LongType, defn.CharType, defn.FloatType, defn.DoubleType)
          // unused: case "BestOfBreed" => List(defn.IntType, defn.DoubleType, defn.BooleanType, defn.UnitType, defn.AnyRefType)
          // unused: case "Unit" =>        List(defn.UnitType)
          case "Arg" =>         List(defn.IntType, defn.LongType, defn.FloatType, defn.DoubleType)
          case "Args" =>        List(defn.IntType, defn.LongType, defn.DoubleType)
          case "Return" =>      List(defn.IntType, defn.LongType, defn.FloatType, defn.DoubleType, defn.BooleanType, defn.UnitType)
      case idents =>
        idents.map { case untpd.Ident(n) => n match
          case nme.Byte => defn.ByteType
          case nme.Short => defn.ShortType
          case nme.Int => defn.IntType
          case nme.Long => defn.LongType
          case nme.Char => defn.CharType
          case nme.Float => defn.FloatType
          case nme.Double => defn.DoubleType
          case nme.Boolean => defn.BooleanType
          case nme.Unit => defn.UnitType
        }

    // not handling arrays here, not needed for the stdlib
    private def castTo(arg: Tree, specializedType: Type)(using Context): Tree =
      if specializedType == defn.UnitType then Block(List(arg), unitLiteral)
      else TypeApply(Select(arg, nme.asInstanceOf_), List(TypeTree(specializedType)))

    private def needsSpecializing(m: ValOrDefDef, specializer: Specializer)(using Context): Boolean =
      val mSym = m.symbol
      // unlike final classes which can be specialized, final members cannot
      // also we don't need any specialized `var`s for the stdlib so let's not do vars/setters
      // (which cause problems later due to specialization name mangling making them not look like setters)
      // (why do we have to check annotations in two places? not clear, but, e.g.,
      //  in Function2 `curried`'s is in m.mods.annotations while `tupled`'s is in mSym.annotations)
      val optOut = mSym.isOneOf(Final | Mutable) || mSym.isSetter || (m.mods.annotations.iterator ++ mSym.annotations.iterator.map(_.tree)).exists {
        case Apply(Select(New(Select(Ident(ourNames.annotation), ourNames.unspecialized)), nme.CONSTRUCTOR), Nil) => true
        case _ => false
      }
      // however, we must replicate param accessors in the subclass, even if they're not specialized
      val optIn = mSym.is(ParamAccessor)
      // to check if a member needs specializing, we check if its type under specialization would change
      // except that only arrays are specialized as applied types, e.g., `def x: Iterable[T]` is not specialized for a specialized `T`
      // (whereas `def x: Array[T]` and `def x: T` are)
      optIn || (!optOut && {
        val tp = mSym.info
        val specTp = specializer.specialize(tp, onlyArrays = true)
        !(tp =:= specTp)
      })

    private def specializedFlags(sym: Symbol, specializedName: Name)(using Context): FlagSet =
      val flags = (sym.flags &~ Deferred) | Synthetic
      val overridesParent = sym.owner.asClass.parentSyms.exists(_.info.member(specializedName).exists)
      if overridesParent then flags | Override else flags

    private def specializedForSuperType(dd: DefDef, specialization: Specialization)(using Context): Option[DefDef] =
      val originalSym = dd.symbol
      val originalOwner = originalSym.owner
      val name = specializedName(dd.name, specialization, originalSym.info)
      if originalOwner.info.member(name).exists then
        // Can happen when multiple specializations share a member,
        // e.g., `case class X[A, B](a: A, b: B)` will have a `copy$default$1` for `a`;
        // if `A` is specialized and `B` has more than one specialization,
        // then we will specialize `copy$default$1` under multiple specializations with the same A
        None
      else
        val specializer = Specializer(specialization, specializationsByType, originalOwner)
        val info = specializer.specialize(originalSym.info)
        val flags = specializedFlags(originalSym, name)
        val sym = newSymbol(originalOwner, name, flags, info).entered

        // Replace TypeParamRefs to a method's polytype with the actual method param
        def fixup(t: Type, paramss: List[List[Tree]]): Type = (originalSym.info, info) match
          case (ogPoly: PolyType, poly: PolyType) =>
            val map = new TypeMap:
              override def apply(tp: Type): Type = tp match
                case tpr: TypeParamRef =>
                  val ogIdx = ogPoly.paramRefs.indexOf(tpr)
                  val idx = if ogIdx == -1 then poly.paramRefs.indexOf(tpr) else ogIdx
                  if idx == -1 then mapOver(tp)
                  else paramss.head(idx).tpe
                case _ =>
                  mapOver(tp)
            map.apply(t)
          case _ =>
            t

        // Some of the casts may not be necessary, but they won't hurt either
        Some(DefDef(sym, paramss =>
          var base = ref(originalSym)
          var remainingParamss = paramss
          if paramss.headOption.exists(_.headOption.exists(_.isInstanceOf[TypeTree])) then
            base = base.appliedToTypeTrees(paramss.head)
            remainingParamss = paramss.tail
          val call = remainingParamss.zip(originalSym.info.paramInfoss).foldLeft(base) {
            case (fn, (args, infos)) =>
              // box to the generic parameter which is the argument type of the callee
              fn.appliedToArgs(args.zip(infos).map((a, i) => castTo(a, fixup(i, paramss))))
          }
          // unbox to the specialized type which is the result type of the caller
          castTo(call, fixup(info.finalResultType, paramss))
        ))

    private def specializedForSubType(vd: ValDef,
                                      specialization: Specialization,
                                      superTypeSymbol: Symbol, subTypeSymbol: Symbol)(using Context): ValDef =
      val originalSym = vd.symbol
      val specializer = Specializer(specialization, specializationsByType, superTypeSymbol, subTypeSymbol)
      val specInfo = specializer.specialize(originalSym.info)
      // we can be in one of three cases:
      // - param accessor that needs specialization => generate just the specialized form
      // - param accessor that doesn't need specialization => generate just the override form
      // - non-param accessor that needs specialization => generate just the specialized form
      if specInfo =:= originalSym.info then
        // must be a param accessor, or we wouldn't even be looking at this
        ValDef(newSymbol(subTypeSymbol, vd.name, originalSym.flags | Synthetic | Override, specInfo).entered)
      else
        val specName = specializedName(vd.name, specialization, originalSym.info)
        val flags = specializedFlags(originalSym, specName)
        val specSym = newSymbol(subTypeSymbol, specName, flags, specInfo).entered
        ValDef(specSym, specializer.specialize(vd.rhs))

    private def specializedForSubType(dd: DefDef, specialization: Specialization,
                                      unspecializedMembers: List[ValOrDefDef], specializedMembers: List[ValOrDefDef],
                                      superTypeSymbol: Symbol, subTypeSymbol: Symbol)(using Context): List[DefDef] =
      // Each def has two variants in subtypes:
      val originalSym = dd.symbol
      // the specialized one, which has a copy of the body but specialized,
      val specName = specializedName(dd.name, specialization, originalSym.info)
      val partialSpecializer = Specializer(specialization, specializationsByType, superTypeSymbol, subTypeSymbol, unspecializedMembers.map(_.symbol), specializedMembers.map(_.symbol))
      val specInfo = partialSpecializer.specialize(originalSym.info)
      val specSym = newSymbol(subTypeSymbol, specName, originalSym.flags | Synthetic | Override, specInfo).entered
      val fullSpecializer = partialSpecializer.withExtraMemberSubst(originalSym, specSym)
      val specDef = DefDef(specSym, paramss =>
        val withSubstParams = dd.rhs.subst(dd.paramss.flatten.map(_.symbol), paramss.flatten.map(_.symbol))
        fullSpecializer.specialize(withSubstParams)
      )
      // and the original one, which merely forwards
      val newSym = newSymbol(subTypeSymbol, dd.name, originalSym.flags | Synthetic | Override, specInfo).entered
      val newDef = DefDef(newSym, paramss => paramss.foldLeft(ref(specSym)) { case (fn, args) => fn.appliedToArgs(args) })
      List(specDef, newDef)

  /** Removes 'final' from specializable classes (step 4.a.ii) and generates unspecialized getters for specialized vals (4.b.i) */
  final class PreErasure extends Phase:
    override def phaseName: String =
      "scala2SpecializationForStdLib.PreErasure"

    override def isEnabled(using Context): Boolean =
      ctx.settings.YcompilingStdLib.value

    override def changesMembers: Boolean = true

    override protected def run(using Context): Unit =
      val ogPhase = ctx.base.phases.collectFirst{ case p: PrePickler => p }.get
      ctx.compilationUnit.tpdTree = handleStat(ctx.compilationUnit.tpdTree, ogPhase.specializationsByType)

    private def handleStat(stat: Tree, specializationsByType: collection.Map[Name, ParameterSpecializations])(using Context): Tree = stat match
      case p@PackageDef(pid, stats) =>
        val newStats = stats.mapConserve(s => handleStat(s, specializationsByType))
        if stats `ne` newStats then cpy.PackageDef(p)(pid = pid, stats = newStats)
        else p
      case td: TypeDef =>
        handleTypeDef(td, specializationsByType)
      case _ =>
        stat

    private def handleTypeDef(td: TypeDef, specializationsByType: collection.Map[Name, ParameterSpecializations])(using Context): Tree =
      if specializationsByType.contains(td.name) then
        td.symbol.resetFlag(Final)
        td
      else if td.name.endsWith(ourNames.specializationSuffix) then
        addUnspecializedGetters(td)
      else
        td.rhs match
          case t: Template =>
            val nested = t.body.mapConserve(s => handleStat(s, specializationsByType))
            if t.body `eq` nested then td
            else cpy.TypeDef(td)(rhs = cpy.Template(t)(body = nested))
          case _ =>
            td

    private def addUnspecializedGetters(td: TypeDef)(using Context): TypeDef =
      val tdSym = td.symbol
      td.rhs match
        case t: Template =>
          val unspecializedGetters = t.body.flatMap {
            case dd: DefDef if dd.name.endsWith(ourNames.specializationSuffix) =>
              val ogName = unspecializedName(dd.name)
              // Unfortunately, we have to match names as strings because DerivedNames can't be suffixed (they add the suffix before the derivation),
              // so we end up with different name kinds but the same name.
              if tdSym.info.decls.exists(_.name.toString == ogName.toString) then None
              else
                val ddSym = dd.symbol
                // some of the specialized defs are param accessors, but the unspecialized versions never are
                val ogSym = newSymbol(tdSym, ogName, ddSym.flags &~ ParamAccessor, ddSym.info).entered
                val ogDef = DefDef(ogSym, paramss => paramss.foldLeft(ref(ddSym)) { case (fn, args) => fn.appliedToArgs(args) })
                Some(ogDef)
            case _ => None
          }
          if unspecializedGetters.isEmpty then td
          else cpy.TypeDef(td)(rhs = cpy.Template(t)(body = t.body ++ unspecializedGetters))
        case _ =>
          td
