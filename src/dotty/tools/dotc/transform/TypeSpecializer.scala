package dotty.tools.dotc.transform

import dotty.tools.dotc.ast.{tpd, TreeTypeMap}
import dotty.tools.dotc.ast.Trees.{TypeApply, SeqLiteral}
import dotty.tools.dotc.ast.tpd._
import dotty.tools.dotc.core.Annotations.Annotation
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Decorators.StringDecorator
import dotty.tools.dotc.core.DenotTransformers.InfoTransformer
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.{Symbols, Flags}
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.transform.TreeTransforms.{TransformerInfo, MiniPhaseTransform}
import scala.collection.mutable

class TypeSpecializer extends MiniPhaseTransform  with InfoTransformer {
  
  override def phaseName = "specialize"

  final val maxTparamsToSpecialize = 2

  private final def nameToSpecialisedType(implicit ctx: Context) =
    Map("Byte" -> ctx.definitions.ByteType,
      "Boolean" -> ctx.definitions.BooleanType,
      "Short" -> ctx.definitions.ShortType,
      "Int" -> ctx.definitions.IntType,
      "Long" -> ctx.definitions.LongType,
      "Float" -> ctx.definitions.FloatType,
      "Double" -> ctx.definitions.DoubleType,
      "Char" -> ctx.definitions.CharType,
      "Unit" -> ctx.definitions.UnitType)

  private final def specialisedTypeToSuffix(implicit ctx: Context) =
    Map(ctx.definitions.ByteType -> "$mcB$sp",
      ctx.definitions.BooleanType -> "$mcZ$sp",
      ctx.definitions.ShortType -> "$mcS$sp",
      ctx.definitions.IntType -> "$mcI$sp",
      ctx.definitions.LongType -> "$mcJ$sp",
      ctx.definitions.FloatType -> "$mcF$sp",
      ctx.definitions.DoubleType -> "$mcD$sp",
      ctx.definitions.CharType -> "$mcC$sp",
      ctx.definitions.UnitType -> "$mcV$sp")

  private def primitiveTypes(implicit ctx: Context) =
    List(ctx.definitions.ByteType,
      ctx.definitions.BooleanType,
      ctx.definitions.ShortType,
      ctx.definitions.IntType,
      ctx.definitions.LongType,
      ctx.definitions.FloatType,
      ctx.definitions.DoubleType,
      ctx.definitions.CharType,
      ctx.definitions.UnitType
    )

  private val specializationRequests: mutable.HashMap[Symbols.Symbol, List[List[Type]]] = mutable.HashMap.empty

  private val newSymbolMap: mutable.HashMap[Symbol, List[mutable.HashMap[List[Type], Symbols.Symbol]]] = mutable.HashMap.empty

  override def transformInfo(tp: Type, sym: Symbol)(implicit ctx: Context): Type = {
    def generateSpecializations(remainingTParams: List[Name], remainingBounds: List[TypeBounds])
                               (instantiations: List[Type], names: List[String], poly: PolyType, decl: Symbol)
                               (implicit ctx: Context): List[Symbol] = {
      if (remainingTParams.nonEmpty) {
        val bounds = remainingBounds.head
        val specTypes = primitiveTypes.filter{ tpe => bounds.contains(tpe)}
        val specializations = (for (tpe <- specTypes) yield {
          generateSpecializations(remainingTParams.tail, remainingBounds.tail)(tpe :: instantiations, specialisedTypeToSuffix(ctx)(tpe) :: names, poly, decl)
        }).flatten
        specializations
      }
      else {
        generateSpecializedSymbols(instantiations.reverse, names.reverse, poly, decl)
      }
    }
    def generateSpecializedSymbols(instantiations: List[Type], names: List[String], poly: PolyType, decl: Symbol)
                                  (implicit ctx: Context): List[Symbol] = {
      val newSym =
        ctx.newSymbol(decl.owner, (decl.name + names.mkString).toTermName,
          decl.flags | Flags.Synthetic, poly.instantiate(instantiations.toList)) // Who should the owner be ? decl.owner ? sym ? sym.owner ? ctx.owner ?
      // TODO I think there might be a bug in the assertion at dotty.tools.dotc.transform.TreeChecker$Checker.dotty$tools$dotc$transform$TreeChecker$Checker$$checkOwner(TreeChecker.scala:244)
      // Shouldn't the owner remain the original one ? In this instance, the assertion always expects the owner to be `class specialization` (the test I run), even for methods that aren't
      //defined by the test itself, such as `instanceOf` (to which my implementation gives owner `class Any`).
      val prevMaps = newSymbolMap.getOrElse(decl, List()).reverse
      val newMap: mutable.HashMap[List[Type], Symbols.Symbol] = mutable.HashMap(instantiations -> newSym)
      newSymbolMap.put(decl, (newMap :: prevMaps.reverse).reverse)
      (newSym :: prevMaps.flatMap(_.values).reverse).reverse // All those reverse are probably useless
    }

    if((sym ne ctx.definitions.ScalaPredefModule.moduleClass) && !(sym is Flags.Package) && !sym.isAnonymousClass) {
      sym.info match {
        case classInfo: ClassInfo =>
          val newDecls = classInfo.decls.flatMap(decl => {
            if (shouldSpecialize(decl)) {
              decl.info.widen match {
                case poly: PolyType =>
                  if (poly.paramNames.length <= maxTparamsToSpecialize && poly.paramNames.length > 0)
                    generateSpecializations(poly.paramNames, poly.paramBounds)(List.empty, List.empty, poly, decl)
                  else Nil
                case nil => Nil
              }
            } else Nil
          })
          if (newDecls.nonEmpty) {
            val decls = classInfo.decls.cloneScope
            newDecls.foreach(decls.enter)
            classInfo.derivedClassInfo(decls = decls)
          }
        case nil =>
      }
      tp
    } else tp
  }

  def shouldSpecialize(decl: Symbol)(implicit ctx: Context): Boolean =
    specializationRequests.contains(decl) ||
      (ctx.settings.Yspecialize.value != "" && decl.name.contains(ctx.settings.Yspecialize.value)) ||
      ctx.settings.Yspecialize.value == "all"

  def registerSpecializationRequest(method: Symbols.Symbol)(arguments: List[Type])(implicit ctx: Context) = {
    if(ctx.phaseId > this.treeTransformPhase.id)
      assert(ctx.phaseId <= this.treeTransformPhase.id)
    val prev = specializationRequests.getOrElse(method, List.empty)
    specializationRequests.put(method, arguments :: prev)
  }

  def specializeForAll(sym: Symbols.Symbol)(implicit ctx: Context): List[List[Type]] = {
    registerSpecializationRequest(sym)(primitiveTypes)
    println("Specializing for all primitive types")
    specializationRequests.getOrElse(sym, Nil)
  }

  def specializeForSome(sym: Symbols.Symbol)(annotationArgs: List[Type])(implicit ctx: Context): List[List[Type]] = {
    registerSpecializationRequest(sym)(annotationArgs)
    println(s"specializationRequests : $specializationRequests")
    specializationRequests.getOrElse(sym, Nil)
  }

  def specializeFor(sym: Symbols.Symbol)(implicit ctx: Context): List[List[Type]] = {
    sym.denot.getAnnotation(ctx.definitions.specializedAnnot).getOrElse(Nil) match {
      case annot: Annotation =>
        annot.arguments match {
          case List(SeqLiteral(types)) =>
            specializeForSome(sym)(types.map(tpeTree => //tpeTree.tpe.widen))
              nameToSpecialisedType(ctx)(tpeTree.tpe.asInstanceOf[TermRef].name.toString()))) // Not sure how to match TermRefs rather than types. comment on line above was an attempt.
          case List() => specializeForAll(sym)
        }
      case nil =>
        if(ctx.settings.Yspecialize.value == "all") {println("Yspecialize set to all"); specializeForAll(sym) }
        else Nil
    }
  }

  override def transformDefDef(tree: DefDef)(implicit ctx: Context, info: TransformerInfo): Tree = {

    tree.tpe.widen match {

      case poly: PolyType if !(tree.symbol.isPrimaryConstructor
        || (tree.symbol is Flags.Label)) =>
        val origTParams = tree.tparams.map(_.symbol)
        val origVParams = tree.vparamss.flatten.map(_.symbol)
        println(s"specializing ${tree.symbol} for Tparams: $origTParams")

        def specialize(decl : Symbol): List[Tree] = {
          val declSpecs = newSymbolMap(decl)
          val newSyms = declSpecs.map(_.values).flatten
          /*for (newSym <- newSyms) {
            println(newSym)
          }*/
          val instantiations = declSpecs.flatMap(_.keys).flatten
          newSyms.map{newSym =>
            polyDefDef(newSym.asTerm, { tparams => vparams => {
              assert(tparams.isEmpty)
              //println(newSym + " ; " + origVParams + " ; " + vparams + " ; " + vparams.flatten + " ; " + vparams.flatten.map(_.tpe))
              new TreeTypeMap( //TODO Figure out what is happening with newSym. Why do some symbols have unmatching vparams and origVParams ?
                typeMap = _
                  .substDealias(origTParams, instantiations)
                  .subst(origVParams, vparams.flatten.map(_.tpe)),
                oldOwners = tree.symbol :: Nil,
                newOwners = newSym :: Nil
              ).transform(tree.rhs)
            }})
          }
        }
        //specializeFor(tree.symbol)  -> necessary ? This registers specialization requests, but do they still make sense at this point ? Symbols have already been generated
        val specializedMethods = newSymbolMap.keys.map(specialize).flatten.toList
        Thicket(tree :: specializedMethods)
      case _ => tree
    }
  }

  override def transformTypeApply(tree: tpd.TypeApply)(implicit ctx: Context, info: TransformerInfo): Tree = {
    val TypeApply(fun,args) = tree
    val newSymInfo = newSymbolMap(fun.symbol).flatten.toMap
    val specializationType: List[Type] = args.map(_.tpe.asInstanceOf[TypeVar].instanceOpt)
    val t = fun.symbol.info.decls
    if (t.nonEmpty) {
      t.cloneScope.lookupEntry(args.head.symbol.name)
      val newSym = newSymInfo(specializationType)
    }
    tree
  }
}
