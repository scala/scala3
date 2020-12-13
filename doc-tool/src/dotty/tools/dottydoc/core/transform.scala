package dotty.tools
package dottydoc
package core

import dotc.CompilationUnit
import dotc.core.Contexts.{Context, ctx}
import dotc.core.Phases.Phase
import model._
import model.internal._
import util.syntax._
import util.traversing._

object transform {
  /**
   * The idea behind DocMiniTransformations is to fuse transformations to the
   * doc AST, much like `MiniPhase` in dotty core - but in a much simpler
   * implementation
   *
   * Usage
   * -----
   *
   * Create a `DocMiniPhase` which overrides the relevant method:
   *
   * ```
   * override def transformDef(using Context) = {
   *   case x if shouldTransform(x) => x.copy(newValue = ...) :: Nil
   * }
   * ```
   *
   * On each node in the AST, the appropriate method in `DocMiniPhase` will be
   * called in the order that they are supplied in
   * `DocMiniphaseTransformations`.
   *
   * There won't be a match-error as `transformX` is composed with an
   * `identity` function.
   *
   * The transformations in `DocMiniTransformations` will apply transformations
   * to all nodes - this means that you do _not_ need to transform children in
   * `transformPackage`, because `transformX` will be called for the relevant
   * children. If you want to add children to `Package` you need to do that in
   * `transformPackage`, these additions will be persisted.
   *
   * Deleting nodes in the AST
   * -------------------------
   * To delete a node in the AST, simply return an empty list from transforming method
   */
  abstract class DocMiniTransformations extends Phase {
    def transformations: List[DocMiniPhase]

    override def runOn(units: List[CompilationUnit])(using Context): List[CompilationUnit] = {
      for {
        pack <- rootPackages(ctx.docbase.packages)
        transformed =  performPackageTransform(pack)
      } {
        ctx.docbase.packagesMutable -= pack.name
        transformed.foreach(p => ctx.docbase.packagesMutable += p.name -> p)
      }

      units
    }

    private def performPackageTransform(pack: Package)(using Context): List[Package] = {
      def transformEntity[E <: Entity](e: E, f: DocMiniPhase => E => List[E])(createNew: E => E): List[Entity] = {
        val transformEntities = transformations.foldLeft(e :: Nil) { case (oldEs, transf) =>
          oldEs.flatMap(f(transf))
        }
        transformEntities.map(createNew)
      }

      def traverse(ent: Entity): List[Entity] = ent match {
        case p: Package => transformEntity(p, _.packageTransformation) { p =>
          val newPackage = PackageImpl(
            p.symbol,
            p.annotations,
            p.name,
            p.members.flatMap(traverse),
            p.path,
            p.superTypes,
            p.comment,
            p.parent
          )

          // Update reference in context to newPackage
          ctx.docbase.packagesMutable += (newPackage.path.mkString(".") -> newPackage)

          newPackage
        }
        case t: TypeAlias => transformEntity(t, _.typeAliasTransformation) { t =>
          TypeAliasImpl(
            t.symbol,
            t.annotations,
            t.modifiers,
            t.name,
            t.path,
            t.alias,
            t.typeParams,
            t.comment,
            t.parent
          )
        }
        case c: Class => transformEntity(c, _.classTransformation) { cls =>
          ClassImpl(
            cls.symbol,
            cls.annotations,
            cls.name,
            cls.members.flatMap(traverse),
            cls.modifiers,
            cls.path,
            cls.typeParams,
            cls.constructors,
            cls.superTypes,
            cls.comment,
            cls.companionPath,
            cls.parent
          )
        }
        case cc: CaseClass => transformEntity(cc, _.caseClassTransformation) { cc =>
          CaseClassImpl(
            cc.symbol,
            cc.annotations,
            cc.name,
            cc.members.flatMap(traverse),
            cc.modifiers,
            cc.path,
            cc.typeParams,
            cc.constructors,
            cc.superTypes,
            cc.comment,
            cc.companionPath,
            cc.parent
          )
        }
        case trt: Trait => transformEntity(trt, _.traitTransformation) { trt =>
          TraitImpl(
            trt.symbol,
            trt.annotations,
            trt.name,
            trt.members.flatMap(traverse),
            trt.modifiers,
            trt.path,
            trt.typeParams,
            trt.traitParams,
            trt.superTypes,
            trt.comment,
            trt.companionPath,
            trt.parent
          )
        }
        case obj: Object => transformEntity(obj, _.objectTransformation) { obj =>
          ObjectImpl(
            obj.symbol,
            obj.annotations,
            obj.name,
            obj.members.flatMap(traverse),
            obj.modifiers,
            obj.path,
            obj.superTypes,
            obj.comment,
            obj.companionPath,
            obj.parent
          )
        }
        case df: Def => transformEntity(df, _.defTransformation) { df =>
          DefImpl(
            df.symbol,
            df.annotations,
            df.name,
            df.modifiers,
            df.path,
            df.returnValue,
            df.typeParams,
            df.paramLists,
            df.comment,
            df.implicitlyAddedFrom,
            df.parent
          )
        }
        case vl: Val => transformEntity(vl, _.valTransformation) { vl =>
          ValImpl(
            vl.symbol,
            vl.annotations,
            vl.name,
            vl.modifiers,
            vl.path,
            vl.returnValue,
            vl.kind,
            vl.comment,
            vl.implicitlyAddedFrom,
            vl.parent
          )
        }
      }

      traverse(pack).asInstanceOf[List[Package]]
    }

    override def run(using Context): Unit = ()
  }

  object DocMiniTransformations {
    private var previousPhase = 0
    def apply(miniPhases: DocMiniPhase*) =
      new DocMiniTransformations {
        val transformations = miniPhases.toList
        val packages = Map.empty[String, Package]

        def phaseName = s"MiniTransformation${ previousPhase += 1 }"
      }
  }

  trait DocMiniPhase { phase =>
    private def identity[E]: PartialFunction[E, List[E]] = {
      case id: E @unchecked => id :: Nil
    }

    def transformPackage(using Context): PartialFunction[Package, List[Package]] = identity
    def transformTypeAlias(using Context): PartialFunction[TypeAlias, List[TypeAlias]] = identity
    def transformClass(using Context): PartialFunction[Class, List[Class]] = identity
    def transformCaseClass(using Context): PartialFunction[CaseClass, List[CaseClass]] = identity
    def transformTrait(using Context): PartialFunction[Trait, List[Trait]] = identity
    def transformObject(using Context): PartialFunction[Object, List[Object]] = identity
    def transformDef(using Context): PartialFunction[Def, List[Def]] = identity
    def transformVal(using Context): PartialFunction[Val, List[Val]] = identity

    private[transform] def packageTransformation(p: Package)(using Context) = (transformPackage orElse identity)(p)
    private[transform] def typeAliasTransformation(alias: TypeAlias)(using Context) = (transformTypeAlias orElse identity)(alias)
    private[transform] def classTransformation(cls: Class)(using Context) = (transformClass orElse identity)(cls)
    private[transform] def caseClassTransformation(cc: CaseClass)(using Context) = (transformCaseClass orElse identity)(cc)
    private[transform] def traitTransformation(trt: Trait)(using Context) = (transformTrait orElse identity)(trt)
    private[transform] def objectTransformation(obj: Object)(using Context) = (transformObject orElse identity)(obj)
    private[transform] def defTransformation(df: Def)(using Context) = (transformDef orElse identity)(df)
    private[transform] def valTransformation(vl: Val)(using Context) = (transformVal orElse identity)(vl)
  }
}
