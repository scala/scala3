package dotty.tools
package dottydoc
package core

import dotc.CompilationUnit
import dotc.core.Contexts.Context
import dotc.core.Comments.ContextDocstrings
import dotc.core.Phases.Phase
import model._
import model.internal._
import util.syntax._
import util.traversing._

object transform {
  /**
   * The idea behind DocMiniTransformations is to fuse transformations to the
   * doc AST, much like `MiniPhaseTransform` in dotty core - but in a much more
   * simple implementation
   *
   * Usage
   * -----
   *
   * Create a `DocMiniPhase` which overrides the relevant method:
   *
   * ```
   * override def transformDef(implicit ctx: Context) = {
   *   case x if shouldTransform(x) => x.copy(newValue = ...)
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
   * To delete a node in the AST, simply return `NonEntity` from transforming method
   */
  trait DocMiniTransformations extends Phase {
    def transformations: List[DocMiniPhase]

    override def runOn(units: List[CompilationUnit])(implicit ctx: Context): List[CompilationUnit] = {
      for {
        pack <- rootPackages(ctx.docbase.packages)
        transformed =  performPackageTransform(pack)
      } yield ctx.docbase.packagesMutable(pack.name) = transformed

      ctx.docbase.packagesMutable.foreach { case (key, value) =>
        if (value eq NonEntity) ctx.docbase.packagesMutable -= key
      }

      units
    }

    private def performPackageTransform(pack: Package)(implicit ctx: Context): Package = {
      def transformEntity[E <: Entity](e: E, f: DocMiniPhase => E => E)(createNew: E => E): Entity = {
        val transformedEntity = transformations.foldLeft(e) { case (oldE, transf) =>
          f(transf)(oldE)
        }

        if (transformedEntity eq NonEntity) NonEntity
        else createNew(transformedEntity)
      }

      def traverse(ent: Entity): Entity = ent match {
        case p: Package => transformEntity(p, _.packageTransformation) { p =>
          val newPackage = PackageImpl(
            p.symbol,
            p.annotations,
            p.name,
            p.members.map(traverse).filterNot(_ eq NonEntity),
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
            cls.members.map(traverse).filterNot(_ eq NonEntity),
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
            cc.members.map(traverse).filterNot(_ eq NonEntity),
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
            trt.members.map(traverse).filterNot(_ eq NonEntity),
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
            obj.members.map(traverse).filterNot(_ eq NonEntity),
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

      traverse(pack).asInstanceOf[Package]
    }

    override def run(implicit ctx: Context): Unit = ()
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
    private def identity[E]: PartialFunction[E, E] = {
      case id: E @unchecked => id
    }

    def transformPackage(implicit ctx: Context): PartialFunction[Package, Package] = identity
    def transformTypeAlias(implicit ctx: Context): PartialFunction[TypeAlias, TypeAlias] = identity
    def transformClass(implicit ctx: Context): PartialFunction[Class, Class] = identity
    def transformCaseClass(implicit ctx: Context): PartialFunction[CaseClass, CaseClass] = identity
    def transformTrait(implicit ctx: Context): PartialFunction[Trait, Trait] = identity
    def transformObject(implicit ctx: Context): PartialFunction[Object, Object] = identity
    def transformDef(implicit ctx: Context): PartialFunction[Def, Def] = identity
    def transformVal(implicit ctx: Context): PartialFunction[Val, Val] = identity

    private[transform] def packageTransformation(p: Package)(implicit ctx: Context) = (transformPackage orElse identity)(p)
    private[transform] def typeAliasTransformation(alias: TypeAlias)(implicit ctx: Context) = (transformTypeAlias orElse identity)(alias)
    private[transform] def classTransformation(cls: Class)(implicit ctx: Context) = (transformClass orElse identity)(cls)
    private[transform] def caseClassTransformation(cc: CaseClass)(implicit ctx: Context) = (transformCaseClass orElse identity)(cc)
    private[transform] def traitTransformation(trt: Trait)(implicit ctx: Context) = (transformTrait orElse identity)(trt)
    private[transform] def objectTransformation(obj: Object)(implicit ctx: Context) = (transformObject orElse identity)(obj)
    private[transform] def defTransformation(df: Def)(implicit ctx: Context) = (transformDef orElse identity)(df)
    private[transform] def valTransformation(vl: Val)(implicit ctx: Context) = (transformVal orElse identity)(vl)
  }
}
