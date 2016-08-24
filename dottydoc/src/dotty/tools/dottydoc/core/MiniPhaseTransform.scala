package dotty.tools
package dottydoc
package core

import dotc.CompilationUnit
import dotc.core.Contexts.Context
import dotc.core.Phases.Phase
import model._
import model.internal._

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
   * {{{
   *    override def transformDef(implicit ctx: Context) = {
   *      case x if shouldTransform(x) => x.copy(newValue = ...)
   *    }
   * }}}
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
   */
  abstract class DocMiniTransformations(transformations: List[DocMiniPhase]) extends Phase {

    override def runOn(units: List[CompilationUnit])(implicit ctx: Context): List[CompilationUnit] = {
      for {
        rootName    <- rootPackages
        pack        =  ctx.docbase.packages[Package](rootName)
        transformed =  performPackageTransform(pack)
      } yield ctx.docbase.packages(rootName) = transformed
      super.runOn(units)
    }

    private def rootPackages(implicit ctx: Context): List[String] = {
      var currentDepth = Int.MaxValue
      var packs = List.empty[String]

      for (key <- ctx.docbase.packages.keys) {
        val keyDepth = key.split("\\.").length
        packs =
          if (keyDepth < currentDepth) {
            currentDepth = keyDepth
            key :: Nil
          } else if (keyDepth == currentDepth) {
            key :: packs
          } else packs
      }
      packs
    }

    private def performPackageTransform(pack: Package)(implicit ctx: Context): Package = {
      def transformEntity[E <: Entity](e: E, f: DocMiniPhase => E => E)(createNew: E => E): E = {
        val transformedEntity = transformations.foldLeft(e) { case (oldE, transf) =>
          f(transf)(oldE)
        }
        createNew(transformedEntity)
      }

      def traverse(ent: Entity): Entity = ent match {
        case p: Package => transformEntity(p, _.packageTransformation) { p =>
          val newPackage = PackageImpl(
            p.symbol,
            p.name,
            p.members.map(traverse),
            p.path,
            p.comment
          )

          // Update reference in context to newPackage
          ctx.docbase.packages[Package] += (newPackage.path.mkString(".") -> newPackage)

          newPackage
        }
        case c: Class => transformEntity(c, _.classTransformation) { cls =>
          ClassImpl(
            cls.symbol,
            cls.name,
            cls.members.map(traverse),
            cls.modifiers,
            cls.path,
            cls.typeParams,
            cls.constructors,
            cls.superTypes,
            cls.comment
          )
        }
        case cc: CaseClass => transformEntity(cc, _.caseClassTransformation) { cc =>
          CaseClassImpl(
            cc.symbol,
            cc.name,
            cc.members.map(traverse),
            cc.modifiers,
            cc.path,
            cc.typeParams,
            cc.constructors,
            cc.superTypes,
            cc.comment
          )
        }
        case trt: Trait => transformEntity(trt, _.traitTransformation) { trt =>
          TraitImpl(
            trt.symbol,
            trt.name,
            trt.members.map(traverse),
            trt.modifiers,
            trt.path,
            trt.typeParams,
            trt.traitParams,
            trt.superTypes,
            trt.comment
          )
        }
        case obj: Object => transformEntity(obj, _.objectTransformation) { obj =>
          ObjectImpl(
            obj.symbol,
            obj.name,
            obj.members.map(traverse),
            obj.modifiers,
            obj.path,
            obj.superTypes,
            obj.comment
          )
        }
        case df: Def => transformEntity(df, _.defTransformation) { df =>
          DefImpl(
            df.symbol,
            df.name,
            df.modifiers,
            df.path,
            df.returnValue,
            df.typeParams,
            df.paramLists,
            df.comment,
            df.implicitlyAddedFrom
          )
        }
        case vl: Val => transformEntity(vl, _.valTransformation) { vl =>
          ValImpl(
            vl.symbol,
            vl.name,
            vl.modifiers,
            vl.path,
            vl.returnValue,
            vl.comment,
            vl.implicitlyAddedFrom
          )
        }
      }

      traverse(pack).asInstanceOf[Package]
    }

    override def run(implicit ctx: Context): Unit = ()
  }

  object DocMiniTransformations {
    private var previousPhase = 0
    def apply(transformations: DocMiniPhase*) =
      new DocMiniTransformations(transformations.toList) {
        val packages = Map.empty[String, Package]

        def phaseName = s"MiniTransformation${ previousPhase += 1 }"
      }
  }

  trait DocMiniPhase { phase =>
    private def identity[E]: PartialFunction[E, E] = {
      case id => id
    }

    // Partial functions instead????
    def transformPackage(implicit ctx: Context): PartialFunction[Package, Package] = identity
    def transformClass(implicit ctx: Context): PartialFunction[Class, Class] = identity
    def transformCaseClass(implicit ctx: Context): PartialFunction[CaseClass, CaseClass] = identity
    def transformTrait(implicit ctx: Context): PartialFunction[Trait, Trait] = identity
    def transformObject(implicit ctx: Context): PartialFunction[Object, Object] = identity
    def transformDef(implicit ctx: Context): PartialFunction[Def, Def] = identity
    def transformVal(implicit ctx: Context): PartialFunction[Val, Val] = identity

    private[transform] def packageTransformation(p: Package)(implicit ctx: Context) = (transformPackage orElse identity)(p)
    private[transform] def classTransformation(cls: Class)(implicit ctx: Context) = (transformClass orElse identity)(cls)
    private[transform] def caseClassTransformation(cc: CaseClass)(implicit ctx: Context) = (transformCaseClass orElse identity)(cc)
    private[transform] def traitTransformation(trt: Trait)(implicit ctx: Context) = (transformTrait orElse identity)(trt)
    private[transform] def objectTransformation(obj: Object)(implicit ctx: Context) = (transformObject orElse identity)(obj)
    private[transform] def defTransformation(df: Def)(implicit ctx: Context) = (transformDef orElse identity)(df)
    private[transform] def valTransformation(vl: Val)(implicit ctx: Context) = (transformVal orElse identity)(vl)
  }
}
