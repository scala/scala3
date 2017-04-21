package dotty.tools.dotc
package transform

import TreeTransforms._
import core.DenotTransformers._
import core.Symbols._
import core.Contexts._
import ast.TreeTypeMap
import core.Types._
import core.Flags._
import core.Decorators._
import collection.mutable
import ast.Trees._
import core.Names.TermName
import core.NameKinds.SuperArgName
import SymUtils._

/** This phase hoists complex arguments to supercalls out of the enclosing class.
 *  Example:
 *
 *      class B(y: Int) extends A({ def f(x: Int) = x * x; f(y)})
 *
 *  is translated to
 *
 *      class B(y: Int) extends A(B#B$superArg$1(this.y)) {
 *        private <static> def B$superArg$1(y: Int): Int = {
 *          def f(x: Int): Int = x.*(x); f(y)
 *        }
 *      }
 *
 *  An argument is complex if it contains a method or template definition, a this or a new,
 *  or it contains an identifier which needs a `this` prefix to be accessed. This is the case
 *  if the identifer neither a global reference nor a reference to a parameter of the enclosing class.
 *  @see needsHoist for an implementation.
 *
 *  A hoisted argument definition gets the parameters of the class it is hoisted from
 *  as method parameters. The definition is installed in the scope enclosing the class,
 *  or, if that is a package, it is made a static method of the class itself.
 */
class HoistSuperArgs extends MiniPhaseTransform with IdentityDenotTransformer { thisTransform =>
  import ast.tpd._

  def phaseName = "hoistSuperArgs"

  override def runsAfter = Set(classOf[ByNameClosures])
    // By name closures need to be introduced first in order to be hoisted out here.
    // There's an interaction with by name closures in that the <cbn-arg> marker
    // application should not be hoisted, but be left at the point of call.

  /** Hoist complex arguments in super call `parent` out of the class.
   *  @return A pair consisting of the transformed super call and a list of super argument
   *          defininitions.
   */
  def hoistSuperArgs(parent: Tree, cls: Symbol)(implicit ctx: Context): (Tree, List[DefDef]) = {
    lazy val constr = cls.primaryConstructor
    lazy val allParams = // The parameters that can be accessed in the supercall
      cls.info.decls.filter(d => d.is(TypeParam) || d.is(TermParamAccessor))
    val superArgDefs = new mutable.ListBuffer[DefDef]

    /** The parameter references defined by the primary constructor info */
    def allParamRefs(tp: Type): List[ParamRef] = tp match {
      case tp: LambdaType => tp.paramRefs ++ allParamRefs(tp.resultType)
      case _ => Nil
    }

    /** Splice `restpe` in final result type position of `tp` */
    def replaceResult(tp: Type, restpe: Type): Type = tp match {
      case tp: LambdaType =>
        tp.derivedLambdaType(resType = replaceResult(tp.resultType, restpe))
      case _ => restpe
    }

    /** A method representing a hoisted supercall argument */
    def newSuperArgMethod(argType: Type) = {
      val (staticFlag, methOwner) =
        if (cls.owner.is(Package)) (JavaStatic, cls) else (EmptyFlags, cls.owner)
      val argTypeWrtConstr = argType.subst(allParams, allParamRefs(constr.info))
        // argType with references to paramRefs of the primary constructor instead of
        // local parameter accessors
      val meth = ctx.newSymbol(
          owner = methOwner,
          name = SuperArgName.fresh(cls.name.toTermName),
          flags = Synthetic | Private | Method | staticFlag,
          info = replaceResult(constr.info, argTypeWrtConstr),
          coord = constr.coord)
      if (methOwner.isClass) meth.enteredAfter(thisTransform) else meth
    }

    def refNeedsHoist(tp: Type): Boolean = tp match {
      case tp: ThisType => !tp.cls.isStaticOwner && tp.cls != cls
      case tp: TermRef => refNeedsHoist(tp.prefix)
      case _ => false
    }

    /** Super call argument is complex, needs to be hoisted */
    def needsHoist(tree: Tree) = tree match {
      case _: DefDef => true
      case _: Template => true
      case _: New => !tree.tpe.typeSymbol.isStatic
      case _: RefTree | _: This => refNeedsHoist(tree.tpe)
      case _ => false
    }

    /** If argument is complex, hoist it out into its own method and refer to the
     *  method instead.
     *  @return  The argument after possible hoisting
     *  Might append a method definition to `superArgs` as a side effect.
     */
    def hoistSuperArg(arg: Tree): Tree = arg match {
      case Apply(fn, arg1 :: Nil) if fn.symbol == defn.cbnArg =>
        cpy.Apply(arg)(fn, hoistSuperArg(arg1) :: Nil)
      case _ if (arg.existsSubTree(needsHoist)) =>
        val superMeth = newSuperArgMethod(arg.tpe)
        val superArgDef = polyDefDef(superMeth, trefs => vrefss => {
          val paramSyms = trefs.map(_.typeSymbol) ::: vrefss.flatten.map(_.symbol)
          val tmap = new TreeTypeMap(
            typeMap = new TypeMap {
              def apply(tp: Type) = tp match {
                case tp: NamedType if tp.symbol.owner == cls && tp.symbol.is(ParamOrAccessor) =>
                  val mappedSym = allParams.zip(paramSyms).toMap.apply(tp.symbol)
                  if (tp.symbol.isType) mappedSym.typeRef else mappedSym.termRef
                case _ =>
                  mapOver(tp)
              }
            },
            treeMap = {
              case tree: RefTree if paramSyms.contains(tree.symbol) =>
                cpy.Ident(tree)(tree.name).withType(tree.tpe)
              case tree =>
                tree
            }
          )
          tmap(arg).changeOwnerAfter(constr, superMeth, thisTransform)
        })
        superArgDefs += superArgDef
        def termParamRefs(tp: Type): List[List[Tree]] = tp match {
          case tp: PolyType =>
            termParamRefs(tp.resultType)
          case tp: MethodType =>
            def paramRef(name: TermName) =
              ref(cls.info.decl(name).suchThat(_.is(ParamAccessor)).symbol)
            tp.paramNames.map(paramRef) :: termParamRefs(tp.resultType)
          case _  =>
            Nil
        }
        val res = ref(superMeth)
          .appliedToTypes(cls.typeParams.map(_.typeRef))
          .appliedToArgss(termParamRefs(constr.info))
        ctx.log(i"hoist $arg, cls = $cls = $res")
        res
      case _ => arg
    }

    def recur(tree: Tree): Tree = tree match {
      case Apply(fn, args) => cpy.Apply(tree)(recur(fn), args.mapconserve(hoistSuperArg))
      case _ => tree
    }
    (recur(parent), superArgDefs.toList)
  }

  override def transformTypeDef(tdef: TypeDef)(implicit ctx: Context, info: TransformerInfo): Tree =
    tdef.rhs match {
      case impl @ Template(_, superCall :: others, _, _) =>
        val cls = tdef.symbol
        val (hoisted, superArgDefs) = hoistSuperArgs(superCall, cls)
        if (superArgDefs.isEmpty) tdef
        else {
          val (staticSuperArgDefs, enclSuperArgDefs) =
            superArgDefs.partition(_.symbol.is(JavaStatic))
          flatTree(
              cpy.TypeDef(tdef)(
                  rhs = cpy.Template(impl)(
                      parents = hoisted :: others,
                      body = impl.body ++ staticSuperArgDefs)) ::
              enclSuperArgDefs)
        }
      case _ =>
        tdef
    }
}
