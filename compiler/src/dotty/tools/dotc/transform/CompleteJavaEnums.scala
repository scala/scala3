package dotty.tools.dotc
package transform

import core._
import Names._
import StdNames.{nme, tpnme}
import Types._
import dotty.tools.dotc.transform.MegaPhase._
import Flags._
import Contexts.{Context, ctx}
import Symbols._
import Constants._
import Decorators._
import DenotTransformers._
import dotty.tools.dotc.ast.Trees._
import SymUtils._

object CompleteJavaEnums {
  val name: String = "completeJavaEnums"

  private val nameParamName: TermName = "_$name".toTermName
  private val ordinalParamName: TermName = "_$ordinal".toTermName
}

/** For Scala enums that inherit from java.lang.Enum:
 *  Add constructor parameters for `name` and `ordinal` to pass from each
 *  case to the java.lang.Enum class.
 */
class CompleteJavaEnums extends MiniPhase with InfoTransformer { thisPhase =>
  import CompleteJavaEnums._
  import ast.tpd._

  override def phaseName: String = CompleteJavaEnums.name

  override def relaxedTypingInGroup: Boolean = true
    // Because it adds additional parameters to some constructors

  def transformInfo(tp: Type, sym: Symbol)(using Context): Type =
    if (sym.isConstructor && (
         sym == defn.JavaEnumClass.primaryConstructor ||
         sym.owner.derivesFromJavaEnum))
      addConstrParams(sym.info)
    else tp

  /** Add constructor parameters `$name: String` and `$ordinal: Int` to the end of
   *  the last parameter list of (method- or poly-) type `tp`.
   */
  private def addConstrParams(tp: Type)(using Context): Type = tp match {
    case tp: PolyType =>
      tp.derivedLambdaType(resType = addConstrParams(tp.resType))
    case tp: MethodType =>
      tp.resType match {
        case restpe: MethodType =>
          tp.derivedLambdaType(resType = addConstrParams(restpe))
        case _ =>
          tp.derivedLambdaType(
            paramNames = tp.paramNames ++ List(nameParamName, ordinalParamName),
            paramInfos = tp.paramInfos ++ List(defn.StringType, defn.IntType))
      }
  }

  /** The list of parameter definitions `$name: String, $ordinal: Int`, in given `owner`
   *  with given flags (either `Param` or `ParamAccessor`)
   */
  private def addedParams(owner: Symbol, flag: FlagSet)(using Context): List[ValDef] = {
    val nameParam = ctx.newSymbol(owner, nameParamName, flag | Synthetic, defn.StringType, coord = owner.span)
    val ordinalParam = ctx.newSymbol(owner, ordinalParamName, flag | Synthetic, defn.IntType, coord = owner.span)
    List(ValDef(nameParam), ValDef(ordinalParam))
  }

  /** Add arguments `args` to the parent constructor application in `parents` that invokes
   *  a constructor of `targetCls`,
   */
  private def addEnumConstrArgs(targetCls: Symbol, parents: List[Tree], args: List[Tree])(using Context): List[Tree] =
    parents.map {
      case app @ Apply(fn, args0) if fn.symbol.owner == targetCls => cpy.Apply(app)(fn, args0 ++ args)
      case p => p
    }

  /** If this is a constructor of a enum class that extends, add $name and $ordinal parameters to it. */
  override def transformDefDef(tree: DefDef)(using Context): DefDef = {
    val sym = tree.symbol
    if (sym.isConstructor && sym.owner.derivesFromJavaEnum)
      val tree1 = cpy.DefDef(tree)(
        vparamss = tree.vparamss.init :+ (tree.vparamss.last ++ addedParams(sym, Param)))
      sym.setParamssFromDefs(tree1.tparams, tree1.vparamss)
      tree1
    else tree
  }

  /** Return a list of forwarders for enum values defined in the companion object
   *  for java interop.
   */
  private def addedEnumForwarders(clazz: Symbol)(using Context): List[ValDef] = {
    val moduleCls = clazz.companionClass
    val moduleRef = ref(clazz.companionModule)

    val enums = moduleCls.info.decls.filter(member => member.isAllOf(EnumValue))
    for { enumValue <- enums }
    yield {
      val fieldSym = ctx.newSymbol(clazz, enumValue.name.asTermName, EnumValue | JavaStatic, enumValue.info)
      fieldSym.addAnnotation(Annotations.Annotation(defn.ScalaStaticAnnot))
      ValDef(fieldSym, moduleRef.select(enumValue))
    }
  }

  /** 1. If this is an enum class, add $name and $ordinal parameters to its
   *     parameter accessors and pass them on to the java.lang.Enum constructor.
   *
   *  2. If this is an anonymous class that implement a value enum case,
   *     pass $name and $ordinal parameters to the enum superclass. The class
   *     looks like this:
   *
   *       class $anon extends E(...) {
   *          ...
   *          def ordinal = N
   *          def toString = S
   *          ...
   *       }
   *
   *     After the transform it is expanded to
   *
   *       class $anon extends E(..., N, S) {
   *         "same as before"
   *       }
   */
  override def transformTemplate(templ: Template)(using Context): Template = {
    val cls = templ.symbol.owner
    if (cls.derivesFromJavaEnum) {
      val (params, rest) = decomposeTemplateBody(templ.body)
      val addedDefs = addedParams(cls, ParamAccessor)
      val addedSyms = addedDefs.map(_.symbol.entered)
      val addedForwarders = addedEnumForwarders(cls)
      cpy.Template(templ)(
        parents = addEnumConstrArgs(defn.JavaEnumClass, templ.parents, addedSyms.map(ref)),
        body = params ++ addedDefs ++ addedForwarders ++ rest)
    }
    else if (cls.isAnonymousClass && ((cls.owner.name eq nme.DOLLAR_NEW) || cls.owner.isAllOf(EnumCase)) &&
             cls.owner.owner.linkedClass.derivesFromJavaEnum) {
      def rhsOf(name: TermName) =
        templ.body.collect {
          case mdef: DefDef if mdef.name == name => mdef.rhs
        }.head
      val args = List(rhsOf(nme.toString_), rhsOf(nme.ordinalDollar))
      cpy.Template(templ)(
        parents = addEnumConstrArgs(cls.owner.owner.linkedClass, templ.parents, args))
    }
    else templ
  }
}
