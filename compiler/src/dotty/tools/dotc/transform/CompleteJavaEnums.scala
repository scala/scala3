package dotty.tools
package dotc
package transform

import core.*
import Names.*
import StdNames.nme
import Types.*
import dotty.tools.dotc.transform.MegaPhase.*
import Flags.*
import Contexts.*
import Symbols.*
import Constants.*
import Decorators.*
import DenotTransformers.*



object CompleteJavaEnums {
  val name: String = "completeJavaEnums"
  val description: String = "fill in constructors for Java enums"

  private val nameParamName: TermName = "_$name".toTermName
  private val ordinalParamName: TermName = "_$ordinal".toTermName
}

/** For Scala enums that inherit from java.lang.Enum:
 *  Add constructor parameters for `name` and `ordinal` to pass from each
 *  case to the java.lang.Enum class.
 */
class CompleteJavaEnums extends MiniPhase with InfoTransformer { thisPhase =>
  import CompleteJavaEnums.*
  import ast.tpd.*

  override def phaseName: String = CompleteJavaEnums.name

  override def description: String = CompleteJavaEnums.description

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
  private def addedParams(owner: Symbol, isLocal: Boolean, flag: FlagSet)(using Context): List[ValDef] = {
    val flags = flag | Synthetic | (if isLocal then Private | Deferred else EmptyFlags)
    val nameParam = newSymbol(owner, nameParamName, flags, defn.StringType, coord = owner.span)
    val ordinalParam = newSymbol(owner, ordinalParamName, flags, defn.IntType, coord = owner.span)
    List(ValDef(nameParam), ValDef(ordinalParam))
  }

  /** Add arguments `args` to the parent constructor application in `parents` that invokes
   *  a constructor of `targetCls`,
   */
  private def addEnumConstrArgs(targetCls: Symbol, parents: List[Tree], args: List[Tree])(using Context): List[Tree] =
    parents.map {
      case app @ Apply(fn, args0) if fn.symbol.owner == targetCls =>
        if args0.nonEmpty && targetCls == defn.JavaEnumClass then
          report.error(em"the constructor of java.lang.Enum cannot be called explicitly", app.sourcePos)
        cpy.Apply(app)(fn, args0 ++ args)
      case p => p
    }

  /** If this is a constructor of a enum class that extends, add $name and $ordinal parameters to it. */
  override def transformDefDef(tree: DefDef)(using Context): DefDef = {
    val sym = tree.symbol
    if sym.isConstructor && sym.owner.derivesFromJavaEnum then
      val tree1 = cpy.DefDef(tree)(
        paramss = tree.paramss.init
          :+ (tree.paramss.last.asInstanceOf[List[ValDef]]
              ++ addedParams(sym, isLocal=false, Param)))
      sym.setParamssFromDefs(tree1.paramss)
      tree1
    else tree
  }

  /** Return a list of forwarders for enum values defined in the companion object
   *  for java interop.
   */
  private def addedEnumForwarders(clazz: Symbol)(using Context): List[MemberDef] = {
    val moduleCls = clazz.companionClass
    val moduleRef = ref(clazz.companionModule)

    val enums = moduleCls.info.decls.filter(member => member.isAllOf(EnumValue))
    val forwarderSyms = scala.collection.mutable.ListBuffer[Symbol]()

    val result = for { enumValue <- enums }
    yield {
      def forwarderSym(flags: FlagSet, info: Type): Symbol { type ThisName = TermName } =
        val sym = newSymbol(clazz, enumValue.name.asTermName, flags, info)
        sym.addAnnotation(Annotations.Annotation(defn.ScalaStaticAnnot, sym.span))
        sym
      val body = moduleRef.select(enumValue)
      if ctx.settings.scalajs.value then
        // Scala.js has no support for <clinit> so we must avoid assigning static fields in the enum class.
        // However, since the public contract for reading static fields in the IR ABI is to call "static getters",
        // we achieve the right contract with static forwarders instead.
        DefDef(forwarderSym(EnumValue | Method | JavaStatic, MethodType(Nil, enumValue.info)), body)
      else
        val sym = forwarderSym(EnumValue | JavaStatic | Mutable, enumValue.info)
        forwarderSyms += sym
        ValDef(sym, body)
    }

    // Store forwarder symbols for later use in companion initialization
    if forwarderSyms.nonEmpty then
      enumForwarders(clazz) = forwarderSyms.toList

    result
  }

  /** Generate assignment to initialize enum forwarders in the companion object,
   *  so that forwarders are initialized when comapnion object is touched first.
   *  For each enum value, generates: EnumClass.enumValue = Module.enumValue
   *  see: https://github.com/scala/scala3/issues/12637
   */
  private def enumForwarderInitializers(moduleCls: Symbol)(using Context): List[Tree] = {
    if ctx.settings.scalajs.value then
      Nil // Scala.js uses methods, no initialization needed
    else
      val enumClass = moduleCls.linkedClass
      val forwarderSyms = enumForwarders.get(enumClass).getOrElse(Nil)
      val enums = moduleCls.info.decls.filter(member => member.isAllOf(EnumValue)).toList

      forwarderSyms.zip(enums).map { case (forwarderSym, enumValue) =>
        val lhs = ref(forwarderSym)
        val rhs = ref(enumValue)
        Assign(lhs, rhs)
      }
  }

  private def isJavaEnumValueImpl(cls: Symbol)(using Context): Boolean =
    cls.isAnonymousClass
    && (((cls.owner.name eq nme.DOLLAR_NEW) && cls.owner.isAllOf(Private|Synthetic)) || cls.owner.isAllOf(EnumCase))
    && cls.owner.owner.linkedClass.derivesFromJavaEnum

  private val enumCaseOrdinals = MutableSymbolMap[Int]()
  private val enumForwarders = MutableSymbolMap[List[Symbol]]()

  private def registerEnumClass(cls: Symbol)(using Context): Unit =
    cls.children.zipWithIndex.foreach(enumCaseOrdinals.update)

  private def ordinalFor(enumCase: Symbol): Int =
    enumCaseOrdinals.remove(enumCase).nn

  /** 1. If this is an enum class, add $name and $ordinal parameters to its
   *     parameter accessors and pass them on to the java.lang.Enum constructor.
   *
   *  2. If this is an anonymous class that implement a singleton enum case,
   *     pass $name and $ordinal parameters to the enum superclass. The class
   *     looks like this:
   *
   *       class $anon extends E(...) {
   *          ...
   *       }
   *
   *     After the transform it is expanded to
   *
   *       class $anon extends E(..., $name, _$ordinal) { // if class implements a simple enum case
   *          "same as before"
   *       }
   *
   *       class $anon extends E(..., "A", 0) { // if class implements a value enum case `A` with ordinal 0
   *          "same as before"
   *       }
   */
  override def transformTemplate(templ: Template)(using Context): Tree = {
    val cls = templ.symbol.owner
    if cls.derivesFromJavaEnum then
      registerEnumClass(cls) // invariant: class is visited before cases: see tests/pos/enum-companion-first.scala
      val (params, rest) = decomposeTemplateBody(templ.body)
      val addedDefs = addedParams(cls, isLocal=true, ParamAccessor)
      val addedSyms = addedDefs.map(_.symbol.entered)
      val addedForwarders = addedEnumForwarders(cls)
      cpy.Template(templ)(
        parents = addEnumConstrArgs(defn.JavaEnumClass, templ.parents, addedSyms.map(ref)),
        body = params ++ addedDefs ++ addedForwarders ++ rest)
    else if isJavaEnumValueImpl(cls) then
      def creatorParamRef(name: TermName) =
        ref(cls.owner.paramSymss.head.find(_.name == name).get)
      val args =
        if cls.owner.isAllOf(EnumCase) then
          List(Literal(Constant(cls.owner.name.toString)), Literal(Constant(ordinalFor(cls.owner))))
        else
          List(creatorParamRef(nme.nameDollar), creatorParamRef(nme.ordinalDollar_))
      cpy.Template(templ)(
        parents = addEnumConstrArgs(cls.owner.owner.linkedClass, templ.parents, args),
      )
    else if cls.linkedClass.derivesFromJavaEnum then
      enumCaseOrdinals.clear() // remove simple cases // invariant: companion is visited after cases
      // Add initialization code for enum forwarders
      val initializers = enumForwarderInitializers(cls)
      enumForwarders.remove(cls.linkedClass) // Clear cache after use
      if initializers.isEmpty then
        templ
      else
        cpy.Template(templ)(body = templ.body ++ initializers)
    else templ
  }

  override def checkPostCondition(tree: Tree)(using Context): Unit =
    assert(enumCaseOrdinals.isEmpty, "Java based enum ordinal cache was not cleared")
    assert(enumForwarders.isEmpty, "Java based enum forwarder cache was not cleared")
}
