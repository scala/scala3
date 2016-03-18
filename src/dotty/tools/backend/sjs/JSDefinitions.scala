package dotty.tools.backend.sjs

import dotty.tools.dotc.core._

import Types._
import Contexts._
import Symbols._
import Names._
import StdNames._
import Decorators._

import dotty.tools.dotc.config.SJSPlatform

object JSDefinitions {
  /** The Scala.js-specific definitions for the current context. */
  def jsdefn(implicit ctx: Context): JSDefinitions =
    ctx.platform.asInstanceOf[SJSPlatform].jsDefinitions
}

final class JSDefinitions()(implicit ctx: Context) {

  lazy val InlineAnnotType: TypeRef = ctx.requiredClassRef("scala.inline")
  def InlineAnnot(implicit ctx: Context) = InlineAnnotType.symbol.asClass
  lazy val NoinlineAnnotType: TypeRef = ctx.requiredClassRef("scala.noinline")
  def NoinlineAnnot(implicit ctx: Context) = NoinlineAnnotType.symbol.asClass

  lazy val ScalaJSJSPackageVal = ctx.requiredPackage("scala.scalajs.js")
  lazy val ScalaJSJSPackageClass = ScalaJSJSPackageVal.moduleClass.asClass
    lazy val JSPackage_typeOfR = ScalaJSJSPackageClass.requiredMethodRef("typeOf")
    def JSPackage_typeOf(implicit ctx: Context) = JSPackage_typeOfR.symbol
    lazy val JSPackage_constructorOfR = ScalaJSJSPackageClass.requiredMethodRef("constructorOf")
    def JSPackage_constructorOf(implicit ctx: Context) = JSPackage_constructorOfR.symbol
    lazy val JSPackage_debuggerR = ScalaJSJSPackageClass.requiredMethodRef("debugger")
    def JSPackage_debugger(implicit ctx: Context) = JSPackage_debuggerR.symbol
    lazy val JSPackage_nativeR = ScalaJSJSPackageClass.requiredMethodRef("native")
    def JSPackage_native(implicit ctx: Context) = JSPackage_nativeR.symbol

  lazy val JSNativeAnnotType: TypeRef = ctx.requiredClassRef("scala.scalajs.js.native")
  def JSNativeAnnot(implicit ctx: Context) = JSNativeAnnotType.symbol.asClass

  lazy val JSAnyType: TypeRef = ctx.requiredClassRef("scala.scalajs.js.Any")
  def JSAnyClass(implicit ctx: Context) = JSAnyType.symbol.asClass
  lazy val JSObjectType: TypeRef = ctx.requiredClassRef("scala.scalajs.js.Object")
  def JSObjectClass(implicit ctx: Context) = JSObjectType.symbol.asClass
  lazy val JSBaseThisFunctionType: TypeRef = ctx.requiredClassRef("scala.scalajs.js.ThisFunction")
  def JSBaseThisFunctionClass(implicit ctx: Context) = JSBaseThisFunctionType.symbol.asClass

  lazy val JSDictionaryType: TypeRef = ctx.requiredClassRef("scala.scalajs.js.Dictionary")
  def JSDictionaryClass(implicit ctx: Context) = JSDictionaryType.symbol.asClass
    lazy val JSDictionary_deleteR = JSDictionaryClass.requiredMethodRef("delete")
    def JSDictionary_delete(implicit ctx: Context) = JSDictionary_deleteR.symbol

  lazy val JSGlobalScopeType: TypeRef = ctx.requiredClassRef("scala.scalajs.js.GlobalScope")
  def JSGlobalScopeClass(implicit ctx: Context) = JSGlobalScopeType.symbol.asClass

  lazy val JSArrayType: TypeRef = ctx.requiredClassRef("scala.scalajs.js.Array")
  def JSArrayClass(implicit ctx: Context) = JSArrayType.symbol.asClass

  lazy val JSFunctionType = (0 to 22).map(n => ctx.requiredClassRef("scala.scalajs.js.Function" + n)).toArray
  def JSFunctionClass(n: Int)(implicit ctx: Context) = JSFunctionType(n).symbol.asClass
  lazy val JSThisFunctionType = (0 to 21).map(n => ctx.requiredClassRef("scala.scalajs.js.ThisFunction" + n)).toArray
  def JSThisFunctionClass(n: Int)(implicit ctx: Context) = JSThisFunctionType(n).symbol.asClass

  lazy val RuntimeExceptionType: TypeRef = ctx.requiredClassRef("java.lang.RuntimeException")
  def RuntimeExceptionClass(implicit ctx: Context) = RuntimeExceptionType.symbol.asClass
  lazy val JavaScriptExceptionType: TypeRef = ctx.requiredClassRef("scala.scalajs.js.JavaScriptException")
  def JavaScriptExceptionClass(implicit ctx: Context) = JavaScriptExceptionType.symbol.asClass

  lazy val JSNameAnnotType: TypeRef = ctx.requiredClassRef("scala.scalajs.js.annotation.JSName")
  def JSNameAnnot(implicit ctx: Context) = JSNameAnnotType.symbol.asClass
  lazy val JSFullNameAnnotType: TypeRef = ctx.requiredClassRef("scala.scalajs.js.annotation.JSFullName")
  def JSFullNameAnnot(implicit ctx: Context) = JSFullNameAnnotType.symbol.asClass
  lazy val JSBracketAccessAnnotType: TypeRef = ctx.requiredClassRef("scala.scalajs.js.annotation.JSBracketAccess")
  def JSBracketAccessAnnot(implicit ctx: Context) = JSBracketAccessAnnotType.symbol.asClass
  lazy val JSBracketCallAnnotType: TypeRef = ctx.requiredClassRef("scala.scalajs.js.annotation.JSBracketCall")
  def JSBracketCallAnnot(implicit ctx: Context) = JSBracketCallAnnotType.symbol.asClass
  lazy val JSExportAnnotType: TypeRef = ctx.requiredClassRef("scala.scalajs.js.annotation.JSExport")
  def JSExportAnnot(implicit ctx: Context) = JSExportAnnotType.symbol.asClass
  lazy val JSExportDescendentObjectsAnnotType: TypeRef = ctx.requiredClassRef("scala.scalajs.js.annotation.JSExportDescendentObjects")
  def JSExportDescendentObjectsAnnot(implicit ctx: Context) = JSExportDescendentObjectsAnnotType.symbol.asClass
  lazy val JSExportDescendentClassesAnnotType: TypeRef = ctx.requiredClassRef("scala.scalajs.js.annotation.JSExportDescendentClasses")
  def JSExportDescendentClassesAnnot(implicit ctx: Context) = JSExportDescendentClassesAnnotType.symbol.asClass
  lazy val JSExportAllAnnotType: TypeRef = ctx.requiredClassRef("scala.scalajs.js.annotation.JSExportAll")
  def JSExportAllAnnot(implicit ctx: Context) = JSExportAllAnnotType.symbol.asClass
  lazy val JSExportNamedAnnotType: TypeRef = ctx.requiredClassRef("scala.scalajs.js.annotation.JSExportNamed")
  def JSExportNamedAnnot(implicit ctx: Context) = JSExportNamedAnnotType.symbol.asClass
  lazy val RawJSTypeAnnotType: TypeRef = ctx.requiredClassRef("scala.scalajs.js.annotation.RawJSType")
  def RawJSTypeAnnot(implicit ctx: Context) = RawJSTypeAnnotType.symbol.asClass
  lazy val ExposedJSMemberAnnotType: TypeRef = ctx.requiredClassRef("scala.scalajs.js.annotation.ExposedJSMember")
  def ExposedJSMemberAnnot(implicit ctx: Context) = ExposedJSMemberAnnotType.symbol.asClass

  lazy val JSAnyModuleRef = ctx.requiredModuleRef("scala.scalajs.js.Any")
  def JSAnyModule(implicit ctx: Context) = JSAnyModuleRef.symbol
    lazy val JSAny_fromFunctionR = (0 to 22).map(n => JSAnyModule.requiredMethodRef("fromFunction" + n)).toArray
    def JSAny_fromFunction(n: Int)(implicit ctx: Context) = JSAny_fromFunctionR(n).symbol

  lazy val JSDynamicModuleRef = ctx.requiredModuleRef("scala.scalajs.js.Dynamic")
  def JSDynamicModule(implicit ctx: Context) = JSDynamicModuleRef.symbol
    lazy val JSDynamic_newInstanceR = JSDynamicModule.requiredMethodRef("newInstance")
    def JSDynamic_newInstance(implicit ctx: Context) = JSDynamic_newInstanceR.symbol

  lazy val JSDynamicLiteralModuleRef = JSDynamicModule.moduleClass.requiredValueRef("literal")
  def JSDynamicLiteralModule(implicit ctx: Context) = JSDynamicLiteralModuleRef.symbol
    lazy val JSDynamicLiteral_applyDynamicNamedR = JSDynamicLiteralModule.requiredMethodRef("applyDynamicNamed")
    def JSDynamicLiteral_applyDynamicNamed(implicit ctx: Context) = JSDynamicLiteral_applyDynamicNamedR.symbol
    lazy val JSDynamicLiteral_applyDynamicR = JSDynamicLiteralModule.requiredMethodRef("applyDynamic")
    def JSDynamicLiteral_applyDynamic(implicit ctx: Context) = JSDynamicLiteral_applyDynamicR.symbol

  lazy val JSObjectModuleRef = ctx.requiredModuleRef("scala.scalajs.js.Object")
  def JSObjectModule(implicit ctx: Context) = JSObjectModuleRef.symbol
    lazy val JSObject_hasPropertyR = JSObjectModule.requiredMethodRef("hasProperty")
    def JSObject_hasProperty(implicit ctx: Context) = JSObject_hasPropertyR.symbol
    lazy val JSObject_propertiesR = JSObjectModule.requiredMethodRef("properties")
    def JSObject_properties(implicit ctx: Context) = JSObject_propertiesR.symbol

  lazy val JSArrayModuleRef = ctx.requiredModuleRef("scala.scalajs.js.Array")
  def JSArrayModule(implicit ctx: Context) = JSArrayModuleRef.symbol
    lazy val JSArray_applyR = JSArrayModule.requiredMethodRef(nme.apply)
    def JSArray_apply(implicit ctx: Context) = JSArray_applyR.symbol

  lazy val JSThisFunctionModuleRef = ctx.requiredModuleRef("scala.scalajs.js.ThisFunction")
  def JSThisFunctionModule(implicit ctx: Context) = JSThisFunctionModuleRef.symbol
    lazy val JSThisFunction_fromFunctionR = (1 to 22).map(n => JSThisFunctionModule.requiredMethodRef("fromFunction" + n)).toArray
    def JSThisFunction_fromFunction(n: Int)(implicit ctx: Context) = JSThisFunction_fromFunctionR(n - 1).symbol

  lazy val JSConstructorTagModuleRef = ctx.requiredModuleRef("scala.scalajs.js.ConstructorTag")
  def JSConstructorTagModule(implicit ctx: Context) = JSConstructorTagModuleRef.symbol
    lazy val JSConstructorTag_materializeR = JSConstructorTagModule.requiredMethodRef("materialize")
    def JSConstructorTag_materialize(implicit ctx: Context) = JSConstructorTag_materializeR.symbol

  lazy val RuntimeStringModuleRef = ctx.requiredModuleRef("scala.scalajs.runtime.RuntimeString")
  def RuntimeStringModule(implicit ctx: Context) = RuntimeStringModuleRef.symbol
  def RuntimeStringModuleClass(implicit ctx: Context) = RuntimeStringModule.moduleClass.asClass

  lazy val BooleanReflectiveCallType: TypeRef = ctx.requiredClassRef("scala.scalajs.runtime.BooleanReflectiveCall")
  def BooleanReflectiveCallClass(implicit ctx: Context) = BooleanReflectiveCallType.symbol.asClass
  lazy val NumberReflectiveCallType: TypeRef = ctx.requiredClassRef("scala.scalajs.runtime.NumberReflectiveCall")
  def NumberReflectiveCallClass(implicit ctx: Context) = NumberReflectiveCallType.symbol.asClass
  lazy val IntegerReflectiveCallType: TypeRef = ctx.requiredClassRef("scala.scalajs.runtime.IntegerReflectiveCall")
  def IntegerReflectiveCallClass(implicit ctx: Context) = IntegerReflectiveCallType.symbol.asClass
  lazy val LongReflectiveCallType: TypeRef = ctx.requiredClassRef("scala.scalajs.runtime.LongReflectiveCall")
  def LongReflectiveCallClass(implicit ctx: Context) = LongReflectiveCallType.symbol.asClass

  lazy val RuntimePackageVal = ctx.requiredPackage("scala.scalajs.runtime")
  lazy val RuntimePackageClass = RuntimePackageVal.moduleClass.asClass
    lazy val RuntimePackage_wrapJavaScriptExceptionR = RuntimePackageClass.requiredMethodRef("wrapJavaScriptException")
    def RuntimePackage_typeOf(implicit ctx: Context) = RuntimePackage_wrapJavaScriptExceptionR.symbol
    lazy val RuntimePackage_unwrapJavaScriptExceptionR = RuntimePackageClass.requiredMethodRef("unwrapJavaScriptException")
    def RuntimePackage_unwrapJavaScriptException(implicit ctx: Context) = RuntimePackage_unwrapJavaScriptExceptionR.symbol
    lazy val RuntimePackage_genTraversableOnce2jsArrayR = RuntimePackageClass.requiredMethodRef("genTraversableOnce2jsArray")
    def RuntimePackage_genTraversableOnce2jsArray(implicit ctx: Context) = RuntimePackage_genTraversableOnce2jsArrayR.symbol
    lazy val RuntimePackage_jsTupleArray2jsObjectR = RuntimePackageClass.requiredMethodRef("jsTupleArray2jsObject")
    def RuntimePackage_jsTupleArray2jsObject(implicit ctx: Context) = RuntimePackage_jsTupleArray2jsObjectR.symbol
    lazy val RuntimePackage_constructorOfR = RuntimePackageClass.requiredMethodRef("constructorOf")
    def RuntimePackage_constructorOf(implicit ctx: Context) = RuntimePackage_constructorOfR.symbol
    lazy val RuntimePackage_newConstructorTagR = RuntimePackageClass.requiredMethodRef("newConstructorTag")
    def RuntimePackage_newConstructorTag(implicit ctx: Context) = RuntimePackage_newConstructorTagR.symbol
    lazy val RuntimePackage_propertiesOfR = RuntimePackageClass.requiredMethodRef("propertiesOf")
    def RuntimePackage_propertiesOf(implicit ctx: Context) = RuntimePackage_propertiesOfR.symbol
    lazy val RuntimePackage_environmentInfoR = RuntimePackageClass.requiredMethodRef("environmentInfo")
    def RuntimePackage_environmentInfo(implicit ctx: Context) = RuntimePackage_environmentInfoR.symbol
    lazy val RuntimePackage_linkingInfoR = RuntimePackageClass.requiredMethodRef("linkingInfo")
    def RuntimePackage_linkingInfo(implicit ctx: Context) = RuntimePackage_linkingInfoR.symbol

  lazy val WrappedArrayType: TypeRef = ctx.requiredClassRef("scala.scalajs.js.WrappedArray")
  def WrappedArrayClass(implicit ctx: Context) = WrappedArrayType.symbol.asClass

  lazy val ScalaRunTime_isArrayR = defn.ScalaRuntimeModule.requiredMethodRef("isArray", List(???, ???))
  def ScalaRunTime_isArray(implicit ctx: Context): Symbol = ScalaRunTime_isArrayR.symbol

  lazy val BoxesRunTime_boxToCharacterR = defn.BoxesRunTimeModule.requiredMethodRef("boxToCharacter")
  def BoxesRunTime_boxToCharacter(implicit ctx: Context): Symbol = BoxesRunTime_boxToCharacterR.symbol
  lazy val BoxesRunTime_unboxToCharR = defn.BoxesRunTimeModule.requiredMethodRef("unboxToChar")
  def BoxesRunTime_unboxToChar(implicit ctx: Context): Symbol = BoxesRunTime_unboxToCharR.symbol

  /** If `cls` is a class in the scala package, its name, otherwise EmptyTypeName */
  private def scalajsClassName(cls: Symbol)(implicit ctx: Context): TypeName =
    if (cls.isClass && cls.owner == ScalaJSJSPackageClass) cls.asClass.name
    else EmptyTypeName

  /** Is the given `cls` a class of the form `scala.scalajs.js.prefixN` where
   *  `N` is a number.
   *
   *  This is similar to `isVarArityClass` in `Definitions.scala`.
   */
  private def isScalaJSVarArityClass(cls: Symbol, prefix: Name): Boolean = {
    val name = scalajsClassName(cls)
    name.startsWith(prefix) && name.drop(prefix.length).forall(_.isDigit)
  }

  def isJSFunctionClass(cls: Symbol): Boolean =
    isScalaJSVarArityClass(cls, nme.Function)

  private val ThisFunctionName = termName("ThisFunction")

  def isJSThisFunctionClass(cls: Symbol): Boolean =
    isScalaJSVarArityClass(cls, ThisFunctionName)

}
