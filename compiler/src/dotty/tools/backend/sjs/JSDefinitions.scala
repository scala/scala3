package dotty.tools.backend.sjs

import dotty.tools.dotc.core._

import scala.annotation.threadUnsafe
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

  @threadUnsafe lazy val InlineAnnotType: TypeRef = ctx.requiredClassRef("scala.inline")
  def InlineAnnot(implicit ctx: Context) = InlineAnnotType.symbol.asClass
  @threadUnsafe lazy val NoinlineAnnotType: TypeRef = ctx.requiredClassRef("scala.noinline")
  def NoinlineAnnot(implicit ctx: Context) = NoinlineAnnotType.symbol.asClass

  @threadUnsafe lazy val JavaLangVoidType: TypeRef = ctx.requiredClassRef("java.lang.Void")
  def JavaLangVoidClass(implicit ctx: Context) = JavaLangVoidType.symbol.asClass

  @threadUnsafe lazy val ScalaJSJSPackageVal = ctx.requiredPackage("scala.scalajs.js")
  @threadUnsafe lazy val ScalaJSJSPackageClass = ScalaJSJSPackageVal.moduleClass.asClass
    @threadUnsafe lazy val JSPackage_typeOfR = ScalaJSJSPackageClass.requiredMethodRef("typeOf")
    def JSPackage_typeOf(implicit ctx: Context) = JSPackage_typeOfR.symbol
    @threadUnsafe lazy val JSPackage_constructorOfR = ScalaJSJSPackageClass.requiredMethodRef("constructorOf")
    def JSPackage_constructorOf(implicit ctx: Context) = JSPackage_constructorOfR.symbol
    @threadUnsafe lazy val JSPackage_nativeR = ScalaJSJSPackageClass.requiredMethodRef("native")
    def JSPackage_native(implicit ctx: Context) = JSPackage_nativeR.symbol

  @threadUnsafe lazy val JSNativeAnnotType: TypeRef = ctx.requiredClassRef("scala.scalajs.js.native")
  def JSNativeAnnot(implicit ctx: Context) = JSNativeAnnotType.symbol.asClass

  @threadUnsafe lazy val JSAnyType: TypeRef = ctx.requiredClassRef("scala.scalajs.js.Any")
  def JSAnyClass(implicit ctx: Context) = JSAnyType.symbol.asClass
  @threadUnsafe lazy val JSObjectType: TypeRef = ctx.requiredClassRef("scala.scalajs.js.Object")
  def JSObjectClass(implicit ctx: Context) = JSObjectType.symbol.asClass
  @threadUnsafe lazy val JSBaseThisFunctionType: TypeRef = ctx.requiredClassRef("scala.scalajs.js.ThisFunction")
  def JSBaseThisFunctionClass(implicit ctx: Context) = JSBaseThisFunctionType.symbol.asClass

  @threadUnsafe lazy val JSArrayType: TypeRef = ctx.requiredClassRef("scala.scalajs.js.Array")
  def JSArrayClass(implicit ctx: Context) = JSArrayType.symbol.asClass

  @threadUnsafe lazy val JSFunctionType = (0 to 22).map(n => ctx.requiredClassRef("scala.scalajs.js.Function" + n)).toArray
  def JSFunctionClass(n: Int)(implicit ctx: Context) = JSFunctionType(n).symbol.asClass
  @threadUnsafe lazy val JSThisFunctionType = (0 to 21).map(n => ctx.requiredClassRef("scala.scalajs.js.ThisFunction" + n)).toArray
  def JSThisFunctionClass(n: Int)(implicit ctx: Context) = JSThisFunctionType(n).symbol.asClass

  @threadUnsafe lazy val RuntimeExceptionType: TypeRef = ctx.requiredClassRef("java.lang.RuntimeException")
  def RuntimeExceptionClass(implicit ctx: Context) = RuntimeExceptionType.symbol.asClass
  @threadUnsafe lazy val JavaScriptExceptionType: TypeRef = ctx.requiredClassRef("scala.scalajs.js.JavaScriptException")
  def JavaScriptExceptionClass(implicit ctx: Context) = JavaScriptExceptionType.symbol.asClass

  @threadUnsafe lazy val JSGlobalScopeAnnotType: TypeRef = ctx.requiredClassRef("scala.scalajs.js.annotation.JSGlobalScope")
  def JSGlobalScopeAnnot(implicit ctx: Context) = JSGlobalScopeAnnotType.symbol.asClass
  @threadUnsafe lazy val JSNameAnnotType: TypeRef = ctx.requiredClassRef("scala.scalajs.js.annotation.JSName")
  def JSNameAnnot(implicit ctx: Context) = JSNameAnnotType.symbol.asClass
  @threadUnsafe lazy val JSFullNameAnnotType: TypeRef = ctx.requiredClassRef("scala.scalajs.js.annotation.JSFullName")
  def JSFullNameAnnot(implicit ctx: Context) = JSFullNameAnnotType.symbol.asClass
  @threadUnsafe lazy val JSBracketAccessAnnotType: TypeRef = ctx.requiredClassRef("scala.scalajs.js.annotation.JSBracketAccess")
  def JSBracketAccessAnnot(implicit ctx: Context) = JSBracketAccessAnnotType.symbol.asClass
  @threadUnsafe lazy val JSBracketCallAnnotType: TypeRef = ctx.requiredClassRef("scala.scalajs.js.annotation.JSBracketCall")
  def JSBracketCallAnnot(implicit ctx: Context) = JSBracketCallAnnotType.symbol.asClass
  @threadUnsafe lazy val JSExportAnnotType: TypeRef = ctx.requiredClassRef("scala.scalajs.js.annotation.JSExport")
  def JSExportAnnot(implicit ctx: Context) = JSExportAnnotType.symbol.asClass
  @threadUnsafe lazy val JSExportDescendentObjectsAnnotType: TypeRef = ctx.requiredClassRef("scala.scalajs.js.annotation.JSExportDescendentObjects")
  def JSExportDescendentObjectsAnnot(implicit ctx: Context) = JSExportDescendentObjectsAnnotType.symbol.asClass
  @threadUnsafe lazy val JSExportDescendentClassesAnnotType: TypeRef = ctx.requiredClassRef("scala.scalajs.js.annotation.JSExportDescendentClasses")
  def JSExportDescendentClassesAnnot(implicit ctx: Context) = JSExportDescendentClassesAnnotType.symbol.asClass
  @threadUnsafe lazy val JSExportAllAnnotType: TypeRef = ctx.requiredClassRef("scala.scalajs.js.annotation.JSExportAll")
  def JSExportAllAnnot(implicit ctx: Context) = JSExportAllAnnotType.symbol.asClass
  @threadUnsafe lazy val JSExportNamedAnnotType: TypeRef = ctx.requiredClassRef("scala.scalajs.js.annotation.JSExportNamed")
  def JSExportNamedAnnot(implicit ctx: Context) = JSExportNamedAnnotType.symbol.asClass
  @threadUnsafe lazy val RawJSTypeAnnotType: TypeRef = ctx.requiredClassRef("scala.scalajs.js.annotation.RawJSType")
  def RawJSTypeAnnot(implicit ctx: Context) = RawJSTypeAnnotType.symbol.asClass
  @threadUnsafe lazy val ExposedJSMemberAnnotType: TypeRef = ctx.requiredClassRef("scala.scalajs.js.annotation.ExposedJSMember")
  def ExposedJSMemberAnnot(implicit ctx: Context) = ExposedJSMemberAnnotType.symbol.asClass

  @threadUnsafe lazy val JSAnyModuleRef = ctx.requiredModuleRef("scala.scalajs.js.Any")
  def JSAnyModule(implicit ctx: Context) = JSAnyModuleRef.symbol
    @threadUnsafe lazy val JSAny_fromFunctionR = (0 to 22).map(n => JSAnyModule.requiredMethodRef("fromFunction" + n)).toArray
    def JSAny_fromFunction(n: Int)(implicit ctx: Context) = JSAny_fromFunctionR(n).symbol

  @threadUnsafe lazy val JSDynamicModuleRef = ctx.requiredModuleRef("scala.scalajs.js.Dynamic")
  def JSDynamicModule(implicit ctx: Context) = JSDynamicModuleRef.symbol
    @threadUnsafe lazy val JSDynamic_globalR = JSDynamicModule.requiredMethodRef("global")
    def JSDynamic_global(implicit ctx: Context) = JSDynamic_globalR.symbol
    @threadUnsafe lazy val JSDynamic_newInstanceR = JSDynamicModule.requiredMethodRef("newInstance")
    def JSDynamic_newInstance(implicit ctx: Context) = JSDynamic_newInstanceR.symbol

  @threadUnsafe lazy val JSDynamicLiteralModuleRef = JSDynamicModule.moduleClass.requiredValueRef("literal")
  def JSDynamicLiteralModule(implicit ctx: Context) = JSDynamicLiteralModuleRef.symbol
    @threadUnsafe lazy val JSDynamicLiteral_applyDynamicNamedR = JSDynamicLiteralModule.requiredMethodRef("applyDynamicNamed")
    def JSDynamicLiteral_applyDynamicNamed(implicit ctx: Context) = JSDynamicLiteral_applyDynamicNamedR.symbol
    @threadUnsafe lazy val JSDynamicLiteral_applyDynamicR = JSDynamicLiteralModule.requiredMethodRef("applyDynamic")
    def JSDynamicLiteral_applyDynamic(implicit ctx: Context) = JSDynamicLiteral_applyDynamicR.symbol

  @threadUnsafe lazy val JSObjectModuleRef = ctx.requiredModuleRef("scala.scalajs.js.Object")
  def JSObjectModule(implicit ctx: Context) = JSObjectModuleRef.symbol

  @threadUnsafe lazy val JSArrayModuleRef = ctx.requiredModuleRef("scala.scalajs.js.Array")
  def JSArrayModule(implicit ctx: Context) = JSArrayModuleRef.symbol
    @threadUnsafe lazy val JSArray_applyR = JSArrayModule.requiredMethodRef(nme.apply)
    def JSArray_apply(implicit ctx: Context) = JSArray_applyR.symbol

  @threadUnsafe lazy val JSThisFunctionModuleRef = ctx.requiredModuleRef("scala.scalajs.js.ThisFunction")
  def JSThisFunctionModule(implicit ctx: Context) = JSThisFunctionModuleRef.symbol
    @threadUnsafe lazy val JSThisFunction_fromFunctionR = (1 to 22).map(n => JSThisFunctionModule.requiredMethodRef("fromFunction" + n)).toArray
    def JSThisFunction_fromFunction(n: Int)(implicit ctx: Context) = JSThisFunction_fromFunctionR(n - 1).symbol

  @threadUnsafe lazy val JSConstructorTagModuleRef = ctx.requiredModuleRef("scala.scalajs.js.ConstructorTag")
  def JSConstructorTagModule(implicit ctx: Context) = JSConstructorTagModuleRef.symbol
    @threadUnsafe lazy val JSConstructorTag_materializeR = JSConstructorTagModule.requiredMethodRef("materialize")
    def JSConstructorTag_materialize(implicit ctx: Context) = JSConstructorTag_materializeR.symbol

  @threadUnsafe lazy val RuntimePackageVal = ctx.requiredPackage("scala.scalajs.runtime")
  @threadUnsafe lazy val RuntimePackageClass = RuntimePackageVal.moduleClass.asClass
    @threadUnsafe lazy val RuntimePackage_wrapJavaScriptExceptionR = RuntimePackageClass.requiredMethodRef("wrapJavaScriptException")
    def Runtime_wrapJavaScriptException(implicit ctx: Context) = RuntimePackage_wrapJavaScriptExceptionR.symbol
    @threadUnsafe lazy val Runtime_unwrapJavaScriptExceptionR = RuntimePackageClass.requiredMethodRef("unwrapJavaScriptException")
    def Runtime_unwrapJavaScriptException(implicit ctx: Context) = Runtime_unwrapJavaScriptExceptionR.symbol
    @threadUnsafe lazy val Runtime_toScalaVarArgsR = RuntimePackageClass.requiredMethodRef("toScalaVarArgs")
    def Runtime_toScalaVarArgs(implicit ctx: Context) = Runtime_toScalaVarArgsR.symbol
    @threadUnsafe lazy val Runtime_toJSVarArgsR = RuntimePackageClass.requiredMethodRef("toJSVarArgs")
    def Runtime_toJSVarArgs(implicit ctx: Context) = Runtime_toJSVarArgsR.symbol
    @threadUnsafe lazy val Runtime_constructorOfR = RuntimePackageClass.requiredMethodRef("constructorOf")
    def Runtime_constructorOf(implicit ctx: Context) = Runtime_constructorOfR.symbol
    @threadUnsafe lazy val Runtime_newConstructorTagR = RuntimePackageClass.requiredMethodRef("newConstructorTag")
    def Runtime_newConstructorTag(implicit ctx: Context) = Runtime_newConstructorTagR.symbol
    @threadUnsafe lazy val Runtime_linkingInfoR = RuntimePackageClass.requiredMethodRef("linkingInfo")
    def Runtime_linkingInfo(implicit ctx: Context) = Runtime_linkingInfoR.symbol

  @threadUnsafe lazy val SpecialPackageVal = ctx.requiredPackage("scala.scalajs.js.special")
  @threadUnsafe lazy val SpecialPackageClass = SpecialPackageVal.moduleClass.asClass
    @threadUnsafe lazy val Special_debuggerR = SpecialPackageClass.requiredMethodRef("debugger")
    def Special_debugger(implicit ctx: Context) = Special_debuggerR.symbol
    @threadUnsafe lazy val Special_deleteR = SpecialPackageClass.requiredMethodRef("delete")
    def Special_delete(implicit ctx: Context) = Special_deleteR.symbol
    @threadUnsafe lazy val Special_forinR = SpecialPackageClass.requiredMethodRef("forin")
    def Special_forin(implicit ctx: Context) = Special_forinR.symbol
    @threadUnsafe lazy val Special_inR = SpecialPackageClass.requiredMethodRef("in")
    def Special_in(implicit ctx: Context) = Special_inR.symbol
    @threadUnsafe lazy val Special_instanceofR = SpecialPackageClass.requiredMethodRef("instanceof")
    def Special_instanceof(implicit ctx: Context) = Special_instanceofR.symbol

  @threadUnsafe lazy val WrappedArrayType: TypeRef = ctx.requiredClassRef("scala.scalajs.js.WrappedArray")
  def WrappedArrayClass(implicit ctx: Context) = WrappedArrayType.symbol.asClass

  @threadUnsafe lazy val ScalaRunTime_isArrayR = defn.ScalaRuntimeModule.requiredMethodRef("isArray", List(???, ???))
  def ScalaRunTime_isArray(implicit ctx: Context): Symbol = ScalaRunTime_isArrayR.symbol

  @threadUnsafe lazy val BoxesRunTime_boxToCharacterR = defn.BoxesRunTimeModule.requiredMethodRef("boxToCharacter")
  def BoxesRunTime_boxToCharacter(implicit ctx: Context): Symbol = BoxesRunTime_boxToCharacterR.symbol
  @threadUnsafe lazy val BoxesRunTime_unboxToCharR = defn.BoxesRunTimeModule.requiredMethodRef("unboxToChar")
  def BoxesRunTime_unboxToChar(implicit ctx: Context): Symbol = BoxesRunTime_unboxToCharR.symbol

  @threadUnsafe lazy val EnableReflectiveInstantiationAnnotType: TypeRef = ctx.requiredClassRef("scala.scalajs.reflect.annotation.EnableReflectiveInstantiation")
  def EnableReflectiveInstantiationAnnot(implicit ctx: Context) = EnableReflectiveInstantiationAnnotType.symbol.asClass

  @threadUnsafe lazy val ReflectModuleRef = ctx.requiredModuleRef("scala.scalajs.reflect.Reflect")
  def ReflectModule(implicit ctx: Context) = ReflectModuleRef.symbol
    @threadUnsafe lazy val Reflect_registerLoadableModuleClassR = ReflectModule.requiredMethodRef("registerLoadableModuleClass")
    def Reflect_registerLoadableModuleClass(implicit ctx: Context) = Reflect_registerLoadableModuleClassR.symbol
    @threadUnsafe lazy val Reflect_registerInstantiatableClassR = ReflectModule.requiredMethodRef("registerInstantiatableClass")
    def Reflect_registerInstantiatableClass(implicit ctx: Context) = Reflect_registerInstantiatableClassR.symbol

  /** If `cls` is a class in the scala package, its name, otherwise EmptyTypeName */
  private def scalajsClassName(cls: Symbol)(implicit ctx: Context): TypeName =
    if (cls.isClass && cls.owner == ScalaJSJSPackageClass) cls.asClass.name
    else EmptyTypeName

  /** Is the given `cls` a class of the form `scala.scalajs.js.prefixN` where
   *  `N` is a number.
   *
   *  This is similar to `isVarArityClass` in `Definitions.scala`.
   */
  private def isScalaJSVarArityClass(cls: Symbol, prefix: String): Boolean = {
    val name = scalajsClassName(cls)
    name.startsWith(prefix) && name.toString.drop(prefix.length).forall(_.isDigit)
  }

  def isJSFunctionClass(cls: Symbol): Boolean =
    isScalaJSVarArityClass(cls, str.Function)

  def isJSThisFunctionClass(cls: Symbol): Boolean =
    isScalaJSVarArityClass(cls, "ThisFunction")

  /** Definitions related to the treatment of JUnit boostrappers. */
  object junit {
    @threadUnsafe lazy val TestAnnotType: TypeRef = ctx.requiredClassRef("org.junit.Test")
    def TestAnnotClass(implicit ctx: Context): ClassSymbol = TestAnnotType.symbol.asClass

    @threadUnsafe lazy val BeforeAnnotType: TypeRef = ctx.requiredClassRef("org.junit.Before")
    def BeforeAnnotClass(implicit ctx: Context): ClassSymbol = BeforeAnnotType.symbol.asClass

    @threadUnsafe lazy val AfterAnnotType: TypeRef = ctx.requiredClassRef("org.junit.After")
    def AfterAnnotClass(implicit ctx: Context): ClassSymbol = AfterAnnotType.symbol.asClass

    @threadUnsafe lazy val BeforeClassAnnotType: TypeRef = ctx.requiredClassRef("org.junit.BeforeClass")
    def BeforeClassAnnotClass(implicit ctx: Context): ClassSymbol = BeforeClassAnnotType.symbol.asClass

    @threadUnsafe lazy val AfterClassAnnotType: TypeRef = ctx.requiredClassRef("org.junit.AfterClass")
    def AfterClassAnnotClass(implicit ctx: Context): ClassSymbol = AfterClassAnnotType.symbol.asClass

    @threadUnsafe lazy val IgnoreAnnotType: TypeRef = ctx.requiredClassRef("org.junit.Ignore")
    def IgnoreAnnotClass(implicit ctx: Context): ClassSymbol = IgnoreAnnotType.symbol.asClass

    @threadUnsafe lazy val BootstrapperType: TypeRef = ctx.requiredClassRef("org.scalajs.junit.Bootstrapper")

    @threadUnsafe lazy val TestMetadataType: TypeRef = ctx.requiredClassRef("org.scalajs.junit.TestMetadata")

    @threadUnsafe lazy val NoSuchMethodExceptionType: TypeRef = ctx.requiredClassRef("java.lang.NoSuchMethodException")

    @threadUnsafe lazy val FutureType: TypeRef = ctx.requiredClassRef("scala.concurrent.Future")
    def FutureClass(implicit ctx: Context): ClassSymbol = FutureType.symbol.asClass

    @threadUnsafe private lazy val FutureModule_successfulR = ctx.requiredModule("scala.concurrent.Future").requiredMethodRef("successful")
    def FutureModule_successful(implicit ctx: Context): Symbol = FutureModule_successfulR.symbol

    @threadUnsafe private lazy val SuccessModule_applyR = ctx.requiredModule("scala.util.Success").requiredMethodRef(nme.apply)
    def SuccessModule_apply(implicit ctx: Context): Symbol = SuccessModule_applyR.symbol
  }

}
