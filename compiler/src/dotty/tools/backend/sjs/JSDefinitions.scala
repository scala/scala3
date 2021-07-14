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
  def jsdefn(using Context): JSDefinitions =
    ctx.platform.asInstanceOf[SJSPlatform].jsDefinitions
}

final class JSDefinitions()(using Context) {

  @threadUnsafe lazy val InlineAnnotType: TypeRef = requiredClassRef("scala.inline")
  def InlineAnnot(using Context) = InlineAnnotType.symbol.asClass
  @threadUnsafe lazy val NoinlineAnnotType: TypeRef = requiredClassRef("scala.noinline")
  def NoinlineAnnot(using Context) = NoinlineAnnotType.symbol.asClass

  @threadUnsafe lazy val JavaLangVoidType: TypeRef = requiredClassRef("java.lang.Void")
  def JavaLangVoidClass(using Context) = JavaLangVoidType.symbol.asClass

  @threadUnsafe lazy val ScalaJSJSPackageVal = requiredPackage("scala.scalajs.js")
  @threadUnsafe lazy val ScalaJSJSPackageClass = ScalaJSJSPackageVal.moduleClass.asClass
    @threadUnsafe lazy val JSPackage_typeOfR = ScalaJSJSPackageClass.requiredMethodRef("typeOf")
    def JSPackage_typeOf(using Context) = JSPackage_typeOfR.symbol
    @threadUnsafe lazy val JSPackage_constructorOfR = ScalaJSJSPackageClass.requiredMethodRef("constructorOf")
    def JSPackage_constructorOf(using Context) = JSPackage_constructorOfR.symbol
    @threadUnsafe lazy val JSPackage_nativeR = ScalaJSJSPackageClass.requiredMethodRef("native")
    def JSPackage_native(using Context) = JSPackage_nativeR.symbol
    @threadUnsafe lazy val JSPackage_undefinedR = ScalaJSJSPackageClass.requiredMethodRef("undefined")
    def JSPackage_undefined(using Context) = JSPackage_undefinedR.symbol

  @threadUnsafe lazy val JSNativeAnnotType: TypeRef = requiredClassRef("scala.scalajs.js.native")
  def JSNativeAnnot(using Context) = JSNativeAnnotType.symbol.asClass

  @threadUnsafe lazy val JSAnyType: TypeRef = requiredClassRef("scala.scalajs.js.Any")
  def JSAnyClass(using Context) = JSAnyType.symbol.asClass
  @threadUnsafe lazy val JSObjectType: TypeRef = requiredClassRef("scala.scalajs.js.Object")
  def JSObjectClass(using Context) = JSObjectType.symbol.asClass
  @threadUnsafe lazy val JSFunctionType: TypeRef = requiredClassRef("scala.scalajs.js.Function")
  def JSFunctionClass(using Context) = JSFunctionType.symbol.asClass
  @threadUnsafe lazy val JSThisFunctionType: TypeRef = requiredClassRef("scala.scalajs.js.ThisFunction")
  def JSThisFunctionClass(using Context) = JSThisFunctionType.symbol.asClass

  @threadUnsafe lazy val PseudoUnionType: TypeRef = requiredClassRef("scala.scalajs.js.|")
  def PseudoUnionClass(using Context) = PseudoUnionType.symbol.asClass

  @threadUnsafe lazy val PseudoUnionModuleRef = requiredModuleRef("scala.scalajs.js.|")
  def PseudoUnionModule(using Context) = PseudoUnionModuleRef.symbol
    @threadUnsafe lazy val PseudoUnion_fromR = PseudoUnionModule.requiredMethodRef("from")
    def PseudoUnion_from(using Context) = PseudoUnion_fromR.symbol
    @threadUnsafe lazy val PseudoUnion_fromTypeConstructorR = PseudoUnionModule.requiredMethodRef("fromTypeConstructor")
    def PseudoUnion_fromTypeConstructor(using Context) = PseudoUnion_fromTypeConstructorR.symbol

  @threadUnsafe lazy val UnionOpsModuleRef = requiredModuleRef("scala.scalajs.js.internal.UnitOps")

  @threadUnsafe lazy val JSArrayType: TypeRef = requiredClassRef("scala.scalajs.js.Array")
  def JSArrayClass(using Context) = JSArrayType.symbol.asClass
  @threadUnsafe lazy val JSDynamicType: TypeRef = requiredClassRef("scala.scalajs.js.Dynamic")
  def JSDynamicClass(using Context) = JSDynamicType.symbol.asClass

  @threadUnsafe lazy val RuntimeExceptionType: TypeRef = requiredClassRef("java.lang.RuntimeException")
  def RuntimeExceptionClass(using Context) = RuntimeExceptionType.symbol.asClass
  @threadUnsafe lazy val JavaScriptExceptionType: TypeRef = requiredClassRef("scala.scalajs.js.JavaScriptException")
  def JavaScriptExceptionClass(using Context) = JavaScriptExceptionType.symbol.asClass

  @threadUnsafe lazy val JSGlobalAnnotType: TypeRef = requiredClassRef("scala.scalajs.js.annotation.JSGlobal")
  def JSGlobalAnnot(using Context) = JSGlobalAnnotType.symbol.asClass
  @threadUnsafe lazy val JSImportAnnotType: TypeRef = requiredClassRef("scala.scalajs.js.annotation.JSImport")
  def JSImportAnnot(using Context) = JSImportAnnotType.symbol.asClass
  @threadUnsafe lazy val JSGlobalScopeAnnotType: TypeRef = requiredClassRef("scala.scalajs.js.annotation.JSGlobalScope")
  def JSGlobalScopeAnnot(using Context) = JSGlobalScopeAnnotType.symbol.asClass
  @threadUnsafe lazy val JSNameAnnotType: TypeRef = requiredClassRef("scala.scalajs.js.annotation.JSName")
  def JSNameAnnot(using Context) = JSNameAnnotType.symbol.asClass
  @threadUnsafe lazy val JSFullNameAnnotType: TypeRef = requiredClassRef("scala.scalajs.js.annotation.JSFullName")
  def JSFullNameAnnot(using Context) = JSFullNameAnnotType.symbol.asClass
  @threadUnsafe lazy val JSBracketAccessAnnotType: TypeRef = requiredClassRef("scala.scalajs.js.annotation.JSBracketAccess")
  def JSBracketAccessAnnot(using Context) = JSBracketAccessAnnotType.symbol.asClass
  @threadUnsafe lazy val JSBracketCallAnnotType: TypeRef = requiredClassRef("scala.scalajs.js.annotation.JSBracketCall")
  def JSBracketCallAnnot(using Context) = JSBracketCallAnnotType.symbol.asClass
  @threadUnsafe lazy val JSExportTopLevelAnnotType: TypeRef = requiredClassRef("scala.scalajs.js.annotation.JSExportTopLevel")
  def JSExportTopLevelAnnot(using Context) = JSExportTopLevelAnnotType.symbol.asClass
  @threadUnsafe lazy val JSExportAnnotType: TypeRef = requiredClassRef("scala.scalajs.js.annotation.JSExport")
  def JSExportAnnot(using Context) = JSExportAnnotType.symbol.asClass
  @threadUnsafe lazy val JSExportStaticAnnotType: TypeRef = requiredClassRef("scala.scalajs.js.annotation.JSExportStatic")
  def JSExportStaticAnnot(using Context) = JSExportStaticAnnotType.symbol.asClass
  @threadUnsafe lazy val JSExportAllAnnotType: TypeRef = requiredClassRef("scala.scalajs.js.annotation.JSExportAll")
  def JSExportAllAnnot(using Context) = JSExportAllAnnotType.symbol.asClass

  def JSAnnotPackage(using Context) = JSGlobalAnnot.owner.asClass

  @threadUnsafe lazy val JSTypeAnnotType: TypeRef = requiredClassRef("scala.scalajs.js.annotation.internal.JSType")
  def JSTypeAnnot(using Context) = JSTypeAnnotType.symbol.asClass
  @threadUnsafe lazy val JSOptionalAnnotType: TypeRef = requiredClassRef("scala.scalajs.js.annotation.internal.JSOptional")
  def JSOptionalAnnot(using Context) = JSOptionalAnnotType.symbol.asClass
  @threadUnsafe lazy val ExposedJSMemberAnnotType: TypeRef = requiredClassRef("scala.scalajs.js.annotation.internal.ExposedJSMember")
  def ExposedJSMemberAnnot(using Context) = ExposedJSMemberAnnotType.symbol.asClass

  @threadUnsafe lazy val JSImportNamespaceModuleRef = requiredModuleRef("scala.scalajs.js.annotation.JSImport.Namespace")
  def JSImportNamespaceModule(using Context) = JSImportNamespaceModuleRef.symbol

  @threadUnsafe lazy val JSAnyModuleRef = requiredModuleRef("scala.scalajs.js.Any")
  def JSAnyModule(using Context) = JSAnyModuleRef.symbol
    @threadUnsafe lazy val JSAny_fromFunctionR = (0 to 22).map(n => JSAnyModule.requiredMethodRef("fromFunction" + n)).toArray
    def JSAny_fromFunction(n: Int)(using Context) = JSAny_fromFunctionR(n).symbol

  @threadUnsafe lazy val JSDynamicModuleRef = requiredModuleRef("scala.scalajs.js.Dynamic")
  def JSDynamicModule(using Context) = JSDynamicModuleRef.symbol
    @threadUnsafe lazy val JSDynamic_globalR = JSDynamicModule.requiredMethodRef("global")
    def JSDynamic_global(using Context) = JSDynamic_globalR.symbol
    @threadUnsafe lazy val JSDynamic_newInstanceR = JSDynamicModule.requiredMethodRef("newInstance")
    def JSDynamic_newInstance(using Context) = JSDynamic_newInstanceR.symbol

  @threadUnsafe lazy val JSDynamicLiteralModuleRef = JSDynamicModule.moduleClass.requiredValueRef("literal")
  def JSDynamicLiteralModule(using Context) = JSDynamicLiteralModuleRef.symbol
    @threadUnsafe lazy val JSDynamicLiteral_applyDynamicNamedR = JSDynamicLiteralModule.requiredMethodRef("applyDynamicNamed")
    def JSDynamicLiteral_applyDynamicNamed(using Context) = JSDynamicLiteral_applyDynamicNamedR.symbol
    @threadUnsafe lazy val JSDynamicLiteral_applyDynamicR = JSDynamicLiteralModule.requiredMethodRef("applyDynamic")
    def JSDynamicLiteral_applyDynamic(using Context) = JSDynamicLiteral_applyDynamicR.symbol

  @threadUnsafe lazy val JSObjectModuleRef = requiredModuleRef("scala.scalajs.js.Object")
  def JSObjectModule(using Context) = JSObjectModuleRef.symbol

  @threadUnsafe lazy val JSArrayModuleRef = requiredModuleRef("scala.scalajs.js.Array")
  def JSArrayModule(using Context) = JSArrayModuleRef.symbol
    @threadUnsafe lazy val JSArray_applyR = JSArrayModule.requiredMethodRef(nme.apply)
    def JSArray_apply(using Context) = JSArray_applyR.symbol

  @threadUnsafe lazy val JSThisFunctionModuleRef = requiredModuleRef("scala.scalajs.js.ThisFunction")
  def JSThisFunctionModule(using Context) = JSThisFunctionModuleRef.symbol
    @threadUnsafe lazy val JSThisFunction_fromFunctionR = (1 to 22).map(n => JSThisFunctionModule.requiredMethodRef("fromFunction" + n)).toArray
    def JSThisFunction_fromFunction(n: Int)(using Context) = JSThisFunction_fromFunctionR(n - 1).symbol

  @threadUnsafe lazy val JSConstructorTagModuleRef = requiredModuleRef("scala.scalajs.js.ConstructorTag")
  def JSConstructorTagModule(using Context) = JSConstructorTagModuleRef.symbol
    @threadUnsafe lazy val JSConstructorTag_materializeR = JSConstructorTagModule.requiredMethodRef("materialize")
    def JSConstructorTag_materialize(using Context) = JSConstructorTag_materializeR.symbol

  @threadUnsafe lazy val JSImportModuleRef = requiredModuleRef("scala.scalajs.js.import")
  def JSImportModule(using Context) = JSImportModuleRef.symbol
    @threadUnsafe lazy val JSImport_applyR = JSImportModule.requiredMethodRef(nme.apply)
    def JSImport_apply(using Context) = JSImport_applyR.symbol
    @threadUnsafe lazy val JSImport_metaR = JSImportModule.requiredMethodRef("meta")
    def JSImport_meta(using Context) = JSImport_metaR.symbol

  @threadUnsafe lazy val RuntimePackageVal = requiredPackage("scala.scalajs.runtime")
  @threadUnsafe lazy val RuntimePackageClass = RuntimePackageVal.moduleClass.asClass
    @threadUnsafe lazy val RuntimePackage_wrapJavaScriptExceptionR = RuntimePackageClass.requiredMethodRef("wrapJavaScriptException")
    def Runtime_wrapJavaScriptException(using Context) = RuntimePackage_wrapJavaScriptExceptionR.symbol
    @threadUnsafe lazy val Runtime_unwrapJavaScriptExceptionR = RuntimePackageClass.requiredMethodRef("unwrapJavaScriptException")
    def Runtime_unwrapJavaScriptException(using Context) = Runtime_unwrapJavaScriptExceptionR.symbol
    @threadUnsafe lazy val Runtime_toScalaVarArgsR = RuntimePackageClass.requiredMethodRef("toScalaVarArgs")
    def Runtime_toScalaVarArgs(using Context) = Runtime_toScalaVarArgsR.symbol
    @threadUnsafe lazy val Runtime_toJSVarArgsR = RuntimePackageClass.requiredMethodRef("toJSVarArgs")
    def Runtime_toJSVarArgs(using Context) = Runtime_toJSVarArgsR.symbol
    @threadUnsafe lazy val Runtime_privateFieldsSymbolR = RuntimePackageClass.requiredMethodRef("privateFieldsSymbol")
    def Runtime_privateFieldsSymbol(using Context) = Runtime_privateFieldsSymbolR.symbol
    @threadUnsafe lazy val Runtime_constructorOfR = RuntimePackageClass.requiredMethodRef("constructorOf")
    def Runtime_constructorOf(using Context) = Runtime_constructorOfR.symbol
    @threadUnsafe lazy val Runtime_newConstructorTagR = RuntimePackageClass.requiredMethodRef("newConstructorTag")
    def Runtime_newConstructorTag(using Context) = Runtime_newConstructorTagR.symbol
    @threadUnsafe lazy val Runtime_createInnerJSClassR = RuntimePackageClass.requiredMethodRef("createInnerJSClass")
    def Runtime_createInnerJSClass(using Context) = Runtime_createInnerJSClassR.symbol
    @threadUnsafe lazy val Runtime_createLocalJSClassR = RuntimePackageClass.requiredMethodRef("createLocalJSClass")
    def Runtime_createLocalJSClass(using Context) = Runtime_createLocalJSClassR.symbol
    @threadUnsafe lazy val Runtime_withContextualJSClassValueR = RuntimePackageClass.requiredMethodRef("withContextualJSClassValue")
    def Runtime_withContextualJSClassValue(using Context) = Runtime_withContextualJSClassValueR.symbol
    @threadUnsafe lazy val Runtime_linkingInfoR = RuntimePackageClass.requiredMethodRef("linkingInfo")
    def Runtime_linkingInfo(using Context) = Runtime_linkingInfoR.symbol

  @threadUnsafe lazy val SpecialPackageVal = requiredPackage("scala.scalajs.js.special")
  @threadUnsafe lazy val SpecialPackageClass = SpecialPackageVal.moduleClass.asClass
    @threadUnsafe lazy val Special_debuggerR = SpecialPackageClass.requiredMethodRef("debugger")
    def Special_debugger(using Context) = Special_debuggerR.symbol
    @threadUnsafe lazy val Special_deleteR = SpecialPackageClass.requiredMethodRef("delete")
    def Special_delete(using Context) = Special_deleteR.symbol
    @threadUnsafe lazy val Special_forinR = SpecialPackageClass.requiredMethodRef("forin")
    def Special_forin(using Context) = Special_forinR.symbol
    @threadUnsafe lazy val Special_inR = SpecialPackageClass.requiredMethodRef("in")
    def Special_in(using Context) = Special_inR.symbol
    @threadUnsafe lazy val Special_instanceofR = SpecialPackageClass.requiredMethodRef("instanceof")
    def Special_instanceof(using Context) = Special_instanceofR.symbol
    @threadUnsafe lazy val Special_strictEqualsR = SpecialPackageClass.requiredMethodRef("strictEquals")
    def Special_strictEquals(using Context) = Special_strictEqualsR.symbol

  @threadUnsafe lazy val WrappedArrayType: TypeRef = requiredClassRef("scala.scalajs.js.WrappedArray")
  def WrappedArrayClass(using Context) = WrappedArrayType.symbol.asClass

  @threadUnsafe lazy val ScalaRunTime_isArrayR = defn.ScalaRuntimeModule.requiredMethodRef("isArray", List(???, ???))
  def ScalaRunTime_isArray(using Context): Symbol = ScalaRunTime_isArrayR.symbol

  @threadUnsafe lazy val BoxesRunTime_boxToCharacterR = defn.BoxesRunTimeModule.requiredMethodRef("boxToCharacter")
  def BoxesRunTime_boxToCharacter(using Context): Symbol = BoxesRunTime_boxToCharacterR.symbol
  @threadUnsafe lazy val BoxesRunTime_unboxToCharR = defn.BoxesRunTimeModule.requiredMethodRef("unboxToChar")
  def BoxesRunTime_unboxToChar(using Context): Symbol = BoxesRunTime_unboxToCharR.symbol

  @threadUnsafe lazy val EnableReflectiveInstantiationAnnotType: TypeRef = requiredClassRef("scala.scalajs.reflect.annotation.EnableReflectiveInstantiation")
  def EnableReflectiveInstantiationAnnot(using Context) = EnableReflectiveInstantiationAnnotType.symbol.asClass

  @threadUnsafe lazy val ReflectModuleRef = requiredModuleRef("scala.scalajs.reflect.Reflect")
  def ReflectModule(using Context) = ReflectModuleRef.symbol
    @threadUnsafe lazy val Reflect_registerLoadableModuleClassR = ReflectModule.requiredMethodRef("registerLoadableModuleClass")
    def Reflect_registerLoadableModuleClass(using Context) = Reflect_registerLoadableModuleClassR.symbol
    @threadUnsafe lazy val Reflect_registerInstantiatableClassR = ReflectModule.requiredMethodRef("registerInstantiatableClass")
    def Reflect_registerInstantiatableClass(using Context) = Reflect_registerInstantiatableClassR.symbol

  @threadUnsafe lazy val ReflectSelectableType: TypeRef = requiredClassRef("scala.reflect.Selectable")
  def ReflectSelectableClass(using Context) = ReflectSelectableType.symbol.asClass
    @threadUnsafe lazy val ReflectSelectable_selectDynamicR = ReflectSelectableClass.requiredMethodRef("selectDynamic")
    def ReflectSelectable_selectDynamic(using Context) = ReflectSelectable_selectDynamicR.symbol
    @threadUnsafe lazy val ReflectSelectable_applyDynamicR = ReflectSelectableClass.requiredMethodRef("applyDynamic")
    def ReflectSelectable_applyDynamic(using Context) = ReflectSelectable_applyDynamicR.symbol

  @threadUnsafe lazy val ReflectSelectableModuleRef = requiredModuleRef("scala.reflect.Selectable")
  def ReflectSelectableModule(using Context) = ReflectSelectableModuleRef.symbol
    @threadUnsafe lazy val ReflectSelectable_reflectiveSelectableR = ReflectSelectableModule.requiredMethodRef("reflectiveSelectable")
    def ReflectSelectable_reflectiveSelectable(using Context) = ReflectSelectable_reflectiveSelectableR.symbol

  @threadUnsafe lazy val SelectableModuleRef = requiredModuleRef("scala.Selectable")
  def SelectableModule(using Context) = SelectableModuleRef.symbol
    @threadUnsafe lazy val Selectable_reflectiveSelectableFromLangReflectiveCallsR = SelectableModule.requiredMethodRef("reflectiveSelectableFromLangReflectiveCalls")
    def Selectable_reflectiveSelectableFromLangReflectiveCalls(using Context) = Selectable_reflectiveSelectableFromLangReflectiveCallsR.symbol

  private var allRefClassesCache: Set[Symbol] = _
  def allRefClasses(using Context): Set[Symbol] = {
    if (allRefClassesCache == null) {
      val baseNames = List("Object", "Boolean", "Character", "Byte", "Short",
          "Int", "Long", "Float", "Double")
      val fullNames = baseNames.flatMap { base =>
        List(s"scala.runtime.${base}Ref", s"scala.runtime.Volatile${base}Ref")
      }
      allRefClassesCache = fullNames.map(name => requiredClass(name)).toSet
    }
    allRefClassesCache
  }

  /** Definitions related to the treatment of JUnit bootstrappers. */
  object junit {
    @threadUnsafe lazy val TestAnnotType: TypeRef = requiredClassRef("org.junit.Test")
    def TestAnnotClass(using Context): ClassSymbol = TestAnnotType.symbol.asClass

    @threadUnsafe lazy val BeforeAnnotType: TypeRef = requiredClassRef("org.junit.Before")
    def BeforeAnnotClass(using Context): ClassSymbol = BeforeAnnotType.symbol.asClass

    @threadUnsafe lazy val AfterAnnotType: TypeRef = requiredClassRef("org.junit.After")
    def AfterAnnotClass(using Context): ClassSymbol = AfterAnnotType.symbol.asClass

    @threadUnsafe lazy val BeforeClassAnnotType: TypeRef = requiredClassRef("org.junit.BeforeClass")
    def BeforeClassAnnotClass(using Context): ClassSymbol = BeforeClassAnnotType.symbol.asClass

    @threadUnsafe lazy val AfterClassAnnotType: TypeRef = requiredClassRef("org.junit.AfterClass")
    def AfterClassAnnotClass(using Context): ClassSymbol = AfterClassAnnotType.symbol.asClass

    @threadUnsafe lazy val IgnoreAnnotType: TypeRef = requiredClassRef("org.junit.Ignore")
    def IgnoreAnnotClass(using Context): ClassSymbol = IgnoreAnnotType.symbol.asClass

    @threadUnsafe lazy val BootstrapperType: TypeRef = requiredClassRef("org.scalajs.junit.Bootstrapper")

    @threadUnsafe lazy val TestMetadataType: TypeRef = requiredClassRef("org.scalajs.junit.TestMetadata")

    @threadUnsafe lazy val NoSuchMethodExceptionType: TypeRef = requiredClassRef("java.lang.NoSuchMethodException")

    @threadUnsafe lazy val FutureType: TypeRef = requiredClassRef("scala.concurrent.Future")
    def FutureClass(using Context): ClassSymbol = FutureType.symbol.asClass

    @threadUnsafe private lazy val FutureModule_successfulR = requiredModule("scala.concurrent.Future").requiredMethodRef("successful")
    def FutureModule_successful(using Context): Symbol = FutureModule_successfulR.symbol

    @threadUnsafe private lazy val SuccessModule_applyR = requiredModule("scala.util.Success").requiredMethodRef(nme.apply)
    def SuccessModule_apply(using Context): Symbol = SuccessModule_applyR.symbol
  }

}
