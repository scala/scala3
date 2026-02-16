package dotty.tools.backend.sjs

import dotty.tools.dotc.core.*
import Names.TermName
import Types.*
import Contexts.*
import Symbols.*
import Decorators.em

import dotty.tools.dotc.ast.tpd.*
import dotty.tools.backend.jvm.DottyPrimitives
import dotty.tools.dotc.report
import dotty.tools.dotc.util.ReadOnlyMap

object JSPrimitives {

  inline val FirstJSPrimitiveCode = 300

  inline val DYNNEW = FirstJSPrimitiveCode + 1 // Instantiate a new JavaScript object

  inline val ARR_CREATE = DYNNEW + 1 // js.Array.apply (array literal syntax)

  inline val TYPEOF = ARR_CREATE + 1 // typeof x
  inline val JS_NATIVE = TYPEOF + 1  // js.native. Marker method. Fails if tried to be emitted.

  inline val UNITVAL = JS_NATIVE + 1 // () value, which is undefined

  inline val JS_NEW_TARGET = UNITVAL + 1 // js.new.target

  inline val JS_IMPORT = JS_NEW_TARGET + 1  // js.import.apply(specifier)
  inline val JS_IMPORT_META = JS_IMPORT + 1 // js.import.meta

  inline val JS_ASYNC = JS_IMPORT_META + 1 // js.async
  inline val JS_AWAIT = JS_ASYNC + 1       // js.await

  inline val CONSTRUCTOROF = JS_AWAIT + 1                               // runtime.constructorOf(clazz)
  inline val CREATE_INNER_JS_CLASS = CONSTRUCTOROF + 1                  // runtime.createInnerJSClass
  inline val CREATE_LOCAL_JS_CLASS = CREATE_INNER_JS_CLASS + 1          // runtime.createLocalJSClass
  inline val WITH_CONTEXTUAL_JS_CLASS_VALUE = CREATE_LOCAL_JS_CLASS + 1 // runtime.withContextualJSClassValue
  inline val DYNAMIC_IMPORT = WITH_CONTEXTUAL_JS_CLASS_VALUE + 1        // runtime.dynamicImport

  inline val STRICT_EQ = DYNAMIC_IMPORT + 1                // js.special.strictEquals
  inline val IN = STRICT_EQ + 1                            // js.special.in
  inline val INSTANCEOF = IN + 1                           // js.special.instanceof
  inline val DELETE = INSTANCEOF + 1                       // js.special.delete
  inline val FORIN = DELETE + 1                            // js.special.forin
  inline val JS_THROW = FORIN + 1                          // js.special.throw
  inline val JS_TRY_CATCH = JS_THROW + 1                   // js.special.tryCatch
  inline val WRAP_AS_THROWABLE = JS_TRY_CATCH + 1          // js.special.wrapAsThrowable
  inline val UNWRAP_FROM_THROWABLE = WRAP_AS_THROWABLE + 1 // js.special.unwrapFromThrowable
  inline val DEBUGGER = UNWRAP_FROM_THROWABLE + 1          // js.special.debugger

  inline val LINKTIME_IF = DEBUGGER + 1 // LinkingInfo.linkTimeIf

  inline val THROW = LINKTIME_IF + 1 // <special-ops>.throw
  inline val NEW_ARRAY = THROW + 1   // scala.runtime.Arrays.newArray

  inline val UNION_FROM = NEW_ARRAY + 1                   // js.|.from
  inline val UNION_FROM_TYPE_CONSTRUCTOR = UNION_FROM + 1 // js.|.fromTypeConstructor

  inline val REFLECT_SELECTABLE_SELECTDYN = UNION_FROM_TYPE_CONSTRUCTOR + 1 // scala.reflect.Selectable.selectDynamic
  inline val REFLECT_SELECTABLE_APPLYDYN = REFLECT_SELECTABLE_SELECTDYN + 1 // scala.reflect.Selectable.applyDynamic

  inline val LastJSPrimitiveCode = REFLECT_SELECTABLE_APPLYDYN

  def isJSPrimitive(code: Int): Boolean =
    code >= FirstJSPrimitiveCode && code <= LastJSPrimitiveCode

}

class JSPrimitives(ictx: Context) extends DottyPrimitives(ictx) {
  import JSPrimitives.*

  private lazy val jsPrimitives: ReadOnlyMap[Symbol, Int] = initJSPrimitives(using ictx)

  override def getPrimitive(sym: Symbol): Int =
    jsPrimitives.getOrElse(sym, super.getPrimitive(sym))

  override def getPrimitive(app: Apply, tpe: Type): Int =
    jsPrimitives.getOrElse(app.fun.symbol(using ictx), super.getPrimitive(app, tpe))

  override def isPrimitive(sym: Symbol): Boolean =
    jsPrimitives.contains(sym) || super.isPrimitive(sym)

  override def isPrimitive(fun: Tree): Boolean =
    jsPrimitives.contains(fun.symbol(using ictx)) || super.isPrimitive(fun)

  /** Initialize the primitive map */
  private def initJSPrimitives(using Context): ReadOnlyMap[Symbol, Int] = {

    val primitives = MutableSymbolMap[Int]()

    // !!! Code duplicate with DottyPrimitives
    /** Add a primitive operation to the map */
    def addPrimitive(s: Symbol, code: Int): Unit = {
      assert(!primitives.contains(s), "Duplicate primitive " + s)
      primitives(s) = code
    }

    def addPrimitives(cls: Symbol, method: TermName, code: Int)(using Context): Unit = {
      val alts = cls.info.member(method).alternatives.map(_.symbol)
      if (alts.isEmpty) {
        report.error(em"Unknown primitive method $cls.$method")
      } else {
        for (s <- alts)
          addPrimitive(s, code)
      }
    }

    val jsdefn = JSDefinitions.jsdefn

    addPrimitive(jsdefn.JSDynamic_newInstance, DYNNEW)

    addPrimitive(jsdefn.JSArray_apply, ARR_CREATE)

    addPrimitive(jsdefn.JSPackage_typeOf, TYPEOF)
    addPrimitive(jsdefn.JSPackage_native, JS_NATIVE)
    addPrimitive(jsdefn.JSPackage_async, JS_ASYNC)
    addPrimitive(jsdefn.JSPackage_await, JS_AWAIT)

    addPrimitive(defn.BoxedUnit_UNIT, UNITVAL)

    addPrimitive(jsdefn.JSNew_target, JS_NEW_TARGET)

    addPrimitive(jsdefn.JSImport_apply, JS_IMPORT)
    addPrimitive(jsdefn.JSImport_meta, JS_IMPORT_META)

    addPrimitive(jsdefn.Runtime_constructorOf, CONSTRUCTOROF)
    addPrimitive(jsdefn.Runtime_createInnerJSClass, CREATE_INNER_JS_CLASS)
    addPrimitive(jsdefn.Runtime_createLocalJSClass, CREATE_LOCAL_JS_CLASS)
    addPrimitive(jsdefn.Runtime_withContextualJSClassValue, WITH_CONTEXTUAL_JS_CLASS_VALUE)
    addPrimitive(jsdefn.Runtime_dynamicImport, DYNAMIC_IMPORT)

    addPrimitive(jsdefn.Special_strictEquals, STRICT_EQ)
    addPrimitive(jsdefn.Special_in, IN)
    addPrimitive(jsdefn.Special_instanceof, INSTANCEOF)
    addPrimitive(jsdefn.Special_delete, DELETE)
    addPrimitive(jsdefn.Special_forin, FORIN)
    addPrimitive(jsdefn.Special_throw, JS_THROW)
    addPrimitive(jsdefn.Special_tryCatch, JS_TRY_CATCH)
    addPrimitive(jsdefn.Special_wrapAsThrowable, WRAP_AS_THROWABLE)
    addPrimitive(jsdefn.Special_unwrapFromThrowable, UNWRAP_FROM_THROWABLE)
    addPrimitive(jsdefn.Special_debugger, DEBUGGER)

    addPrimitive(jsdefn.LinkingInfo_linkTimeIf, LINKTIME_IF)

    addPrimitive(defn.throwMethod, THROW)
    addPrimitive(defn.newArrayMethod, NEW_ARRAY)

    addPrimitive(jsdefn.PseudoUnion_from, UNION_FROM)
    addPrimitive(jsdefn.PseudoUnion_fromTypeConstructor, UNION_FROM_TYPE_CONSTRUCTOR)

    addPrimitive(jsdefn.ReflectSelectable_selectDynamic, REFLECT_SELECTABLE_SELECTDYN)
    addPrimitive(jsdefn.ReflectSelectable_applyDynamic, REFLECT_SELECTABLE_APPLYDYN)

    primitives
  }

}
