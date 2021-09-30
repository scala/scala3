package dotty.tools.backend.sjs

import dotty.tools.dotc.core._
import Names.TermName
import StdNames._
import Types._
import Contexts._
import Symbols._

import dotty.tools.dotc.ast.tpd._
import dotty.tools.backend.jvm.DottyPrimitives
import dotty.tools.dotc.report
import dotty.tools.dotc.util.ReadOnlyMap

import scala.collection.mutable

object JSPrimitives {

  final val FirstJSPrimitiveCode = 300

  final val DYNNEW = FirstJSPrimitiveCode + 1 // Instantiate a new JavaScript object

  final val ARR_CREATE = DYNNEW + 1 // js.Array.apply (array literal syntax)

  final val TYPEOF = ARR_CREATE + 1 // typeof x
  final val JS_NATIVE = TYPEOF + 1  // js.native. Marker method. Fails if tried to be emitted.

  final val UNITVAL = JS_NATIVE + 1 // () value, which is undefined

  final val JS_IMPORT = UNITVAL + 1        // js.import.apply(specifier)
  final val JS_IMPORT_META = JS_IMPORT + 1 // js.import.meta

  final val CONSTRUCTOROF = JS_IMPORT_META + 1                         // runtime.constructorOf(clazz)
  final val CREATE_INNER_JS_CLASS = CONSTRUCTOROF + 1                  // runtime.createInnerJSClass
  final val CREATE_LOCAL_JS_CLASS = CREATE_INNER_JS_CLASS + 1          // runtime.createLocalJSClass
  final val WITH_CONTEXTUAL_JS_CLASS_VALUE = CREATE_LOCAL_JS_CLASS + 1 // runtime.withContextualJSClassValue
  final val LINKING_INFO = WITH_CONTEXTUAL_JS_CLASS_VALUE + 1          // runtime.linkingInfo

  final val STRICT_EQ = LINKING_INFO + 1 // js.special.strictEquals
  final val IN = STRICT_EQ + 1           // js.special.in
  final val INSTANCEOF = IN + 1          // js.special.instanceof
  final val DELETE = INSTANCEOF + 1      // js.special.delete
  final val FORIN = DELETE + 1           // js.special.forin
  final val DEBUGGER = FORIN + 1         // js.special.debugger

  final val THROW = DEBUGGER + 1

  final val UNION_FROM = THROW + 1                       // js.|.from
  final val UNION_FROM_TYPE_CONSTRUCTOR = UNION_FROM + 1 // js.|.fromTypeConstructor

  final val REFLECT_SELECTABLE_SELECTDYN = UNION_FROM_TYPE_CONSTRUCTOR + 1 // scala.reflect.Selectable.selectDynamic
  final val REFLECT_SELECTABLE_APPLYDYN = REFLECT_SELECTABLE_SELECTDYN + 1 // scala.reflect.Selectable.applyDynamic

  final val LastJSPrimitiveCode = REFLECT_SELECTABLE_APPLYDYN

  def isJSPrimitive(code: Int): Boolean =
    code >= FirstJSPrimitiveCode && code <= LastJSPrimitiveCode

}

class JSPrimitives(ictx: Context) extends DottyPrimitives(ictx) {
  import JSPrimitives._
  import dotty.tools.backend.ScalaPrimitivesOps._

  private lazy val jsPrimitives: ReadOnlyMap[Symbol, Int] = initJSPrimitives(using ictx)

  override def getPrimitive(sym: Symbol): Int =
    jsPrimitives.getOrElse(sym, super.getPrimitive(sym))

  override def getPrimitive(app: Apply, tpe: Type)(using Context): Int =
    jsPrimitives.getOrElse(app.fun.symbol, super.getPrimitive(app, tpe))

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
      assert(!(primitives contains s), "Duplicate primitive " + s)
      primitives(s) = code
    }

    def addPrimitives(cls: Symbol, method: TermName, code: Int)(using Context): Unit = {
      val alts = cls.info.member(method).alternatives.map(_.symbol)
      if (alts.isEmpty) {
        report.error(s"Unknown primitive method $cls.$method")
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

    addPrimitive(defn.BoxedUnit_UNIT, UNITVAL)

    addPrimitive(jsdefn.JSImport_apply, JS_IMPORT)
    addPrimitive(jsdefn.JSImport_meta, JS_IMPORT_META)

    addPrimitive(jsdefn.Runtime_constructorOf, CONSTRUCTOROF)
    addPrimitive(jsdefn.Runtime_createInnerJSClass, CREATE_INNER_JS_CLASS)
    addPrimitive(jsdefn.Runtime_createLocalJSClass, CREATE_LOCAL_JS_CLASS)
    addPrimitive(jsdefn.Runtime_withContextualJSClassValue, WITH_CONTEXTUAL_JS_CLASS_VALUE)
    addPrimitive(jsdefn.Runtime_linkingInfo, LINKING_INFO)

    addPrimitive(jsdefn.Special_strictEquals, STRICT_EQ)
    addPrimitive(jsdefn.Special_in, IN)
    addPrimitive(jsdefn.Special_instanceof, INSTANCEOF)
    addPrimitive(jsdefn.Special_delete, DELETE)
    addPrimitive(jsdefn.Special_forin, FORIN)
    addPrimitive(jsdefn.Special_debugger, DEBUGGER)

    addPrimitive(defn.throwMethod, THROW)

    addPrimitive(jsdefn.PseudoUnion_from, UNION_FROM)
    addPrimitive(jsdefn.PseudoUnion_fromTypeConstructor, UNION_FROM_TYPE_CONSTRUCTOR)

    addPrimitive(jsdefn.ReflectSelectable_selectDynamic, REFLECT_SELECTABLE_SELECTDYN)
    addPrimitive(jsdefn.ReflectSelectable_applyDynamic, REFLECT_SELECTABLE_APPLYDYN)

    primitives
  }

}
