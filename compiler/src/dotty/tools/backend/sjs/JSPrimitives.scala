package dotty.tools.backend.sjs

import dotty.tools.dotc.core._
import Names.TermName
import StdNames._
import Types._
import Contexts._
import Symbols._

import dotty.tools.dotc.ast.tpd._
import dotty.tools.dotc.backend.jvm.DottyPrimitives

import scala.collection.mutable

object JSPrimitives {

  final val GETCLASS = 301 // jl.Object.getClass()

  final val F2JS = 302     // js.Any.fromFunctionN
  final val F2JSTHIS = 303 // js.ThisFunction.fromFunctionN

  final val DYNNEW = 304     // js.Dynamic.newInstance
  final val DYNLIT = 305     // js.Dynamic.literal.applyDynamic{,Named}
  final val DICT_DEL = 306   // js.Dictionary.delete
  final val ARR_CREATE = 307 // js.Array.apply (array literal syntax)

  final val TYPEOF = 308    // js.typeOf(x)
  final val DEBUGGER = 309  // js.debugger()
  final val HASPROP = 310   // js.Object.hasProperty(o, p), equiv to `p in o` in JS
  final val OBJPROPS = 311  // js.Object.properties(o), equiv to `for (p in o)` in JS
  final val JS_NATIVE = 312 // js.native. Marker method. Fails if tried to be emitted.

  final val UNITVAL = 313  // () value, which is undefined
  final val UNITTYPE = 314 // BoxedUnit.TYPE (== classOf[Unit])

  final val CONSTRUCTOROF = 315 // runtime.constructorOf(clazz)
  final val ENV_INFO = 316      // runtime.environmentInfo
  final val LINKING_INFO = 317  // runtime.linkingInfo

  final val THROW = 318 // <special-ops>.throw

}

class JSPrimitives(ctx: Context) extends DottyPrimitives(ctx) {
  import JSPrimitives._
  import scala.tools.nsc.backend.ScalaPrimitives._

  private lazy val jsPrimitives: Map[Symbol, Int] = initJSPrimitives(ctx)

  override def getPrimitive(sym: Symbol): Int =
    jsPrimitives.getOrElse(sym, super.getPrimitive(sym))

  override def getPrimitive(app: Apply, tpe: Type)(implicit ctx: Context): Int =
    jsPrimitives.getOrElse(app.fun.symbol, super.getPrimitive(app, tpe))

  override def isPrimitive(fun: Tree): Boolean =
    jsPrimitives.contains(fun.symbol(ctx)) || super.isPrimitive(fun)

  /** Initialize the primitive map */
  private def initJSPrimitives(implicit ctx: Context): Map[Symbol, Int] = {

    val primitives = new mutable.HashMap[Symbol, Int]()

    // !!! Code duplicate with DottyPrimitives
    /** Add a primitive operation to the map */
    def addPrimitive(s: Symbol, code: Int): Unit = {
      assert(!(primitives contains s), "Duplicate primitive " + s)
      primitives(s) = code
    }

    def addPrimitives(cls: Symbol, method: TermName, code: Int)(implicit ctx: Context): Unit = {
      val alts = cls.info.member(method).alternatives.map(_.symbol)
      if (alts.isEmpty) {
        ctx.error(s"Unknown primitive method $cls.$method")
      } else {
        for (s <- alts)
          addPrimitive(s, code)
      }
    }

    val jsdefn = JSDefinitions.jsdefn

    addPrimitive(defn.Any_getClass, GETCLASS)

    for (i <- 0 to 22)
      addPrimitive(jsdefn.JSAny_fromFunction(i), F2JS)
    for (i <- 1 to 22)
      addPrimitive(jsdefn.JSThisFunction_fromFunction(i), F2JSTHIS)

    addPrimitive(jsdefn.JSDynamic_newInstance, DYNNEW)

    addPrimitive(jsdefn.JSDynamicLiteral_applyDynamicNamed, DYNLIT)
    addPrimitive(jsdefn.JSDynamicLiteral_applyDynamic, DYNLIT)

    addPrimitive(jsdefn.JSDictionary_delete, DICT_DEL)

    //addPrimitive(jsdefn.JSArray_create, ARR_CREATE)

    addPrimitive(jsdefn.JSPackage_typeOf, TYPEOF)
    addPrimitive(jsdefn.JSPackage_debugger, DEBUGGER)
    addPrimitive(jsdefn.JSPackage_native, JS_NATIVE)

    addPrimitive(jsdefn.JSObject_hasProperty, HASPROP)
    addPrimitive(jsdefn.JSObject_properties, OBJPROPS)

    addPrimitive(defn.BoxedUnit_UNIT, UNITVAL)
    //addPrimitive(defn.BoxedUnit_TYPE, UNITTYPE)

    //addPrimitive(jsdefn.Runtime_constructorOf, CONSTRUCTOROF)
    //addPrimitive(jsdefn.Runtime_environmentInfo, ENV_INFO)
    //addPrimitive(jsdefn.Runtime_linkingInfo, LINKING_INFO)

    addPrimitive(defn.throwMethod, THROW)

    primitives.toMap
  }

}
