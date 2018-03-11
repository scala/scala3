package dotty.tools.dotc
package core

import scala.language.implicitConversions
import scala.collection.{mutable, immutable}
import scala.annotation.switch
import Names._
import Symbols._
import Contexts._
import Decorators.StringDecorator
import util.NameTransformer
import scala.collection.breakOut

object StdNames {

/** Base strings from which synthetic names are derived. */

  object str {
    final val SETTER_SUFFIX            = "_="
    final val EXPAND_SEPARATOR         = "$$"
    final val TRAIT_SETTER_SEPARATOR   = "$_setter_$"
    final val SUPER_PREFIX             = "super$"
    final val INITIALIZER_PREFIX       = "initial$"
    final val AVOID_CLASH_SUFFIX       = "$_avoid_name_clash_$"
    final val MODULE_SUFFIX            = "$"
    final val NAME_JOIN                = "$"
    final val DEFAULT_GETTER           = "$default$"
    final val LOCALDUMMY_PREFIX        = "<local "       // owner of local blocks
    final val ANON_CLASS               = "$anon"
    final val ANON_FUN                 = "$anonfun"

    final val REPL_SESSION_LINE  = "rs$line$"
    final val REPL_ASSIGN_SUFFIX = "$assign"
    final val REPL_RES_PREFIX    = "res"

    final val MODULE_INSTANCE_FIELD      = "MODULE$"

    final val Function                   = "Function"
    final val ErasedFunction             = "ErasedFunction"
    final val ImplicitFunction           = "ImplicitFunction"
    final val ErasedImplicitFunction     = "ErasedImplicitFunction"
    final val AbstractFunction           = "AbstractFunction"
    final val Tuple                      = "Tuple"
    final val Product                    = "Product"

    def sanitize(str: String) = str.replaceAll("""[<>]""", """\$""")
  }

  abstract class DefinedNames[N <: Name] {
    protected def n(s: String): N
    protected def fromName(name: Name): N = n(name.toString)

    private val kws = mutable.Set[N]()
    protected def kw(s: String) = { val name = n(s); kws += name; name }

    final val keywords: collection.Set[N] = kws
  }

  abstract class ScalaNames[N <: Name] extends DefinedNames[N] {
    protected def encode(s: String): N = fromName(n(s).encode)

// Keywords, need to come first -----------------------

    final val ABSTRACTkw: N  = kw("abstract")
    final val CASEkw: N      = kw("case")
    final val CLASSkw: N     = kw("class")
    final val CATCHkw: N     = kw("catch")
    final val DEFkw: N       = kw("def")
    final val DOkw: N        = kw("do")
    final val ELSEkw: N      = kw("else")
    final val EXTENDSkw: N   = kw("extends")
    final val FALSEkw: N     = kw("false")
    final val FINALkw: N     = kw("final")
    final val FINALLYkw: N   = kw("finally")
    final val FORkw: N       = kw("for")
    final val FORSOMEkw: N   = kw("forSome")
    final val IFkw: N        = kw("if")
    final val IMPLICITkw: N  = kw("implicit")
    final val IMPORTkw: N    = kw("import")
    final val INLINEkw: N    = kw("inline")
    final val LAZYkw: N      = kw("lazy")
    final val MACROkw: N     = kw("macro")
    final val MATCHkw: N     = kw("match")
    final val NEWkw: N       = kw("new")
    final val NULLkw: N      = kw("null")
    final val OBJECTkw: N    = kw("object")
    final val OVERRIDEkw: N  = kw("override")
    final val PACKAGEkw: N   = kw("package")
    final val PRIVATEkw: N   = kw("private")
    final val PROTECTEDkw: N = kw("protected")
    final val RETURNkw: N    = kw("return")
    final val SEALEDkw: N    = kw("sealed")
    final val SUPERkw: N     = kw("super")
    final val THENkw: N      = kw("then")
    final val THISkw: N      = kw("this")
    final val THROWkw: N     = kw("throw")
    final val TRAITkw: N     = kw("trait")
    final val TRUEkw: N      = kw("true")
    final val TRYkw: N       = kw("try")
    final val TYPEkw: N      = kw("type")
    final val VALkw: N       = kw("val")
    final val VARkw: N       = kw("var")
    final val WITHkw: N      = kw("with")
    final val WHILEkw: N     = kw("while")
    final val YIELDkw: N     = kw("yield")
    final val DOTkw: N       = kw(".")
    final val USCOREkw: N    = kw("_")
    final val COLONkw: N     = kw(":")
    final val EQUALSkw: N    = kw("=")
    final val ARROWkw: N     = kw("=>")
    final val LARROWkw: N    = kw("<-")
    final val SUBTYPEkw: N   = kw("<:")
    final val VIEWBOUNDkw: N = kw("<%")
    final val SUPERTYPEkw: N = kw(">:")
    final val HASHkw: N      = kw("#")
    final val ATkw: N        = kw("@")

    val ANON_CLASS: N                 = n(str.ANON_CLASS)
    val ANON_FUN: N                   = n(str.ANON_FUN)
    val BITMAP_PREFIX: N              = n("bitmap$")  // @darkdimius: $bitmap?
    val DEFAULT_GETTER: N             = n(str.DEFAULT_GETTER)
    val DEFAULT_GETTER_INIT: N        = n("$lessinit$greater")
    val DO_WHILE_PREFIX: N            = n("doWhile$")
    val DOLLAR_VALUES: N              = n("$values")
    val DOLLAR_NEW: N                 = n("$new")
    val EMPTY: N                      = n("")
    val EMPTY_PACKAGE: N              = n("<empty>")
    val EXCEPTION_RESULT_PREFIX: N    = n("exceptionResult")
    val EXPAND_SEPARATOR: N           = n(str.EXPAND_SEPARATOR)
    val IMPL_CLASS_SUFFIX: N          = n("$class")
    val IMPORT: N                     = n("<import>")
    val MODULE_SUFFIX: N              = n(str.MODULE_SUFFIX)
    val OPS_PACKAGE: N                = n("<special-ops>")
    val OVERLOADED: N                 = n("<overloaded>")
    val PACKAGE: N                    = n("package")
    val ROOT: N                       = n("<root>")
    val SPECIALIZED_SUFFIX: N         = n("$sp")
    val SUPER_PREFIX: N               = n("super$")
    val WHILE_PREFIX: N               = n("while$")
    val DEFAULT_EXCEPTION_NAME: N     = n("ex$")
    val INITIALIZER_PREFIX: N         = n("initial$")
    val COMPANION_MODULE_METHOD: N    = n("companion$module")
    val COMPANION_CLASS_METHOD: N     = n("companion$class")
    val BOUNDTYPE_ANNOT: N            = n("$boundType$")
    val QUOTE: N                      = n("'")
    val TYPE_QUOTE: N                 = n("type_'")
    val TRAIT_SETTER_SEPARATOR: N     = n(str.TRAIT_SETTER_SEPARATOR)

    // value types (and AnyRef) are all used as terms as well
    // as (at least) arguments to the @specialize annotation.
    final val Boolean: N = n("Boolean")
    final val Byte: N    = n("Byte")
    final val Char: N    = n("Char")
    final val Double: N  = n("Double")
    final val Float: N   = n("Float")
    final val Int: N     = n("Int")
    final val Long: N    = n("Long")
    final val Short: N   = n("Short")
    final val Unit: N    = n("Unit")

    final val ScalaValueNames: scala.List[N] =
      scala.List(Byte, Char, Short, Int, Long, Float, Double, Boolean, Unit)

    // some types whose companions we utilize
    final val AnyRef: N      = n("AnyRef")
    final val Array: N       = n("Array")
    final val List: N        = n("List")
    final val Seq: N         = n("Seq")
    final val Symbol: N      = n("Symbol")
    final val ClassTag: N    = n("ClassTag")
    final val classTag: N    = n("classTag")
    final val WeakTypeTag: N = n("WeakTypeTag")
    final val TypeTag : N    = n("TypeTag")
    final val typeTag: N     = n("typeTag")
    final val Expr: N        = n("Expr")
    final val String: N      = n("String")
    final val Annotation: N  = n("Annotation")

    // fictions we use as both types and terms
    final val ERROR: N    = n("<error>")
    final val NO_NAME: N  = n("<none>")  // formerly NOSYMBOL
    final val WILDCARD: N = n("_")

// ----- Type names -----------------------------------------

    final val BYNAME_PARAM_CLASS: N             = n("<byname>")
    final val EQUALS_PATTERN: N                 = n("<equals>")
    final val LOCAL_CHILD: N                    = n("<local child>")
    final val REPEATED_PARAM_CLASS: N           = n("<repeated>")
    final val WILDCARD_STAR: N                  = n("_*")
    final val REIFY_TREECREATOR_PREFIX: N       = n("$treecreator")
    final val REIFY_TYPECREATOR_PREFIX: N       = n("$typecreator")

    final val Any: N                 = n("Any")
    final val AnyVal: N              = n("AnyVal")
    final val ExprApi: N             = n("ExprApi")
    final val Mirror: N              = n("Mirror")
    final val Nothing: N             = n("Nothing")
    final val Null: N                = n("Null")
    final val Object: N              = n("Object")
    final val PartialFunction: N     = n("PartialFunction")
    final val PrefixType: N          = n("PrefixType")
    final val Serializable: N        = n("Serializable")
    final val Singleton: N           = n("Singleton")
    final val Throwable: N           = n("Throwable")

    final val ClassfileAnnotation: N = n("ClassfileAnnotation")
    final val ClassManifest: N       = n("ClassManifest")
    final val Enum: N                = n("Enum")
    final val Group: N               = n("Group")
    final val Tree: N                = n("Tree")
    final val Type : N               = n("Type")
    final val TypeTree: N            = n("TypeTree")

    // Annotation simple names, used in Namer
    final val BeanPropertyAnnot: N = n("BeanProperty")
    final val BooleanBeanPropertyAnnot: N = n("BooleanBeanProperty")
    final val bridgeAnnot: N = n("bridge")

    // Classfile Attributes
    final val AnnotationDefaultATTR: N      = n("AnnotationDefault")
    final val BridgeATTR: N                 = n("Bridge")
    final val ClassfileAnnotationATTR: N    = n("RuntimeInvisibleAnnotations") // RetentionPolicy.CLASS. Currently not used (Apr 2009).
    final val CodeATTR: N                   = n("Code")
    final val ConstantValueATTR: N          = n("ConstantValue")
    final val DeprecatedATTR: N             = n("Deprecated")
    final val ExceptionsATTR: N             = n("Exceptions")
    final val InnerClassesATTR: N           = n("InnerClasses")
    final val LineNumberTableATTR: N        = n("LineNumberTable")
    final val LocalVariableTableATTR: N     = n("LocalVariableTable")
    final val RuntimeAnnotationATTR: N      = n("RuntimeVisibleAnnotations")   // RetentionPolicy.RUNTIME
    final val RuntimeParamAnnotationATTR: N = n("RuntimeVisibleParameterAnnotations") // RetentionPolicy.RUNTIME (annotations on parameters)
    final val ScalaATTR: N                  = n("Scala")
    final val ScalaSignatureATTR: N         = n("ScalaSig")
    final val TASTYATTR: N                  = n("TASTY")
    final val SignatureATTR: N              = n("Signature")
    final val SourceFileATTR: N             = n("SourceFile")
    final val SyntheticATTR: N              = n("Synthetic")

// ----- Term names -----------------------------------------

    // Compiler-internal
    val ANYname: N                  = n("<anyname>")
    val CONSTRUCTOR: N              = n("<init>")
    val STATIC_CONSTRUCTOR: N       = n("<clinit>")
    val DEFAULT_CASE: N             = n("defaultCase$")
    val EVT2U: N                    = n("evt2u$")
    val EQEQ_LOCAL_VAR: N           = n("eqEqTemp$")
    val FAKE_LOCAL_THIS: N          = n("this$")
    val LAZY_FIELD_OFFSET: N        = n("OFFSET$")
    val LAZY_SLOW_SUFFIX: N         = n("$lzycompute")
    val UNIVERSE_BUILD_PREFIX: N    = n("$u.build.")
    val UNIVERSE_BUILD: N           = n("$u.build")
    val UNIVERSE_PREFIX: N          = n("$u.")
    val UNIVERSE_SHORT: N           = n("$u")
    val MIRROR_PREFIX: N            = n("$m.")
    val MIRROR_SHORT: N             = n("$m")
    val MIRROR_UNTYPED: N           = n("$m$untyped")
    val REIFY_FREE_PREFIX: N        = n("free$")
    val REIFY_FREE_THIS_SUFFIX: N   = n("$this")
    val REIFY_FREE_VALUE_SUFFIX: N  = n("$value")
    val REIFY_SYMDEF_PREFIX: N      = n("symdef$")
    val OUTER: N                    = n("$outer")
    val REFINE_CLASS: N             = n("<refinement>")
    val ROOTPKG: N                  = n("_root_")
    val SELECTOR_DUMMY: N           = n("<unapply-selector>")
    val SELF: N                     = n("$this")
    val SKOLEM: N                   = n("<skolem>")
    val SPECIALIZED_INSTANCE: N     = n("specInstance$")
    val THIS: N                     = n("_$this")
    val TRAIT_CONSTRUCTOR: N        = n("$init$")
    val U2EVT: N                    = n("u2evt$")
    val ALLARGS: N                  = n("$allArgs")

    final val Nil: N                = n("Nil")
    final val Predef: N             = n("Predef")
    final val ScalaRunTime: N       = n("ScalaRunTime")
    final val Some: N               = n("Some")

    val x_0 : N  = n("x$0")
    val x_1 : N  = n("x$1")
    val x_2 : N  = n("x$2")
    val x_3 : N  = n("x$3")
    val x_4 : N  = n("x$4")
    val x_5 : N  = n("x$5")
    val x_6 : N  = n("x$6")
    val x_7 : N  = n("x$7")
    val x_8 : N  = n("x$8")
    val x_9 : N  = n("x$9")
    val _1 : N   = n("_1")
    val _2 : N   = n("_2")
    val _3 : N   = n("_3")
    val _4 : N   = n("_4")
    val _5 : N   = n("_5")
    val _6 : N   = n("_6")
    val _7 : N   = n("_7")
    val _8 : N   = n("_8")
    val _9 : N   = n("_9")
    val _10 : N  = n("_10")
    val _11 : N  = n("_11")
    val _12 : N  = n("_12")
    val _13 : N  = n("_13")
    val _14 : N  = n("_14")
    val _15 : N  = n("_15")
    val _16 : N  = n("_16")
    val _17 : N  = n("_17")
    val _18 : N  = n("_18")
    val _19 : N  = n("_19")
    val _20 : N  = n("_20")
    val _21 : N  = n("_21")
    val _22 : N  = n("_22")

    val ??? : N = n("???")

    val genericWrapArray: N     = n("genericWrapArray")
    def wrapRefArray: N         = n("wrapRefArray")
    def wrapXArray(clsName: Name): N = n("wrap" + clsName + "Array")

    // Compiler utilized names

    val AnnotatedType: N          = n("AnnotatedType")
    val AppliedTypeTree: N        = n("AppliedTypeTree")
    val ArrayAnnotArg: N          = n("ArrayAnnotArg")
    val Constant: N               = n("Constant")
    val ConstantType: N           = n("ConstantType")
    val doubleHash: N             = n("doubleHash")
    val ExistentialTypeTree: N    = n("ExistentialTypeTree")
    val Flag : N                  = n("Flag")
    val floatHash: N              = n("floatHash")
    val Ident: N                  = n("Ident")
    val Import: N                 = n("Import")
    val Literal: N                = n("Literal")
    val LiteralAnnotArg: N        = n("LiteralAnnotArg")
    val longHash: N               = n("longHash")
    val Modifiers: N              = n("Modifiers")
    val NestedAnnotArg: N         = n("NestedAnnotArg")
    val NoFlags: N                = n("NoFlags")
    val NoPrefix: N               = n("NoPrefix")
    val NoSymbol: N               = n("NoSymbol")
    val NoType: N                 = n("NoType")
    val Pair: N                   = n("Pair")
    val Ref: N                    = n("Ref")
    val RootPackage: N            = n("RootPackage")
    val RootClass: N              = n("RootClass")
    val Scala2: N                 = n("Scala2")
    val Select: N                 = n("Select")
    val StringContext: N          = n("StringContext")
    val This: N                   = n("This")
    val ThisType: N               = n("ThisType")
    val Tuple2: N                 = n("Tuple2")
    val TYPE_ : N                 = n("TYPE")
    val TypeApply: N              = n("TypeApply")
    val TypeRef: N                = n("TypeRef")
    val UNIT : N                  = n("UNIT")
    val add_ : N                  = n("add")
    val annotation: N             = n("annotation")
    val anyHash: N                = n("anyHash")
    val anyValClass: N            = n("anyValClass")
    val append: N                 = n("append")
    val apply: N                  = n("apply")
    val applyDynamic: N           = n("applyDynamic")
    val applyDynamicNamed: N      = n("applyDynamicNamed")
    val applyOrElse: N            = n("applyOrElse")
    val args : N                  = n("args")
    val argv : N                  = n("argv")
    val arrayClass: N             = n("arrayClass")
    val arrayElementClass: N      = n("arrayElementClass")
    val arrayValue: N             = n("arrayValue")
    val array_apply : N           = n("array_apply")
    val array_clone : N           = n("array_clone")
    val array_length : N          = n("array_length")
    val array_update : N          = n("array_update")
    val arraycopy: N              = n("arraycopy")
    val asTerm: N                 = n("asTerm")
    val asModule: N               = n("asModule")
    val asMethod: N               = n("asMethod")
    val asType: N                 = n("asType")
    val asClass: N                = n("asClass")
    val asInstanceOf_ : N         = n("asInstanceOf")
    val assert_ : N               = n("assert")
    val assume_ : N               = n("assume")
    val box: N                    = n("box")
    val build : N                 = n("build")
    val bytes: N                  = n("bytes")
    val canEqual_ : N             = n("canEqual")
    val cbnArg: N                 = n("<cbn-arg>")
    val checkInitialized: N       = n("checkInitialized")
    val ClassManifestFactory: N   = n("ClassManifestFactory")
    val classOf: N                = n("classOf")
    val clone_ : N                = n("clone")
    val conforms_ : N             = n("conforms")
    val copy: N                   = n("copy")
    val currentMirror: N          = n("currentMirror")
    val create: N                 = n("create")
    val definitions: N            = n("definitions")
    val delayedInit: N            = n("delayedInit")
    val delayedInitArg: N         = n("delayedInit$body")
    val drop: N                   = n("drop")
    val dynamics: N               = n("dynamics")
    val elem: N                   = n("elem")
    val emptyValDef: N            = n("emptyValDef")
    val ensureAccessible : N      = n("ensureAccessible")
    val enumTag: N                = n("enumTag")
    val eq: N                     = n("eq")
    val eqInstance: N             = n("eqInstance")
    val equalsNumChar : N         = n("equalsNumChar")
    val equalsNumNum : N          = n("equalsNumNum")
    val equalsNumObject : N       = n("equalsNumObject")
    val equals_ : N               = n("equals")
    val error: N                  = n("error")
    val eval: N                   = n("eval")
    val eqAny: N                  = n("eqAny")
    val ex: N                     = n("ex")
    val experimental: N           = n("experimental")
    val f: N                      = n("f")
    val false_ : N                = n("false")
    val filter: N                 = n("filter")
    val finalize_ : N             = n("finalize")
    val find_ : N                 = n("find")
    val flagsFromBits : N         = n("flagsFromBits")
    val flatMap: N                = n("flatMap")
    val foreach: N                = n("foreach")
    val genericArrayOps: N        = n("genericArrayOps")
    val get: N                    = n("get")
    val getClass_ : N             = n("getClass")
    val getOrElse: N              = n("getOrElse")
    val hasNext: N                = n("hasNext")
    val hashCode_ : N             = n("hashCode")
    val hash_ : N                 = n("hash")
    val head: N                   = n("head")
    val higherKinds: N            = n("higherKinds")
    val identity: N               = n("identity")
    val implicitly: N             = n("implicitly")
    val in: N                     = n("in")
    val info: N                   = n("info")
    val inlinedEquals: N          = n("inlinedEquals")
    val isArray: N                = n("isArray")
    val isDefinedAt: N            = n("isDefinedAt")
    val isDefinedAtImpl: N        = n("$isDefinedAt")
    val isDefined: N              = n("isDefined")
    val isEmpty: N                = n("isEmpty")
    val isInstanceOf_ : N         = n("isInstanceOf")
    val isInstanceOfPM: N         = n("$isInstanceOf$")
    val java: N                   = n("java")
    val key: N                    = n("key")
    val lang: N                   = n("lang")
    val length: N                 = n("length")
    val lengthCompare: N          = n("lengthCompare")
    val `macro` : N               = n("macro")
    val macroThis : N             = n("_this")
    val macroContext : N          = n("c")
    val main: N                   = n("main")
    val manifest: N               = n("manifest")
    val ManifestFactory: N        = n("ManifestFactory")
    val manifestToTypeTag: N      = n("manifestToTypeTag")
    val map: N                    = n("map")
    val materializeClassTag: N    = n("materializeClassTag")
    val materializeWeakTypeTag: N = n("materializeWeakTypeTag")
    val materializeTypeTag: N     = n("materializeTypeTag")
    val mirror : N                = n("mirror")
    val moduleClass : N           = n("moduleClass")
    val name: N                   = n("name")
    val ne: N                     = n("ne")
    val newFreeTerm: N            = n("newFreeTerm")
    val newFreeType: N            = n("newFreeType")
    val newScopeWith: N           = n("newScopeWith")
    val next: N                   = n("next")
    val nmeNewTermName: N         = n("newTermName")
    val nmeNewTypeName: N         = n("newTypeName")
    val noAutoTupling: N          = n("noAutoTupling")
    val normalize: N              = n("normalize")
    val notifyAll_ : N            = n("notifyAll")
    val notify_ : N               = n("notify")
    val null_ : N                 = n("null")
    val ofDim: N                  = n("ofDim")
    val origin: N                 = n("origin")
    val prefix : N                = n("prefix")
    val productArity: N           = n("productArity")
    val productElement: N         = n("productElement")
    val productIterator: N        = n("productIterator")
    val productPrefix: N          = n("productPrefix")
    val readResolve: N            = n("readResolve")
    val reflect : N               = n("reflect")
    val reify : N                 = n("reify")
    val rootMirror : N            = n("rootMirror")
    val run: N                    = n("run")
    val runOrElse: N              = n("runOrElse")
    val runtime: N                = n("runtime")
    val runtimeClass: N           = n("runtimeClass")
    val runtimeMirror: N          = n("runtimeMirror")
    val sameElements: N           = n("sameElements")
    val scala_ : N                = n("scala")
    val scalaShadowing : N        = n("scalaShadowing")
    val selectDynamic: N          = n("selectDynamic")
    val selectDynamicMethod: N    = n("selectDynamicMethod")
    val selectOverloadedMethod: N = n("selectOverloadedMethod")
    val selectTerm: N             = n("selectTerm")
    val selectType: N             = n("selectType")
    val self: N                   = n("self")
    val seqToArray: N             = n("seqToArray")
    val setAccessible: N          = n("setAccessible")
    val setAnnotations: N         = n("setAnnotations")
    val setSymbol: N              = n("setSymbol")
    val setType: N                = n("setType")
    val setTypeSignature: N       = n("setTypeSignature")
    val splice: N                 = n("splice")
    val staticClass : N           = n("staticClass")
    val staticModule : N          = n("staticModule")
    val staticPackage : N         = n("staticPackage")
    val strictEquality: N         = n("strictEquality")
    val synchronized_ : N         = n("synchronized")
    val tag: N                    = n("tag")
    val tail: N                   = n("tail")
    val `then` : N                = n("then")
    val this_ : N                 = n("this")
    val thisPrefix : N            = n("thisPrefix")
    val throw_ : N                = n("throw")
    val toArray: N                = n("toArray")
    val toList: N                 = n("toList")
    val toObjectArray : N         = n("toObjectArray")
    val toSeq: N                  = n("toSeq")
    val toString_ : N             = n("toString")
    val toTypeConstructor: N      = n("toTypeConstructor")
    val tpe : N                   = n("tpe")
    val tree : N                  = n("tree")
    val true_ : N                 = n("true")
    val typedProductIterator: N   = n("typedProductIterator")
    val typeTagToManifest: N      = n("typeTagToManifest")
    val unapply: N                = n("unapply")
    val unapplySeq: N             = n("unapplySeq")
    val unbox: N                  = n("unbox")
    val universe: N               = n("universe")
    val update: N                 = n("update")
    val updateDynamic: N          = n("updateDynamic")
    val value: N                  = n("value")
    val valueOf : N               = n("valueOf")
    val values: N                 = n("values")
    val view_ : N                 = n("view")
    val wait_ : N                 = n("wait")
    val withFilter: N             = n("withFilter")
    val withFilterIfRefutable: N  = n("withFilterIfRefutable$")
    val wrap: N                   = n("wrap")
    val zero: N                   = n("zero")
    val zip: N                    = n("zip")
    val nothingRuntimeClass: N    = n("scala.runtime.Nothing$")
    val nullRuntimeClass: N       = n("scala.runtime.Null$")

    val synthSwitch: N            = n("$synthSwitch")
    val _scope: N                 = n("$scope")

    val nothingClass: N           = n("Nothing$")
    val nullClass: N              = n("Null$")

    val falseModuleClassNames = Set(nothingClass, nullClass, nothingRuntimeClass, nullRuntimeClass)

    // unencoded operators
    object raw {
      final val AMP  : N  = n("&")
      final val BANG : N  = n("!")
      final val BAR  : N  = n("|")
      final val DOLLAR: N = n("$")
      final val GE: N     = n(">=")
      final val LE: N     = n("<=")
      final val MINUS: N  = n("-")
      final val NE: N     = n("!=")
      final val PLUS : N  = n("+")
      final val SLASH: N  = n("/")
      final val STAR : N  = n("*")
      final val TILDE: N  = n("~")

      final val isUnary: Set[Name] = Set(MINUS, PLUS, TILDE, BANG)
    }

    object specializedTypeNames {
      final val Boolean: N = n("Z")
      final val Byte: N    = n("B")
      final val Char: N    = n("C")
      final val Short: N   = n("S")
      final val Int: N     = n("I")
      final val Long: N    = n("J")
      final val Float: N   = n("F")
      final val Double: N  = n("D")
      final val Void: N    = n("V")
      final val Object: N  = n("L")

      final val prefix: N = n("$m")
      final val separator: N = n("c")
      final val suffix: N = n("$sp")
    }

    // value-conversion methods
    val toByte: N   = n("toByte")
    val toShort: N  = n("toShort")
    val toChar: N   = n("toChar")
    val toInt: N    = n("toInt")
    val toLong: N   = n("toLong")
    val toFloat: N  = n("toFloat")
    val toDouble: N = n("toDouble")

    // primitive operation methods for structural types mostly
    // overlap with the above, but not for these two.
    val toCharacter: N = n("toCharacter")
    val toInteger: N   = n("toInteger")

    def newLazyValSlowComputeName(lzyValName: N) = lzyValName ++ LAZY_SLOW_SUFFIX

    // ASCII names for operators
    val ADD      : N = n("+")
    val AND      : N = n("&")
    val ASR      : N = n(">>")
    val DIV      : N = n("/")
    val EQ       : N = n("==")
    val EQL      : N = n("=")
    val GE       : N = n(">=")
    val GT       : N = n(">")
    val HASHHASH : N = n("##")
    val LE       : N = n("<=")
    val LSL      : N = n("<<")
    val LSR      : N = n(">>>")
    val LT       : N = n("<")
    val MINUS    : N = n("-")
    val MOD      : N = n("%")
    val MUL      : N = n("*")
    val NE       : N = n("!=")
    val OR       : N = n("|")
    val PLUS     = ADD    // technically redundant, but ADD looks funny with MINUS
    val SUB      = MINUS  // ... as does SUB with PLUS
    val XOR      : N = n("^")
    val ZAND     : N = n("&&")
    val ZOR      : N = n("||")

    // unary operators
    val UNARY_PREFIX: N = n("unary_")
    val UNARY_~ : N = n("unary_~")
    val UNARY_+ : N = n("unary_+")
    val UNARY_- : N = n("unary_-")
    val UNARY_! : N = n("unary_!")

    // Grouped here so Cleanup knows what tests to perform.
    val CommonOpNames   = Set[Name](OR, XOR, AND, EQ, NE)
    val ConversionNames = Set[Name](toByte, toChar, toDouble, toFloat, toInt, toLong, toShort)
    val BooleanOpNames  = Set[Name](ZOR, ZAND, UNARY_!) ++ CommonOpNames
    val NumberOpNames   = (
         Set[Name](ADD, SUB, MUL, DIV, MOD, LSL, LSR, ASR, LT, LE, GE, GT)
      ++ Set(UNARY_+, UNARY_-, UNARY_!)
      ++ ConversionNames
      ++ CommonOpNames
    )

    val add: N                    = n("add")
    val complement: N             = n("complement")
    val divide: N                 = n("divide")
    val multiply: N               = n("multiply")
    val negate: N                 = n("negate")
    val positive: N               = n("positive")
    val shiftLogicalRight: N      = n("shiftLogicalRight")
    val shiftSignedLeft: N        = n("shiftSignedLeft")
    val shiftSignedRight: N       = n("shiftSignedRight")
    val subtract: N               = n("subtract")
    val takeAnd: N                = n("takeAnd")
    val takeConditionalAnd: N     = n("takeConditionalAnd")
    val takeConditionalOr: N      = n("takeConditionalOr")
    val takeModulo: N             = n("takeModulo")
    val takeNot: N                = n("takeNot")
    val takeOr: N                 = n("takeOr")
    val takeXor: N                = n("takeXor")
    val testEqual: N              = n("testEqual")
    val testGreaterOrEqualThan: N = n("testGreaterOrEqualThan")
    val testGreaterThan: N        = n("testGreaterThan")
    val testLessOrEqualThan: N    = n("testLessOrEqualThan")
    val testLessThan: N           = n("testLessThan")
    val testNotEqual: N           = n("testNotEqual")

    val isBoxedNumberOrBoolean: N = n("isBoxedNumberOrBoolean")
    val isBoxedNumber: N = n("isBoxedNumber")
  }

  class ScalaTermNames extends ScalaNames[TermName] {
    protected def n(s: String): TermName = termName(s)

    @switch def syntheticParamName(i: Int): TermName = i match {
      case 0  => x_0
      case 1  => x_1
      case 2  => x_2
      case 3  => x_3
      case 4  => x_4
      case 5  => x_5
      case 6  => x_6
      case 7  => x_7
      case 8  => x_8
      case 9  => x_9
      case _  => termName("x$" + i)
    }

    @switch def productAccessorName(j: Int): TermName = j match {
      case 1  => nme._1
      case 2  => nme._2
      case 3  => nme._3
      case 4  => nme._4
      case 5  => nme._5
      case 6  => nme._6
      case 7  => nme._7
      case 8  => nme._8
      case 9  => nme._9
      case 10 => nme._10
      case 11 => nme._11
      case 12 => nme._12
      case 13 => nme._13
      case 14 => nme._14
      case 15 => nme._15
      case 16 => nme._16
      case 17 => nme._17
      case 18 => nme._18
      case 19 => nme._19
      case 20 => nme._20
      case 21 => nme._21
      case 22 => nme._22
      case _  => termName("_" + j)
    }

    def localDummyName(clazz: Symbol)(implicit ctx: Context): TermName =
      termName(str.LOCALDUMMY_PREFIX + clazz.name + ">")

    def newBitmapName(bitmapPrefix: TermName, n: Int): TermName = bitmapPrefix ++ n.toString

    def selectorName(x: Int): TermName = n("_" + (x + 1))

    object primitive {
      val arrayApply: TermName  = n("[]apply")
      val arrayUpdate: TermName = n("[]update")
      val arrayLength: TermName = n("[]length")
      val names: Set[Name] = Set(arrayApply, arrayUpdate, arrayLength)
    }

    def isPrimitiveName(name: Name) = primitive.names.contains(name)
  }

  class ScalaTypeNames extends ScalaNames[TypeName] {
    protected def n(s: String): TypeName = typeName(s)

    def syntheticTypeParamName(i: Int): TypeName = n("X" + i)

    final val Conforms = encode("<:<")

    final val Uninstantiated: TypeName = n("?$")
  }

  abstract class JavaNames[N <: Name] extends DefinedNames[N] {
    final val ABSTRACTkw: N     = kw("abstract")
    final val ASSERTkw: N       = kw("assert")
    final val BOOLEANkw: N      = kw("boolean")
    final val BREAKkw: N        = kw("break")
    final val BYTEkw: N         = kw("byte")
    final val CASEkw: N         = kw("case")
    final val CATCHkw: N        = kw("catch")
    final val CHARkw: N         = kw("char")
    final val CLASSkw: N        = kw("class")
    final val CONSTkw: N        = kw("const")
    final val CONTINUEkw: N     = kw("continue")
    final val DEFAULTkw: N      = kw("default")
    final val DOkw: N           = kw("do")
    final val DOUBLEkw: N       = kw("double")
    final val ELSEkw: N         = kw("else")
    final val ENUMkw: N         = kw("enum")
    final val EXTENDSkw: N      = kw("extends")
    final val FINALkw: N        = kw("final")
    final val FINALLYkw: N      = kw("finally")
    final val FLOATkw: N        = kw("float")
    final val FORkw: N          = kw("for")
    final val IFkw: N           = kw("if")
    final val GOTOkw: N         = kw("goto")
    final val IMPLEMENTSkw: N   = kw("implements")
    final val IMPORTkw: N       = kw("import")
    final val INSTANCEOFkw: N   = kw("instanceof")
    final val INTkw: N          = kw("int")
    final val INTERFACEkw: N    = kw("interface")
    final val LONGkw: N         = kw("long")
    final val NATIVEkw: N       = kw("native")
    final val NEWkw: N          = kw("new")
    final val PACKAGEkw: N      = kw("package")
    final val PRIVATEkw: N      = kw("private")
    final val PROTECTEDkw: N    = kw("protected")
    final val PUBLICkw: N       = kw("public")
    final val RETURNkw: N       = kw("return")
    final val SHORTkw: N        = kw("short")
    final val STATICkw: N       = kw("static")
    final val STRICTFPkw: N     = kw("strictfp")
    final val SUPERkw: N        = kw("super")
    final val SWITCHkw: N       = kw("switch")
    final val SYNCHRONIZEDkw: N = kw("synchronized")
    final val THISkw: N         = kw("this")
    final val THROWkw: N        = kw("throw")
    final val THROWSkw: N       = kw("throws")
    final val TRANSIENTkw: N    = kw("transient")
    final val TRYkw: N          = kw("try")
    final val VOIDkw: N         = kw("void")
    final val VOLATILEkw: N     = kw("volatile")
    final val WHILEkw: N        = kw("while")

    final val BoxedBoolean: N       = n("java.lang.Boolean")
    final val BoxedByte: N          = n("java.lang.Byte")
    final val BoxedCharacter: N     = n("java.lang.Character")
    final val BoxedDouble: N        = n("java.lang.Double")
    final val BoxedFloat: N         = n("java.lang.Float")
    final val BoxedInteger: N       = n("java.lang.Integer")
    final val BoxedLong: N          = n("java.lang.Long")
    final val BoxedNumber: N        = n("java.lang.Number")
    final val BoxedShort: N         = n("java.lang.Short")
    final val Class: N              = n("java.lang.Class")
    final val IOOBException: N      = n("java.lang.IndexOutOfBoundsException")
    final val InvTargetException: N = n("java.lang.reflect.InvocationTargetException")
    final val MethodAsObject: N     = n("java.lang.reflect.Method")
    final val NPException: N        = n("java.lang.NullPointerException")
    final val Object: N             = n("java.lang.Object")
    final val String: N             = n("java.lang.String")
    final val Throwable: N          = n("java.lang.Throwable")

    final val ForName: N          = n("forName")
    final val GetCause: N         = n("getCause")
    final val GetClass: N         = n("getClass")
    final val GetClassLoader: N   = n("getClassLoader")
    final val GetComponentType: N = n("getComponentType")
    final val GetMethod: N        = n("getMethod")
    final val Invoke: N           = n("invoke")
    final val JavaLang: N         = n("java.lang")

    final val BeanProperty: N        = n("scala.beans.BeanProperty")
    final val BooleanBeanProperty: N = n("scala.beans.BooleanBeanProperty")
    final val JavaSerializable: N    = n("java.io.Serializable")
   }

  class JavaTermNames extends JavaNames[TermName] {
    protected def n(s: String): TermName = termName(s)
  }
  class JavaTypeNames extends JavaNames[TypeName] {
    protected def n(s: String): TypeName = typeName(s)
  }

  val nme = new ScalaTermNames
  val tpnme = new ScalaTypeNames
  val jnme = new JavaTermNames
  val jtpnme = new JavaTypeNames
}
