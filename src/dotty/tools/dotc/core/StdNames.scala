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

object StdNames {

/** Base strings from which synthetic names are derived. */

  sealed abstract class DefinedNames[N <: Name] {
    protected implicit def fromString(s: String): N

    private val kws = mutable.Set[N]()
    protected final def kw(name: N) = { kws += name; name }

    final val keywords: collection.Set[N] = kws
  }

  sealed abstract class ScalaNames[N <: Name] extends DefinedNames[N] {
    private def encode(s: String): N = fromString(NameTransformer.encode(s))

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

    final val ANON_CLASS: N            = "$anon"
    final val ANON_FUN: N              = "$anonfun"
    final val BITMAP_PREFIX: N              = "bitmap$"
    final val BITMAP_NORMAL: N              = BITMAP_PREFIX         // initialization bitmap for public/protected lazy vals
    final val BITMAP_TRANSIENT: N           = BITMAP_PREFIX + "trans$"    // initialization bitmap for transient lazy vals
    final val BITMAP_CHECKINIT: N           = BITMAP_PREFIX + "init$"      // initialization bitmap for checkinit values
    final val BITMAP_CHECKINIT_TRANSIENT: N = BITMAP_PREFIX + "inittrans$" // initialization bitmap for transient checkinit values
    final val DEFAULT_GETTER: N             = "$default$"
    final val DEFAULT_GETTER_INIT: N        = encode("<init>")
    final val DO_WHILE_PREFIX: N            = "doWhile$"
    final val EMPTY: N                      = ""
    final val EMPTY_PACKAGE: N              = "<empty>"
    final val EVIDENCE_PARAM_PREFIX: N      = "evidence$"
    final val EXCEPTION_RESULT_PREFIX: N    = "exceptionResult"
    final val EXPAND_SEPARATOR: N           = "$$"
    final val IMPL_CLASS_SUFFIX: N          = "$class"
    final val IMPORT: N                     = "<import>"
    final val INTERPRETER_IMPORT_WRAPPER: N = "$iw"
    final val INTERPRETER_LINE_PREFIX: N    = "line"
    final val INTERPRETER_VAR_PREFIX: N     = "res"
    final val INTERPRETER_WRAPPER_SUFFIX: N = "$object"
    final val LOCALDUMMY_PREFIX: N          = "<local "       // owner of local blocks
    final val MODULE_SUFFIX: N              = NameTransformer.MODULE_SUFFIX_STRING
    final val MODULE_VAR_SUFFIX: N          = "$module"
    final val NAME_JOIN: N                  = NameTransformer.NAME_JOIN_STRING
    final val PACKAGE: N                    = "package"
    final val PROTECTED_PREFIX: N           = "protected$"
    final val PROTECTED_SET_PREFIX: N       = PROTECTED_PREFIX + "set"
    final val ROOT: N                       = "<root>"
    final val SINGLETON_SUFFIX: N           = ".type"
    final val SPECIALIZED_SUFFIX: N         = "$sp"
    final val SUPER_PREFIX: N               = "super$"
    final val TRAIT_SETTER_SEPARATOR: N     = "$_setter_$"
    final val WHILE_PREFIX: N               = "while$"

    // value types (and AnyRef) are all used as terms as well
    // as (at least) arguments to the @specialize annotation.
    final val Boolean: N = "Boolean"
    final val Byte: N    = "Byte"
    final val Char: N    = "Char"
    final val Double: N  = "Double"
    final val Float: N   = "Float"
    final val Int: N     = "Int"
    final val Long: N    = "Long"
    final val Short: N   = "Short"
    final val Unit: N    = "Unit"

    final val ScalaValueNames: scala.List[N] =
      scala.List(Byte, Char, Short, Int, Long, Float, Double, Boolean, Unit)

    // some types whose companions we utilize
    final val AnyRef: N     = "AnyRef"
    final val Array: N      = "Array"
    final val List: N       = "List"
    final val Seq: N        = "Seq"
    final val Symbol: N     = "Symbol"
    final val ClassTag: N   = "ClassTag"
    final val WeakTypeTag: N = "WeakTypeTag"
    final val TypeTag : N   = "TypeTag"
    final val Expr: N       = "Expr"
    final val String: N     = "String"
    final val Annotation: N = "Annotation"

    // fictions we use as both types and terms
    final val ERROR: N    = "<error>"
    final val NO_NAME: N  = "<none>"  // formerly NOSYMBOL
    final val WILDCARD: N = "_"

// ----- Type names -----------------------------------------

    final val BYNAME_PARAM_CLASS: N             = "<byname>"
    final val EQUALS_PATTERN: N                 = "<equals>"
    final val JAVA_REPEATED_PARAM_CLASS: N      = "<repeated...>"
    final val LOCAL_CHILD: N                    = "<local child>"
    final val REPEATED_PARAM_CLASS: N           = "<repeated>"
    final val WILDCARD_STAR: N                  = "_*"
    final val REIFY_TREECREATOR_PREFIX: N       = "$treecreator"
    final val REIFY_TYPECREATOR_PREFIX: N       = "$typecreator"

    final val Any: N             = "Any"
    final val AnyVal: N          = "AnyVal"
    final val ExprApi: N         = "ExprApi"
    final val Mirror: N          = "Mirror"
    final val Nothing: N         = "Nothing"
    final val Null: N            = "Null"
    final val Object: N          = "Object"
    final val PartialFunction: N = "PartialFunction"
    final val PrefixType: N      = "PrefixType"
    final val Product: N         = "Product"
    final val Serializable: N    = "Serializable"
    final val Singleton: N       = "Singleton"
    final val Throwable: N       = "Throwable"

    final val ClassfileAnnotation: N = "ClassfileAnnotation"
    final val ClassManifest: N       = "ClassManifest"
    final val Enum: N                = "Enum"
    final val Group: N               = "Group"
    final val Tree: N                = "Tree"
    final val Type : N               = "Type"
    final val TypeTree: N            = "TypeTree"

    // Annotation simple names, used in Namer
    final val BeanPropertyAnnot: N = "BeanProperty"
    final val BooleanBeanPropertyAnnot: N = "BooleanBeanProperty"
    final val bridgeAnnot: N = "bridge"

    // Classfile Attributes
    final val AnnotationDefaultATTR: N      = "AnnotationDefault"
    final val BridgeATTR: N                 = "Bridge"
    final val ClassfileAnnotationATTR: N    = "RuntimeInvisibleAnnotations" // RetentionPolicy.CLASS. Currently not used (Apr 2009).
    final val CodeATTR: N                   = "Code"
    final val ConstantValueATTR: N          = "ConstantValue"
    final val DeprecatedATTR: N             = "Deprecated"
    final val ExceptionsATTR: N             = "Exceptions"
    final val InnerClassesATTR: N           = "InnerClasses"
    final val LineNumberTableATTR: N        = "LineNumberTable"
    final val LocalVariableTableATTR: N     = "LocalVariableTable"
    final val RuntimeAnnotationATTR: N      = "RuntimeVisibleAnnotations"   // RetentionPolicy.RUNTIME
    final val RuntimeParamAnnotationATTR: N = "RuntimeVisibleParameterAnnotations" // RetentionPolicy.RUNTIME (annotations on parameters)
    final val ScalaATTR: N                  = "Scala"
    final val ScalaSignatureATTR: N         = "ScalaSig"
    final val SignatureATTR: N              = "Signature"
    final val SourceFileATTR: N             = "SourceFile"
    final val SyntheticATTR: N              = "Synthetic"

// ----- Term names -----------------------------------------

    // Compiler-internal
    final val ANYname: N                  = "<anyname>"
    final val CONSTRUCTOR: N              = "<init>"
    final val EQEQ_LOCAL_VAR: N           = "eqEqTemp$"
    final val FAKE_LOCAL_THIS: N          = "this$"
    final val INITIALIZER: N              = CONSTRUCTOR // Is this buying us something?
    final val LAZY_LOCAL: N               = "$lzy"
    final val LAZY_SLOW_SUFFIX: N         = "$lzycompute"
    final val LOCAL_SUFFIX: N             = " "
    final val UNIVERSE_BUILD_PREFIX: N    = "$u.build."
    final val UNIVERSE_BUILD: N           = "$u.build"
    final val UNIVERSE_PREFIX: N          = "$u."
    final val UNIVERSE_SHORT: N           = "$u"
    final val MIRROR_PREFIX: N            = "$m."
    final val MIRROR_SHORT: N             = "$m"
    final val MIRROR_UNTYPED: N           = "$m$untyped"
    final val REIFY_FREE_PREFIX: N        = "free$"
    final val REIFY_FREE_THIS_SUFFIX: N   = "$this"
    final val REIFY_FREE_VALUE_SUFFIX: N  = "$value"
    final val REIFY_SYMDEF_PREFIX: N      = "symdef$"
    final val TRAIT_CONSTRUCTOR: N        = "$init$"
    final val MODULE_INSTANCE_FIELD: N    = NameTransformer.MODULE_INSTANCE_NAME  // "MODULE$"
    final val OUTER: N                    = "$outer"
    final val OUTER_LOCAL: N              = "$outer "
    final val OUTER_SYNTH: N              = "<outer>" // emitted by virtual pattern matcher, replaced by outer accessor in explicitouter
    final val REFINE_CLASS: N             = "<refinement>"
    final val ROOTPKG: N                  = "_root_"
    final val SELECTOR_DUMMY: N           = "<unapply-selector>"
    final val SELF: N                     = "$this"
    final val SETTER_SUFFIX: N            = encode("_=")
    final val SPECIALIZED_INSTANCE: N     = "specInstance$"
    final val STAR: N                     = "*"
    final val THIS: N                     = "_$this"

    final val Nil: N                = "Nil"
    final val Predef: N             = "Predef"
    final val ScalaRunTime: N       = "ScalaRunTime"
    final val Some: N               = "Some"

    final val x_0 : N  = "x$0"
    final val x_1 : N  = "x$1"
    final val x_2 : N  = "x$2"
    final val x_3 : N  = "x$3"
    final val x_4 : N  = "x$4"
    final val x_5 : N  = "x$5"
    final val x_6 : N  = "x$6"
    final val x_7 : N  = "x$7"
    final val x_8 : N  = "x$8"
    final val x_9 : N  = "x$9"

    final val ??? = encode("???")

    final val genericWrapArray: N     = "genericWrapArray"
    final def wrapRefArray: N         = "wrapRefArray"
    final def wrapXArray(clsName: Name): N = "wrap" + clsName + "Array"

    // Compiler utilized names

    final val AnnotatedType: N        = "AnnotatedType"
    final val AppliedTypeTree: N      = "AppliedTypeTree"
    final val Apply: N                = "Apply"
    final val ArrayAnnotArg: N        = "ArrayAnnotArg"
    final val Constant: N             = "Constant"
    final val ConstantType: N         = "ConstantType"
    final val EmptyPackage: N         = "EmptyPackage"
    final val EmptyPackageClass: N    = "EmptyPackageClass"
    final val ExistentialTypeTree: N  = "ExistentialTypeTree"
    final val Flag : N                = "Flag"
    final val Ident: N                = "Ident"
    final val Import: N               = "Import"
    final val Literal: N              = "Literal"
    final val LiteralAnnotArg: N      = "LiteralAnnotArg"
    final val Modifiers: N            = "Modifiers"
    final val NestedAnnotArg: N       = "NestedAnnotArg"
    final val NoFlags: N              = "NoFlags"
    final val NoPrefix: N             = "NoPrefix"
    final val NoSymbol: N             = "NoSymbol"
    final val NoType: N               = "NoType"
    final val Ref: N                  = "Ref"
    final val RootPackage: N          = "RootPackage"
    final val RootClass: N            = "RootClass"
    final val Select: N               = "Select"
    final val StringContext: N        = "StringContext"
    final val This: N                 = "This"
    final val ThisType: N             = "ThisType"
    final val Tuple2: N               = "Tuple2"
    final val TYPE_ : N               = "TYPE"
    final val TypeApply: N            = "TypeApply"
    final val TypeRef: N              = "TypeRef"
    final val UNIT : N                = "UNIT"
    final val add_ : N                = "add"
    final val annotation: N           = "annotation"
    final val anyValClass: N          = "anyValClass"
    final val append: N               = "append"
    final val apply: N                = "apply"
    final val applyDynamic: N         = "applyDynamic"
    final val applyDynamicNamed: N    = "applyDynamicNamed"
    final val applyOrElse: N          = "applyOrElse"
    final val args : N                = "args"
    final val argv : N                = "argv"
    final val arrayClass: N           = "arrayClass"
    final val arrayElementClass: N    = "arrayElementClass"
    final val arrayValue: N           = "arrayValue"
    final val array_apply : N         = "array_apply"
    final val array_clone : N         = "array_clone"
    final val array_length : N        = "array_length"
    final val array_update : N        = "array_update"
    final val arraycopy: N            = "arraycopy"
    final val asTerm: N               = "asTerm"
    final val asModule: N             = "asModule"
    final val asMethod: N             = "asMethod"
    final val asType: N               = "asType"
    final val asClass: N              = "asClass"
    final val asInstanceOf_ : N       = "asInstanceOf"
    final val asInstanceOf_Ob : N     = "$asInstanceOf"
    final val assert_ : N             = "assert"
    final val assume_ : N             = "assume"
    final val box: N                  = "box"
    final val build : N               = "build"
    final val bytes: N                = "bytes"
    final val canEqual_ : N           = "canEqual"
    final val checkInitialized: N     = "checkInitialized"
    final val ClassManifestFactory: N = "ClassManifestFactory"
    final val classOf: N              = "classOf"
    final val clone_ : N              = "clone"
    final val conforms: N             = "conforms"
    final val copy: N                 = "copy"
    final val currentMirror: N        = "currentMirror"
    final val definitions: N          = "definitions"
    final val delayedInit: N          = "delayedInit"
    final val delayedInitArg: N       = "delayedInit$body"
    final val drop: N                 = "drop"
    final val elem: N                 = "elem"
    final val emptyValDef: N          = "emptyValDef"
    final val ensureAccessible : N    = "ensureAccessible"
    final val eq: N                   = "eq"
    final val equalsNumChar : N       = "equalsNumChar"
    final val equalsNumNum : N        = "equalsNumNum"
    final val equalsNumObject : N     = "equalsNumObject"
    final val equals_ : N             = "equals"
    final val error: N                = "error"
    final val eval: N                 = "eval"
    final val ex: N                   = "ex"
    final val experimental: N         = "experimental"
    final val f: N                    = "f"
    final val false_ : N              = "false"
    final val filter: N               = "filter"
    final val finalize_ : N           = "finalize"
    final val find_ : N               = "find"
    final val flagsFromBits : N       = "flagsFromBits"
    final val flatMap: N              = "flatMap"
    final val foreach: N              = "foreach"
    final val genericArrayOps: N      = "genericArrayOps"
    final val get: N                  = "get"
    final val getOrElse: N            = "getOrElse"
    final val hasNext: N              = "hasNext"
    final val hashCode_ : N           = "hashCode"
    final val hash_ : N               = "hash"
    final val head: N                 = "head"
    final val identity: N             = "identity"
    final val implicitly: N           = "implicitly"
    final val in: N                   = "in"
    final val info: N                 = "info"
    final val inlinedEquals: N        = "inlinedEquals"
    final val isArray: N              = "isArray"
    final val isDefinedAt: N          = "isDefinedAt"
    final val isEmpty: N              = "isEmpty"
    final val isInstanceOf_ : N       = "isInstanceOf"
    final val isInstanceOf_Ob : N     = "$isInstanceOf"
    final val java: N                 = "java"
    final val key: N                  = "key"
    final val lang: N                 = "lang"
    final val length: N               = "length"
    final val lengthCompare: N        = "lengthCompare"
    final val liftedTree: N           = "liftedTree"
    final val `macro` : N             = "macro"
    final val macroThis : N           = "_this"
    final val macroContext : N        = "c"
    final val main: N                 = "main"
    final val manifest: N             = "manifest"
    final val ManifestFactory: N      = "ManifestFactory"
    final val manifestToTypeTag: N    = "manifestToTypeTag"
    final val map: N                  = "map"
    final val materializeClassTag: N  = "materializeClassTag"
    final val materializeWeakTypeTag: N = "materializeWeakTypeTag"
    final val materializeTypeTag: N   = "materializeTypeTag"
    final val mirror : N              = "mirror"
    final val moduleClass : N         = "moduleClass"
    final val name: N                 = "name"
    final val ne: N                   = "ne"
    final val newArray: N             = "newArray"
    final val newFreeTerm: N          = "newFreeTerm"
    final val newFreeType: N          = "newFreeType"
    final val newNestedSymbol: N      = "newNestedSymbol"
    final val newScopeWith: N         = "newScopeWith"
    final val next: N                 = "next"
    final val nmeNewTermName: N       = "newTermName"
    final val nmeNewTypeName: N       = "newTypeName"
    final val normalize: N            = "normalize"
    final val notifyAll_ : N          = "notifyAll"
    final val notify_ : N             = "notify"
    final val null_ : N               = "null"
    final val ofDim: N                = "ofDim"
    final val origin: N               = "origin"
    final val prefix : N              = "prefix"
    final val productArity: N         = "productArity"
    final val productElement: N       = "productElement"
    final val productIterator: N      = "productIterator"
    final val productPrefix: N        = "productPrefix"
    final val readResolve: N          = "readResolve"
    final val reflect : N             = "reflect"
    final val reify : N               = "reify"
    final val rootMirror : N          = "rootMirror"
    final val runOrElse: N            = "runOrElse"
    final val runtime: N              = "runtime"
    final val runtimeClass: N         = "runtimeClass"
    final val runtimeMirror: N        = "runtimeMirror"
    final val sameElements: N         = "sameElements"
    final val scala_ : N              = "scala"
    final val selectDynamic: N        = "selectDynamic"
    final val selectOverloadedMethod: N = "selectOverloadedMethod"
    final val selectTerm: N           = "selectTerm"
    final val selectType: N           = "selectType"
    final val self: N                 = "self"
    final val setAccessible: N        = "setAccessible"
    final val setAnnotations: N       = "setAnnotations"
    final val setSymbol: N            = "setSymbol"
    final val setType: N              = "setType"
    final val setTypeSignature: N     = "setTypeSignature"
    final val splice: N               = "splice"
    final val staticClass : N         = "staticClass"
    final val staticModule : N        = "staticModule"
    final val staticPackage : N       = "staticPackage"
    final val synchronized_ : N       = "synchronized"
    final val tail: N                 = "tail"
    final val `then` : N              = "then"
    final val this_ : N               = "this"
    final val thisPrefix : N          = "thisPrefix"
    final val throw_ : N              = "throw"
    final val toArray: N              = "toArray"
    final val toList: N               = "toList"
    final val toObjectArray : N       = "toObjectArray"
    final val toSeq: N                = "toSeq"
    final val toString_ : N           = "toString"
    final val toTypeConstructor: N    = "toTypeConstructor"
    final val tpe : N                 = "tpe"
    final val tree : N                = "tree"
    final val true_ : N               = "true"
    final val typedProductIterator: N = "typedProductIterator"
    final val typeTagToManifest: N    = "typeTagToManifest"
    final val unapply: N              = "unapply"
    final val unapplySeq: N           = "unapplySeq"
    final val unbox: N                = "unbox"
    final val universe: N             = "universe"
    final val update: N               = "update"
    final val updateDynamic: N        = "updateDynamic"
    final val value: N                = "value"
    final val valueOf : N             = "valueOf"
    final val values : N              = "values"
    final val view_ : N               = "view"
    final val wait_ : N               = "wait"
    final val withFilter: N           = "withFilter"
    final val wrap: N                 = "wrap"
    final val zip: N                  = "zip"

    final val synthSwitch: N          = "$synthSwitch"

    // unencoded operators
    object raw {
      final val AMP  : N  = "&"
      final val BANG : N  = "!"
      final val BAR  : N  = "|"
      final val DOLLAR: N = "$"
      final val GE: N     = ">="
      final val LE: N     = "<="
      final val MINUS: N  = "-"
      final val NE: N     = "!="
      final val PLUS : N  = "+"
      final val SLASH: N  = "/"
      final val STAR : N  = "*"
      final val TILDE: N  = "~"

      final val isUnary: Set[Name] = Set(MINUS, PLUS, TILDE, BANG)
    }

    // value-conversion methods
    final val toByte: N   = "toByte"
    final val toShort: N  = "toShort"
    final val toChar: N   = "toChar"
    final val toInt: N    = "toInt"
    final val toLong: N   = "toLong"
    final val toFloat: N  = "toFloat"
    final val toDouble: N = "toDouble"

    // primitive operation methods for structural types mostly
    // overlap with the above, but not for these two.
    final val toCharacter: N = "toCharacter"
    final val toInteger: N   = "toInteger"

    final def newLazyValSlowComputeName(lzyValName: N) = lzyValName ++ LAZY_SLOW_SUFFIX

    // ASCII names for operators
    final val ADD      = encode("+")
    final val AND      = encode("&")
    final val ASR      = encode(">>")
    final val DIV      = encode("/")
    final val EQ       = encode("==")
    final val EQL      = encode("=")
    final val GE       = encode(">=")
    final val GT       = encode(">")
    final val HASHHASH = encode("##")
    final val LE       = encode("<=")
    final val LSL      = encode("<<")
    final val LSR      = encode(">>>")
    final val LT       = encode("<")
    final val MINUS    = encode("-")
    final val MOD      = encode("%")
    final val MUL      = encode("*")
    final val NE       = encode("!=")
    final val OR       = encode("|")
    final val PLUS     = ADD    // technically redundant, but ADD looks funny with MINUS
    final val SUB      = MINUS  // ... as does SUB with PLUS
    final val XOR      = encode("^")
    final val ZAND     = encode("&&")
    final val ZOR      = encode("||")

    // unary operators
    final val UNARY_~ = encode("unary_~")
    final val UNARY_+ = encode("unary_+")
    final val UNARY_- = encode("unary_-")
    final val UNARY_! = encode("unary_!")

    // Grouped here so Cleanup knows what tests to perform.
    final val CommonOpNames   = Set[Name](OR, XOR, AND, EQ, NE)
    final val ConversionNames = Set[Name](toByte, toChar, toDouble, toFloat, toInt, toLong, toShort)
    final val BooleanOpNames  = Set[Name](ZOR, ZAND, UNARY_!) ++ CommonOpNames
    final val NumberOpNames   = (
         Set[Name](ADD, SUB, MUL, DIV, MOD, LSL, LSR, ASR, LT, LE, GE, GT)
      ++ Set(UNARY_+, UNARY_-, UNARY_!)
      ++ ConversionNames
      ++ CommonOpNames
    )

    final val add: N                    = "add"
    final val complement: N             = "complement"
    final val divide: N                 = "divide"
    final val multiply: N               = "multiply"
    final val negate: N                 = "negate"
    final val positive: N               = "positive"
    final val shiftLogicalRight: N      = "shiftLogicalRight"
    final val shiftSignedLeft: N        = "shiftSignedLeft"
    final val shiftSignedRight: N       = "shiftSignedRight"
    final val subtract: N               = "subtract"
    final val takeAnd: N                = "takeAnd"
    final val takeConditionalAnd: N     = "takeConditionalAnd"
    final val takeConditionalOr: N      = "takeConditionalOr"
    final val takeModulo: N             = "takeModulo"
    final val takeNot: N                = "takeNot"
    final val takeOr: N                 = "takeOr"
    final val takeXor: N                = "takeXor"
    final val testEqual: N              = "testEqual"
    final val testGreaterOrEqualThan: N = "testGreaterOrEqualThan"
    final val testGreaterThan: N        = "testGreaterThan"
    final val testLessOrEqualThan: N    = "testLessOrEqualThan"
    final val testLessThan: N           = "testLessThan"
    final val testNotEqual: N           = "testNotEqual"

    final val isBoxedNumberOrBoolean: N = "isBoxedNumberOrBoolean"
    final val isBoxedNumber: N = "isBoxedNumber"

    final val reflPolyCacheName: N   = "reflPoly$Cache"
    final val reflClassCacheName: N  = "reflClass$Cache"
    final val reflParamsCacheName: N = "reflParams$Cache"
    final val reflMethodCacheName: N = "reflMethod$Cache"
    final val reflMethodName: N      = "reflMethod$Method"

    private val reflectionCacheNames = Set[N](
      reflPolyCacheName,
      reflClassCacheName,
      reflParamsCacheName,
      reflMethodCacheName,
      reflMethodName
    )

    final def isReflectionCacheName(name: Name) = reflectionCacheNames exists (name startsWith _)
  }

  final class ScalaTermNames extends ScalaNames[TermName] {
    protected def fromString(s: String) = termName(s)

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

    def localDummyName(clazz: Symbol)(implicit ctx: Context): TermName =
      LOCALDUMMY_PREFIX ++ clazz.name ++ ">"

    def newBitmapName(bitmapPrefix: TermName, n: Int): TermName = bitmapPrefix ++ n.toString

  }

  final class ScalaTypeNames extends ScalaNames[TypeName] {
    protected def fromString(s: String) = typeName(s)
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

    final val BoxedBoolean: N       = "java.lang.Boolean"
    final val BoxedByte: N          = "java.lang.Byte"
    final val BoxedCharacter: N     = "java.lang.Character"
    final val BoxedDouble: N        = "java.lang.Double"
    final val BoxedFloat: N         = "java.lang.Float"
    final val BoxedInteger: N       = "java.lang.Integer"
    final val BoxedLong: N          = "java.lang.Long"
    final val BoxedNumber: N        = "java.lang.Number"
    final val BoxedShort: N         = "java.lang.Short"
    final val Class: N              = "java.lang.Class"
    final val IOOBException: N      = "java.lang.IndexOutOfBoundsException"
    final val InvTargetException: N = "java.lang.reflect.InvocationTargetException"
    final val MethodAsObject: N     = "java.lang.reflect.Method"
    final val NPException: N        = "java.lang.NullPointerException"
    final val Object: N             = "java.lang.Object"
    final val String: N             = "java.lang.String"
    final val Throwable: N          = "java.lang.Throwable"

    final val ForName: N          = "forName"
    final val GetCause: N         = "getCause"
    final val GetClass: N         = "getClass"
    final val GetClassLoader: N   = "getClassLoader"
    final val GetComponentType: N = "getComponentType"
    final val GetMethod: N        = "getMethod"
    final val Invoke: N           = "invoke"
    final val JavaLang: N         = "java.lang"

    final val BeanProperty: N        = "scala.beans.BeanProperty"
    final val BooleanBeanProperty: N = "scala.beans.BooleanBeanProperty"
    final val JavaSerializable: N    = "java.io.Serializable"
   }

  final class JavaTermNames extends JavaNames[TermName] {
    protected def fromString(s: String): TermName = termName(s)
  }
  final class JavaTypeNames extends JavaNames[TypeName] {
    protected def fromString(s: String): TypeName = typeName(s)
  }

  val nme = new ScalaTermNames
  val tpnme = new ScalaTypeNames
  val jnme = new JavaTermNames
  val jtpnme = new JavaTypeNames

}
