package dotty.tools.dotc
package transform
package sjs

import core._
import util.SrcPos
import Annotations._
import Constants._
import Contexts._
import Decorators._
import DenotTransformers._
import Flags._
import NameKinds.DefaultGetterName
import NameOps._
import Names._
import Phases._
import Scopes._
import StdNames._
import Symbols._
import SymDenotations._
import SymUtils._
import ast.Trees._
import Types._

import dotty.tools.backend.sjs.JSDefinitions.jsdefn

import org.scalajs.ir.{Trees => js}

/** Additional extensions for `Symbol`s that are only relevant for Scala.js. */
object JSSymUtils {
  /** The result type for `sym.jsName`.
   *
   *  It is either a literal string, or a computed name represented by a reference
   *  to a static `Symbol` (a `dotc.core.Symbols.Symbol`, not a `js.Symbol`).
   */
  enum JSName {
    case Literal(name: String)
    case Computed(sym: Symbol)

    def displayName(using Context): String = this match {
      case Literal(name) => name
      case Computed(sym) => sym.fullName.toString()
    }
  }

  enum JSCallingConvention {
    case Call, BracketAccess, BracketCall
    case Method(name: JSName)
    case Property(name: JSName)
    case UnaryOp(code: js.JSUnaryOp.Code)
    case BinaryOp(code: js.JSBinaryOp.Code)

    def displayName(using Context): String = this match {
      case Call           => "function application"
      case BracketAccess  => "bracket access"
      case BracketCall    => "bracket call"
      case Method(name)   => "method '" + name.displayName + "'"
      case Property(name) => "property '" + name.displayName + "'"
      case UnaryOp(code)  => "unary operator"
      case BinaryOp(code) => "binary operator"
    }
  }

  object JSCallingConvention {
    def of(sym: Symbol)(using Context): JSCallingConvention = {
      assert(sym.isTerm, s"got non-term symbol: $sym")

      if (isJSBracketAccess(sym)) {
        BracketAccess
      } else if (isJSBracketCall(sym)) {
        BracketCall
      } else {
        def default = {
          val jsName = sym.jsName
          if (sym.isJSProperty) Property(jsName)
          else Method(jsName)
        }

        if (!sym.hasAnnotation(jsdefn.JSNameAnnot)) {
          lazy val pc = sym.info.paramNamess.map(_.size).sum

          sym.name match {
            case nme.apply                             => Call
            case JSUnaryOpMethodName(code) if pc == 0  => UnaryOp(code)
            case JSBinaryOpMethodName(code) if pc == 1 => BinaryOp(code)
            case _                                     => default
          }
        } else {
          default
        }
      }
    }
  }

  /** Info about a Scala method param when called as JS method.
   *
   *  @param info
   *    Parameter type (type of a single element if repeated).
   *  @param repeated
   *    Whether the parameter is repeated.
   *  @param capture
   *    Whether the parameter is a capture.
   */
  final class JSParamInfo(
    val info: Type,
    val repeated: Boolean = false,
    val capture: Boolean = false
  ) {
    override def toString(): String =
      s"ParamSpec($info, repeated = $repeated, capture = $capture)"
  }

  extension (sym: Symbol) {
    /** Is this symbol a JavaScript type? */
    def isJSType(using Context): Boolean =
      sym.hasAnnotation(jsdefn.JSTypeAnnot)

    /** Is this symbol a non-native JS class? */
    def isNonNativeJSClass(using Context): Boolean =
      sym.isJSType && !sym.hasAnnotation(jsdefn.JSNativeAnnot)

    /** Is this symbol a nested JS class, i.e., an inner or local JS class? */
    def isNestedJSClass(using Context): Boolean =
      !sym.isStatic && sym.isJSType

    /** Tests whether the given member is exposed, i.e., whether it was
     *  originally a public or protected member of a non-native JS class.
     */
    def isJSExposed(using Context): Boolean = {
      !sym.is(Bridge) && {
        sym.hasAnnotation(jsdefn.ExposedJSMemberAnnot)
          || (sym.is(Accessor) && sym.field.hasAnnotation(jsdefn.ExposedJSMemberAnnot))
      }
    }

    /** Should this symbol be translated into a JS getter? */
    def isJSGetter(using Context): Boolean = {
      sym.is(Module)
        || !sym.is(Method)
        || (sym.info.firstParamTypes.isEmpty && atPhaseNoLater(erasurePhase)(sym.info.isParameterless))
    }

    /** Should this symbol be translated into a JS setter? */
    def isJSSetter(using Context): Boolean =
      sym.originalName.isSetterName && sym.is(Method)

    /** Is this symbol a JS getter or setter? */
    def isJSProperty(using Context): Boolean =
      sym.isJSGetter || sym.isJSSetter

    /** Should this symbol be translated into a JS bracket access? */
    def isJSBracketAccess(using Context): Boolean =
      sym.hasAnnotation(jsdefn.JSBracketAccessAnnot)

    /** Should this symbol be translated into a JS bracket call? */
    def isJSBracketCall(using Context): Boolean =
      sym.hasAnnotation(jsdefn.JSBracketCallAnnot)

    /** Is this symbol a default param accessor for the constructor of a native JS class? */
    def isJSNativeCtorDefaultParam(using Context): Boolean = {
      sym.name.is(DefaultGetterName)
        && sym.name.exclude(DefaultGetterName) == nme.CONSTRUCTOR
        && sym.owner.linkedClass.hasAnnotation(jsdefn.JSNativeAnnot)
    }

    def jsCallingConvention(using Context): JSCallingConvention =
      JSCallingConvention.of(sym)

    def hasJSCallCallingConvention(using Context): Boolean =
      sym.jsCallingConvention == JSCallingConvention.Call

    /** Gets the unqualified JS name of the symbol.
     *
     *  If it is not explicitly specified with an `@JSName` annotation, the
     *  JS name is inferred from the Scala name.
     */
    def jsName(using Context): JSName = {
      sym.getAnnotation(jsdefn.JSNameAnnot).fold[JSName] {
        JSName.Literal(defaultJSName)
      } { annotation =>
        annotation.arguments.head match {
          case Literal(Constant(name: String)) => JSName.Literal(name)
          case tree                            => JSName.Computed(tree.symbol)
        }
      }
    }

    def defaultJSName(using Context): String =
      if (sym.isTerm) sym.asTerm.name.unexpandedName.getterName.toString()
      else sym.name.unexpandedName.stripModuleClassSuffix.toString()

    def jsParamInfos(using Context): List[JSParamInfo] = {
      assert(sym.is(Method), s"trying to take JS param info of non-method: $sym")

      def paramNamesAndTypes(using Context): List[(Names.TermName, Type)] =
        sym.info.paramNamess.flatten.zip(sym.info.paramInfoss.flatten)

      val paramInfosAtElimRepeated = atPhase(elimRepeatedPhase) {
        val list =
          for ((name, info) <- paramNamesAndTypes) yield {
            val v =
              if (info.isRepeatedParam) Some(info.repeatedToSingle.widenDealias)
              else None
            name -> v
          }
        list.toMap
      }

      val paramInfosAtElimEVT = atPhase(elimErasedValueTypePhase) {
        paramNamesAndTypes.toMap
      }

      for ((paramName, paramInfoNow) <- paramNamesAndTypes) yield {
        paramInfosAtElimRepeated.get(paramName) match {
          case None =>
            // This is a capture parameter introduced by erasure or lambdalift
            new JSParamInfo(paramInfoNow, capture = true)

          case Some(Some(info)) =>
            new JSParamInfo(info, repeated = true)

          case Some(None) =>
            val info = paramInfosAtElimEVT.getOrElse(paramName, paramInfoNow)
            new JSParamInfo(info)
        }
      }
    }
  }

  private object JSUnaryOpMethodName {
    private val map = Map(
      nme.UNARY_+ -> js.JSUnaryOp.+,
      nme.UNARY_- -> js.JSUnaryOp.-,
      nme.UNARY_~ -> js.JSUnaryOp.~,
      nme.UNARY_! -> js.JSUnaryOp.!
    )

    def unapply(name: TermName): Option[js.JSUnaryOp.Code] =
      map.get(name)
  }

  private object JSBinaryOpMethodName {
    private val map = Map(
      nme.ADD -> js.JSBinaryOp.+,
      nme.SUB -> js.JSBinaryOp.-,
      nme.MUL -> js.JSBinaryOp.*,
      nme.DIV -> js.JSBinaryOp./,
      nme.MOD -> js.JSBinaryOp.%,

      nme.LSL -> js.JSBinaryOp.<<,
      nme.ASR -> js.JSBinaryOp.>>,
      nme.LSR -> js.JSBinaryOp.>>>,
      nme.OR  -> js.JSBinaryOp.|,
      nme.AND -> js.JSBinaryOp.&,
      nme.XOR -> js.JSBinaryOp.^,

      nme.LT -> js.JSBinaryOp.<,
      nme.LE -> js.JSBinaryOp.<=,
      nme.GT -> js.JSBinaryOp.>,
      nme.GE -> js.JSBinaryOp.>=,

      nme.ZAND -> js.JSBinaryOp.&&,
      nme.ZOR  -> js.JSBinaryOp.||
    )

    def unapply(name: TermName): Option[js.JSBinaryOp.Code] =
      map.get(name)
  }
}
