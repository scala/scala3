package dotty.tools.dotc
package transform
package sjs

import core.*
import Constants.*
import Contexts.*
import Flags.*
import NameOps.*
import Names.*
import Phases.*
import StdNames.*
import Symbols.*

import ast.Trees.*
import Types.*

import dotty.tools.backend.sjs.JSDefinitions.jsdefn

import dotty.tools.sjs.ir.{Trees => js}

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
            case nme.apply =>
              Call
            case JSUnaryOpMethodName(code, defaultsToOp)
                if (defaultsToOp || sym.hasAnnotation(jsdefn.JSOperatorAnnot)) && pc == 0 =>
              UnaryOp(code)
            case JSBinaryOpMethodName(code, defaultsToOp)
                if (defaultsToOp || sym.hasAnnotation(jsdefn.JSOperatorAnnot)) && pc == 1 =>
              BinaryOp(code)
            case _ =>
              default
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
        annotation.argumentConstantString(0) match {
          case Some(name) => JSName.Literal(name)
          case None       => JSName.Computed(annotation.arguments.head.symbol)
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
        // See also JSCodeGen.genActualArgs
        val list =
          for ((name, info) <- paramNamesAndTypes) yield {
            val v =
              if (info.isRepeatedParam) Some(TypeErasure.erasure(info.repeatedToSingle))
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

    /** Tests whether the semantics of Scala.js require a field for this symbol,
     *  irrespective of any optimization we think we can do.
     *
     *  This is the case if one of the following is true:
     *
     *  - it is a member of a JS type, since it needs to be visible as a JavaScript field
     *  - is is exported as static member of the companion class, since it needs to be visible as a JavaScript static field
     *  - it is exported to the top-level, since that can only be done as a true top-level variable, i.e., a field
     */
    def sjsNeedsField(using Context): Boolean =
      ctx.settings.scalajs.value && (
        sym.owner.isJSType
          || sym.hasAnnotation(jsdefn.JSExportTopLevelAnnot)
          || sym.hasAnnotation(jsdefn.JSExportStaticAnnot)
      )
    end sjsNeedsField
  }

  /** Extractor for a `TermName` that *may* be a JS unary operator.
   *
   *  If it may be a JS unary operator, then a method with that name may have
   *  the `@JSOperator` annotation, and it will be treated as such.
   *
   *  If a method has neither `@JSName` nor `@JSOperator`, then a default is
   *  chosen. If the `Boolean` value is `true`, the default is to treat the
   *  method as if it had `@JSOperator`. If it is `false`, the default is *not*
   *  to treat it as an operator.
   *
   *  Currently, all JS unary operators default to `@JSOperator`.
   */
  private object JSUnaryOpMethodName {
    private val map = Map(
      nme.UNARY_+ -> (js.JSUnaryOp.+, true),
      nme.UNARY_- -> (js.JSUnaryOp.-, true),
      nme.UNARY_~ -> (js.JSUnaryOp.~, true),
      nme.UNARY_! -> (js.JSUnaryOp.!, true),
    )

    def unapply(name: TermName): Option[(js.JSUnaryOp.Code, Boolean)] =
      map.get(name)
  }

  /** Extractor for a `TermName` that *may* be a JS binary operator.
   *
   *  If it may be a JS binary operator, then a method with that name may have
   *  the `@JSOperator` annotation, and it will be treated as such.
   *
   *  If a method has neither `@JSName` nor `@JSOperator`, then a default is
   *  chosen. If the `Boolean` value is `true`, the default is to treat the
   *  method as if it had `@JSOperator`. If it is `false`, the default is *not*
   *  to treat it as an operator.
   *
   *  Most JS binary operators default to `@JSOperator`. Currently, the only
   *  exception is `**`, for backward compatibility reasons.
   */
  private object JSBinaryOpMethodName {
    private val map = Map(
      nme.ADD -> (js.JSBinaryOp.+, true),
      nme.SUB -> (js.JSBinaryOp.-, true),
      nme.MUL -> (js.JSBinaryOp.*, true),
      nme.DIV -> (js.JSBinaryOp./, true),
      nme.MOD -> (js.JSBinaryOp.%, true),

      nme.LSL -> (js.JSBinaryOp.<<, true),
      nme.ASR -> (js.JSBinaryOp.>>, true),
      nme.LSR -> (js.JSBinaryOp.>>>, true),
      nme.OR  -> (js.JSBinaryOp.|, true),
      nme.AND -> (js.JSBinaryOp.&, true),
      nme.XOR -> (js.JSBinaryOp.^, true),

      nme.LT -> (js.JSBinaryOp.<, true),
      nme.LE -> (js.JSBinaryOp.<=, true),
      nme.GT -> (js.JSBinaryOp.>, true),
      nme.GE -> (js.JSBinaryOp.>=, true),

      nme.ZAND -> (js.JSBinaryOp.&&, true),
      nme.ZOR  -> (js.JSBinaryOp.||, true),

      termName("**") -> (js.JSBinaryOp.**, false),
    )

    def unapply(name: TermName): Option[(js.JSBinaryOp.Code, Boolean)] =
      map.get(name)
  }
}
