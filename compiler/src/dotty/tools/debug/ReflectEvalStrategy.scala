package dotty.tools.debug

import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.util.Property.*

/**
 * The [[ExtractExpression]] phase attaches an [[ReflectEvalStrategy]] to each `reflectEval` node
 * capturing information about the term that requires evaluation via reflection (because it is
 * inaccessible from the evaluation class).
 * Subsequently, the [[ResolveReflectEval]] phase converts each evaluation strategy into a method
 * call within the expression class.
 */
private enum ReflectEvalStrategy:
  case This(cls: ClassSymbol)
  case Outer(outerCls: ClassSymbol)
  case LocalOuter(outerCls: ClassSymbol) // the $outer param in a constructor
  case LocalValue(variable: TermSymbol, isByName: Boolean)
  case LocalValueAssign(variable: TermSymbol)
  case MethodCapture(variable: TermSymbol, method: TermSymbol, isByName: Boolean)
  case MethodCaptureAssign(variable: TermSymbol, method: TermSymbol)
  case ClassCapture(variable: TermSymbol, cls: ClassSymbol, isByName: Boolean)
  case ClassCaptureAssign(variable: TermSymbol, cls: ClassSymbol)
  case StaticObject(obj: ClassSymbol)
  case Field(field: TermSymbol, isByName: Boolean)
  case FieldAssign(field: TermSymbol)
  case MethodCall(method: TermSymbol)
  case ConstructorCall(ctr: TermSymbol, cls: ClassSymbol)

object ReflectEvalStrategy extends StickyKey[ReflectEvalStrategy]
