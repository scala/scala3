package scala.annotation
package meta

/**
 * This internal annotation encodes arguments passed to annotation superclasses. Example:
 *
 * {{{
 *   class a(x: Int) extends Annotation
 *   class b extends a(42) // the compiler adds `@superArg("x", 42)` to class b
 * }}}
 */
class superArg(p: String, v: Any) extends StaticAnnotation

/**
 * This internal annotation encodes arguments passed to annotation superclasses. Example:
 *
 * {{{
 *   class a(x: Int) extends Annotation
 *   class b(y: Int) extends a(y) // the compiler adds `@superFwdArg("x", "y")` to class b
 * }}}
 */
class superFwdArg(p: String, n: String) extends StaticAnnotation
