package scala.annotation
package unchecked

/** An annotation for mutable variables that are allowed to capture
 *  the root capability `cap`. Allowing this is not capture safe since
 *  it can cause leakage of capabilities from local scopes by assigning
 *  values retaining such capabilties to the annotated variable in
 *  an outer scope.
 */
class uncheckedCaptures extends StaticAnnotation


