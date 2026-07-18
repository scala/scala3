package scala.annotation
package unchecked

import language.experimental.captureChecking

/** An annotation for mutable variables that are allowed to capture
 *  the root capability `any`. Allowing this is not capture safe since
 *  it can cause leakage of capabilities from local scopes by assigning
 *  values retaining such capabilties to the annotated variable in
 *  an outer scope.
 */
class uncheckedCaptures extends StaticAnnotation


