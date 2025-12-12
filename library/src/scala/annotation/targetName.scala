package scala.annotation

import language.experimental.captureChecking
import meta.exportable

/** An annotation that defines an external name for a definition.
 *  If an `targetName(extname)` annotation is given for a method or some other
 *  definition, its implementation will use the name `extname` instead of
 *  the regular name.
 */
@exportable
final class targetName(name: String) extends StaticAnnotation
