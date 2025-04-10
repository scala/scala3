package scala.annotation

import language.experimental.captureChecking

/** An annotation that defines an external name for a definition.
 *  If an `alpha(extname)` annotation is given for a method or some other
 *  definition, its implementation will use the name `extname` instead of
 *  the regular name. An `alpha` annotation is mandatory for definitions
 *  with symbolic names.
 */
@deprecated("use @targetName instead")
final class alpha(externalName: String) extends StaticAnnotation
