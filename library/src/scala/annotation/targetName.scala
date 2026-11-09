package scala.annotation

import language.experimental.captureChecking

/** An annotation that defines an external name for a definition.
 *  If an `targetName(extname)` annotation is given for a method or some other
 *  definition, its implementation will use the name `extname` instead of
 *  the regular name.
 *
 *  @param name the external name to use for the annotated definition
 */
final class targetName(name: String) extends StaticAnnotation
