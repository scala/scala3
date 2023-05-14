package scala.annotation
import annotation.experimental

/** An annotation on a parameter type that allows implicit conversions
 *  for its arguments. Intended for use by Scala 2, to annotate Scala 2
 *  libraries. Scala 3 uses the `into` modifier on the parameter
 *  type instead.
 */
@experimental
class allowConversions extends scala.annotation.StaticAnnotation
