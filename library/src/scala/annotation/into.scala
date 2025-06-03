package scala.annotation
import annotation.experimental

/** An annotation on (part of) a parameter type that allows implicit conversions
 *  for its arguments. The `into` modifier on parameter types in Scala 3 is
 *  mapped to this annotation. The annotation is intended to be used directly in
 *  Scala 2 sources only. For Scala 3, the `into` modifier should be preferred.
 */
@experimental
class into() extends annotation.StaticAnnotation
