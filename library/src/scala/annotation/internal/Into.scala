package scala.annotation.internal
import annotation.experimental

/** An annotation on (part of) a parameter type that allows implicit conversions
 *  for its arguments. The `into` modifier on parameter types in Scala 3 is
 *  mapped to this annotation. We can also install a more generally accessible
 *  alias so that Scala 2 libraries can use the feature.
 */
@experimental
class into() extends annotation.StaticAnnotation

@experimental
class $into() extends annotation.StaticAnnotation

