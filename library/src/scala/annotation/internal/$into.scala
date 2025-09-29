package scala.annotation.internal

/** An internal annotation on (part of) a parameter type that serves as a marker where
 *  the original type was of the form `into[T]`. These annotated types are mapped back
 *  to `into[T]` types when forming a method types from the parameter types. The idea is
 *  that `T @$into` is equivalent to `T`, whereas `into[T]` is only a known supertype of
 *  `T`. Hence, we don't need to use `.underlying` to go from an into type to its
 *  underlying type in the types of local parameters.
 */
@preview
class $into extends annotation.StaticAnnotation