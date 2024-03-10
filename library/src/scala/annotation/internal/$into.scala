package scala.annotation.internal
import annotation.experimental

/** An internal annotation on (part of) a parameter type that allows implicit conversions
 *  for its arguments. The publicly visible `into` annotation in the parent package
 *  `annotation` gets mapped to `$into` by the compiler in all places where
 *  conversions should be allowed. The reason for the split into two annotations
 *  is that `annotation.into` is given in source code and may propagate in unspecified
 *  ways through type inference. By contrast `$into` is constrained to occur only
 *  on parameters of method types. This makes implicit conversion insertion
 *  predictable and independent of the un-specified aspects of type inference.
 */
@experimental
class $into() extends annotation.StaticAnnotation

