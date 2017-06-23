package scala.annotation.internal.link

/** Assert at compile time that the symbol of a method is not reachable.
  *
  * This does not work on methods that get created later like getters,
  * setters, bridges, ...
  */
final class AssertNotReachable extends scala.annotation.StaticAnnotation
