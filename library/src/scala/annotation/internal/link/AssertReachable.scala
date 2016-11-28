package scala.annotation.internal.link

/** Assert at compile time that the symbol of a method is reachable.
  *
  * This does not work on methods that get created later like getters,
  * setters, bridges, ...
  */
final class AssertReachable extends scala.annotation.StaticAnnotation
