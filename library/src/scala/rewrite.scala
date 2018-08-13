package scala

/** An annotation on methods that is equivalent to Dotty `rewrite` modifier.
 *
 *  The annotation should be used instead of the `rewrite` modifier in code
 *  that needs to cross compile between Scala 2 and Dotty.
 *
 *  Note that Scala 2 ignores the `@rewrite` annotation, and one must use
 *  both the `@inline` and `@rewrite` annotation to inline across the
 *  two compilers. E.g.
 *
 *  ```scala
 *  @inline @`rewrite` def foo = ...
 *  ```
 */
class `rewrite` extends scala.annotation.StaticAnnotation
