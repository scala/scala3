package snippetCompiler

/**
  * ```scala sc:compile&failing
  * def a = 2
  * val x = 1 + List()
  * a
  * ```
  *
  * ```scala sc:compile&failing
  * def a = 2
  * ```
  *
  * ```scala sc:nocompile
  * def a = 3
  * a()
  * ```
  */
class A { }
