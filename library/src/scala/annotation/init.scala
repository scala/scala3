package scala.annotation

/** Annotations to control the behavior of the compiler check for safe initialization of static obects.
 *
 *  Programmers usually do not need to use any annotations. They are intended for complex initialization
 *  code in static objects.
 */
object init:

  /** Widen the abstract value of the argument so that its height is below the specified height.
   *
   *  It can be used to mark method or constructor arguments, as the following example shows:
   *
   *     class A(x: Int):
   *       def squre(): Int = x*x
   *
   *     object B:
   *       val a = build(new A(10): @init.widen(1))   // <-- usage
   *
   *       def build(o: A) = new A(o.square())        // calling methods on parameter
   *
   *  By default, method and constructor arguments are cold aliases, which means they may not be actively
   *  used --- calling methods or accessing fields on them are forbidden. By using `@widen(1)` to the
   *  method argument, we tell the compiler that the abstract value for the argument may have a maximum
   *  height of 1. As a result, the parameter is not a cold alias any more, so we may call methods
   *  on the parameter.
   */
  final class widen(hight: Int) extends StaticAnnotation
