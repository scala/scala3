package scala.annotation

/** Annotations to control the behavior of the compiler check for safe initialization of static obects.
 *
 *  Programmers usually do not need to use any annotations. They are intended for complex initialization
 *  code in static objects.
 */
object init:

  /** Widen the abstract value of the argument so that its height is below the specified height.
   *
   *  This is an advanced version of `@init.expose`, where we tell the compiler the annotated method or
   *  constructor argument are not cold aliases and its abstract value may have a maximum height of the
   *  specified value.
   */
  final class widen(hight: Int) extends StaticAnnotation

  /** Expose the method argument or constructor argument so that it may be actively used during the
   *  initialization of static objects.
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
   *  used --- calling methods or accessing fields on them are forbidden. By using `@expose` to the
   *  method argument, we tell the compiler that the argument is not a cold alias. As a result, we may
   *  call methods on the parameter.
   *
   *  It is semantically equivalent to `@init.widen(1)`.
   */
  final class expose extends StaticAnnotation
