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
   *       val a = build(new A(10): @init.expose)   // <-- usage
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

  /** Mark a region context.
   *
   *  The same mutable field of objects in the same region have the same shape. The concept of regions is an
   *  attempt to make context-sensitivity explainable and customizable.
   *
   *  Example:
   *
   *      trait B { def foo(): Int }
   *      class C(var x: Int) extends B { def foo(): Int = 20 }
   *      class D(var y: Int) extends B { def foo(): Int = A.m }
   *      class Box(var value: B)
   *
   *       object A:
   *         val box1: Box = new Box(new C(5)): @init.region
   *         val box2: Box = new Box(new D(10)): @init.region
   *         val m: Int = box1.value.foo()
   *
   *  In the above, without the two region annotation, the two objects `box1` and `box2` are of the same region.
   *  Therefore, the field `box1.value` and `box2.value` points to both instances of `C` and `D`. Consequently,
   *  the method call `box1.value.foo()` will be invalid, because it reaches `A.m`, which is not yet initialized.
   *  The explicit context annotation solves the problem.
   */
  final class region extends StaticAnnotation