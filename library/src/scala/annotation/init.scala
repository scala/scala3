package scala.annotation

import language.experimental.captureChecking

/** Annotations to control the behavior of the compiler check for safe initialization of static obects.
 *
 *  Programmers usually do not need to use any annotations. They are intended for complex initialization
 *  code in static objects.
 */
@experimental
object init:

  /** Widen the abstract value of the argument so that its height is below the specified height.
   *
   *  It can be used to mark method or constructor arguments, as the following example shows:
   *
   *     class A(x: Int):
   *       def square(): Int = x*x
   *
   *     class C(val a: A)
   *
   *     object B:
   *       val a = build(new C(new A(10)): @widen(2))      // <-- usage
   *
   *       def build(c: C) = new A(c.a.square())           // calling methods on parameter
   *
   *  By default, method and constructor arguments are widened to height 1. In
   *  the code above, without using `@widen(2)` we will have the abstract value
   *  `C(a = Cold)` for the argument `c` of the method `build`. Consequently,
   *  the checker will issue a warning for the method call `c.a.square()` because
   *  it is forbidden to call methods or access fields on cold values.
   *
   *  @param height the maximum height (depth of the abstract object tree) to which the argument value is widened
   */
  @experimental
  final class widen(height: Int) extends StaticAnnotation

  /** Introduce a region context.
   *
   *  The same mutable field in the same region have the same abstract representation.
   *
   *  The concept of regions is intended to make context-sensitivity tunable for complex use cases.
   *
   *  Example:
   *
   *      trait B { def foo(): Int }
   *      class C(var x: Int) extends B { def foo(): Int = 20 }
   *      class D(var y: Int) extends B { def foo(): Int = A.m }
   *      class Box(var value: B)
   *
   *       object A:
   *         val box1: Box = region { new Box(new C(5))  }
   *         val box2: Box = region { new Box(new D(10)) }
   *         val m: Int = box1.value.foo()
   *
   *  In the above, without the two region annotations, the two objects `box1` and `box2` are in the same region.
   *  Therefore, the field `box1.value` and `box2.value` points to both instances of `C` and `D`. Consequently,
   *  the method call `box1.value.foo()` will be invalid, because it reaches `A.m`, which is not yet initialized.
   *  The explicit context annotation solves the problem.
   *
   *  @tparam T the type of the value computed within the region
   *  @param v the expression to evaluate within a fresh region context
   *  @return the value `v` unchanged at runtime; at the checker level, mutable fields within this region receive distinct abstract representations
   */
  @experimental
  def region[T](v: T): T = v
