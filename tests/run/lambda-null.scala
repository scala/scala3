object Test {
  def genericCall1[A, B](foo: A => B) = {
    foo(null.asInstanceOf[A])
  }
  def genericCall3[A, B](foo: (A, A, A) => B) = {
    foo(null.asInstanceOf[A], null.asInstanceOf[A], null.asInstanceOf[A])
  }

  def makeGeneric1[A]: A => A = x => {
    println("generic Function1: " + x)
    x
  }

  def makeGeneric3[A]: (A, A, A) => A = (x, y, z) => {
    println("generic Function3: " + x + " " + y + " " + z)
    x
  }


  def main(args: Array[String]): Unit = {
    // - The invokedynamic lambda has type JFunction1$mcII$sp after
    // specialization by phase FunctionalInterfaces
    // - The closure method has type (int): int, it implements
    //   the only abstract method in JFunction1cII$sp: `apply$mcII$sp`
    //
    // In a generic call, `apply(Object): Object` will be called,
    // JFunction1$mcII$sp implements this method to forward to apply$mcIIsp with
    // appropriate boxing/unboxing.
    val if1_specialized: Int => Int = x => {
      // assert(x == 0)
      println("specialized Function1: " + x)
      x
    }

    // - The invokedynamic lambda has type Function1
    // - The closure method has type (Object): Object, it implements
    //   which is the only abstract method in Function1: `apply`
    //
    // In a specialized call, `apply$mcII$sp(int): int` will be called
    // (currently, Dotty never generates this call because specialization is not
    // implemented, but they can still happen when passing a lambda to Scala 2 code),
    // Function1 implements this method to forward to `apply` with appropriate
    // boxing/unboxing.
    val if1_generic: Int => Int = makeGeneric1[Int]


    // - The invokedynamic lambda has type Function3
    // - The closure method has type (int, int, int): int, it _does not_
    //   implement the only abstract method in Function3: `apply` which has
    //   type (Object, Object, Object): Object
    //
    // FIXME: This means that LambdaMetaFactory will generate a bridge from
    // `apply` to the closure method. this work most of the time, but LMF does
    // not unbox null to 0 as required by Scala semantics, instead it throws an NPE.
    val if3_unspecialized: (Int, Int, Int) => Int = (x, y, z) => {
      println("unspecialized Function3: " + x + " " + y + " " + z)
      x + y + z
    }

    // - The invokedynamic lambda has type Function3
    // - The closure method has type (Object, Object, Object): Object, it implements
    //   the only abstract method in Function3: `apply`
    //
    // Calls are never specialized, they always go through the `apply` method,
    // boxing/unboxing is handled at the call site.
    val if3_generic: (Int, Int, Int) => Int = makeGeneric3[Int]

    println("# Specific calls")
    assert(if1_specialized(0) == 0)
    assert(if1_generic(0) == 0)
    assert(if3_unspecialized(0, 0, 0) == 0)
    assert(if3_generic(0, 0, 0) == 0)

    println("# Generic calls")
    assert(genericCall1(if1_specialized) == 0)
    assert(genericCall1(if1_generic) == 0)
    // FIXME: throws NullPointerException, see above
    // assert(genericCall3(if3_unspecialized) == 0)
    assert(genericCall3(if3_generic) == 0)
  }
}
