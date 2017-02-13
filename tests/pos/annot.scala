import java.beans.Transient
import annotation.unchecked.uncheckedVariance

class Test {

// testing combinations of annotation syntax

  @SuppressWarnings(Array("hi")) def foo() = ??? // evalutation of annotation on type cannot be deferred as requires implicit resolution(only generic Array$.apply applies here)

  @SuppressWarnings(Array("hi", "foo")) def foo2() = ??? //can be deferred as there is a non-generic method

  @SuppressWarnings(Array("hi")) def foo3() = ??? // can be written in java and is serialized this way in bytecode. doesn't typecheck

  @Transient(false) def bar = ???

  @Transient() def baz = ???

// testing annotations in types

  class A
  trait B

  val x: A @uncheckedVariance with B @uncheckedVariance = ???

  class C extends A @uncheckedVariance () with B @uncheckedVariance { val x = 10 }

  val f: (Int => Int) @uncheckedVariance = ???
}

