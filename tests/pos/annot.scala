import java.beans.Transient

class Test {

  @SuppressWarnings(Array("hi")) def foo() = ??? // evalutation of annotation on type cannot be deffered as requires implicit resolution(only generic Array$.apply applies here)

  @SuppressWarnings(Array("hi", "foo")) def foo2() = ??? //can be deffered as there is a non-generic method

//  @SuppressWarnings("hi") def foo3() = ??? // can be written in java and is serialized this way in bytecode. doesn't typecheck

  @Transient(false) def bar = ???

  @Transient() def baz = ???
}

