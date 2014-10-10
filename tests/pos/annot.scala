import java.beans.Transient

class Test {

  @SuppressWarnings(Array("hi")) def foo() = ???

  @Transient(false) def bar = ???

  @Transient() def baz = ???
}

