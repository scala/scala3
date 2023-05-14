// scalac: -Xfatal-warnings

class UnableToTransformValue extends Enumeration {
  val a = {
    println("oh, oh!")
    Value // error
  }
  val b = {
    println("oh, oh!")
    Value(4) // error
  }
}

class ValueWithNullName extends Enumeration {
  val a = Value(null) // error
  val b = Value(10, null) // error
}

class NewValWithNoName extends Enumeration {
  val a = new Val // error
  val b = new Val(10) // error
}

class NewValWithNullName extends Enumeration {
  val a = new Val(null) // error
  val b = new Val(10, null) // error
}

class ExtendsValWithNoName extends Enumeration {
  protected class Val1 extends Val // error
  protected class Val2 extends Val(1) // error
}

class ExtendsValWithNullName extends Enumeration {
  protected class Val1 extends Val(null) // error
  protected class Val2 extends Val(1, null) // error
}
