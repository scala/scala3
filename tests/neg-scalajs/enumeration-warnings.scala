//> using options -Xfatal-warnings

class UnableToTransformValue extends Enumeration {
  val a = {
    println("oh, oh!")
    Value // warn
  }
  val b = {
    println("oh, oh!")
    Value(4) // warn
  }
}

class ValueWithNullName extends Enumeration {
  val a = Value(null) // warn
  val b = Value(10, null) // warn
}

class NewValWithNoName extends Enumeration {
  val a = new Val // warn
  val b = new Val(10) // warn
}

class NewValWithNullName extends Enumeration {
  val a = new Val(null) // warn
  val b = new Val(10, null) // warn
}

class ExtendsValWithNoName extends Enumeration {
  protected class Val1 extends Val // warn
  protected class Val2 extends Val(1) // warn
}

class ExtendsValWithNullName extends Enumeration {
  protected class Val1 extends Val(null) // warn
  protected class Val2 extends Val(1, null) // warn
}

// nopos-error: No warnings can be incurred under -Werror.
