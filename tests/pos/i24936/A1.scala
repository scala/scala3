// Test for issue #24936: Compiler crash based on file name ordering
// The crash occurred when a trait with a self-type referring to an object
// was compiled before the object, due to incorrect outer accessor path computation.

package example

// Original failing case: trait with self-type to O1
trait A1 { self: O1.type =>
  import O2.someKey
  def taskImpl: Unit = println(someKey)
}

// Test case: accessing nested object member directly
trait A2 { self: O1.type =>
  def accessNested: Int = O2.someKey
}

// Test case: accessing nested object method
trait A3 { self: O1.type =>
  def callNested: String = O2.getInfo
}

// Test case: multiple levels of nesting
trait A4 { self: O1.type =>
  def deepAccess: Boolean = O2.Nested.flag
}

// Test case: using type from nested object
trait A5 { self: O1.type =>
  def useType: O2.MyType = O2.myValue
}

// Test case: pattern matching on nested object
trait A6 { self: O1.type =>
  def patternMatch(x: Int): String = x match {
    case O2.someKey => "matched"
    case _ => "not matched"
  }
}
