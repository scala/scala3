// Test is pending because we have no good way to test it.
// We need to: Compile Fork.java, and then compile Test.scala
// with Fork.class on the classpath.
class Fork(value: Int = -1, jvmArgs: Array[String] = Array("nope"))
extends annotation.Annotation
