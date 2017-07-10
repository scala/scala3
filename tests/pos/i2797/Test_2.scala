// Test is pending because we have no good way to test it.
// We need to: Compile Fork.java, and then compile Test.scala
// with Fork.class on the classpath.
@Fork_1(jvmArgs = Array("I'm", "hot"))
class Test
