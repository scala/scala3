package covtest

// Test case for default arguments with coverage.
// Default getter methods ($default$N) should not be instrumented/lifted
// as this can cause issues with ScalaJS IR (https://github.com/scala/scala3/issues/20255).

class DefaultArgs:
  def methodWithDefaults(
    a: String,
    b: Int = 0,
    c: Boolean = true
  ): String =
    s"$a, $b, $c"

  def caller(): String =
    // This call uses default arguments for b and c
    methodWithDefaults("test")

  def callerPartial(): String =
    // This call uses default argument only for c
    methodWithDefaults("test", 42)

object DefaultArgs:
  def staticMethod(x: Int = 10, y: Int = 20): Int =
    x + y

  def staticCaller(): Int =
    staticMethod() + staticMethod(5) + staticMethod(5, 15)

