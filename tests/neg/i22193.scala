
def fn2(arg: String, arg2: String)(f: String => Unit): Unit = f(arg)

def fn3(arg: String, arg2: String)(f: => Unit): Unit = f

def test1() =

  // ok baseline
  fn2(arg = "blue sleeps faster than tuesday", arg2 = "the quick brown fox jumped over the lazy dog"): env =>
    val x = env
    println(x)

  fn2( // error not a legal formal parameter for a function literal
      arg = "blue sleeps faster than tuesday",
      arg2 = "the quick brown fox jumped over the lazy dog"): env =>
  val x = env // error
  println(x)

  fn2(
      arg = "blue sleeps faster than tuesday",
      arg2 = "the quick brown fox jumped over the lazy dog"):
  env => // error indented definitions expected, identifier env found
      val x = env
      println(x)

def test2() =

  fn3( // error missing argument list for value of type (=> Unit) => Unit
    arg = "blue sleeps faster than tuesday",
    arg2 = "the quick brown fox jumped over the lazy dog"):
  val x = "Hello" // error
  println(x) // error
