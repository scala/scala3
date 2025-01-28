
def fn2(arg: String, arg2: String)(f: String => Unit): Unit = f(arg)

def fn3(arg: String, arg2: String)(f: => Unit): Unit = f

def test() =

  fn2(arg = "blue sleeps faster than tuesday", arg2 = "the quick brown fox jumped over the lazy dog"): env =>
    val x = env
    println(x)

  // doesn't compile
  fn2(
      arg = "blue sleeps faster than tuesday",
      arg2 = "the quick brown fox jumped over the lazy dog"): env =>
      val x = env
      println(x)

  fn2(
      arg = "blue sleeps faster than tuesday",
      arg2 = "the quick brown fox jumped over the lazy dog"): env =>
       val x = env
       println(x)

  // does compile
  fn2(
      arg = "blue sleeps faster than tuesday",
      arg2 = "the quick brown fox jumped over the lazy dog"):
    env =>
      val x = env
      println(x)

  // does compile
  fn2(
      arg = "blue sleeps faster than tuesday",
      arg2 = "the quick brown fox jumped over the lazy dog"
  ): env =>
      val x = env
      println(x)

  fn3(
    arg = "blue sleeps faster than tuesday",
    arg2 = "the quick brown fox jumped over the lazy dog"):
    val x = "Hello"
    println(x)

  fn3(
      arg = "blue sleeps faster than tuesday",
      arg2 = "the quick brown fox jumped over the lazy dog"):
      val x = "Hello"
      println(x)

// don't turn innocent empty cases into functions
def regress(x: Int) =
  x match
  case 42 =>
  case _ =>
