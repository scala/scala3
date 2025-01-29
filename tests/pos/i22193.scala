
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

  fn3( // arg at 3, body at 3
     arg = "blue sleeps faster than tuesday",
     arg2 = "the quick brown fox jumped over the lazy dog"):
     val x = "Hello"
     println(x)

  fn3( // arg at 3, body at 1: not sure if sig indent of 1 is allowed, saw some comments from odersky
     arg = "blue sleeps faster than tuesday",
     arg2 = "the quick brown fox jumped over the lazy dog"):
   val x = "Hello"
   println(x)

  fn3( // arg at 3, body at 2: even if sig indent of 1 is not allowed, body is at fn3+2, not arg2-1
     arg = "blue sleeps faster than tuesday",
     arg2 = "the quick brown fox jumped over the lazy dog"):
    val x = "Hello"
    println(x)

  fn3( // arg at 3, body at 4
     arg = "blue sleeps faster than tuesday",
     arg2 = "the quick brown fox jumped over the lazy dog"):
      val x = "Hello"
      println(x)

// don't turn innocent empty cases into functions
def regress(x: Int) =
  x match
  case 42 =>
  case _ =>

// previously lookahead calculated indent width at the colon
def k(xs: List[Int]) =
  xs.foldLeft(
      0)
                                            : (acc, x) =>
        acc + x

def `test kit`(xs: List[Int]): Unit =
  def addOne(i: Int): Int = i + 1
  def isPositive(i: Int): Boolean = i > 0
  // doesn't compile but would be nice
  // first body is indented "twice", or, rather, first outdent establishes an intermediate indentation level
  xs.map: x =>
      x + 1
    .filter: x =>
      x > 0
  xs.map:
      addOne
    .filter:
      isPositive

  // does compile
  xs
    .map: x =>
      x + 1
    .filter: x =>
      x > 0

  // does compile but doesn't look good, at least, to some people
  xs.map: x =>
      x + 1
  .filter: x =>
      x > 0

def `tested kit`(xs: List[Int]): Unit =
  {
    def addOne(i: Int): Int = i.+(1)
    def isPositive(i: Int): Boolean = i.>(0)
    xs.map[Int]((x: Int) => x.+(1)).filter((x: Int) => x.>(0))
    xs.map[Int]((i: Int) => addOne(i)).filter((i: Int) => isPositive(i))
    xs.map[Int]((x: Int) => x.+(1)).filter((x: Int) => x.>(0))
    {
      xs.map[Int]((x: Int) => x.+(1)).filter((x: Int) => x.>(0))
      ()
    }
  }
