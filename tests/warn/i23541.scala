//> using options -Wrecurse-with-default

def fun(x: Int)(using p: Int, q: Int = 0): Int =
  if x <= 0 then p * q
  else fun(x - 1)(using p = p + x) // warn recurse uses default (instead of given passed down the stack)

def gun(x: Int)(p: Int, q: Int = 0): Int =
  if x <= 0 then p * q
  else gun(x - 1)(p = p + x) // warn recurse uses default (value not passed down the stack)

def nested(using x: Int, y: Int = 42): Int =
  def f: Int = nested(using x) // nowarn only self-recursive tailrec is eligible for warning
  f

def f(using s: String, i: Int = 1): String = s * i
def g(using s: String)(using i: Int = 1): String = s * i

@main def Test =
  println(fun(3)(using p = 0, q = 1))
  locally:
    given String = "ab"
    println(f) // prints "ab"
    println(g) // prints "ab"
  locally:
    println(f(using s = "ab")) // prints "ab"
    println(g(using s = "ab")) // prints "ab"
  locally:
    given Int = 2
    println(f(using s = "ab")) // warn uses default instead of given // prints "ab"
    println(g(using s = "ab")) // prints "abab"
