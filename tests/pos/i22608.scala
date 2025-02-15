
def f(i: Int) = i
def g(i: Int, j: Int) = i+j

def t =
  val y = f(
    if (true)// then
      val x = 1
      5
    else 7
  )
  y

def u(j: Int) =
  val y = g(
    if (true)// then
      val x = 1
      5
    else 7,
    j
  )
  y

def b(k: Boolean): Int =
  f(
    if (
       k
    && b(!k) > 0
    ) then 27
    else 42
  )

def p(b: Boolean) =
  import collection.mutable.ListBuffer
  val xs, ys = ListBuffer.empty[String]
  (if (b)
    xs
  else
    ys) += "hello, world"
  (xs.toString, ys.toString)

def q(b: Boolean) =
  import collection.mutable.ListBuffer
  val xs, ys = ListBuffer.empty[String]
  (if (b)
   then xs
   else ys) += "hello, world"
  (xs.toString, ys.toString)
