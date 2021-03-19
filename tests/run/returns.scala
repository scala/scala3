def foo(x: Int): Int =
  if x == 1 then
    return
      val y = 2
      x + y
  else
    2

def bar(): Unit =
  return
  assert(false)

@main def Test =
  assert(foo(1) == 3)
  bar()


