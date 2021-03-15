def foo(x: Int): Int =
  if x == 1 then
    return
      x
  else
    2

def bar(): Unit =
  return
  assert(false)

@main def Test =
  assert(foo(1) == 1)
  bar()


