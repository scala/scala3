@main def Test: Unit =
  println(f(0))
  println(f(1))
  println(f(2))

def f(i: Int): String =
  switch(i)(
    "hello",
    "word",
    "bye"
  )
