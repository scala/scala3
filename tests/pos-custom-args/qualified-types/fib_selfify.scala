def fib(n: Int): {res: Int with
  res == (
    if n == 0 then 0
    else if n == 1 then 1
    else fib(n - 1) + fib(n - 2)
  )
} =
  val res =
    if n == 0 then 0
    else if n == 1 then 1
    else fib(n - 1) + fib(n - 2)
  res
