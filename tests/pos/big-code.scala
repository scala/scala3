// This creates a lot of code and can hit ASM size limits if they're set too low

inline def rep(inline n: Int)(inline body: Unit): Unit =
  inline if n == 0 then body
  else
    rep(n - 1)(body)
    rep(n - 1)(body)

def f(x: Int): Int =
  var acc = 0
  try
    rep(11):
      val y = x + acc
      if y > 3 then acc += y
  catch case _: Exception => ()
  acc
