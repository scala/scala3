object A:
   def fn: Unit =
    if true then
  println(1)
  println(2) // error: start of line does not match previous indentation widths
    else
  println(2)