def f(x: Int) =
  assert(
    if x > 0 then
      true
    else
      false
  )
  assert(
    if x > 0 then
      true
    else
      false)
  assert(
    if x > 0 then
      true
    else
      false, "fail")
  assert(
    if x > 0 then
      true
    else
      if x < 0 then
        true
      else
        false, "fail")
  (
    if x > 0 then
      println(x)
      x
    else
      s"""foo${
        if x > 0 then
          println(x)
          x
        else
          -x
        }"""
  )