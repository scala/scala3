
def Test(b: Boolean) =
  val a =
    if b then
       1
    else if !b then
       2
  val _: Unit = a
