// Using a checkfile to verify where the carets point to.
// Originally they were pointing to "cc," and "ff,"
// including the trailing comma

class Test:
  def t1 =
    (
      aa, // error
      bb, // error
      cc, // error
    )

  def meth(a: Int, b: Int, c: Int) = a + b + c
  def t2 =
    meth(
      dd, // error
      ee, // error
      ff, // error
    )
