transparent def bar: Any = 2  // error: transparent can be used only with inline

object test1:

  def x: Int = baz(true) // error: type mismatch
  inline def baz(x: Boolean): Any =
    if x then 1 else ""
  inline def bam(x: Boolean): Any =
    if x then 1 else ""
  def y: Int = bam(true) // error: type mismatch

object test2:

  def x: 1 = baz(true) // OK
  transparent inline def baz(x: Boolean) =
    if x then 1 else ""
  transparent inline def bam(x: Boolean) =
    if x then 1 else ""
  def y: 1 = bam(true) // OK

object test3:

  def x: Int = baz(true) // error: type mismatch
  inline def baz(x: Boolean) =
    if x then 1 else ""
  inline def bam(x: Boolean) =
    if x then 1 else ""
  def y: Int = bam(true) // error: type mismatch

object test4:

  def x: 1 = baz(true) // OK
  transparent inline def baz(x: Boolean): Any =
    if x then 1 else ""
  transparent inline def bam(x: Boolean): Any =
    if x then 1 else ""
  def y: 1 = bam(true) // OK
