//> using options -Wnonunit-statement

class C {
  def f1 = -2.abs // warn funky precedence
  def f2 = - 2.abs // warn meaningless space
  def f3 = - 2 // warn meaningless space
  def f4 = 42
    -2.abs // warn precedence // hides warn unused expression
  def f5 = 42
    - 2.abs // nowarn infix
  def f6 = (-2).abs // nowarn explicit precedence
  def f7 = -3.14 // nowarn decimal point

  def f8 = -2L.abs // warn funky precedence
  def f9 = -2f.abs // warn funky precedence
  def f10 = -2d.abs // warn funky precedence

  def f11 = - 2L.abs // warn meaningless space
  def f12 = - 2f.abs // warn meaningless space
  def f13 = - 2d.abs // warn meaningless space

  def f14 = (-2L).abs // nowarn explicit precedence
  def f15 = (-2f).abs // nowarn explicit precedence
  def f16 = (-2d).abs // nowarn explicit precedence
}