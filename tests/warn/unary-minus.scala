//> using options -Wsyntax -Wnonunit-statement

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
}
