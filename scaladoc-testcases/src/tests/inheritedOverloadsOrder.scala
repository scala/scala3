package tests.inheritedOverloadsOrder

trait T:
  def a(): Int = 0
  def a(i: Int): Int = i
  def a(i: Int, j: Int): Int = i + j

class C extends T
