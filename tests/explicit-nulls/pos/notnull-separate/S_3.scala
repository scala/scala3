// Test that NotNull annotations are working in class files.

import javax.annotation.J_2

class S_3 {
  def kk: String = J_2.k
  def ll: String = J_2.l
  def mm: String = (new J_2).m
  def nn: String = (new J_2).n
  def ff(i: Int): String = J_2.f(i)
  def gg(i: Int): String = J_2.g(i)
  def hh(i: Int): String = (new J_2).h(i)
  def genericff(a: String | Null): Array[String | Null] = (new J_2).genericf(a)
  def genericgg(a: String | Null): java.util.List[String] = (new J_2).genericg(a)
}
