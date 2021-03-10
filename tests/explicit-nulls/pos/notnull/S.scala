// Test that NotNull annotations are working in Java files.

import javax.annotation.J

class S_3 {
  def kk: String = J.k
  def ll: String = J.l
  def mm: String = (new J).m
  def nn: String = (new J).n
  def ff(i: Int): String = J.f(i)
  def gg(i: Int): String = J.g(i)
  def hh(i: Int): String = (new J).h(i)
  def genericff(a: String | Null): Array[String | Null] = (new J).genericf(a)
  def genericgg(a: String | Null): java.util.List[String] = (new J).genericg(a)
}
