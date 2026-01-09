// Test that Nullable annotations are working in Java files.

import javax.annotation.J

class S_3 {
  def kk: String = J.k // error
  def ll: String = J.l // error
  def mm: String = (new J).m // error
  def nn: String = (new J).n // error
  def ff(i: Int): String = J.f(i) // error
  def gg(i: Int): String = J.g(i) // error
  def hh(i: Int): String = (new J).h(i) // error
  def qq(s: String): String | Null = (new J).q(s)
  def genericff(a: String): Array[String] = (new J).genericf(a) // error
  def genericgg(a: String): java.util.List[String] = (new J).genericg(a) // error
  def LList(s: String): java.util.List[String] = (new J).listS("") // error
  def stringFromNullableList(s: String): String = (new J).listS("").get(0) // error
}
