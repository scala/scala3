// https://github.com/scala/scala3/issues/13216
import scala.annotation.targetName

class C(s: String) extends AnyVal {
  def m(xs: Seq[Int]): Unit = {}
  @targetName("m_seq2")
  def m(xs: Seq[Seq[Int]]): Unit = {}
}

@main def Test =
  new C("").m(Seq(123))
