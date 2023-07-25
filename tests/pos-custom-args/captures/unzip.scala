class Seqq[A]:
  def unzip[A1, A2](using asPair: A -> (A1, A2)): (Seq[A1], Seq[A2]) = ???

def Test =
  val s: Seqq[(String, Int)] = ???
  s.unzip(using Predef.$conforms[(String, Int)])

