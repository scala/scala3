def m(i: Int*) = i.sum
val f1: Seq[Int] => Int = m
val f2 = i => m(i*)

def n(i: Seq[Int]) = i.sum
val g1 = n
val g2 = i => n(i)
