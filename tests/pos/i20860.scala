def `i20860 use result to check selector bound`: Unit =
  import Ordering.Implicits.given Ordering[?]
  summon[Ordering[Seq[Int]]]
