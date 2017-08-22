object Test {
  // reachability error as Tuple2 is final.
  (1, 2) match { case Seq() => 0; case _ => 1 } // error
}
