trait Extractor {
  inline def unapplySeq(inline tn: String): Option[Seq[String]]
}

transparent inline def (inline sc: StringContext).poql: Extractor = new Extractor {
  inline def unapplySeq(inline tn: String): Option[Seq[String]] = ??? // error: Implementation restriction: nested inline methods are not supported
}

object x {
  "x" match {
    case poql" $x" => x // error: Deferred inline method unapplySeq in trait Extractor cannot be invoked
  }
}
