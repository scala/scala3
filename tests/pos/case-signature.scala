// If `translateFromRepeated` translated wildcards by default, the following
// would break because of the use of wildcards in signatures.
case class Benchmark[A](params: List[A],
  sqlInsert: (benchId: Long, params: A, session: Int) => Unit,
  fun: List[A])
