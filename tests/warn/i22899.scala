case class CaseClass(a: Int)

object ProductMatch_CaseClass {
  def unapply(int: Int): CaseClass = CaseClass(int)
}

object ProductMatch_NamedTuple {
  def unapply(int: Int): (a: Int) = (a = int)
}

object NameBasedMatch_CaseClass {
  def unapply(int: Int): Some[CaseClass] = Some(CaseClass(int))
}

object NameBasedMatch_NamedTuple {
  def unapply(int: Int): Some[(a: Int)] = Some((a = int))
}

object Test {
  val ProductMatch_CaseClass(a = x1) = 1    // ok, was pattern's type (x1 : Int) is more specialized than the right hand side expression's type Int
  val ProductMatch_NamedTuple(a = x2) = 2   // ok, was pattern binding uses refutable extractor `org.test.ProductMatch_NamedTuple`
  val NameBasedMatch_CaseClass(a = x3) = 3  // ok, was pattern's type (x3 : Int) is more specialized than the right hand side expression's type Int
  val NameBasedMatch_NamedTuple(a = x4) = 4 // ok, was pattern's type (x4 : Int) is more specialized than the right hand side expression's type Int

  val CaseClass(a = x5) = CaseClass(5)      // ok, was pattern's type (x5 : Int) is more specialized than the right hand side expression's type Int
  val (a = x6) = (a = 6)                    // ok
}