// regression test - originally scala.boundary.apply used @unchecked,
// so reintroduce the old implementation here,
// so that in Test_2.scala we can assert that post-inlining this annotation is rejected in safe mode.
object boundaryOld:
  import scala.util.boundary.{Break, Label}
  inline def apply[T](inline body: Label[T] ?=> T): T =
    val local = Label[T]()
    try body(using local)
    catch case ex: Break[T] @unchecked =>
      if ex.isSameLabelAs(local) then ex.value
      else throw ex
