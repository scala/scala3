// Verify that enum values aren't nullified.

class S {
  val p: Planet = Planet.MARS // ok: accessing static member
  val p2: Planet = p.next()   // error: expected Planet but got Planet|Null
}
