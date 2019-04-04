
// Test that JavaNull is "see through"
class S {
  val j: J2 = new J2()
  j.getJ1().getJ2().getJ1().getJ2().getJ1().getJ2()
}
