package benchmark

import scala.collection._

// class Main {
object Test extends App {
  val m = new Bench()
  val debug = false
  val points = m.identityPoints
  val valhallaPoints = m.valhallaPoints
  val means = m.identityMeans
  val valhallaMeans = m.valhallaMeans

  val seqPts = m.seqPoints
  val seqMeans = m.seqMeans

  def testingKmeans(debug: Boolean = false, seqPt : Boolean = false) : Seq[IdentityPoint] = {
    val ret =
      if (!seqPt) m.kMeans(points, means)
      else m.seqPtsKMeans(seqPts, seqMeans)
    ret
  }
  def testingKmeansValhalla() : Seq[ValhallaPoint] = {
    val ret = m.kMeansValhalla(valhallaPoints, valhallaMeans)
    ret
  }
  @main def main() : Unit = {
  // def main() : Unit = {
    println("entering main")
    testingKmeans(false, false)
    testingKmeansValhalla()
    // testingKmeans(false, true)
  }
}
