package benchmark

// import org.openjdk.jmh.annotations.*

import scala.collection.immutable.Seq
import scala.util.Random

// @State(Scope.Benchmark)
// @Fork(value = 5)
// @Warmup(iterations = 5)
// @Measurement(iterations = 5)
// @BenchmarkMode(Array(Mode.Throughput))
//class Bench(val debug1 : Boolean, val ptCnt1 : Int, val k1: Int) {
class Bench(){
  val k = 32
  val xRange = 100
  val yRange = 200
  val zRange = 500
  val ptCnt = 50000
  val debug = false
  val tolerance = 0.0000001
//  val k = k1
//  val ptCnt = ptCnt1
//  val debug = debug1
  val identityPoints : Seq[IdentityPoint] = generatePoints(k, ptCnt)
  val identityMeans : Seq[IdentityPoint] = initializeMeans(k, identityPoints)
  val seqPoints: SeqPoints = new SeqPoints(identityPoints)
  val seqMeans: SeqPoints = new SeqPoints(identityMeans)

  val valhallaPoints : Seq[ValhallaPoint] = generateValhallaPoints(k, ptCnt)
  val valhallaMeans : Seq[ValhallaPoint] = initializeMeans(k, valhallaPoints)

  def generatePoints(k: Int, pointsCount: Int): Seq[IdentityPoint] = {
    val randX = new Random(1)
    val randY = new Random(3)
    val randZ = new Random(5)
    (0 until pointsCount).map({ i =>
      val x = randX.nextDouble() * xRange
      val y = randY.nextDouble() * yRange
      val z = randZ.nextDouble() * zRange
      new IdentityPoint(x, y, z)
    })
  }

  def generateValhallaPoints(k: Int, pointsCount: Int): Seq[ValhallaPoint] = {
    val randX = new Random(1)
    val randY = new Random(3)
    val randZ = new Random(5)
    (0 until pointsCount).map({ i =>
      val x = randX.nextDouble() * xRange
      val y = randY.nextDouble() * yRange
      val z = randZ.nextDouble() * zRange
      new ValhallaPoint(x, y, z)
    })
  }

  /** Randomly select k points as the initial centroids */
  def initializeMeans[A](k: Int, points: Seq[A]): Seq[A] = {
    val rand = new Random(37)
    (0 until k)
      .map(_ => points(rand.nextInt(points.length)))
  }

  // @Benchmark
  def runKmeans: Seq[IdentityPoint] = kMeans(identityPoints, identityMeans)
  // @Benchmark
  def runKmeansSeqPts: Seq[IdentityPoint] = seqPtsKMeans(seqPoints, seqMeans)
  // @Benchmark
  def runKMeansValhalla: Seq[ValhallaPoint] = kMeansValhalla(valhallaPoints, valhallaMeans)

  /** Given point p and the centroids, return the centroid the p is the closest to. */
  def findClosest(p: IdentityPoint, centroids: Seq[IdentityPoint]): IdentityPoint = {
    assert(centroids.nonEmpty)
    var minDistance = p.squareDistance(centroids(0))
    var closest = centroids(0)

    for (mean <- centroids.tail) {
      val distance = p.squareDistance(mean)
      if (distance < minDistance) {
        minDistance = distance
        closest = mean
      }
    }
    closest
  }

  def findClosestValhalla(p: ValhallaPoint, centroids: Seq[ValhallaPoint]): ValhallaPoint = {
    assert(centroids.nonEmpty)
    var minDistance = p.squareDistance(centroids(0))
    var closest = centroids(0)

    for (mean <- centroids.tail) {
      val distance = p.squareDistance(mean)
      if (distance < minDistance) {
        minDistance = distance
        closest = mean
      }
    }
    closest
  }

  /** Given all points `points` in a cluster with mean `oldMean`, return the average of `points`. */
  def findAverage(oldMean: IdentityPoint, points: Seq[IdentityPoint]): IdentityPoint = {
    if (points.isEmpty) then
      oldMean
    else
      var x = 0.0
      var y = 0.0
      var z = 0.0
      points.foreach { p =>
        x += p.x
        y += p.y
        z += p.z
      }
      new IdentityPoint(x / points.length, y / points.length, z / points.length)
  }

  def findAverageValhalla(oldMean: ValhallaPoint, points: Seq[ValhallaPoint]): ValhallaPoint = {
    if (points.isEmpty) then
      oldMean
    else
      var x = 0.0
      var y = 0.0
      var z = 0.0
      points.foreach { p =>
        x += p.x
        y += p.y
        z += p.z
      }
      new ValhallaPoint(x / points.length, y / points.length, z / points.length)
  }

  /** Assign each point to closest centroid */
  def classify(points: Seq[IdentityPoint], centroids: Seq[IdentityPoint]): Map[IdentityPoint, Seq[IdentityPoint]] = {
    val grouped = points.groupBy(p => findClosest(p, centroids))

    centroids.foldLeft(grouped) {
      (map, mean) => if (map.contains(mean)) map else map.updated(mean, Seq())
    }
  }

  def classifyValhalla(points: Seq[ValhallaPoint], centroids: Seq[ValhallaPoint]): Map[ValhallaPoint, Seq[ValhallaPoint]] = {
    val grouped = points.groupBy(p => findClosestValhalla(p, centroids))

    centroids.foldLeft(grouped) {
      (map, mean) => if (map.contains(mean)) map else map.updated(mean, Seq())
    }
  }
  
  /** Given the classify map and the oldMeans, update each cluster's average. */
  def updateMeans(classified: Map[IdentityPoint, Seq[IdentityPoint]], oldMeans: Seq[IdentityPoint]): Seq[IdentityPoint] =
    oldMeans.map(mean => findAverage(mean, classified(mean)))

  def seqPtsUpdateMeans(classified: Map[IdentityPoint, Seq[IdentityPoint]], oldMeans: Seq[IdentityPoint]): Seq[IdentityPoint] =
    SeqPoints(oldMeans.map(mean => findAverage(mean, classified(mean))))

  def updateMeansValhalla(classified: Map[ValhallaPoint, Seq[ValhallaPoint]], oldMeans: Seq[ValhallaPoint]): Seq[ValhallaPoint] =
    oldMeans.map(mean => findAverageValhalla(mean, classified(mean)))

  /** Given oldMeans and newMeans, check if euclidean distance between the two is less than the tolerance.*/
  def converged(oldMeans: Seq[IdentityPoint], newMeans: Seq[IdentityPoint]): Boolean =
    (oldMeans zip newMeans).map({
      (oldMean, newMean) => oldMean l1Distance newMean
    }).forall(_ <= tolerance)

  def convergedValhalla(oldMeans: Seq[ValhallaPoint], newMeans: Seq[ValhallaPoint]): Boolean =
    (oldMeans zip newMeans).map({
      (oldMean, newMean) => oldMean l1Distance newMean
    }).forall(_ <= tolerance)

  @annotation.tailrec
  final def seqPtsKMeans(points: Seq[IdentityPoint], centroids: Seq[IdentityPoint]): Seq[IdentityPoint] = {
    val classifiedPoints = classify(points, centroids)
    val newMeans = seqPtsUpdateMeans(classifiedPoints, centroids)

    if (converged(centroids, newMeans)) then
      newMeans
    else
      seqPtsKMeans(points, newMeans)
  }

  @annotation.tailrec
  final def kMeans(points: Seq[IdentityPoint], centroids: Seq[IdentityPoint]): Seq[IdentityPoint] = {
    println("Entering kMeans.")
    val classifiedPoints = classify(points, centroids)
    val newMeans = updateMeans(classifiedPoints, centroids)

    if (converged(centroids, newMeans)) then
      newMeans
    else
      kMeans(points, newMeans)
  }

  @annotation.tailrec
  final def kMeansValhalla(points: Seq[ValhallaPoint], centroids: Seq[ValhallaPoint]): Seq[ValhallaPoint] = {
    println("Entering kMeansValhalla.")
    val classifiedPoints = classifyValhalla(points, centroids)
    val newMeans = updateMeansValhalla(classifiedPoints, centroids)

    if (convergedValhalla(centroids, newMeans)) then
      newMeans
    else
      kMeansValhalla(points, newMeans)
  }
}