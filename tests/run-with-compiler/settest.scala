trait Generator[+T]:
  self =>
  def generate: T
  def map[S](f: T => S) = new Generator[S]:
    def generate: S = f(self.generate)
  def flatMap[S](f: T => Generator[S]) = new Generator[S]:
    def generate: S = f(self.generate).generate

object Generator:
  val NumLimit = 300
  val Iterations = 10000

  given integers: Generator[Int] with
    val rand = new java.util.Random
    def generate = rand.nextInt()

  given booleans: Generator[Boolean] =
    integers.map(x => x > 0)

  def range(end: Int): Generator[Int] =
    integers.map(x => (x % end).abs)

  enum Op:
    case Lookup, Update, Remove
  export Op._

  given ops: Generator[Op] =
    range(10).map {
      case 0 | 1 | 2 | 3 => Lookup
      case 4 | 5 | 6 | 7 => Update
      case 8 | 9         => Remove
    }

  val nums: Generator[Integer] = range(NumLimit).map(Integer(_))

@main def Test =
  import Generator.*

  val set1 = dotty.tools.dotc.util.HashSet[Int]()
  val set2 = scala.collection.mutable.HashSet[Int]()

  def checkSame() =
    assert(set1.size == set2.size)
    for e <- set1.iterator do
      assert(set2.contains(e))
    for e <- set2.iterator do
      assert(set1.contains(e))

  def lookupTest(num: Integer) =
    val res1 = set1.contains(num)
    val res2 = set2.contains(num)
    assert(res1 == res2)

  def updateTest(num: Integer) =
    lookupTest(num)
    set1 += num
    set2 += num
    checkSame()

  def removeTest(num: Integer) =
    //println(s"test remove $num")
    set1 -= num
    set2 -= num
    checkSame()

  for i <- 0 until Iterations do
    val num = nums.generate
    Generator.ops.generate match
      case Lookup => lookupTest(num)
      case Update => updateTest(num)
      case Remove => removeTest(num)
