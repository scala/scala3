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

  val map1 = dotty.tools.dotc.util.HashMap[Integer, Integer]()
  val map2 = scala.collection.mutable.HashMap[Integer, Integer]()

  def checkSame() =
    assert(map1.size == map2.size)
    for (k, v) <- map1.iterator do
      assert(map2.get(k) == Some(v))
    for (k, v) <- map2.iterator do
      assert(Option(map1.lookup(k)) == Some(v))

  def lookupTest(num: Integer) =
    //println(s"test lookup $num")
    val res1 = Option(map1.lookup(num))
    val res2 = map2.get(num)
    assert(res1 == res2)

  def updateTest(num: Integer) =
    //println(s"test update $num")
    lookupTest(num)
    map1(num) = num
    map2(num) = num
    checkSame()

  def removeTest(num: Integer) =
    //println(s"test remove $num")
    map1.remove(num)
    map2.remove(num)
    checkSame()

  for i <- 0 until Iterations do
    //if i % 1000 == 0 then println(map1.size)
    val num = nums.generate
    Generator.ops.generate match
      case Lookup => lookupTest(num)
      case Update => updateTest(num)
      case Remove => removeTest(num)
