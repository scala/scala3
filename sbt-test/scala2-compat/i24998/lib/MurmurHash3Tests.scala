import scala.util.hashing.MurmurHash3

object MurmurHash3Tests {
  final val Seed = 42

  def apply(): Unit = {
    arrayHashTest()
    arrayHashingTest()
  }

  def arrayHashTest() = {
    assert(MurmurHash3.arrayHash(Array(1: Byte, Seed)) != 0)
    assert(MurmurHash3.arrayHash(Array(1: Char, Seed)) != 0)
    assert(MurmurHash3.arrayHash(Array(1: Short, Seed)) != 0)
    assert(MurmurHash3.arrayHash(Array(1: Int, Seed)) != 0)
    assert(MurmurHash3.arrayHash(Array(1: Long, Seed)) != 0)
    assert(MurmurHash3.arrayHash(Array(1: Float, Seed)) != 0)
    assert(MurmurHash3.arrayHash(Array(1: Double, Seed)) != 0)
    assert(MurmurHash3.arrayHash(Array(true: Boolean, Seed)) != 0)
    assert(MurmurHash3.arrayHash(Array((): Unit), Seed) != 0)

    assert(MurmurHash3.arrayHash(Array(1: Byte)) != 0)
    assert(MurmurHash3.arrayHash(Array(1: Char)) != 0)
    assert(MurmurHash3.arrayHash(Array(1: Short)) != 0)
    assert(MurmurHash3.arrayHash(Array(1: Int)) != 0)
    assert(MurmurHash3.arrayHash(Array(1: Long)) != 0)
    assert(MurmurHash3.arrayHash(Array(1: Float)) != 0)
    assert(MurmurHash3.arrayHash(Array(1: Double)) != 0)
    assert(MurmurHash3.arrayHash(Array(true: Boolean)) != 0)
    assert(MurmurHash3.arrayHash(Array((): Unit)) != 0)
  }

  def arrayHashingTest() = {
    assert(MurmurHash3.arrayHashing[Byte].hash(Array(1: Byte)) != 0)
    assert(MurmurHash3.arrayHashing[Char].hash(Array(1: Char)) != 0)
    assert(MurmurHash3.arrayHashing[Short].hash(Array(1: Short)) != 0)
    assert(MurmurHash3.arrayHashing[Int].hash(Array(1: Int)) != 0)
    assert(MurmurHash3.arrayHashing[Long].hash(Array(1: Long)) != 0)
    assert(MurmurHash3.arrayHashing[Float].hash(Array(1: Float)) != 0)
    assert(MurmurHash3.arrayHashing[Double].hash(Array(1: Double)) != 0)
    assert(MurmurHash3.arrayHashing[Boolean].hash(Array(true: Boolean)) != 0)
    assert(MurmurHash3.arrayHashing[Unit].hash(Array((): Unit)) != 0)
  }
}
