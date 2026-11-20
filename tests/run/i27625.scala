// scalajs: --skip

// `MurmurHash3.arrayHash(a, seed)` and its Scala 2 specialized variants must remain in the bytecode
// for binary compatibility with code compiled against earlier versions of the standard library.
// Reflection simulates call sites compiled by Scala 3.10.1 (generic) and Scala 2.13 (specialized).

import scala.util.hashing.MurmurHash3

@main def Test(): Unit =
  val cls = MurmurHash3.getClass
  val seed = 42

  def check[T](arr: Array[T], specializedName: String, specializedArrayClass: Class[?]): Unit =
    val expected = MurmurHash3.arrayHash(arr, seed)

    val generic = cls.getMethod("arrayHash", classOf[Object], classOf[Int])
    assert(generic.invoke(MurmurHash3, arr, seed) == expected, s"generic arrayHash for $specializedName")

    val specialized = cls.getMethod(specializedName, specializedArrayClass, classOf[Int])
    assert(specialized.invoke(MurmurHash3, arr, seed) == expected, s"$specializedName")

  check(Array(true, false, true), "arrayHash$mZc$sp", classOf[Array[Boolean]])
  check(Array[Byte](1, 2, 3), "arrayHash$mBc$sp", classOf[Array[Byte]])
  check(Array('a', 'b', 'c'), "arrayHash$mCc$sp", classOf[Array[Char]])
  check(Array(1.0, 2.0, 3.0), "arrayHash$mDc$sp", classOf[Array[Double]])
  check(Array(1.0f, 2.0f, 3.0f), "arrayHash$mFc$sp", classOf[Array[Float]])
  check(Array(1, 2, 3), "arrayHash$mIc$sp", classOf[Array[Int]])
  check(Array(1L, 2L, 3L), "arrayHash$mJc$sp", classOf[Array[Long]])
  check(Array[Short](1, 2, 3), "arrayHash$mSc$sp", classOf[Array[Short]])
  check(Array((), (), ()), "arrayHash$mVc$sp", classOf[Array[scala.runtime.BoxedUnit]])

  // Value from the issue, as computed by Scala 3.10.1
  val generic = cls.getMethod("arrayHash", classOf[Object], classOf[Int])
  assert(generic.invoke(MurmurHash3, Array(1, 2, 3), seed) == -1198380091)
