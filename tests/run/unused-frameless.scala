import scala.annotation.implicitNotFound

// Subset of shapeless
// ----------------------------------------------------------------------------

sealed trait HList
sealed trait HNil extends HList
final case object HNil extends HNil
final case class ::[H, T <: HList](h: H, t: T) extends HList

/** Generic representation os type T as a labelled sum of product. */
trait LabelledGeneric[T] {
  type Repr
}

final case class R[K <: String, V](v: V)

trait Selector[L <: HList, K, V]

object Selector {
  implicit def caseFound[T <: HList, K <: String, V]
  : Selector[R[K, V] :: T, K, V] = {
    println("Selector.caseFound")
    null
  }

  implicit def caseRecur[H, T <: HList, K <: String, V]
  (implicit i: Selector[T, K, V])
  : Selector[H :: T, K, V] = {
    println("Selector.caseRecur")
    null
  }
}

// Subset of Frameless
// ----------------------------------------------------------------------------

trait Dataset[T] {
  def select[A](c: Column[T, A]): Dataset[A] =
    this.asInstanceOf[Dataset[A]] // Use c.label to do an untyped select on actual Spark Dataset, and
  // cast the result to TypedDataset[A]

  def col[S <: String, A](s: S)(implicit unused ev: Exists[T, s.type, A]) =
    new Column[T, A](s) // ev is only here to check than this is safe, it's
  // never used at runtime!

  def collect(): Vector[T] =
    Vector.empty[T] // Uses collect of the underlying Spark structure plus a cast
}

object Dataset {
  def create[T](values: Seq[T]): Dataset[T] = new Dataset[T] { }
}

/** Expression used in `select`-like constructions.
 *
 *  @tparam T type of dataset
 *  @tparam A type of column/expression
 */
case class Column[T, A](label: String)

// Note: this type could be merged with Selector, but Selector comes from
// shapeless while this is frameless specific.
@implicitNotFound(msg = "No column ${K} in type ${T}")
trait Exists[T, K, V]

object Exists {
  implicit def derive[T, H <: HList, K, V]
  (implicit
   g: LabelledGeneric[T] { type Repr = H },
   s: Selector[H, K, V]
  ): Exists[T, K, V] = {
    println("Exists.derive")
    null
  }
}

object UnusedExists {
  implicit unused def derive[T, H <: HList, K, V]
  (implicit
   g: LabelledGeneric[T] { type Repr = H },
   s: Selector[H, K, V]
  ): Exists[T, K, V] = {
    println("UnusedExists.derive")
    null
  }
}

// X4 Example
// ----------------------------------------------------------------------------

case class X4[A, B, C, D](a: A, b: B, c: C, d: D)

object X4 {
  // Macro generated
  implicit def x4Repr[A, B, C, D]: LabelledGeneric[X4[A, B, C, D]] {
    type Repr = R["a", A] :: R["b", B] :: R["c", C] :: R["d", D] :: HNil
  } = {
    println("X4.x4Repr")
    null
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    val source: Vector[X4[Int, String, Double, Boolean]] =
      Vector(X4(1, "s", 1.1, true), X4(2, "t", 1.2, false))
    val outColl : Vector[Boolean] = source.map(_.d)

    val ds: Dataset[X4[Int, String, Double, Boolean]] =
      Dataset.create(source)

    {
      import UnusedExists._
      println("unused")
      val unusedD = ds.col("d")
      val outSpark1: Vector[Boolean] = ds.select(unusedD).collect()
      // FIXME implement Dataset opertations
      // assert(outSpark1 == outColl)
    }

    println("used")
    val usedD = ds.col("d")
    val outSpark2: Vector[Boolean] = ds.select(usedD).collect()
    // FIXME implement Dataset opertations
    // assert(outSpark2 == outColl)

    println("end")
  }
}
