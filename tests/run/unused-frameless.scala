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
  implicit unused def caseFound[T <: HList, K <: String, V]
  : Selector[R[K, V] :: T, K, V] = null

  implicit unused def caseRecur[H, T <: HList, K <: String, V]
  (implicit i: Selector[T, K, V])
  : Selector[H :: T, K, V] = null
}

// Subset of Frameless
// ----------------------------------------------------------------------------

trait Dataset[T] {
  def select[A](unused c: Column[T, A]): Dataset[A] =
    ??? // Use c.label to do an untyped select on actual Spark Dataset, and
  // cast the result to TypedDataset[A]

  def col[S <: String, A](s: S)(implicit unused ev: Exists[T, s.type, A]) =
    new Column[T, A](s) // ev is only here to check than this is safe, it's
  // never used at runtime!

  def collect(): Vector[T] =
    ??? // Uses collect of the underlying Spark structure plus a cast
}

object Dataset {
  def create[T](values: Seq[T]): Dataset[T] =
    ???
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
  (implicit unused
   g: LabelledGeneric[T] { type Repr = H },
   s: Selector[H, K, V]
  ): Exists[T, K, V] = null
}

// X4 Example
// ----------------------------------------------------------------------------

case class X4[A, B, C, D](a: A, b: B, c: C, d: D)

object X4 {
  // Macro generated
  implicit def x4Repr[A, B, C, D]: LabelledGeneric[X4[A, B, C, D]] {
    type Repr = R["a", A] :: R["b", B] :: R["c", C] :: R["d", D] :: HNil
  } = null
}

object Demo {
  val source: Vector[X4[Int, String, Double, Boolean]] =
    Vector(X4(1, "s", 1.1, true), X4(2, "t", 1.2, false))

  val ds: Dataset[X4[Int, String, Double, Boolean]] =
    Dataset.create(source)

  val D = ds.col("d")

  val outSpark: Vector[Boolean] = ds.select(D).collect()
  val outColl : Vector[Boolean] = source.map(_.d)

  assert(outSpark == outColl)
}
