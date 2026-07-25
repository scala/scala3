// https://github.com/scala/scala3/issues/15830
import scala.compiletime.{constValue, summonInline}

trait Constraint[T, C]:
  inline def test(value: T): Boolean
  inline def message: String

final class Greater[V <: Int]

inline given [V <: Int]: Constraint[Int, Greater[V]] with
  override inline def test(value: Int): Boolean = value > constValue[V]
  override inline def message: String = "Should be greater"

final class DescribedAs[C, V <: String]

inline given [T, C, Impl <: Constraint[T, C], V <: String](using Impl): Constraint[T, DescribedAs[C, V]] with
  override inline def test(value: T): Boolean = summonInline[Impl].test(value)
  override inline def message: String = constValue[V]

def test() =
  macros.assertCondition(summonInline[Constraint[Int, Greater[0]]].test(1), "test")
  macros.assertCondition(summonInline[Constraint[Int, DescribedAs[Greater[0], "pos"]]].test(1), "test")
