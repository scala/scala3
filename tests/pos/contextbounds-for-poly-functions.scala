import scala.language.experimental.modularity
import scala.language.future

trait Ord[X]:
  def compare(x: X, y: X): Int
  type T

trait Show[X]:
  def show(x: X): String

val less0: [X: Ord] => (X, X) => Boolean = ???

val less1 = [X: Ord] => (x: X, y: X) => summon[Ord[X]].compare(x, y) < 0

type PolyTest1 = [X] => X => Ord[X] ?=> Boolean

val less1_type_test: [X: Ord] => (X, X) => Boolean =
  [X: Ord] => (x: X, y: X) => summon[Ord[X]].compare(x, y) < 0

val less2 = [X: Ord as ord] => (x: X, y: X) => ord.compare(x, y) < 0

val less2_type_test: [X: Ord as ord] => (X, X) => Boolean =
  [X: Ord as ord] => (x: X, y: X) => ord.compare(x, y) < 0

type CtxFunctionRef = Ord[Int] ?=> Boolean
type ComparerRef = [X] => (x: X, y: X) => Ord[X] ?=> Boolean
type Comparer = [X: Ord] => (x: X, y: X) => Boolean
val less3: Comparer = [X: Ord as ord] => (x: X, y: X) => ord.compare(x, y) < 0

type CmpRest[X] = X => Boolean
type CmpMid[X] = X => CmpRest[X]
type Cmp3 = [X: Ord] => X => CmpMid[X]
val lessCmp3: Cmp3 = [X: Ord] => (x: X) => (y: X) => (z: X) => summon[Ord[X]].compare(x, y) < 0
val lessCmp3_1: Cmp3 = [X: Ord as ord] => (x: X) => (y: X) => (z: X) => ord.compare(x, y) < 0

// type Cmp[X] = (x: X, y: X) => Boolean
// type Comparer2 = [X: Ord] => Cmp[X]
// val less4: Comparer2 = [X: Ord] => (x: X, y: X) => summon[Ord[X]].compare(x, y) < 0

type CmpWeak[X] = X => Boolean
type Comparer2Weak = [X: Ord] => X => CmpWeak[X]
val less4_0: [X: Ord] => X => X => Boolean =
  [X: Ord] => (x: X) => (y: X) => summon[Ord[X]].compare(x, y) < 0
val less4_1: Comparer2Weak =
  [X: Ord] => (x: X) => (y: X) => summon[Ord[X]].compare(x, y) < 0

val less5 = [X: [X] =>> Ord[X]] => (x: X, y: X) => summon[Ord[X]].compare(x, y) < 0

val less5_type_test: [X: [X] =>> Ord[X]] => (X, X) => Boolean =
  [X: [X] =>> Ord[X]] => (x: X, y: X) => summon[Ord[X]].compare(x, y) < 0

val less6 = [X: {Ord, Show}] => (x: X, y: X) => summon[Ord[X]].compare(x, y) < 0

val less6_type_test: [X: {Ord, Show}] => (X, X) => Boolean =
  [X: {Ord, Show}] => (x: X, y: X) => summon[Ord[X]].compare(x, y) < 0

val less7 = [X: {Ord as ord, Show}] => (x: X, y: X) => ord.compare(x, y) < 0

val less7_type_test: [X: {Ord as ord, Show}] => (X, X) => Boolean =
  [X: {Ord as ord, Show}] => (x: X, y: X) => ord.compare(x, y) < 0

val less8 = [X: {Ord, Show as show}] => (x: X, y: X) => summon[Ord[X]].compare(x, y) < 0

val less8_type_test: [X: {Ord, Show as show}] => (X, X) => Boolean =
  [X: {Ord, Show as show}] => (x: X, y: X) => summon[Ord[X]].compare(x, y) < 0

val less9 = [X: {Ord as ord, Show as show}] => (x: X, y: X) => ord.compare(x, y) < 0

val less9_type_test: [X: {Ord as ord, Show as show}] => (X, X) => Boolean =
  [X: {Ord as ord, Show as show}] => (x: X, y: X) => ord.compare(x, y) < 0

type CmpNested = [X: Ord] => X => [Y: Ord] => Y => Boolean
val less10: CmpNested = [X: Ord] => (x: X) => [Y: Ord] => (y: Y) => true
val less10Explicit: CmpNested = [X] => (x: X) => (ordx: Ord[X]) ?=> [Y] => (y: Y) => (ordy: Ord[Y]) ?=> true

type CmpAlias[X] = X => Boolean
type CmpNestedAliased = [X: Ord] => X => [Y] => Y => CmpAlias[Y]

val less11: CmpNestedAliased = [X: Ord] => (x: X) => [Y] => (y: Y) => (y1: Y) => true
val less11Explicit: CmpNestedAliased = [X] => (x: X) => (ordx: Ord[X]) ?=> [Y] => (y: Y) => (y1: Y) => true

val notationalExample: [X: Ord] => X => [Y: Ord] => Y => Int =
  [X] => (x: X) => (ordx: Ord[X]) ?=> [Y] => (y: Y) => (ordy: Ord[Y]) ?=> 1

val namedConstraintRef = [X: {Ord as ord}] => (x: ord.T) => x
type DependentCmp = [X: {Ord as ord}] => ord.T => Boolean
type DependentCmp1 = [X: {Ord as ord}] => (ord.T, Int) => ord.T => Boolean
val dependentCmp: DependentCmp = [X: {Ord as ord}] => (x: ord.T) => true
val dependentCmp_1: [X: {Ord as ord}] => ord.T => Boolean = [X: {Ord as ord}] => (x: ord.T) => true

val dependentCmp1: DependentCmp1 = [X: {Ord as ord}] => (x: ord.T, y: Int) => (z: ord.T) => true
val dependentCmp1_1: [X: {Ord as ord}] => (ord.T, Int) => ord.T => Boolean =
  [X: {Ord as ord}] => (x: ord.T, y: Int) => (z: ord.T) => true
