import scala.language.experimental.modularity
import scala.language.future

trait Ord[X]:
  def compare(x: X, y: X): Int

trait Show[X]:
  def show(x: X): String

val less0: [X: Ord] => (X, X) => Boolean = ???

val less1 = [X: Ord] => (x: X, y: X) => summon[Ord[X]].compare(x, y) < 0

val less1_type_test: [X: Ord] => (X, X) => Boolean =
  [X: Ord] => (x: X, y: X) => summon[Ord[X]].compare(x, y) < 0

val less2 = [X: Ord as ord] => (x: X, y: X) => ord.compare(x, y) < 0

val less2_type_test: [X: Ord as ord] => (X, X) => Boolean =
  [X: Ord as ord] => (x: X, y: X) => ord.compare(x, y) < 0

type CtxFunctionRef = Ord[Int] ?=> Boolean
type ComparerRef = [X] => (x: X, y: X) => Ord[X] ?=> Boolean
type Comparer = [X: Ord] => (x: X, y: X) => Boolean
val less3: Comparer = [X: Ord as ord] => (x: X, y: X) => ord.compare(x, y) < 0

// type Cmp[X] = (x: X, y: X) => Boolean
// type Comparer2 = [X: Ord] => Cmp[X]
// val less4: Comparer2 = [X: Ord] => (x: X, y: X) => summon[Ord[X]].compare(x, y) < 0

// type CmpWeak[X] = (x: X, y: X) => Boolean
// type Comparer2Weak = [X: Ord] => (x: X) => CmpWeak[X]
// val less4: Comparer2Weak = [X: Ord] => (x: X) => (x: X, y: X) => summon[Ord[X]].compare(x, y) < 0

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
