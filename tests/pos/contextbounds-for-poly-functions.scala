import scala.language.experimental.modularity
import scala.language.future


trait Ord[X]:
  def compare(x: X, y: X): Int

val less1 = [X: Ord] => (x: X, y: X) => summon[Ord[X]].compare(x, y) < 0

// type Comparer = [X: Ord] => (x: X, y: X) => Boolean
// val less2: Comparer = [X: Ord as ord] => (x: X, y: X) => ord.compare(x, y) < 0

// type Cmp[X] = (x: X, y: X) => Boolean
// type Comparer2 = [X: Ord] => Cmp[X]
// val less3: Comparer2 = [X: Ord] => (x: X, y: X) => summon[Ord[X]].compare(x, y) < 0
