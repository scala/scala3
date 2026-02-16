//> using options -language:experimental.modularity -source future

class Context
class Ord[T]

class Ordered:
  type Self

class Monoid[T]
import compiletime.deferred

def IntOrd[A](): Ord[Int] = ???
def ListOrd[A](using Ord[A]): Ord[List[A]] = ???
def curCtx: Context = ???

trait anon1:
  // Simple typeclass
  given Ord[Int]:
    def compare(x: Int, y: Int) = ???

  // Simple type class with extension method:
  given Monoid[Int]:
    extension (x: Int)
      def combine(y: Int) = x + y
    def unit = 0

  // Parameterized typeclass with context bound
  given [A: Ord] => Ord[List[A]]:
    def compare(x: List[A], y: List[A]) = ???

trait anon2:
  // Parameterized typeclass with context parameter
  given [A] => Ord[A] => Ord[List[A]]:
    def compare(x: List[A], y: List[A]) = ???

trait anon3:
  // Parameterized typeclass with named context parameter
  given [A] => (ord: Ord[A]) => Ord[List[A]]:
    def compare(x: List[A], y: List[A]) = ???

  // Simple alias
  given Ord[Int] = IntOrd()

trait anon4:
  // Parameterized alias with context bound
  given [A: Ord] => Ord[List[A]] =
    ListOrd[A]

trait anon5:
  // Parameterized alias with context parameter
  given [A] => Ord[A] => Ord[List[A]] =
    ListOrd[A]

  given [A] => A is Ordered => List[A] is Ordered =
    ???

trait anon6:
  // Parameterized alias with named context parameter
  given [A] => (ord: Ord[A]) => Ord[List[A]] =
    ListOrd[A](using ord)

  given [A] => (A is Ordered) => List[A] is Ordered =
    ???

  // Concrete class instance
  given Context()

trait anon7:
  // Abstract or deferred given
  given Context = deferred

trait anon8:
  // By-name given
  given () => Context = curCtx

trait named:
  given intOrd: Ord[Int]:
    def compare(x: Int, y: Int) = ???

  // Simple type class with extension method:
  given intMonoid: Monoid[Int]:
    extension (x: Int)
      def combine(y: Int) = x + y
    def unit = 0

  // Parameterized typeclass with context bound
  given listOrd: [A: Ord] => Ord[List[A]]:
    def compare(x: List[A], y: List[A]) = ???

  // Parameterized typeclass with context parameter
  given listOrd2: [A] => Ord[A] => Ord[List[A]]:
    def compare(x: List[A], y: List[A]) = ???

  // Parameterized typeclass with named context parameter
  given listOrd3: [A] => (ord: Ord[A]) => Ord[List[A]]:
    def compare(x: List[A], y: List[A]) = ???

  // Simple alias
  given intOrd2: Ord[Int] = IntOrd()

  // Parameterized alias with context bound
  given listOrd4: [A: Ord] => Ord[List[A]] =
    ListOrd[A]

  // Parameterized alias with context parameter
  given listOrd5: [A] => Ord[A] => Ord[List[A]] =
    ListOrd[A]

  // Parameterized alias with named context parameter
  given listOrd6: [A] => (ord: Ord[A]) => Ord[List[A]] =
    ListOrd[A](using ord)

  // Concrete class instance
  given context: Context()

  // Abstract or deferred given
  given context2: Context = deferred

  // By-name given
  given context3: () => Context = curCtx