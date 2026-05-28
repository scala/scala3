//> using options -language:experimental.specializedTraits

class Animal
class BigCat extends Animal
class Lion extends BigCat

inline trait A[T: Specialized]:
    def foo = Thread.currentThread.getStackTrace()(1).getClassName()

abstract class methods:
    // Specialization takes place
    def bar1(a: A[Int]): Unit    = assert(a.foo == "A$impl$scala$Int", a.foo)
    def bar2(a: A[String]): Unit = assert(a.foo == "A$impl$java$lang$String", a.foo)
    def bar3(a: A[Lion]): Unit   = assert(a.foo == "A$impl$Animal", a.foo)
    def bar4(a: A[BigCat]): Unit = assert(a.foo == "A$impl$Animal", a.foo)
    def bar5(a: A[Animal]): Unit = assert(a.foo == "A$impl$Animal", a.foo)

    // SpecType is a top class -> no $impl$ class generated, just an anonymous class.
    def bar6(a: A[List[Int]]): Unit       = assert(!a.foo.contains("$impl$"), a.foo)
    def bar7(a: A[IArray[Boolean]]): Unit = assert(!a.foo.contains("$impl$"), a.foo)
    def bar8(a: A[Object]): Unit          = assert(!a.foo.contains("$impl$"), a.foo)
    def bar9(a: A[AnyVal]): Unit          = assert(!a.foo.contains("$impl$"), a.foo)
    def bar10(a: A[AnyRef]): Unit         = assert(!a.foo.contains("$impl$"), a.foo)

@main def Test =
    val m = new methods {}
    m.bar1(new A[Int]() {})
    m.bar2(new A[String]() {})
    m.bar3(new A[Lion]() {})
    m.bar4(new A[BigCat]() {})
    m.bar5(new A[Animal]() {})
    m.bar6(new A[List[Int]]() {})
    m.bar7(new A[IArray[Boolean]]() {})
    m.bar8(new A[Object]() {})
    m.bar9(new A[AnyVal]() {})
    m.bar10(new A[AnyRef]() {})
