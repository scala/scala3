// scalajs: --skip
// (getName() reflection is not supported in ScalaJS)

//> using options -language:experimental.specializedTraits

class Animal
class BigCat extends Animal
class Lion extends BigCat

inline trait A[T: Specialized]:
    def foo = Thread.currentThread.getStackTrace()(1).getClassName()

abstract class methods:
    def bar1(a: A[Int]): Int // A[Int] -> A$sp$Int
    def bar2(a: A[String]): String // A[String] -> A$sp$String
    def bar3(a: A[Lion]): Lion // A[Lion] -> A$sp$Animal
    def bar4(a: A[BigCat]): Lion // A[BigCat] -> A$sp$Animal
    def bar5(a: A[Animal]): Lion // A[Animal] -> A$sp$Animal

    def bar6(a: A[List[Int]]): Lion // A[List[Int]] -> A
    def bar7(a: A[IArray[Boolean]]): Lion // A[IArray[Bool]] -> A
    def bar8(a: A[Object]): Lion // A[Object] -> A
    def bar9(a: A[AnyVal]): Lion // A[AnyVal] -> A
    def bar10(a: A[AnyRef]): Lion // A[AnyRef] -> A

@main def Test =
    val expectedParamErasure = Map(
        "bar1"  -> "A$$sp$scala$Int",
        "bar2"  -> "A$$sp$java$lang$String",
        "bar3"  -> "A$$sp$Animal",
        "bar4"  -> "A$$sp$Animal",
        "bar5"  -> "A$$sp$Animal",
        "bar6"  -> "A",
        "bar7"  -> "A",
        "bar8"  -> "A",
        "bar9"  -> "A",
        "bar10" -> "A",
    )

    val actualParamErasure = classOf[methods].getDeclaredMethods.iterator
        .filter(_.getName.startsWith("bar"))
        .map(m => m.getName -> m.getParameterTypes.head.getName)
        .toMap

    for (name, expected) <- expectedParamErasure do
        val actual = actualParamErasure.getOrElse(
            name, sys.error(s"method $name not found on class methods"))
        assert(actual == expected, s"$name: expected param type $expected, got $actual")
