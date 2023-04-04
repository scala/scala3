package dotty.tools.benchmarks.lazyvals
import java.util.concurrent.CountDownLatch
object LazyVals {

    trait Foo
    class Bar1 extends Foo
    class Bar2 extends Foo
    class Bar3 extends Foo
    class Bar4 extends Foo
    class Bar5 extends Bar4

    class LazyStringHolder {

        lazy val value: String = {
            System.nanoTime() % 5 match {
                case 0 => "abc"
                case 1 => "def"
                case 2 => "ghi"
                case 3 => "jkl"
                case 4 => "mno"
            }
        }
    }

    class LazyHolder {

        lazy val value: List[Int] = {
            System.nanoTime() % 5 match {
                case 0 => 1 :: 2 :: Nil
                case 1 => Nil
                case 2 => 1 :: Nil
                case 3 => Nil
                case 4 => 1 :: 2 :: 3 :: Nil
            }
        }
    }

    class LazyGenericHolder[A](v: => A) {
        lazy val value: A = v
    }

    class LazyAnyHolder {
        lazy val value: Any = {
            System.nanoTime() % 5 match {
                case 0 => new Bar1
                case 1 => new Bar2
                case 2 => new Bar3
                case 3 => new Bar4
                case 4 => new Bar4
            }
        }
    }

    class LazyIntHolder {
        lazy val value: Int = {
            (System.nanoTime() % 1000).toInt
        }
    }

    object ObjectHolder {
        lazy val value: String = {
            System.nanoTime() % 5 match {
                case 0 => "abc"
                case 1 => "def"
                case 2 => "ghi"
                case 3 => "jkl"
                case 4 => "mno"
            }
        }
    }
}
