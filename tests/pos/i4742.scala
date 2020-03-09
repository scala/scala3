import scala.reflect.ClassTag

class Test {
  def foo[T <: String: ClassTag](f: T => Int) = 1
  def bar(f: String => Int) = foo(f)
}