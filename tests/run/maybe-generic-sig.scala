//> using options -Yexplicit-nulls
// scalajs: --skip
import language.experimental.errorHandling

// Java generic signatures of maybe types follow their erasure: `T ? E` is
// represented as `T` if it erases to the erasure of `T`, and as `Object` otherwise.

class C:
  def f(x: String?): String? = x
  def f1[T <: String](x: T?): String = ""
  def f2[T <: String?](x: T): String = ""
  def f3(x: List[String]?): List[String]? = x
  def f4[T](x: List[T]?): List[T]? = x
  def f5(x: Int?): Int? = x                                     // Object: Int is not a reference type
  def f6(x: String ? Exception): String ? Exception = x         // Object: a Fail may be returned
  def f7[T](x: T?): T? = x                                      // Object: T may be null
  def f8[T <: (String ? Exception)](x: T): T = x               // Object bound

@main def Test =
  for m <- classOf[C].getDeclaredMethods.sortBy(_.getName) do
    println(m.toGenericString)
