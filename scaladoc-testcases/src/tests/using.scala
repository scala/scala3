package tests
package usings

class Named
class ClassTag[T]
given ClassTag[Int] = ClassTag[Int]
given ClassTag[String] = ClassTag[String]

/** ...
 *
 *  @param t: this is an explicit parameter
 *  @param named: this is an implicit paramter
 *  @param ClassTag[String]: this is next implicit parameter
 */
def f[T](t: T)(using ClassTag[Int])(using ClassTag[String])(using named: Named)(using ClassTag[T]): Unit = ???
def f2[T](t: T)(using ClassTag[Int])(using ClassTag[Int], ClassTag[String]): Unit = ???
