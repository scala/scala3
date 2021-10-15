package tests
package fboundedTypeParameters

trait AOps[X[_]]
trait A[T] extends AOps[A]
