package e

import caps.*

trait A[T]
trait B[T] extends caps.SharedCapability

def wantsA1[C^, T: (A^{C})](x: T): A[T] = summon[A[T]] // error
def wantsA2[T: (A^)](x: T): A[T] = summon[A[T]]        // error
def wantsA3[T: B](x: T): B[T] = summon[B[T]]           // ok

