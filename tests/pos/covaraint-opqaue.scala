import annotation.unchecked.uncheckedVariance

opaque type O[+T] = Array[T @uncheckedVariance]
