val myNull: Null = null

val n1: Int = null // error
val n2: Int = myNull // error

val b1: Boolean = null // error
val b2: Boolean = myNull // error

val v1: AnyVal = null // error
val v2: AnyVal = myNull // error