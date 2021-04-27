def get: 1 = { println("hi"); 1 }
import get._ // error: import prefix is not a pure expression
val x = get.toLong

