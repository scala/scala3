final class A
class B extends A // error: cannot extend final class A
class C extends Option[Int] // error: cannot extend sealed class Option in different compilation unit

