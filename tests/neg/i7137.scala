
object App {
  val any: Any = 3
  val str: String = any + "a" // error
  val str2: String = any2stringadd(any) + "a" // error
  val str3: String = Predef.any2stringadd(any) + "a" // error
}
