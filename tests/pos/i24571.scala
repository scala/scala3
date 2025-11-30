val n: Byte = 2
val n2: Byte | Null = 2
val n3: Int = 2
val n4: Int | Null = 2222
val n5: Int | Byte = 2
val n6: Byte | Int = 10000
val n7: 1 | Null = 1
val n8: Byte | String = 2

val x: Option[Byte] = Option(2)
val x2: Option[Byte] = Option[Byte](2)
val x3: Option[Int] = Option(2)
val x4: Option[Null] = Option(null)
val x5: Option[Byte | Null] = Option(2)
val x6: Option[1 | Null] = Option(1)
val x7: Option[Byte | String] = Option(2)

trait MyOption[+T]

object MyOption:
  def apply[T](x: T | Null): MyOption[T] = ???
  def applyOld[T](x: T): MyOption[T] = ???

val test1: MyOption[Byte] = MyOption(2)
val test2: MyOption[Byte] = MyOption.applyOld(2)
val test3: MyOption[Int] = MyOption(2)
val test4: MyOption[Null] = MyOption(null)
val test5: MyOption[Byte | Null] = MyOption(2)
val test6: MyOption[Byte | String] = MyOption(2)
