trait MyOutput[T]
final type MyOutputAlias1[T] = MyOutput[T]
final type MyOutputAlias2 = MyOutput

trait MyType
object MyType {
  val value1: MyOutput[MyType] = ???
  val value2: MyOutputAlias1[MyType] = ???
  val value3: MyOutputAlias2[MyType] = ??? // was COMPILATION ERROR
}

val value1: MyOutput[MyType] = ???
val value2: MyOutputAlias1[MyType] = ???
val value3: MyOutputAlias2[MyType] = ???
