trait MyOutput[T]
type MyOutputAlias2 = MyOutput

object MyType:
  val v: MyOutputAlias2[String] = ??? // was COMPILATION ERROR

@main def Test = ()
