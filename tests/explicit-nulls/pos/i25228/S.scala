def createConfig(user: Option[String]): Foo =
  Foo(user.orNull)

def createConfig2(user: Option[String]): Foo =
  Foo(user.getOrElse(null))

def createConfig3(user: String | Null): Foo =
  Foo(user)