object UserName {
  opaque type T = String

  def apply(s: String): T = s
}

type UserName = UserName.T

class Wrapper1[-A] {
  inline def getTypeInfo(inline source: String): String =
    ${ getTypeInfoImpl[A]('source) }
  def createWrapper2 = Wrapper2(this)
}

class Wrapper2[-A](val self: Wrapper1[A]) {
  inline def getTypeInfo(inline source: String): String =
    ${getTypeInfoImpl[A]('source)}
}


@main def Test() = {
  println(getTypeInfo[UserName.T]("UserName.T - Directly"))
  println(getTypeInfo[UserName]("UserName.T - Directly"))

  val foreignWrapper = ForeignWrapper1[UserName.T]()
  println(foreignWrapper.getTypeInfo("ForeignWrapper1[UserName.T]"))
  println(foreignWrapper.createWrapper2.getTypeInfo("ForeignWrapper2[UserName.T]"))

  val foreignWrapper2 = ForeignWrapper1[UserName]()
  println(foreignWrapper2.getTypeInfo("ForeignWrapper1[UserName]"))
  println(foreignWrapper2.createWrapper2.getTypeInfo("ForeignWrapper2[UserName]"))

  val wrapper = Wrapper1[UserName.T]()
  println(wrapper.getTypeInfo("Wrapper1[UserName.T]"))
  println(wrapper.createWrapper2.getTypeInfo("Wrapper2[UserName.T]"))

  val wrapper2 = Wrapper1[UserName]()
  println(wrapper2.getTypeInfo("Wrapper1[UserName]"))
  println(wrapper2.createWrapper2.getTypeInfo("Wrapper2[UserName]"))

}
