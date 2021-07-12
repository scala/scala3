object Outer {

  object Inner {
    class Bar(x: Int)
    object Bar
  }

  export Inner.Bar._

  val _ = apply(2)  // error (constructor proxies are not exported)

}
object Outer2 {

  object Inner {
    class Bar(x: Int)
    object Bar
  }

  export Inner.Bar.apply  // error: no eligible member

  val _ = apply(2)  // error (constructor proxies are not exported)

}
