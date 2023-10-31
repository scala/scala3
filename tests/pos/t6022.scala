class Test {
  (null: Any) match {
    case x: AnyRef if false =>
    case list: Option[?] =>
    case product: Product => // change Product to String and it's all good
  }
}
