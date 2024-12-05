object Test {
  type MTWithBind[X] = X match {
    case List[t] => t
  }
}
