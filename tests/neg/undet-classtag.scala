object Test {
  def f[T: reflect.ClassTag](x: T) = ???

  f(???) // error: undetermined ClassTag
}
