object nothing_specialization {
  def ret_nothing[@specialized(Char) T] = {
    def apply[@specialized(Char) X](xs : X*) : List[X] = List(xs:_*)
    def apply6[@specialized(Char) X](xs : Nothing*) : List[Nothing] = List(xs: _*)
    def apply2[@specialized(Long) U] = 1.asInstanceOf[U]
  }
}
