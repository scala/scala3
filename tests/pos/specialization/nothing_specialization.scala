object nothing_specialization {
  def ret_nothing[@specialized T] = {
    //val a: List[T] = List[Nothing]()
    def apply[@specialized X](xs : X*) : List[X] = List(xs:_*)
    def apply6[@specialized X](xs : Nothing*) : List[Nothing] = List(xs: _*)
  }
}
