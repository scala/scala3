object type_check_specialization {
  def inner[@specialized(Char) I](i: I): Unit = i.isInstanceOf[Int]
}