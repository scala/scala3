// Test that NotNull annotations not in the list are not working in Java files.

class S {
  // TODO: this will not be an error after #7483
  def kk: String = J.k // error: k is not a constant and the NotNull annotation is not in the list

  def ll: String = J.l // error: the NotNull annotation is not in the list
}