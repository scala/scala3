enum A { // error
  def newA(tag: Int) = new A { val ordinal = tag } // error 
}
