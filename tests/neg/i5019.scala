enum A { // error
  def newA(tag: Int) = new A { val enumTag = tag } // error 
}
