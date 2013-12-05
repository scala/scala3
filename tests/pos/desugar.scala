object desugar {

  // variables
  var x: Int = 2
  var y = x * x
  
  { var z: Int = y }
 
  def foo0(first: Int, second: Int = 2, third: Int = 3) = first + second
  def foo1(first: Int, second: Int = 2)(third: Int = 3) = first + second
  def foo2(first: Int)(second: Int = 2)(third: Int = 3) = first + second

}