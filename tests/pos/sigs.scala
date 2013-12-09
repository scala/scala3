object sigs {
  
  type Lst[A] = List[A]
  
  type Twin[B] = (B, B)
  
  var x = 7 * 9
  
  class Base {
    
    def foo(x: Int): Any = 33
    
    def foo: Object = "x"
    
  }
  
  class Sub extends Base {
    
   override def foo = "abc"
      
   override def foo(x: Int) = "abc"
  }


}