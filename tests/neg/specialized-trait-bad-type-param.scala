//> using options -experimental -language:experimental.specializedTraits

object Snippet {
  inline trait RecyclingBin[-T: Specialized]:
      def recycle(x: T) = println(s"Recycling ${x}")

  def recycleAnInteger(bin: RecyclingBin[Int]) = bin.recycle(100)
  def recycleAnInteger(bin: Int) = ()
  
  recycleAnInteger(new RecyclingBin[Anyval]() {}) // error
  //                                ^^^^^^
  //                                Not found: type Anyval - did you mean AnyVal?
}
