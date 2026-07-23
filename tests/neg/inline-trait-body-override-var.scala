//> using options -language:experimental.inlineTraits
inline trait A:
  var x: Int = 1

class B extends A:
  override var x = 2 // error
