// https://github.com/scala/scala3/issues/13468
trait Container[T]
opaque type MyOpaque[W <: Int] = Int
extension [W <: Int](myOpaque: MyOpaque[W]) def getContainer: Container[W] = ???
