object Main {
    abstract class Factory[T <: Int] {
      def size: T
      def create: Array[T] = Array.ofDim(size) // error: No ClassTag available
    }
}
