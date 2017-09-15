object Test {
  def main(args: Array[String]): Unit = {
    println(strawman.collection.Hash.hash(42))
  }
}

package strawman {
  package collection {
    object Hash {
      def hash(n: Int) = Hashing.computeHash(n)
    }
  }
}
