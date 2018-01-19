trait Iterator[A] {
  def next(): A
}

class IntMapIterator[T] extends Iterator[T] {
  def next: T = ??? // error: overriding method next in trait Iterator of type (): IntMapIterator.this.T;
                    // method next of type => IntMapIterator.this.T has incompatible type
}
