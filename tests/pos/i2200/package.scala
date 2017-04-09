package object bar {
  type StreamTree[T] = Stream[Int]
  type DocTree = Fix[StreamTree]
}
