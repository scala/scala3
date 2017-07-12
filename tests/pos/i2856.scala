package io.grpc {
  trait Grpc
}
package bar {
  import io.grpc.Grpc
  object a extends Grpc
}
