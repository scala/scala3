abstract class ZPartialServerEndpoint[R, A, B, I, E, O, -C]
    extends EndpointOps[A, I, E, O, C]{
  override type ThisType[-_R] = ZPartialServerEndpoint[R, A, B, I, E, O, _R]
  override type EndpointType[_A, _I, _E, _O, -_R] =ZPartialServerEndpoint[R, _A, B, _I, _E, _O, _R]
}

trait EndpointOps[A, I, E, O, -R] {
  type EndpointType[_A, _I, _E, _O, -_R]
  type ThisType[-_R]
  def out[T]: EndpointType[A, I, E, T, R]
  def description(d: String): ThisType[R]
}

object Test {
  def basicEndpoint[R](): ZPartialServerEndpoint[R, Any, Any, Unit, Any, Unit, Any] = ???

  // commonts next to `.out[Any]` contain information about compilation time when chaining up to N `out` functions
  val case1 =
     basicEndpoint() // 1.5s
        .out[Any] // 1.6s
        .out[Any] // 1.7s
        .out[Any] // 2s
        .out[Any] // 4s
        .out[Any] // 33s
        .out[Any] // aborted after 5 min
}