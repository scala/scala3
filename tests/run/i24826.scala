trait Client[F[_]]
type Middleware[F[_]] = Client[F] => Client[F]

trait ClientMiddleware[F[_]] extends Middleware[F] {
  def wrapClient(client: Client[F]): Client[F]
  final def apply(client: Client[F]): Client[F] = wrapClient(client)
  final def wrapMiddleware(that: Middleware[F]): ClientMiddleware[F] =
    (client: Client[F]) => wrapClient(that(client))
}

trait IO[T]
def middleware: ClientMiddleware[IO] = (client: Client[IO]) => client

// java.lang.AbstractMethodError: Receiver class i24826$package$$$Lambda/0x00000008000c2800 does not define or inherit an implementation
// of the resolved method 'abstract java.lang.Object apply(java.lang.Object)' of interface scala.Function1.
@main def Test =
  middleware
  .wrapMiddleware(middleware)
  .wrapClient(new Client[IO] {})
