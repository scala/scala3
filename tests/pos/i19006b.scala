import java.util.function.Function

trait HttpClient
trait HttpRequest
trait HttpResponse
trait ClientRequestContext

trait DecoratingHttpClientFunction {
  def execute(delegate: HttpClient, ctx: ClientRequestContext, req: HttpRequest): HttpResponse
}

class AbstractClientOptionsBuilder:
  def decorator(fn: Function[? <: HttpClient, ? <: HttpClient]): AbstractClientOptionsBuilder = ???
  def decorator(fn: DecoratingHttpClientFunction): AbstractClientOptionsBuilder = ???

class WebClientBuilder extends AbstractClientOptionsBuilder:
  override def decorator(fn: Function[? <: HttpClient, ? <: HttpClient]): WebClientBuilder = ???
  override def decorator(fn: DecoratingHttpClientFunction): WebClientBuilder = ???

class ArmeriaClientBuilder[F[_]]:
  type DecoratingFunction = (HttpClient, ClientRequestContext, HttpRequest) => HttpResponse
  def clientBuilder: WebClientBuilder = ???

  def withDecorator(decorator: DecoratingFunction): ArmeriaClientBuilder[F] = {
    clientBuilder.decorator(decorator(_, _, _))
    this
  }
