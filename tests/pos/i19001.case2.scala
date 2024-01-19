import scala.util.{Try, Success, Failure}

trait ActorRef[-T]
trait ActorContext[T]:
  def ask[Req, Res](target: ActorRef[Req], createRequest: ActorRef[Res] => Req)(mapResponse: Try[Res] => T): Unit

@main def Test =
  val context: ActorContext[Int] = ???
  val askMeRef: ActorRef[Request] = ???

  case class Request(replyTo: ActorRef[Int])

  context.ask(askMeRef, Request.apply) {
    case Success(res) => res // error: expected Int, got Any
    case Failure(ex)  => throw ex
  }
