import java.util.concurrent.CompletionStage
import scala.concurrent.Future

trait ActorRef[-T]:
   def ask[Res](replyTo: ActorRef[Res] => T): Future[Res] = ???

implicit final class FutureOps[T](private val f: Future[T]) extends AnyVal:
  def asJava: CompletionStage[T] = ???

class AskPattern[Req, Res]:
  val actor: ActorRef[Req] = ???
  val messageFactory: ActorRef[Res] => Req = ???

  def failing(): CompletionStage[Res] = actor.ask(messageFactory.apply).asJava
  def workaround1(): CompletionStage[Res] = actor.ask[Res](messageFactory.apply).asJava
  def workaround2(): CompletionStage[Res] = actor.ask(messageFactory).asJava

  val jMessageFactory: java.util.function.Function[ActorRef[Res], Req] = ???
  def originalFailingCase(): CompletionStage[Res] = actor.ask(jMessageFactory.apply).asJava
