import language.experimental.namedTuples
import NamedTuple.*

@FailsWith[HttpError]
trait GreetService derives HttpService:
  @HttpInfo("GET", "/greet/{name}")
  def greet(@HttpPath name: String): String
  @HttpInfo("POST", "/greet/{name}")
  def setGreeting(@HttpPath name: String, @HttpBody greeting: String): Unit

@main def Test =

  val e = HttpService.endpoints[GreetService]

  println(e.greet.describe)
  println(e.setGreeting.describe)

  // Type-safe server logic, driven by the ops-mirror,
  // requires named tuple with same labels in the same order,
  // and function that matches the required signature.
  val logic = e.serverLogic:
    (
      greet = (name) => Right("Hello, " + name),
      setGreeting = (name, greeting) => Right(())
    )

  val server = ServerBuilder()
    .handleAll(logic)
    .create(port = 8080)

  sys.addShutdownHook(server.close())

end Test

// IMPLEMENTATION DETAILS FOLLOW

/** Assume existence of macro to generate this */
given (OpsMirror.Of[GreetService] {
  type MirroredType = GreetService
  type OperationLabels = ("greet", "setGreeting")
  type Operations = (
    OpsMirror.Operation { type InputTypes = (String *: EmptyTuple); type OutputType = String; type ErrorType = HttpError },
    OpsMirror.Operation { type InputTypes = (String *: String *: EmptyTuple); type OutputType = Unit; type ErrorType = HttpError }
  )
}) = new OpsMirror:
  type MirroredType = GreetService
  type OperationLabels = ("greet", "setGreeting")
  type Operations = (
    OpsMirror.Operation { type InputTypes = (String *: EmptyTuple); type OutputType = String; type ErrorType = HttpError },
    OpsMirror.Operation { type InputTypes = (String *: String *: EmptyTuple); type OutputType = Unit; type ErrorType = HttpError }
  )

object OpsMirror:
  type Of[T] = OpsMirror { type MirroredType = T }

  type Operation_I[I <: Tuple] = Operation { type InputTypes = I }
  type Operation_O[O] = Operation { type OutputType = O }
  type Operation_E[E] = Operation { type ErrorType = E }

  trait Operation:
    type InputTypes <: Tuple
    type OutputType
    type ErrorType

trait OpsMirror:
  type MirroredType
  type OperationLabels <: Tuple
  type Operations <: Tuple

trait HttpService[T]:
  def route(str: String): Route
trait Route

type Func[I <: Tuple, O, E] = I match
  case EmptyTuple => Either[E, O]
  case t *: EmptyTuple => t => Either[E, O]
  case t *: u *: EmptyTuple => (t, u) => Either[E, O]

type ToFunc[T] = T match
  case HttpService.Endpoint[i, o, e] => Func[i, o, e]

final class FailsWith[E] extends scala.annotation.Annotation
final class HttpInfo(method: String, route: String) extends scala.annotation.Annotation
final class HttpBody() extends scala.annotation.Annotation
final class HttpPath() extends scala.annotation.Annotation

sealed trait HttpError

object HttpService:
  opaque type Endpoint[I <: Tuple, O, E] = Route

  extension [I <: Tuple, O, E](e: Endpoint[I, O, E])
    def describe: String = ??? // some thing that looks inside the Route to debug it

  type ToEndpoints[Ops <: Tuple] <: Tuple = Ops match
    case EmptyTuple => EmptyTuple
    case op *: ops => (op, op, op) match
      case (OpsMirror.Operation_I[i]) *: (OpsMirror.Operation_O[o]) *: (OpsMirror.Operation_E[e]) *: _ =>
        Endpoint[i, o, e] *: ToEndpoints[ops]

  trait Handler

  class Endpoints[T](val model: HttpService[T]) extends Selectable:
    type Fields <: AnyNamedTuple
    def selectDynamic(name: String): Route = model.route(name)

    def serverLogic(funcs: NamedTuple[Names[Fields], Tuple.Map[DropNames[Fields], ToFunc]]): List[Handler] = ???

  def derived[T](using OpsMirror.Of[T]): HttpService[T] = ??? // inline method to create routes

  def endpoints[T](using model: HttpService[T], m: OpsMirror.Of[T]): Endpoints[T] {
      type Fields = NamedTuple[m.OperationLabels, ToEndpoints[m.Operations]]
  } =
    new Endpoints(model) { type Fields = NamedTuple[m.OperationLabels, ToEndpoints[m.Operations]] }

class ServerBuilder():
  def handleAll(hs: List[HttpService.Handler]): this.type = this
  def create(port: Int): Server = Server()

class Server():
  def close(): Unit = ()
