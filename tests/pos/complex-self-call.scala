// An example extracted from akka that demonstrated a spurious
// "forward reference not allowed from self constructor invocation" error.
class Resizer
class SupervisorStrategy
class Pool
object Pool:
  def defaultSupervisorStrategy: SupervisorStrategy = ???
object Dispatchers:
  def DefaultDispatcherId = ???
object Resizer:
  def fromConfig(config: Config): Option[Resizer] = ???

class Config:
  def getInt(str: String): Int = ???
  def hasPath(str: String): Boolean = ???

final case class BroadcastPool(
    nrOfInstances: Int,
    val resizer: Option[Resizer] = None,
    val supervisorStrategy: SupervisorStrategy = Pool.defaultSupervisorStrategy,
    val routerDispatcher: String = Dispatchers.DefaultDispatcherId,
    val usePoolDispatcher: Boolean = false)
    extends Pool:

  def this(config: Config) =
    this(
      nrOfInstances = config.getInt("nr-of-instances"),
      resizer = Resizer.fromConfig(config),
      usePoolDispatcher = config.hasPath("pool-dispatcher"))
