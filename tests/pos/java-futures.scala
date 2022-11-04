import java.util.concurrent.{TimeUnit, TimeoutException, Future, Executors => JExecutors}

class TestSource
trait LoggedRunnable extends Runnable



object Test:

  val filteredSources: List[TestSource] = ???

  def encapsulatedCompilation(testSource: TestSource): LoggedRunnable = ???

  def testSuite(): this.type =
    val pool = JExecutors.newWorkStealingPool(Runtime.getRuntime.availableProcessors())
    val eventualResults = for target <- filteredSources yield
      pool.submit(encapsulatedCompilation(target))

    for fut <- eventualResults do
      try fut.get()
      catch case ex: Exception =>
        System.err.println(ex.getMessage)
        ex.printStackTrace()

    this

