object Messages {
  private var logs: Int = 0
  def logMessage(a: String): Unit =
    logs += 1
    println(a)
  def count = logs
}

@main def Test: Unit =
  assert(Messages.count == 0)
  visitExportsExprMap(new Logger { export Messages.{logMessage => log} })(
    _.log("visited exports with ExprMap")
  )
  assert(Messages.count == 1)
  visitExportsTreeMap(new Logger { export Messages.{logMessage => log} })(
    _.log("visited exports with TreeMap")
  )
  assert(Messages.count == 2)
  visitExportsTreeAccumulator(new Logger { export Messages.{logMessage => log}; export Messages.count })(
    exportStrings => println(s"extracted with TreeAccumulator: $exportStrings")
  )
  println("reflection show:")
  visitExportsShow({ object Observer { export Messages.count } })
  println("reflection show extractors:")
  visitExportsShowExtract({ object Observer { export Messages.count } })
  val localLogger = new Logger { def log(a: String): Unit = println(a) }
  visitExportsSplice(localLogger).log("visited exports with splice")
  visitExportsSpliceInverse(logger => new Logger {
    private val delegate = logger
    export delegate._
  }).log("visited exports with splice inverted")
