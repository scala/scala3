package concurrent

/** A context that allows to suspend waiting for asynchronous data sources */
trait Async:

  /** Wait for completion of async source `src` and return the result */
  def await[T](src: Async.Source[T]): T

  /** Wait for completion of the first of the sources `src1`, `src2`
   *  @return  `Left(r1)`  if `src1` completed first with `r1`
   *           `Right(r2)` if `src2` completed first with `r2`
  */
  def awaitEither[T1, T2](src1: Async.Source[T1], src2: Async.Source[T2]): Either[T1, T2]

  /** The cancellable runner underlying this async computation. */
  def runner: Cancellable

  /** The scheduler for runnables defined in this async computation */
  def scheduler: Scheduler

object Async:

  /** The currently executing Async context */
  inline def current(using async: Async): Async = async

  /** An asynchronous data source. Sources can be persistent or ephemeral.
   *  A persistent source will always return the same data to calls to `poll`
   *  and pass the same data to calls of `handle`. An ephemeral source might pass new
   *  data in every call. An example of a persistent source is  `Future`. An
   *  example of an ephemeral source is `Channel`.
   */
  trait Source[+T]:
    thisSource =>

    /** Poll whether data is available
     *  @return  The data or None in an option. Depending on the nature of the
     *           source, data might be returned only once in a poll. E.g. if
     *           the source is a channel, a Some result might skip to the next
     */
    def poll: Option[T]

    /** When data is available, pass it to function `k`.
     */
    def handleWith(k: T => Unit): Unit

    def map[U](f: T => U): Source[U] = new Source:
      def poll = thisSource.poll.map(f)
      def handleWith(k: U => Unit): Unit = thisSource.handleWith(f.andThen(k))

  end Source

end Async

