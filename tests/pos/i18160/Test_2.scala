class SynchronizedReevaluation
class SynchronizedReevaluationApi[Api <: RescalaInterface](val api: Api){
  import api._

  def SynchronizedReevaluation[A](evt: Event[A])(implicit
      turnSource: CreationTicket
  ): (SynchronizedReevaluation, Event[A]) = {
    val sync = new SynchronizedReevaluation
    (sync, evt.map(identity)(turnSource))
  }
}
