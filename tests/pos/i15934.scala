trait ReplicatedData
trait ActorRef[-T] {
  def tell(msg: T): Unit = ???
}

// shared in both domains
abstract class Key[+T1 <: ReplicatedData]

// domain 1
object dd {
  sealed abstract class GetResponse[A1 <: ReplicatedData] {
    def key: Key[A1]
  }
  case class GetSuccess[A2 <: ReplicatedData](key: Key[A2]) extends GetResponse[A2]
  case class GetFailure[A3 <: ReplicatedData](key: Key[A3]) extends GetResponse[A3]
}

// domain 2
object JReplicator {
  final case class Get[A4 <: ReplicatedData](
      key: Key[A4],
      replyTo: ActorRef[GetResponse[A4]]
  )
  sealed abstract class GetResponse[A5 <: ReplicatedData] {
    def key: Key[A5]
  }
  case class GetSuccess[A6 <: ReplicatedData](key: Key[A6]) extends GetResponse[A6]
  case class GetFailure[A7 <: ReplicatedData](key: Key[A7]) extends GetResponse[A7]
}

val _ = null.asInstanceOf[Any] match {
  case cmd: JReplicator.Get[d] =>
    val reply =
      util
        .Try[dd.GetResponse[d]](???)
        .map/*[JReplicator.GetResponse[d]]*/ {
          // Needs at least 2 cases to triger failure
          case rsp: dd.GetSuccess[d1]  => JReplicator.GetSuccess(rsp.key)
          case rsp: dd.GetResponse[d2] => JReplicator.GetFailure(rsp.key)
        }
        // needs recover to trigger failure
        .recover { case _ => new JReplicator.GetFailure(cmd.key) }
    reply.foreach { cmd.replyTo tell _ } // error
}
