trait MillRpcMessage {
  type Response
}

trait MillRpcChannel {
  def apply(requestId: Long, input: MillRpcMessage): input.Response
}

object MillRpcChannel {
  def createChannel: MillRpcChannel = {
    // Note that the signature is wrong here
    (msg: String) => ???
  }
}
