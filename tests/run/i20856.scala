// scalajs: --skip

import java.io.*

class Message(content: String) extends Serializable:
  //@transient
  lazy val bomb: String =
    Thread.sleep(200)
    "BOMB: " + content
end Message

object Test:
  def serialize(obj: Message): Array[Byte] =
    val byteStream = ByteArrayOutputStream()
    val objectStream = ObjectOutputStream(byteStream)
    try
      objectStream.writeObject(obj)
      byteStream.toByteArray
    finally
      objectStream.close()
      byteStream.close()
  end serialize

  def deserialize(bytes: Array[Byte]): Message =
    val byteStream = ByteArrayInputStream(bytes)
    val objectStream = ObjectInputStream(byteStream)
    try
      objectStream.readObject().asInstanceOf[Message]
    finally
      objectStream.close()
      byteStream.close()
  end deserialize

  def main(args: Array[String]): Unit =
    val bytes =
      val msg = Message("test")

      val touch = Thread(() => {
        msg.bomb // start evaluation before serialization
        ()
      })
      touch.start()

      Thread.sleep(50) // give some time for the fork to start lazy val rhs eval

      serialize(msg) // serialize in the meantime so that we capture Waiting state
    end bytes

    val deserializedMsg = deserialize(bytes)

    @volatile var msg = ""
    @volatile var started = false
    val read = Thread(() => {
      started = true
      msg = deserializedMsg.bomb
      ()
    })
    read.start()

    Thread.sleep(1000)
    if !started then
      throw Exception("ouch, the thread has not started yet after 1s")

    if !msg.isEmpty() then
      println(s"succeeded: $msg")
    else
      read.interrupt()
      throw new AssertionError("failed to read bomb in 1s!")
  end main
end Test
