// scalajs: --skip

import java.io.*

class Config(s: String)

class ConfigException(@transient val config: Config = null)
    extends java.io.Serializable

object Test {
  def main(args: Array[String]): Unit = {
    val e = new ConfigException(new Config("not serializable"))
    val byteStream = new ByteArrayOutputStream()
    val objectStream = new ObjectOutputStream(byteStream)
    objectStream.writeObject(e)
    objectStream.close()
    val bytes = byteStream.toByteArray()
    val inStream = new ByteArrayInputStream(bytes)
    val inObjectStream = new ObjectInputStream(inStream)
    val copy = inObjectStream.readObject()
    inObjectStream.close()
  }
}
