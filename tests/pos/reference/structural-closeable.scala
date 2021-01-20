type Closeable = {
  def close(): Unit
}

class FileInputStream with
  def close(): Unit = ()

class Channel with
  def close(): Unit = ()

import scala.reflect.Selectable.reflectiveSelectable

def autoClose(f: Closeable)(op: Closeable => Unit): Unit =
  try op(f) finally f.close()