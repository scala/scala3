type Closeable = {
  def close(): Unit
}

class FileInputStream:
  def close(): Unit = ()

class Channel:
  def close(): Unit = ()

import scala.reflect.Selectable.reflectiveSelectable

def autoClose(f: Closeable)(op: Closeable => Unit): Unit =
  try op(f) finally f.close()