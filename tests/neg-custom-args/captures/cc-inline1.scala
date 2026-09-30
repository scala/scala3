import scala.language.experimental.{captureChecking}
import caps.unsafe.untrackedCaptures

object Fs {

  class FileHandle {
    @untrackedCaptures var isClosed = false
    def write(x: Int): Unit = {
      if isClosed then
        throw new IllegalStateException("File is closed")
      else
        println(s"Writing $x to file")
    }
    def close(): Unit = {
      isClosed = true
    }
  }

  inline def useFile[T](body: FileHandle^ => T): T =
    val fileHandle: FileHandle = new FileHandle
    try {
      body(fileHandle)
    } finally {
      fileHandle.close()
    }

  inline def useFile1[T](inline body: FileHandle^ => T): T =
    val fileHandle: FileHandle^ = new FileHandle
    try {
      body(fileHandle)
    } finally {
      fileHandle.close()
    }

  inline def useFile1Leaky[T](inline body: FileHandle^ => T): T =
    val fileHandle: FileHandle = new FileHandle   // need to be declared FileHandle^, or its not a capability
    try {
      body(fileHandle)
    } finally {
      fileHandle.close()
    }

  transparent inline def useFile2[T](body: FileHandle^ => T): T =
    val fileHandle: FileHandle = new FileHandle
    try {
      body(fileHandle)
    } finally {
      fileHandle.close()
    }

  transparent inline def useFile3[T](inline body: FileHandle^ => T): T =
    val fileHandle: FileHandle^ = new FileHandle
    try {
      body(fileHandle)
    } finally {
      fileHandle.close()
    }
}
// Example.scala
import scala.collection.immutable.LazyListIterable

def testInline = {
  val llist = LazyListIterable(1, 2, 3)
  val fileHandle: Fs.FileHandle^ = new Fs.FileHandle()
  val leaked1 = Fs.useFile(f => llist.map({x => f.write(x); x + 1})) // error
  val leaked2 = Fs.useFile(l => l) // error
  val leaked3 = Fs.useFile(f => () => f.write(42)) // error
  val leaked4 =
    val proxy: (f: Fs.FileHandle^) -> () -> Unit = f => () => f.write(42) // error
    ()
}
def testInline1 = {
  val llist = LazyListIterable(1, 2, 3)
  val leaked1 = Fs.useFile1(f => llist.map({x => f.write(x); x + 1}))
  val _: LazyListIterable[Int] = leaked1 // error
  val leaked2 = Fs.useFile1(l => l)
  val _: Fs.FileHandle = leaked2 // error
  val leaked3 = Fs.useFile1(f => () => f.write(42))
  val _: () -> Unit = leaked3 // error
}
def testTransparent = {
  val llist = LazyListIterable(1, 2, 3)
  val leaked1 = Fs.useFile2(f => llist.map({x => f.write(x); x + 1})) // error
  val leaked2 = Fs.useFile2(l => l) // error
  val leaked3 = Fs.useFile2(f => () => f.write(42)) // error
}
def testTransparent1 = {
  val llist = LazyListIterable(1, 2, 3)
  val leaked1 = Fs.useFile3(f => llist.map({x => f.write(x); x + 1}))
  val _: LazyListIterable[Int] = leaked1 // error
  val leaked2 = Fs.useFile3(l => l)
  val _: Fs.FileHandle = leaked2 // error
  val leaked3 = Fs.useFile3(f => () => f.write(42))
  val _: () -> Unit = leaked3 // error
}

def testInlineLeaky = {
  val llist = LazyListIterable(1, 2, 3)
  val leaked1 = Fs.useFile1Leaky(f => llist.map({x => f.write(x); x + 1}))
  val _: LazyListIterable[Int] = leaked1 // ok
  val leaked2 = Fs.useFile1Leaky(l => l)
  val _: Fs.FileHandle = leaked2 // ok
  val leaked3 = Fs.useFile1Leaky(f => () => f.write(42))
  val _: () -> Unit = leaked3 // ok
}

