import language.experimental.captureChecking
trait IO
def withIO(op: IO^ => Unit): Unit = ???
def test(): Unit =
  withIO: io1 =>
    var myIO: IO^ = io1  // error: separation
    def setIO(io: IO^): Unit =
      myIO = io  // error, level mismatch
    withIO(setIO)
    withIO: (io2: IO^) =>
      myIO = io2  // error, level mismatch
    withIO: io3 =>
      myIO = io3  // error, level mismatch
