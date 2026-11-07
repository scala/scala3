import language.experimental.captureChecking
import caps.{any, Control}
import scala.util.Try

def runOnNewThread(body: () ->{any.except[Control]} Unit): Unit = ()
def onlyIO(f: () ->{any.only[IO]} Unit): Unit = ()
def onlyControl(f: () ->{any.only[Control]} Unit): Unit = ()

def test1(l: Lbl^): Unit = runOnNewThread(() => l.break()) // error
def test2(l: Lbl^): Unit = onlyIO(() => l.break()) // error
def test3(l: Lbl^): Try[Unit] = Try(l.break()) // error
def test4(l: Lbl^): Unit = onlyControl(() => l.break()) // ok

def test5(b: Both^): Unit = runOnNewThread(() => b.break()) // error
def test6(b: Both^): Try[Unit] = Try(b.break()) // error

// An intersection with one of its classifiers does not make `Both` classified.
def test7(b: (Both & IO)^): Unit = runOnNewThread(() => b.break()) // error
def test8(b: (Both & IO)^): Try[Unit] = Try(b.break()) // error
