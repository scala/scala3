import language.experimental.captureChecking
import caps.{any, Control}
import scala.util.Try

def runOnNewThread(body: () ->{any.except[Control]} Unit): Unit = ()
def onlyIO(f: () ->{any.only[IO]} Unit): Unit = ()
def onlyControl(f: () ->{any.only[Control]} Unit): Unit = ()

// A non-capability trait extending `Classifier` does not change the classifier
// of a capability class that mixes it in.
def test1(l: Lbl^): Unit = onlyControl(() => l.break())
def test2(fs: FS^): Unit = runOnNewThread(() => fs.read())
def test3(fs: FS^): Unit = onlyIO(() => fs.read())
def test4(fs: FS^): Try[Unit] = Try(fs.read())

