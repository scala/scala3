import language.experimental.captureChecking
import caps.{any, Classifier, SharedCapability, Control}

trait IO extends SharedCapability, Classifier
trait Async extends Control

def runOnNewThread(body: () ->{any.except[Control]} Unit): Unit = ()
def onlyIO(f: () ->{any.only[IO]} Unit): Unit = ()
def onlyControl(f: () ->{any.only[Control]} Unit): Unit = ()

// `Control & IO` mixes two unrelated classifiers. It is treated as unclassified,
// not as classified by `Nothing`, which would be disjoint from every classifier.
def test1(x: (Control & IO)^): Unit = runOnNewThread(() => x.toString) // error
def test2(x: (Control & IO)^): Unit = onlyIO(() => x.toString) // error

// Intersections of related classifiers keep the least one.
def test3(x: (Async & SharedCapability)^): Unit = onlyControl(() => x.toString) // ok
def test4(x: (Async & SharedCapability)^): Unit = runOnNewThread(() => x.toString) // error

