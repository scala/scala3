// Compiled without capture checking, so Setup.checkClassifiedInheritance does not run.
import caps.{Classifier, SharedCapability, Control}

trait Weird extends Classifier
trait IO extends SharedCapability, Classifier

class Lbl extends Control, Weird:   // classifier Control: Weird is not a capability
  def break(): Unit = ()

class Both extends Control, IO:     // two unrelated classifiers: unclassified
  def break(): Unit = ()

