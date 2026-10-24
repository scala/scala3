import caps.{Classifier, SharedCapability, Control}

trait Weird extends Classifier
trait IO extends SharedCapability, Classifier

class Lbl extends Control, Weird:
  def break(): Unit = ()

class FS extends IO, Weird:
  def read(): Unit = ()

