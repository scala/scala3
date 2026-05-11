import language.experimental.captureChecking
import language.experimental.separationChecking
import caps.*

trait Collection[+T] extends caps.Stateful, caps.ExclusiveCapability:
  type Index

  def at(index: Index): T
  def nextOf(index: Index): Index
  def startIndex: Index
  def endIndex: Index

extension [T](coll: Collection[T])
  def findByIndex(pred: coll.Index ->{any, coll.rd} Boolean): coll.Index =
    var idx = coll.startIndex
    while idx != coll.endIndex && !pred(idx) do idx = coll.nextOf(idx)
    idx

  def find(pred: T -> {any, coll.rd} Boolean): coll.Index =
    findByIndex(idx => pred(coll.at(idx)))
