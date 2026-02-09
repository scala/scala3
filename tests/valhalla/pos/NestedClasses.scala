import scala.annotation.valhalla

class Graph:
  class Node:
    var connectedNodes: List[Node] = Nil
    def connectTo(node: Node): Unit =
      if !connectedNodes.exists(node.equals) then
        connectedNodes = node :: connectedNodes

  var nodes: List[Node] = Nil
  def newNode: Node =
    val res = Node()
    nodes = res :: nodes
    res

@valhalla
class IntList(val list: List[Int], val size: Int) extends AnyVal:
  class IntListIterator:
    var nextIndex = 0;
        
    def hasNext(): Boolean = (nextIndex <= size - 1)

    def next(): Int = {
      val res = list(nextIndex)
      nextIndex+=1
      res
    }

  def appendFront(e: Int): IntList =
    IntList(e :: list, size + 1)

  def show(): Unit = {
    val iterator = IntListIterator()

    while (iterator.hasNext()) {
      print(iterator.next() + " ");
    }
    println();
  }

class Book:
  @valhalla class Chapter(val index: Int, val content: String, val lastEdited: Int) extends AnyVal:
    def update(time: Int): Chapter =
      Chapter(index, content, time)
    def read: Unit =
      println(s"Chapter ${index}\nLast edited on ${lastEdited}\n${content}")

  var chapters: scala.collection.mutable.ListBuffer[Chapter] = scala.collection.mutable.ListBuffer.empty[Chapter]
  var numChapters = 0

  def newChapter(content: String, time: Int): Chapter =
    numChapters+=1
    val res = Chapter(numChapters, content, time)
    chapters = chapters :+ res
    res
  def read: Unit =
    for chapter <- chapters do chapter.read

class Main:
  def main = {
    val graph = new Graph
    for i <- 1 to 10 do graph.newNode

    val intList = new IntList(Nil, 0)
    val newlist1 = intList.appendFront(1)
    val newlist2 = newlist1.appendFront(2)
    val newlist3 = newlist2.appendFront(3)
    newlist3.show()

    val book = Book()
    for time <- 1 to 10 do
      book.newChapter("Oliver", time)
    book.chapters(3) = book.chapters(3).update(11)
    book.read
  }