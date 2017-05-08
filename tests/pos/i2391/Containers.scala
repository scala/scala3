// Containers.scala
package foo

trait ParentContainer {
  sealed trait Entry
}

class ChildContainer extends ParentContainer {
  trait LazyEntry extends Entry
}
