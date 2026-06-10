// Test case for RefinedType handling in jsig
// jsig matches: case RefinedType(parent, _, _)

class RefinedTypesTest:
  // Refined type with methods
  type ListWithSize = List[Int] { def size: Int }
  
  def processRefined(lst: ListWithSize): ListWithSize =
    lst.map(_ + 1)

  // Refined type as return type
  def getRefinedList(): List[String] { def isEmpty: Boolean } =
    List("a", "b", "c")

  // Refined type with multiple refinements
  type RefinedCollection = scala.collection.Seq[Int] {
    def isEmpty: Boolean
    def size: Int
  }

  def operateOnRefined(c: RefinedCollection): Int =
    if c.isEmpty then 0 else c.size

  // Nested refinement
  type NestedRefined = List[List[Int] { def head: Int }]

  def processNestedRefined(nested: NestedRefined): Int =
    nested.head.head
