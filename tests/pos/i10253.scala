object test:
  def foo(qc: QC): Unit =
    object treeMap extends qc.reflect.TreeMap

trait QC:
  val reflect: Reflection
  trait Reflection:
    trait TreeMap:
      def transformTree: Unit = ???