object test with
  def foo(qc: QC): Unit =
    object treeMap extends qc.reflect.TreeMap

trait QC with
  val reflect: Reflection
  trait Reflection with
    trait TreeMap with
      def transformTree: Unit = ???