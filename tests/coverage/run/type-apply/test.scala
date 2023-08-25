@main
def Test: Unit =
  // verifies a problematic case where the TypeApply instrumentation was added to the coverage file,
  // but was never marked as invoked
  println(List(1,2,3).map(a => List(a)))
