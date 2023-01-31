// We first compile Macro using 3.1 to generate the old accessors
// Then we compile this file to link against the old accessors
// Finally we recompile Macro using the current compiler to generate a version
// of Macro that contains the new accessors (and the old for backwards compat)

@main def Test =
  // Check that both accessors exist in the bytecode
  val barMethods =
    java.lang.Class.forName("Macro").getMethods()
      .filter(_.getName().contains("inline"))
      .filter(x => (x.getModifiers() & java.lang.reflect.Modifier.STATIC) == 0)
      .map(_.getName())
  val barObjectMethods =
    java.lang.Class.forName("Macro$inline$accessors").getMethods()
    .filter(_.getName().contains("inline"))
    .map(_.getName())

  println("3.0-3.3 inline accessor: " + barMethods.toList)
  println("3.4+ inline accessor: " + barObjectMethods.toList)
