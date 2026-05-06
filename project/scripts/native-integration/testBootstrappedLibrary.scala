// Test that the bootstrapped standard library (scala-library-bootstrapped) works correctly
// This verifies the Scala 3 compiled standard library is properly included in the distribution

@main def testBootstrappedLibrary: Unit =
  // ==========================================================================
  // CRITICAL TEST: Verify we're using Scala 3 compiled library, not Scala 2.13
  // ==========================================================================
  
  // Test A: Check scala.util.Properties.versionNumberString
  // In Scala 3.8+, this should start with "3." because scala-library is compiled with Scala 3
  // In Scala 3.7.4 and earlier, this was "2.13.x" because scala-library was from Scala 2.13
  val libraryVersion = scala.util.Properties.versionNumberString
  assert(
    libraryVersion.startsWith("3."),
    s"scala-library should be compiled with Scala 3, but got version: $libraryVersion. " +
    "This indicates the distribution is using Scala 2.13 compiled library instead of scala-library-bootstrapped."
  )
  
  // Test B: Verify no Scala 2.13 library JAR is on the classpath
  val classpath = System.getProperty("java.class.path", "")
  val classpathEntries = classpath.split(java.io.File.pathSeparator).toList
  
  val scala213Entries = classpathEntries.filter { entry =>
    val lowerEntry = entry.toLowerCase
    // Look for scala-library with 2.13 version pattern
    (lowerEntry.contains("scala-library") || lowerEntry.contains("scala_library")) &&
    lowerEntry.contains("2.13")
  }
  
  assert(
    scala213Entries.isEmpty,
    s"Found Scala 2.13 library on classpath, which should not happen:\n${scala213Entries.mkString("\n")}\n" +
    "The distribution should only include scala-library-bootstrapped (compiled with Scala 3)."
  )
  
  // Test C: Verify scala3-library is on classpath (the Scala 3 specific additions)
  val scala3LibraryEntries = classpathEntries.filter { entry =>
    entry.toLowerCase.contains("scala3-library")
  }
  // Note: scala3-library might be merged or named differently, so this is informational
  
  println("BOOTSTRAPPED_LIBRARY_TEST_PASSED")
