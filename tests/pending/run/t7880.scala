object Test extends dotty.runtime.LegacyApp {
  // This should terminate in one way or another, but it shouldn't loop forever.
  try {
    val buffer = collection.mutable.ArrayBuffer.fill(Int.MaxValue / 2 + 1)(0)
    buffer append 1
  } catch { case _: OutOfMemoryError => }
}
