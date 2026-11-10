package foo

// Protected members of a Java class are also accessible in its package.
object SamePackage:
  def poll: String = Task.poll()
