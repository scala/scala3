package bar

// only accessible in the package and within subclasses (JLS 6.6.2.1)
def unrelated = foo.Task.poll() // error
def unrelatedInherited = foo.JSub.poll() // error
def unrelatedField = foo.Task.counter // error

class Sub extends foo.Task
object Sub:
  def companion = foo.Task.poll() // error: the companion of a subclass is not a subclass
