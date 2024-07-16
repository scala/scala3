package bar;

object Client extends foo.Task {
  def run = foo.Task.poll()
}
