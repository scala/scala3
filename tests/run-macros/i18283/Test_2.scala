opaque type Id = Long

class Task:
  opaque type Title = String

  def a: Title = "a"

  def run =
    println("Task.run")
    println(Macro.getType[Id])
    println(Macro.getType[Title])

@main def Test =
  val task = new Task
  println(Macro.getType[Id])
  println(Macro.getType[task.Title])
  task.run
