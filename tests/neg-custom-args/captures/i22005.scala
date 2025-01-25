import language.future // sepchecks on
import caps.*

class IO
class File(io: IO^)

class Handler[C^]:
  def f(file: File^): File^{C^} = file // error
  def g(@consume file: File^{C^}): File^ = file // ok
