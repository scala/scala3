
import caps.*

class IO
class File(io: IO^)

class Handler[cap C]:
  def f(file: File^): File^{C} = file // error
  def g(@consume file: File^{C}): File^ = file // ok
