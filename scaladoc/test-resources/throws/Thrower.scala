package pkg
import exceptions.*

class Thrower:
  /** @throws MyException */
  def t(): Unit = throw new MyException()