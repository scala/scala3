import scala.language.experimental.modularity
import scala.language.future

tracked trait F // error

trait G:
  tracked def f: F // error

tracked object O // error

tracked class C // error

def f =
  tracked val x = 1 // error
