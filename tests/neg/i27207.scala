import scala.reflect.classTag

def f[X] = classTag[IArray[X]] // error

@main def main = 
  println(f[Int])
