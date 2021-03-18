#!dist/target/pack/bin/scala -classpath 'dist/target/pack/lib/*'
def main(args: Array[String]): Unit =
  println(sys.props("java.class.path"))
