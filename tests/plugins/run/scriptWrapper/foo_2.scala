// generated code
// script: foo.sc
object foo_sc {
def main(args: Array[String]): Unit = {
  run // assume some macro generates this by scanning for @entrypoint
}
//USER_SRC_FILE:./foo_original_2.scala
//USER_CODE_HERE
import framework.*

def getRandom: Int = brokenRandom // LINE 3;

def brokenRandom: Int = ??? // LINE 5;

@entrypoint
def run = println("Hello, here is a random number: " + getRandom) // LINE 8;
//END_USER_CODE_HERE
}
