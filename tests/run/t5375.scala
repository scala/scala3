/** Hello fellow compiler developer.
    if you are wondering why does test suite hang on this test
    then it's likely that the lambda inside map has been compiled into static method
    unfotrunatelly, as it is executed inside static object initializer,
    it is executed inside class-loader, in a synchronized block that is not source defined.
    
    If the lambda will be static Test$#foo, calling it through a different thread would require grabbing the
    lock inside classloader. Unlike if it not static and is called through This(Test).foo, no lock is grabbed.
    
    @DarkDimius  
*/  
object Test extends dotty.runtime.LegacyApp {
  val foos = (1 to 1000).toSeq
  try
    foos.par.map(i => if (i % 37 == 0) sys.error("i div 37") else i)
  catch {
    case ex: RuntimeException => println("Runtime exception")
  }
}
