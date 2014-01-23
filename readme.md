dotty
=====

The experimental compiler for a Scala dialect based on DOT.

### To get started:
```
  sbt compile
  sbt run
```
The tests (`sbt test`) don''t work yet.


### To use the Scala IDE:
```
  sbt eclipse
```
Notes: 
 * You will need the Scala IDE for 2.11.0-M7
 * There are 2 spurious version incompatibility warnings
 * To run dotty in Eclipse:
   * Navigate to `dotty.tools.dotc.Main`
   * Run As ... > Scala Application
   * then go to Run Configurations > Main$ > Classpath > Bootstrap entries:
     * add the Scala library (Advanced... > Add library ... > Scala library)
     * add the dotty classfiles (Add projects... > [x] dotty)
