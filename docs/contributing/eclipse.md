Building Dotty with Eclipse
===========================

Build setup
-----------
You may need to redo these steps when the build changes.

1. Run `sbt eclipse`
2. In dotty, go to `Properties > java build path > Libraries`.
   Remove the Scala Compiler container (currently 2.11.4) and add as an
   external jar the latest compiler version in the Ivy cache. This is
   currently:

   ```
   .ivy2/cache/me.d-d/scala-compiler/jars/scala-compiler-2.11.5-20160322-171045-e19b30b3cd.jar
   ```

   But that might change in the future. Or, copy the latest scala compiler from
   the cache to a stable name and use that as external jar.

3. It is recommended to change the default output folder (in `Properties > java
   build path > Source`) to `dotty/classes` instead of `dotty/bin` because
   `dotty/bin` is reserved for shell scripts.

If you have `CLASSPATH` defined:

4. Update your classpath to contain any new required external libraries to run
   `./bin/dotc`, `./bin/doti` outside of Eclipse.

5. Open the `Run Configurations` tab, and edit the `tests` configuration so
   that it contains a `CLASSPATH` variable which reflects the current
   `CLASSPATH`.

In order for compilation errors related to `ENUM` to be resolved, make sure
that scala-reflect 2.11.5 is on the classpath.

Running the compiler Main class from Eclipse
--------------------------------------------
1. Navigate to `dotty.tools.dotc.Main`
2. `Run As... > Scala Application`
3. `Run Configurations > Main$ > Classpath > Bootstrap entries`:
   - Add the Scala library (`Advanced...` > `Add library...` > `Scala library`)
   - Add the Dotty classfiles (`Add projects...` > `[x] dotty`)
4. `Run Configurations > Main$ > Arguments` and add
   `${project_loc}/examples/hello.scala`
