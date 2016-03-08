To set up the dotty project on Eclipse, or if the build has changed, perform the following
steps:

1. Run sbt eclipse
2. In dotty, select properties (CTRL-I), select "java build path" and then "Libraries".
   Remove the Scala Compiler container (currently 2.11.4) and add as an external jar
   the latest compiler version in the Ivy cache. This is currently:

     .ivy2/cache/me.d-d/scala-compiler/jars/scala-compiler-2.11.5-20151022-113908-7fb0e653fd.jar

   But that might change in the future. Or, copy the latest scala compiler from the cache to a stable name and use that as external jar.

3. If necessary, change the default output folder (in Properties/java build path/Source). I use dotty/classes instead of dotty/bin, because dotty/bin is reserved for shell scripts.

If you have CLASSPATH defined:

4. Update your classpath to contain any new required external libraries to run dotc, doti outside of eclipse.

5. Open the "Run Configuarations" tab, and edit the "tests" configuration so that it contains
   a CLASSPATH variable which reflects the current CLASSPATH.
