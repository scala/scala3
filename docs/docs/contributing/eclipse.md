---
layout: doc-page
title: Building Dotty with Eclipse
---

Build setup
-----------

1. Run `sbt ;managedSources;eclipse`

2. It is recommended to change the default output folder (in `Properties > java
   build path > Source`) to `dotty/classes` instead of `dotty/bin` because
   `dotty/bin` is reserved for shell scripts.

If you have `CLASSPATH` defined:

3. Update your classpath to contain any new required external libraries to run
   `./bin/dotc`, `./bin/dotr` outside of Eclipse.

4. Open the `Run Configurations` tab, and edit the `tests` configuration so
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
   `${project_loc}/tests/pos/HelloWorld.scala`
