---
layout: doc-page
title: Building Dotty with Intellij IDEA
---

Setup
-----

You can setup IntelliJ [IDEA >= 2016.1](https://www.jetbrains.com/idea/nextversion) to run Dotty.

Assuming you have cloned Dotty's repository from Github (and run `sbt managedSources` as described in [Getting Started](getting-started.md)),
you can now proceed with importing it to IDEA by selecting the
corresponding option from the startup menu. Navigate on the corresponding directory and select it. Next, you need
to select the model of the import and as the screenshot shows, select SBT.

![](../../images/idea/idea-import.png "Import Dotty in IDEA")

Next, you select the version of the JDK that this project relies on and verify that you have selected 1.8 (assuming
its installed on your local machine). Otherwise, specify it by pressing *New*.

![](../../images/idea/idea-sdk.png "Select the JDK")

On the final window we must select which modules we can import. Here, we are presented with the full list of SBT projects
that are defined in Dotty. You can either select all (expect performance degradation by IDEA if you select all) or
select only the `dotty` module. In order to do that, unselect all modules and select on `dotty`. IDEA, then, automatically
selects all the necessary dependencies and you press OK.

![](../../images/idea/idea-sbt.png "Select modules to import")

Running/Debugging
-------

To run the compiler you can do it either as an sbt command or via debugging the compiler.
For the first option you can fire up sbt from the `Terminal` window of IDEA or you can do it externally.
For example, to run a test you can write with or without flags:

```shell
$ dotc tests/pos/Arrays.scala
```

If you are interested in debugging the compiler you can enable the necessary agent on the JVM.
This is done in two steps. For the first step you need to pass the 
necessary flag to the running VM. For convenience, this is already in comments on the `Build.scala` file under the
`project` directory. The string you need to uncomment is the following:

> `"-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=5005"`

Uncomment the line and reload the project file with the `reload` command on sbt. 
Now, each time you run `dotc` the compiler listens at the designated address and waits the agent to connect.
For, the second step you need to create a configuration for Debug through IDEA: 

> Run > Edit Configurations > Add New Configuration > (select) Remote

![](../../images/idea/idea-debug.png "Create a Debug Configuration")

Now every time you run `dotc` you can set your breakpoints and hit the `Debug dotty-debug` button (since we used that name for 
 the sample configuration above). The default data on the configuration match the enabled agent on the VM so, probably,
 you will not need to change anything else.
 
