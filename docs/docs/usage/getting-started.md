---
layout: singlepage-overview
title: Getting Started with Scala 3
languages: ["ja"]
---



## Try Scala without installing anything

To start experimenting with Scala 3 right away, use <a href="https://scastie.scala-lang.org/?target=dotty" target="_blank">“Scastie” in your browser</a>.
_Scastie_ is an online “playground” where you can experiment with Scala examples to see how things work, with access to all Scala compilers and published libraries.



## Install Scala on your computer

Installing Scala means installing various command-line tools and build tools.
We recommend using the Scala installer tool "Coursier" that automatically installs all the requirements, but you can still manually install each tool.


### Using the Scala Installer (recommended way)

The Scala installer is a tool named [Coursier](https://get-coursier.io/docs/cli-overview), whose main command is named `cs`.
It ensures that a JVM and standard Scala tools are installed on your system.
Install it on your system with the following instructions.

<div class="main-download">
  <div id="download-step-one">
    <p>Follow <a href="https://get-coursier.io/docs/cli-overview.html#install-native-launcher" target="_blank">the instructions to install the <code>cs</code> launcher</a> then run:</p>
    <p><code>$ cs install scala3-repl</code></p>
    <p><code>$ cs install scala3-compiler</code></p>
  </div>
</div>

Along with managing JVMs, `cs setup` also installs useful command line tools:

- A JDK
- The [sbt](https://www.scala-sbt.org) and [mill](https://com-lihaoyi.github.io/mill/) build tools
- [Ammonite](https://ammonite.io), an enhanced REPL
- [scalafmt](https://scalameta.org/scalafmt), the Scala formatter
- The [Coursier CLI](https://get-coursier.io/docs/cli-overview), to install further Scala-based applications
- (the `scala` and `scalac` command-line tools for Scala 2.13 -- not Scala 3).

For more information, read the [coursier-cli documentation](https://get-coursier.io/docs/cli-overview).


### ... or manually

You only need two tools to compile, run, test, and package a Scala project: Java 8 or 11, and sbt.
To install these manually:

1. Download Java from [Oracle Java 8](https://www.oracle.com/java/technologies/javase-jdk8-downloads.html), [Oracle Java 11](https://www.oracle.com/java/technologies/javase-jdk11-downloads.html), or [AdoptOpenJDK 8/11](https://adoptopenjdk.net/). Refer to [JDK Compatibility](https://docs.scala-lang.org/overviews/jdk-compatibility/overview.html) for Scala/Java compatibility detail.
2. Install [sbt](https://www.scala-sbt.org/download.html)



## Create a “Hello, world” project with sbt

To create a project, you can either use a command-line tool or an IDE.
If you are familiar with the command line, we recommend that approach.


### Using the command line

sbt is a build tool for Scala.
sbt compiles, runs, and tests your Scala code.
(It can also publish libraries and do many other tasks.)

To create a new Scala project with sbt:

1. `cd` to an empty folder.
1. Run this command `sbt new scala/scala3.g8`.
This pulls the ['hello-world' template][template-url] from GitHub.
It also creates a _target_ folder, which you can ignore.
1. When prompted, name the application `hello world`.
   This will create a project called "hello-world".
1. Let’s take a look at what just got generated:

```
hello-world/
  project/           (sbt uses this for its own files)
    build.properties
  src/main/scala/    (all of your Scala code goes here)
    Main.scala       (entry point of program)
  build.sbt          (sbt’s build definition file)
```
The scala file `Main.scala` in `src/main/scala` is all we need for now.

More documentation about sbt can be found in the [Scala Book](https://docs.scala-lang.org/scala3/book/scala-tools.html) and in the official sbt [documentation](https://www.scala-sbt.org/1.x/docs/index.html)


{% comment %}
### With IntelliJ IDEA

You can skip the rest of this page and go directly to [Building a Scala Project with IntelliJ and sbt](/getting-started/intellij-track/building-a-scala-project-with-intellij-and-sbt.html)
{% endcomment %}


## Open the “Hello, world” project

Let’s use an IDE to open the project.
The most popular ones are IntelliJ IDEA and VS Code.
They both offer rich IDE features, but you can still use [many other editors.](https://scalameta.org/metals/docs/editors/overview.html)

### Using IntelliJ IDEA

1. Download and install [IntelliJ Community Edition](https://www.jetbrains.com/idea/download/)
1. Install the Scala plugin by following [the instructions on how to install IntelliJ plugins](https://www.jetbrains.com/help/idea/managing-plugins.html)
1. Open the _build.sbt_ file, then choose _Open as a project_

### Using VS Code with Metals

1. Download [VS Code](https://code.visualstudio.com/Download)
1. Install the Metals extension from [the Marketplace](https://marketplace.visualstudio.com/items?itemName=scalameta.metals)
1. Next, open the directory containing your _build.sbt_ file.
   When prompted to do so, select _Import build_.

>[Metals](https://scalameta.org/metals) is a “Scala language server” that provides support for writing Scala code in VS Code and other editors like [Atom, Sublime Text, and more](https://scalameta.org/metals/docs/editors/overview.html), using the Language Server Protocol.
>
> Under the hood, Metals communicates with the build tool by using
> the [Build Server Protocol (BSP)](https://build-server-protocol.github.io/). For details on how Metals works, see, [“Write Scala in VS Code, Vim, Emacs, Atom and Sublime Text with Metals”](https://www.scala-lang.org/2019/04/16/metals.html).


### View the source code

View these two files in your IDE:

- _build.sbt_
- _src/main/scala/Main.scala_

When you run your project in the next step, the configuration in _build.sbt_ will be used to run the code in _src/main/scala/Main.scala_.



## Run the “Hello, world” project

If you’re comfortable using your IDE, you can run the code in _Main.scala_ from your IDE.

Otherwise, you can run the application from a terminal with these steps:

1. `cd` into _hello-world_.
1. Run `sbt`.
   This opens up the sbt console.
1. Type `~run`.
   The `~` is optional and causes sbt to re-run on every file save, allowing for a fast edit/run/debug cycle.
   sbt also generates a `target` directory for its own use, which you can ignore.

When you’re finished experimenting with this project, press `[Enter]` to interrupt the `run` command.
Then type `exit` or press `[Ctrl][d]` to exit sbt and return to your command line prompt.



## Next steps

Now that you’ve created a first “Hello, world” example with Scala 3, you’re ready for some next steps.
Consider checking out:

- [The Scala 3 Book](https://docs.scala-lang.org/scala3/book/introduction.html), which provides a set of short lessons introducing Scala’s main features
- [The migration guide](https://docs.scala-lang.org/scala3/guides/migration/compatibility-intro.html) helps you to migrate your existing Scala 2 code base to Scala 3.

When you want to connect with other Scala users, there are several mailing lists and real-time chat rooms available.
Check out our [Scala community page](https://scala-lang.org/community/) for a list of these resources, and for where to reach out for help.


<!-- Hidden elements whose content are used to provide OS-specific download instructions.
 -- This is handled in `resources/js/functions.js`.
 -->
<div style="display:none" id="stepOne-linux">
       <code class="hljs">$ curl -Lo cs https://git.io/coursier-cli-linux && chmod +x cs && ./cs setup </code> <br>
</div>

<div style="display:none" id="stepOne-unix">
    <p>Follow <a href="https://get-coursier.io/docs/cli-overview.html#install-native-launcher" target="_blank">the instructions to install the <code>cs</code> launcher</a> then run:</p>
    <p><code>$ ./cs setup</code></p>
</div>

<div style="display:none" id="stepOne-osx">
    <p>If you use Homebrew:</p>
    <div class="highlight">
        <code class="hljs">$ brew install coursier/formulas/coursier && cs setup </code> <br>
    </div>
    <p>Alternatively, if you don’t use Homebrew:</p>
    <div class="highlight">
        <code class="hljs">$ curl -Lo cs https://git.io/coursier-cli-macos && chmod +x cs &&  (xattr -d com.apple.quarantine cs || true) && ./cs  setup </code> <br>
    </div>
</div>

<div style="display:none" id="stepOne-windows">
    <p>Download and execute <a href="https://git.io/coursier-cli-windows-exe">the Scala installer for Windows</a> based on coursier</p>
</div>

[template-url]: https://github.com/scala/scala3.g8
