---
layout: doc-page
title: Reproducing an Issue
redirectFrom: /docs/contributing/workflow/reproduce.html
---

The first step is to reproduce the issue you're trying to fix. Say you want to
reproduce locally issue [#7710], you would first copy the code from the
*"Minimised Code"* section of the issue to a file named e.g.
`local/i7710.scala`, and then try to compile it from the sbt console opened in
the dotty root directory:

```bash
$ sbt
sbt:scala3> scalac -d local/out local/i7710.scala
```
> Here, the `-d` flag specifies a directory `local/out` where generated code will be output.

You can then verify that the local reproduction has the same behaviour as
originally reported in the issue. If so, then you can start to try and fix it.
Otherwise, perhaps the issue is out of date, or is missing information about how
to accurately reproduce the issue.

## Compiling files with scalac

Let's take a deeper look at this `scalac` command we just used. As we have seen
you can compile a test file either from sbt:

```bash
$ sbt
> scalac <OPTIONS> <FILE>
```

in the same way that you could from terminal:

```bash
$ scalac <OPTIONS> <FILE>
```

Here are some useful debugging `<OPTIONS>`:

* `-Xprint:PHASE1,PHASE2,...` or `-Xprint:all`: prints the `AST` after each
  specified phase. Phase names can be found by examining the
  `dotty.tools.dotc.transform.*` classes for their `phaseName` field e.g., `-Xprint:erasure`.
  You can discover all phases in the `dotty.tools.dotc.Compiler` class
* `-Ylog:PHASE1,PHASE2,...` or `-Ylog:all`: enables `ctx.log("")` logging for
  the specified phase.
* `-Ycheck:all` verifies the consistency of `AST` nodes between phases, in
  particular checks that types do not change. Some phases currently can't be
  `Ycheck`ed, therefore in the tests we run:
  `-Ycheck:tailrec,resolveSuper,mixin,restoreScopes,labelDef`.
* the last frontier of debugging (before actual debugging) is the range of logging capabilities that
can be enabled through the `dotty.tools.dotc.config.Printers` object. Change any of the desired printer from `noPrinter` to
`default` and this will give you the full logging capability of the compiler.

You may also want to further inspect the types of a piece of code to verify the
AST. Check out the section on [How to Inspect
Values](../debugging/inspection.md) for a detailed guide on this.

## Dotty Issue Workspace

Sometimes you will need more complex commands to reproduce an issue, and it is useful to script these, which
can be done with [dotty-issue-workspace]. It allows to bundle sbt commands for issue reproduction in one
file and then run them from the Dotty project's sbt console.

### Try an Example Issue

Let's use [dotty-issue-workspace] to reproduce issue [#7710]:
1.  Follow [the steps in the README][workspace-readme] to install the plugin.
2.  In your Issue Workspace directory (as defined in the plugin's README file,
    "Getting Started" section, step 2), create a subdirectory for the
    issue: `mkdir i7710`.
3.  Create a file for the reproduction: `cd i7710; touch Test.scala`. In that file,
    insert the code from the issue.
4.  In the same directory, create a file `launch.iss` with the following content:
    ```bash
    $ (rm -rv out || true) && mkdir out # clean up compiler output, create `out` dir.

    scala3/scalac -d $here/out $here/Test.scala
    ```

    - The first line, `$ (rm -rv out || true) && mkdir out` specifies a shell command
      (it starts with `$`), in this case to ensure that there is a fresh `out`
      directory to hold compiler output.
    - The next line, `scala3/scalac -d $here/out $here/Test.scala` specifies an sbt
      command, which will compile `Test.scala` and place any output into `out`.
      `$here` is a special variable that will be replaced by the path of the parent
      directory of `launch.iss` when executing the commands.
5.  Now, from a terminal you can run the issue from sbt in the dotty directory
    ([See here](../getting-started.md#compiling-and-running) for a reminder if you have not cloned the repo.):
    ```bash
    $ sbt
    sbt:scala3> issue i7710
    ```
    This will execute all the commands in the `i7710/launch.iss` file one by one.
    If you've set up `dotty-issue-workspace` as described in its README,
    the `issue` task will know where to find the folder by its name.

### Using Script Arguments

You can use script arguments inside `launch.iss` to reduce the number of steps when
working with issues.

Say you have an issue `foo`, with two alternative files that are very similar:
`original.scala`, which reproduces the issue, and `alt.scala`, which does not,
and you want to compile them selectively?

You can achieve this via the following `launch.iss`:

```bash
$ (rm -rv out || true) && mkdir out # clean up compiler output, create `out` dir.

scala3/scalac -d $here/out $here/$1.scala # compile the first argument following `issue foo <arg>`
```

It is similar to the previous example, except now you will compile a file `$1.scala`, referring
to the first argument passed after the issue name. The command invoked would look like
`issue foo original` to compile `original.scala`, and `issue foo alt` for `alt.scala`.

In general, you can refer to arguments passed to the `issue <issue_name>` command using
the dollar notation: `$1` for the first argument, `$2` for the second and so on.

### Multiline Commands

Inside a `launch.iss` file, one command can be spread across multiple lines. For example,
if your command has multiple arguments, you can put each argument on a new line.

Multiline commands can even have comments in-between lines. This is useful
if you want to try variants of a command with optional arguments (such as configuration).
You can put the optional arguments on separate lines, and then decide when they are passed to
the command by placing `#` in front to convert it to a comment (i.e. the argument will
not be passed). This saves typing the same arguments each time you want to use them.

The following `launch.iss` file is an example of how you can use multiline
commands as a template for solving issues that [run compiled
code](../testing.md#checking-program-output). It demonstrates configuring
the `scala3/scalac` command using compiler flags, which are commented out. Put
your favourite flags there for quick usage.

```bash
$ (rm -rv out || true) && mkdir out # clean up compiler output, create `out` dir.

scalac  # Invoke the compiler task defined by the Dotty sbt project
  -d $here/out  # All the artefacts go to the `out` folder created earlier
  # -Xprint:typer  # Useful debug flags, commented out and ready for quick usage. Should you need one, you can quickly access it by uncommenting it.
  # -Ydebug-error
  # -Yprint-debug
  # -Yprint-debug-owners
  # -Yshow-tree-ids
  # -Ydebug-tree-with-id 340
  # -Ycheck:all
  $here/$1.scala  # Invoke the compiler on the file passed as the second argument to the `issue` command. E.g. `issue foo Hello` will compile `Hello.scala` assuming the issue folder name is `foo`.

scala -classpath $here/out Test  # Run main method of `Test` generated by the compiler run.
```

## Conclusion

In this section, you have seen how to reproduce an issue locally, and next you will see
how to try and detect its root cause.

[lampepfl/dotty]: https://github.com/scala/scala3/issues
[#7710]: https://github.com/scala/scala3/issues/7710
[dotty-issue-workspace]: https://github.com/anatoliykmetyuk/dotty-issue-workspace
[workspace-readme]: https://github.com/anatoliykmetyuk/dotty-issue-workspace#getting-started
