---
layout: doc-page
title: Dropped: Delayedinit
---

The special handling of the `DelayedInit` trait is no longer
supported.

One consequence is that the `App` class, which used `DelayedInit` is
now partially broken. You can still use `App` for an easy and concise
way to set up a main program. Example:

    object HelloWorld extends App {
      println("Hello, world!")
    }

However, the code is now run in the initializer of the object, which on
some JVM's means that it will only be interpreted. So, better not use it
for benchmarking! Also, if you want to access the command line arguments,
you need to use an explicit `main` method for that.

    object Hello extends App {
      def main(args: Array[String]) =
        println(s"Hello, ${args(0)}")
    }

