---
layout: doc-page
title: Improving Your Workflow
---

In the previous sections of this chapter, you saw some techniques for
working with the compiler. Some of these techniques can be used
repetitively, e.g.:

- Navigating stack frames
- Printing variables in certain ways
- Instrumenting variable definitions with tracers

The above procedures often take a lot of time when done manually, reducing productivity:
as the cost (in terms of time and effort) is high, you may avoid attempting to do so,
and possibly miss valuable information.

If you're doing those things really frequently, it is recommended to script your editor
to reduce the number of steps. E.g. navigating to the definition of a stack frame
part when you click it, or instrumenting variables for printing.

An example of how it is done for Sublime Text 3 is [here](https://github.com/anatoliykmetyuk/scala-debug-sublime).

True, it takes some time to script your editor, but if you spend a lot of time with issues, it pays off.
