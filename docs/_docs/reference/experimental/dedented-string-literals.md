---
layout: doc-page
title: "Dedented String Literals"
nightlyOf: https://docs.scala-lang.org/scala3/reference/experimental/dedented-string-literals.html
---

## Introduction

Dedented string literals are an experimental implementation of [SIP 72](https://github.com/scala/improvement-proposals/pull/112). They are enabled with the language import
```scala
  import language.experimental.dedentedStringLiterals
```

The import supports a `'''` syntax for dedented multiline string literals that remove leading
indentation at a language level, rather than using the `.stripMargin` library method:

```scala
> def helper =
    val x = '''
    i am cow
    hear me moo
    '''
    x

> println(helper)
i am cow
hear me moo
```

Dedented strings automatically strip:

- The first newline after the opening `'''`
- The final newline and any whitespace before the closing `'''`
- Any indentation on every line up to the position of the closing `'''`

The opening `'''` MUST be followed immediately by a newline, and the trailing `'''` MUST
be preceded by a newline followed by whitespace characters. Lines within the
dedented string MUST be either empty, or have indentation equal-to-or-greater-than
the closing delimiter.

If a user explicitly wants indentation to be present in the string, they
can simply adjust the contents accordingly:

```scala
> def helper = {
    // string with two-space indents before each line
    val x = '''
      i am cow
      hear me moo
    '''
    x
  }

> println(helper)
  i am cow
  hear me moo
```

And if a user wants leading or trailing newlines, they can add those as well:

```scala
> def helper = {
    // string with two-space indents before each line, and leading and trailing newlines
    val x = '''

      i am cow
      hear me moo

    '''
    x
  }

> println(helper)

  i am cow
  hear me moo

```

See [SIP 72](https://github.com/scala/improvement-proposals/pull/112) for more details.