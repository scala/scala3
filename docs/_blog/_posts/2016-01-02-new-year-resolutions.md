---
layout: blog-page
title: New Year Resolutions
author: Martin Odersky
authorImg: images/martin.jpg
date: 2016-01-02
---

For most of us, the change of the year is an occasion for thinking
about what we missed doing last year and where we want to improve. I decided
there are a couple of things where I would like to do better in 2016
than in 2015. The first is that I would like to do more blogging and
writing in general. I have been pretty silent for most of the last
year. This was mostly caused by the fact that I had been heads down to
work on DOT, Scala's foundations, and _Dotty_, the new Scala compiler
platform we are working on. It's been a lot of work, but we are finally
getting good results. DOT now has a mechanized proof of type soundness
and the Dotty compiler [can now compile
itself](http://www.scala-lang.org/blog/2015/10/23/dotty-compiler-bootstraps.html)
as well as large parts of Scala's standard library.

The Dotty compiler has a completely new and quite unusual
architecture, which makes it resemble a functional database or a
functional reactive program. My [talk at the JVM language
summit](https://www.youtube.com/watch?v=WxyyJyB_Ssc) gives an
overview. In the coming months I want to write together with my
collaborators a series of blog posts
 that explain details of the code base. The
aim of these posts will be to present the new architectural patterns
to a larger audience and also to help existing and potential
contributors get familiar with the code base.

My second resolution is to take a larger effort to promote simplicity
in Scala. I believe the recent [blog post by Jim
Plush](http://jimplush.com/talk/2015/12/19/moving-a-team-from-scala-to-golang/) should be a wakeup call for our
community. Scala is a very powerful and un-opinionated language.  This
means we have a large spectrum of choice how to write a Scala
application or library. It's very important for all of us to use this
power wisely, and to promote simplicity of usage wherever possible.
Unfortunately, most of us fall all too easily into the complexity
trap, as Alex Payne's tweet sums it up very nicely.

<blockquote class="twitter-tweet" lang="en"><p lang="en" dir="ltr">“Complexity is like a bug light for smart people. We can&#39;t resist it, even though we know it&#39;s bad for us.” <a href="https://t.co/V9Izi573CF">https://t.co/V9Izi573CF</a></p>&mdash; Alex Payne (@al3x) <a href="https://twitter.com/al3x">January 1, 2016</a></blockquote>
<script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>

I have been as guilty of complication as everybody else. Is
`CanBuildFrom` the most appropriate solution to deal with the
constraints of embedding special types such as arrays and strings in a
collection library? It achieves its purpose of providing a uniform
user-level API on disparate datatypes. But I now think with more
effort we might be able come up with a solution that works as well and
is simpler. Another example, where I have doubts if not regrets are
the `/:` and `:\` operators in scala.collections.  They are cute
synonyms for folds, and I am still fond of the analogy with falling
dominoes they evoke. But in retrospect I think maybe they did give a
bad example for others to go overboard with symbolic operators.

So my main agenda for the coming year is to work on making Scala
simpler: The language, its foundations, its libraries. I hope you
will join me in that venture.

With that thought, I wish you a happy new year 2016.




