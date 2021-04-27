---
layout: blog-page
title: Dotty becomes Scala 3
author: Anatolii Kmetiuk
authorImg: /images/anatolii.png
date: 2020-09-21
---

This article is a heads-up for the upcoming change in the naming of Dotty artefacts (as published to Maven). Currently, the organization name is “ch.epfl.lamp” which will become “org.scala-lang”. The artefact names will be changed from “dotty-xxx” to “scala3-xxx”.

This change will be part of the next Dotty release planned for October 1st which will be known as Scala 3.0.0-M1. We encourage maintainers of tooling (IDEs, build tools, ...) to prepare for this change now by special-casing the way they handle the compiler when its version number starts with `3.` just like they already had to special-case versions starting with `0.` to support existing Dotty releases.
