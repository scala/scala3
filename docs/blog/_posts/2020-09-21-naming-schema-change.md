---
layout: blog-page
title: Dotty becomes Scala 3
author: Anatolii Kmetiuk
authorImg: /images/anatolii.png
date: 2020-09-21
---

This article is a heads-up for the upcoming change in the naming of Dotty artefacts (as published to Maven). Currently, the organization name is “ch.epfl.lamp” which will become “org.scala-lang”. The artefact names will be changed from “dotty-xxx” to “scala3-xxx”.

The change, naturally, will break the existing build tools that work with Dotty. Hence, tooling maintainers need to be prepared to migrate the tooling to the new naming schema. We are planning to introduce the change by October 1st in a form of the next Dotty release, Scala 3.0-M1. The major tooling will be migrated shortly after the release is on Maven.
