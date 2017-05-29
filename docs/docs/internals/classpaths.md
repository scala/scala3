---
layout: doc-page
title: Classpaths
---

When ran from the `dotty` script, this is the classloader stack:

```
=====================================================
class sun.misc.Launcher$AppClassLoader  <= corresponds to java.class.path
sun.misc.Launcher$AppClassLoader@591ce4fe
file:/mnt/data-local/Work/Workspace/dev-2.11/dotty/target/scala-2.11.0-M7/dotty_2.11.0-M7-0.1-SNAPSHOT.jar:file:/home/sun/.ivy2/cache/org.scala-lang/scala-library/jars/scala-library-2.11.0-M7.jar:file:/home/sun/.ivy2/cache/org.scala-lang/scala-reflect/jars/scala-reflect-2.11.0-M7.jar
=====================================================
class sun.misc.Launcher$ExtClassLoader  <= corresponds to sun.boot.class.path
sun.misc.Launcher$ExtClassLoader@77fe0d66
file:/usr/lib/jvm/java-7-oracle/jre/lib/ext/sunpkcs11.jar:file:/usr/lib/jvm/java-7-oracle/jre/lib/ext/localedata.jar:file:/usr/lib/jvm/java-7-oracle/jre/lib/ext/zipfs.jar:file:/usr/lib/jvm/java-7-oracle/jre/lib/ext/sunec.jar:file:/usr/lib/jvm/java-7-oracle/jre/lib/ext/sunjce_provider.jar:file:/usr/lib/jvm/java-7-oracle/jre/lib/ext/dnsns.jar
=====================================================
```

When running from sbt or Eclipse, the classloader stack is:

```
=====================================================
class sbt.classpath.ClasspathUtilities$$anon$1
sbt.classpath.ClasspathUtilities$$anon$1@22a29f97
file:/mnt/data-local/Work/Workspace/dev-2.11/dotty/target/scala-2.11.0-M7/classes/:file:/home/sun/.ivy2/cache/org.scala-lang/scala-library/jars/scala-library-2.11.0-M7.jar:file:/home/sun/.ivy2/cache/org.scala-lang/scala-reflect/jars/scala-reflect-2.11.0-M7.jar:file:/home/sun/.ivy2/cache/org.scala-lang.modules/scala-xml_2.11.0-M7/bundles/scala-xml_2.11.0-M7-1.0.0-RC7.jar
=====================================================
class java.net.URLClassLoader
java.net.URLClassLoader@2167c879
file:/home/sun/.ivy2/cache/org.scala-lang/scala-library/jars/scala-library-2.11.0-M7.jar:file:/home/sun/.ivy2/cache/org.scala-lang/scala-compiler/jars/scala-compiler-2.11.0-M7.jar:file:/home/sun/.ivy2/cache/org.scala-lang/scala-reflect/jars/scala-reflect-2.11.0-M7.jar:file:/home/sun/.ivy2/cache/org.scala-lang.modules/scala-xml_2.11.0-M6/bundles/scala-xml_2.11.0-M6-1.0.0-RC6.jar:file:/home/sun/.ivy2/cache/org.scala-lang.modules/scala-parser-combinators_2.11.0-M6/bundles/scala-parser-combinators_2.11.0-M6-1.0.0-RC4.jar:file:/home/sun/.ivy2/cache/jline/jline/jars/jline-2.11.jar
=====================================================
class xsbt.boot.BootFilteredLoader
xsbt.boot.BootFilteredLoader@73c74402
not a URL classloader
=====================================================
class sun.misc.Launcher$AppClassLoader  <= corresponds to java.class.path
sun.misc.Launcher$AppClassLoader@612dcb8c
file:/home/sun/.sbt/.lib/0.13.0/sbt-launch.jar
=====================================================
class sun.misc.Launcher$ExtClassLoader  <= corresponds to sun.boot.class.path
sun.misc.Launcher$ExtClassLoader@58e862c
file:/usr/lib/jvm/java-7-oracle/jre/lib/ext/sunpkcs11.jar:file:/usr/lib/jvm/java-7-oracle/jre/lib/ext/localedata.jar:file:/usr/lib/jvm/java-7-oracle/jre/lib/ext/zipfs.jar:file:/usr/lib/jvm/java-7-oracle/jre/lib/ext/sunec.jar:file:/usr/lib/jvm/java-7-oracle/jre/lib/ext/sunjce_provider.jar:file:/usr/lib/jvm/java-7-oracle/jre/lib/ext/dnsns.jar
=====================================================
```
Since scala/dotty only pick up `java.class.path` and `sun.boot.class.path`,
it's clear why Dotty crashes in sbt and Eclipse unless we set the boot
classpath explicitly.
