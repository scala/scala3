./../../Thesis/valhalla/build/macosx-aarch64-server-release/images/jdk/bin/javac --enable-preview --release 27 -parameters -d tests/null-restricted/java_interop tests/null-restricted/java_interop/"$1".java
cd tests/null-restricted/java_interop
jar -cf myJar.jar Foo.class "$1".class