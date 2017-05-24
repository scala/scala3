class Dotty < Formula
  desc "Experimental Scala Compiler"
  homepage "http://dotty.epfl.ch/"
  url "http://dotty.epfl.ch/dotty/dotty-0.1.1-bin-SNAPSHOT.tar.gz"
  sha256 "4e1bda148754543844d25290a87076e6bfb0b6b0275535f97c1871e0fc5c2c4c"
  # mirror "https://downloads.typesafe.com/scala/2.12.2/scala-2.12.2.tgz"
  # mirror "https://www.scala-lang.org/files/archive/scala-2.12.2.tgz"

  bottle :unneeded

  depends_on :java => "1.8+"

  def install
    rm_f Dir["bin/*.bat"]
    prefix.install "bin", "lib"
  end

  test do
    file = testpath/"Test.scala"
    file.write <<-EOS.undent
      object Test {
        def main(args: Array[String]) {
          println(s"${2 + 2}")
        }
      }
    EOS

    out = shell_output("#{bin}/dotr #{file}").strip

    assert_equal "4", out
  end
end
