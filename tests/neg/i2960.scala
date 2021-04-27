package org.glavo.dotty {

import scala.collection.mutable

sealed trait Node {
    def mkString(n: Int): String
}

class Tag(val name: String,
          val attributes: mutable.LinkedHashMap[Symbol, String] = mutable.LinkedHashMap(),
          val children: mutable.Buffer[Node] = mutable.Buffer()) extends Node {

    override def mkString(n: Int): String = {
        Tag.spaces(n) + s"<$name ${attributes.map { case (k,v) => k.name + "=" + Tag.unescape(v) }.mkString(" ")}>" +
            (if(children.isEmpty) "\n"
                else children.map(_.mkString(n + 4)).mkString("\n", "\n", "\n")) +
        Tag.spaces(n) + s"</$name>"
    }

    def apply(attrs: (Symbol, String)*): this.type = {
        attributes ++= attrs
        this
    }

    def apply[U](f: Tag ?=> U)(implicit t: Tag = null): this.type = {
        if(t != null) t.children += this
        f(using this)
        this
    }
}

object Tag {
    def spaces(n: Int = 0): String = {
        if(n == 0) ""
        else {
            val cs = new Array[Char](n)
            for (i <- 0 until n)
                cs(i) = 0

            new String(cs)
        }
    }

    def unescape(str: String): String = {
        "\"" + str + "\""
    }

    implicit def symbolToTag(symbol: Symbol): Tag =
        new Tag(symbol.name)

    implicit class PairMaker(val tag: Symbol) extends AnyVal {
        def :=(value: String): (Symbol, String) = (tag, value)
    }
}

class Text(val value: String) extends Node {
    override def mkString(n: Int): String = {
        Tag.spaces(n) + value
    }
}
}

object Test {
import org.glavo.dotty.*
import org.glavo.dotty.Tag.*
'html{} // error
}
