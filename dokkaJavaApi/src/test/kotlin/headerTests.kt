package dokka.java.api

import dokka.java.api.com.virtuslab.dokka.site.RenderingContext
import dokka.java.api.com.virtuslab.dokka.site.TemplateFile
import dokka.java.api.com.virtuslab.dokka.site.loadTemplateFile
import org.junit.Test
import org.junit.Assert.*
import java.nio.file.Files

class TemplateFileTests {
    private fun testTemplate(code: String, ext: String = "html", op: TemplateFile.() -> Unit) {
        val tmpFile = Files.createTempFile("headerTests", ext).toFile()
        try {
            tmpFile.writeText(code)
            val f = loadTemplateFile(tmpFile)
            f.op()
        } finally {
            tmpFile.delete()
        }
    }

    private fun testTemplates(
        props: Map<String, String>,
        template: List<Pair<String, String>>,
        op: (RenderingContext) -> Unit
    ) {
        fun rec(cxt: RenderingContext, remaning: List<Pair<String, String>>) {
            if (remaning.isEmpty()) op(cxt)
            else {
                val (code, ext) = remaning.first()
                testTemplate(code, ext) {
                    rec(cxt.copy(layouts = cxt.layouts + (name() to this)), remaning.drop(1))
                }
            }
        }
        rec(RenderingContext(props), template.toList())
    }


    @Test
    fun testParsingHeaders() {
        testTemplate(
            """
            ---
            title: myTitle
            ---
            code
            """.trimIndent()
        ) {
            assertEquals(rawCode, "code")
            assertEquals(title(), "myTitle")
        }
    }

    @Test
    fun layout() {
        val base =
            """
            ---
            title: myTitle
            name: base
            ---
            Ala {{ content }}. {{p2}}!
            """.trimIndent()

        val content =
            """
                ---
                layout: base
                name: content
                ---
                ma kota w **{{ p1 }}**
                """.trimIndent()

        testTemplates(
            mapOf("p1" to "paski", "p2" to "Hej"),
            listOf(base to "html", content to "md")
        ) {
            assertEquals(
                "<p>Ala <p>ma kota w <strong>paski</strong></p>\n. Hej!</p>",
                it.layouts["content"]!!.resolve(it).html.trim()
            )
        }
    }

    @Test
    fun nestedLayout() {
        val toplevel =
            """
            ---
            name: toplevel
            ---
            <div id="root">{{ content }}</div>
            """.trimIndent()

        val basePage =
            """
            ---
            layout: toplevel
            name: basePage
            ---
            # {{ pageName }}
    
            {{content}}
            
            ## {{ pageName }} end
            """.trimIndent()

        val content =
            """
            ---
            layout: basePage
            name: content
            ---
            Hello {{ name }}!
            """.trimIndent()


        val expected =
            """
                <div id="root"><h1>Test page</h1>
                <p>Hello world!!</p>
                <h2>Test page end</h2>
                </div>
            """.trimIndent()

        testTemplates(
            mapOf("pageName" to "Test page", "name" to "world!"),
            listOf(
                toplevel to "html",
                basePage to "md",
                content to "html"
            )
        ){
            assertEquals(
                expected,
                it.layouts["content"]!!.resolve(it).html.trim()
            )
        }
    }

    @Test
    fun markdown() {
        testTemplate(
            """
            # Hello {{ msg }}!
            """.trimIndent(),
            ext = "md"
        ) {
            assertEquals("<h1>Hello there!</h1>", resolve(RenderingContext(mapOf("msg" to "there"))).html.trim())
        }
    }

    @Test
    fun mixedTeplates() {
        testTemplate(
            """
            # Hello {{ msg }}!
            """.trimIndent(),
            ext = "md"
        ) {
            assertEquals("<h1>Hello there!</h1>", resolve(RenderingContext(mapOf("msg" to "there"))).html.trim())
        }
    }
}