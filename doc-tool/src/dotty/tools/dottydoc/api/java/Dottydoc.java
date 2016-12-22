package dotty.tools.dottydoc.api.java;

import dotty.tools.dottydoc.DocDriver;
import dotty.tools.dottydoc.model.Package;
import dotty.tools.dottydoc.util.OutputWriter;
import java.util.Map;
import java.util.List;
import java.net.URL;

/**
 * The Dottydoc API is fairly simple. The tool creates an index by calling:
 * "createIndex" with the same argument list as you would the compiler - e.g:
 *
 * {{{
 * String[] array = {
 *     "-language:Scala2"
 * };
 *
 * Map<String, Package> index = createIndex(array);
 * }}}
 *
 * Once the index has been generated, the tool can also build a documentation
 * API given a Mustache template and a flat resources structure (i.e. absolute
 * paths to each resource, which will be put in the same directory).
 *
 * {{{
 * buildDocs("path/to/output/dir", templateURL, resources, index);
 * }}}
 *
 * The tool can also generate JSON from the created index using "toJson(index)"
 * or directly using "createJsonIndex"
 */
public class Dottydoc {
    private DocDriver driver = new DocDriver();

    /** Creates index from compiler arguments */
    public Map<String, Package> createIndex(String[] args) {
        return driver.compiledDocsJava(args);
    }

    /** Creates JSON from compiler arguments */
    public String createJsonIndex(String[] args) {
        return driver.indexToJsonJava(createIndex(args));
    }

    public String toJson(Map<String, Package> index) {
        return driver.indexToJsonJava(index);
    }

    /** Creates a documentation from the given parameters */
    public void buildDocs(
        String outputDir,
        URL template,
        List<URL> resources,
        Map<String, Package> index
    ) {
        new OutputWriter().writeJava(index, outputDir, template, resources);
    }

    /** Writes JSON to an output directory as "index.json" */
    public void writeJson(Map<String, Package> index, String outputDir) {
        new OutputWriter().writeJsonJava(index, outputDir);
    }
}
