package dotty.tools.dottydoc.api.java;

import dotty.tools.dottydoc.DocDriver;
import dotty.tools.dottydoc.model.Package;
import dotty.tools.dottydoc.util.OutputWriter;
import java.util.Map;
import java.util.List;
import java.net.URL;

/** FIXME: document me! */
public class Dottydoc extends DocDriver {
    public Map<String, Package> createIndex(String[] args) {
        return compiledDocsJava(args);
    }

    public String createJsonIndex(String[] args) {
        return indexToJson(createIndex(args));
    }

    public void buildDocs(
        String outputDir,
        URL template,
        List<URL> resources,
        Map<String, Package> index
    ) {
        new OutputWriter().writeJava(index, outputDir, template, resources);
    }

    public void writeJson(Map<String, Package> index, String outputDir) {
        new OutputWriter().writeJsonJava(index, outputDir);
    }
}
