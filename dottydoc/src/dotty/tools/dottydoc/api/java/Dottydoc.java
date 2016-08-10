package dotty.tools.dottydoc.api.java;

import dotty.tools.dottydoc.DottyDocDriver;
import dotty.tools.dottydoc.model.Package;
import dotty.tools.dottydoc.util.OutputWriter;
import java.util.Map;
import java.util.List;

/** FIXME: document me! */
public class Dottydoc extends DottyDocDriver {
    public Map<String, Package> createIndex(String[] args) {
        return compiledDocsJava(args);
    }

    public String createJsonIndex(String[] args) {
        return indexToJson(createIndex(args));
    }

    public void buildDocs(
        String outputDir,
        String templatePath,
        List<String> resources,
        Map<String, Package> index
    ) {
        new OutputWriter().writeJava(index, templatePath, outputDir, resources);
    }
}
