package dotty.tools.scaladoc
package snippets

class SnippetE2eTestcase1 extends SnippetsE2eTest("snippetTestcase1", SCFlags.Compile, false)

class SnippetE2eTestcase1Debug extends SnippetsE2eTest("snippetTestcase1", SCFlags.Compile, true)

class SnippetE2eTestcase2 extends SnippetsE2eTest("snippetTestcase2", SCFlags.Compile, false)

class SnippetE2eTestcase2Debug extends SnippetsE2eTest("snippetTestcase2", SCFlags.Compile, true)
