package dotty.tools.languageserver.worksheet

import java.net.URI

/** The parameter for the `worksheet/exec` request. */
case class WorksheetExecParams(uri: URI)

/** The response to a `worksheet/exec` request. */
case class WorksheetExecResponse(success: Boolean)

/**
 * A notification that tells the client that a line of a worksheet
 * produced the specified output.
 */
case class WorksheetExecOutput(uri: URI, line: Int, content: String)
