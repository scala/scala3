import * as vscode from 'vscode'
import { RequestType, NotificationType } from 'vscode-jsonrpc'
import { VersionedTextDocumentIdentifier } from 'vscode-languageserver-protocol'

import { client } from './extension'

/** The parameters for the `worksheet/exec` request. */
export interface WorksheetExecParams {
  textDocument: VersionedTextDocumentIdentifier
}

/** The result of the `worksheet/exec` request. */
export interface WorksheetExecResult {
  success: boolean
}

/** The parameters for the `worksheet/publishOutput` notification. */
export interface WorksheetPublishOutputParams {
  textDocument: VersionedTextDocumentIdentifier
  line: number
  content: string
}

// TODO: Can be removed once https://github.com/Microsoft/vscode-languageserver-node/pull/421
// is merged.
export function asVersionedTextDocumentIdentifier(textDocument: vscode.TextDocument): VersionedTextDocumentIdentifier {
	return {
		uri: client.code2ProtocolConverter.asUri(textDocument.uri),
		version: textDocument.version
	}
}

export function asWorksheetExecParams(textDocument: vscode.TextDocument): WorksheetExecParams {
	return {
    textDocument: asVersionedTextDocumentIdentifier(textDocument)
	}
}

/** The `worksheet/exec` request */
export namespace WorksheetExecRequest {
 export const type = new RequestType<WorksheetExecParams, WorksheetExecResult, void, void>("worksheet/exec")
}

/** The `worksheet/publishOutput` notification */
export namespace WorksheetPublishOutputNotification {
	export const type = new NotificationType<WorksheetPublishOutputParams, void>("worksheet/publishOutput")
}
