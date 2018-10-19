import * as vscode from 'vscode'
import { RequestType, NotificationType } from 'vscode-jsonrpc'
import { VersionedTextDocumentIdentifier } from 'vscode-languageserver-protocol'

import { client } from './extension'

/** The parameters for the `worksheet/run` request. */
export interface WorksheetRunParams {
  textDocument: VersionedTextDocumentIdentifier
}

/** The result of the `worksheet/run` request. */
export interface WorksheetRunResult {
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

export function asWorksheetRunParams(textDocument: vscode.TextDocument): WorksheetRunParams {
	return {
    textDocument: asVersionedTextDocumentIdentifier(textDocument)
	}
}

/** The `worksheet/run` request */
export namespace WorksheetRunRequest {
 export const type = new RequestType<WorksheetRunParams, WorksheetRunResult, void, void>("worksheet/run")
}

/** The `worksheet/publishOutput` notification */
export namespace WorksheetPublishOutputNotification {
	export const type = new NotificationType<WorksheetPublishOutputParams, void>("worksheet/publishOutput")
}
