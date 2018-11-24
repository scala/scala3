import * as vscode from 'vscode'
import {
  BaseLanguageClient, ClientCapabilities, DynamicFeature, ServerCapabilities,
  TextDocumentFeature, TextDocumentRegistrationOptions,
} from 'vscode-languageclient'
import { generateUuid } from 'vscode-languageclient/lib/utils/uuid'
import { DocumentSelector } from 'vscode-languageserver-protocol'
import { Disposable } from 'vscode-jsonrpc'

import { WorksheetRunRequest, TastyDecompileRequest } from './protocol'
import { WorksheetProvider } from './worksheet'
import { TastyDecompilerProvider, DecompiledDocumentProvider } from './tasty-decompiler'

// Remove this if
// https://github.com/Microsoft/vscode-languageserver-node/issues/423 is fixed.
function ensure<T, K extends keyof T>(target: T, key: K): T[K] {
  if (target[key] === void 0) {
    target[key] = {} as any
  }
  return target[key]
}

export interface WorksheetClientCapabilities {
  worksheet?: {
    run?: {
      dynamicRegistration?: boolean
    }
  }
}

export interface WorksheetServerCapabilities {
  /**
   * The server provides support for running worksheets.
   */
  worksheetRunProvider?: boolean
}

export class WorksheetRunFeature extends TextDocumentFeature<TextDocumentRegistrationOptions> {
  constructor(client: BaseLanguageClient) {
    super(client, WorksheetRunRequest.type)
  }

  public fillClientCapabilities(capabilities: ClientCapabilities & WorksheetClientCapabilities): void {
    ensure(ensure(capabilities, "worksheet")!, "run")!.dynamicRegistration = true
  }

  public initialize(capabilities: ServerCapabilities & WorksheetServerCapabilities, documentSelector: DocumentSelector): void {
    if (!capabilities.worksheetRunProvider) {
      return
    }

    const selector: DocumentSelector = [ { language: 'scala', pattern: '**/*.sc' } ]
    this.register(this.messages, {
      id: generateUuid(),
      registerOptions: { documentSelector: selector }
    })
  }

  protected registerLanguageProvider(options: TextDocumentRegistrationOptions): Disposable {
    let client = this._client
    return new WorksheetProvider(client, options.documentSelector!)
  }
}

export interface TastyDecompilerServerCapabilities {
  /**
   * The server provides support for decompiling Tasty files.
   */
  tastyDecompiler?: boolean
}

export interface TastyDecompilerClientCapabilities {
  tasty?: {
    decompile?: {
      dynamicRegistration?: boolean
    }
  }
}

export class TastyDecompilerFeature extends TextDocumentFeature<TextDocumentRegistrationOptions> {
  constructor(client: BaseLanguageClient, readonly provider: DecompiledDocumentProvider) {
    super(client, TastyDecompileRequest.type)
  }

  fillClientCapabilities(capabilities: ClientCapabilities & TastyDecompilerClientCapabilities): void {
    ensure(ensure(capabilities, "tasty")!, "decompile")!.dynamicRegistration = true
  }

  initialize(capabilities: ServerCapabilities & TastyDecompilerServerCapabilities, documentSelector: DocumentSelector): void {
    if (!capabilities.tastyDecompiler) {
      return
    }

    const selector: DocumentSelector = [ { language: 'tasty' } ]
    this.register(this.messages, {
      id: generateUuid(),
      registerOptions: { documentSelector: selector }
    })
  }

  protected registerLanguageProvider(options: TextDocumentRegistrationOptions): vscode.Disposable {
    let client = this._client
    return new TastyDecompilerProvider(client, options.documentSelector!, this.provider)
  }
}