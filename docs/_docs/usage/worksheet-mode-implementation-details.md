---
layout: doc-page
title: "Worksheet Mode - Implementation details"




---


In brief, the worksheets extend the Language Server Protocol and rely on the
Dotty REPL to evaluate code.

## Evaluation
Each of the individual expressions and statements of the worksheet is extracted
and passed to the Dotty REPL. After the REPL has finished evaluating one unit of
input, it emits a special delimiter that indicates the end of the output for
this input. (See `dotty.tools.languageserver.worksheet.InputStreamConsumer`)

This process continues until all input has been evaluated.

The Dotty REPL is run in a separate JVM. The `Evaluator` (see
`dotty.tools.languageserver.worksheet.Evaluator`) will re-use a JVM if the
configuration of the project hasn't changed.

## Communication with the client
The worksheets extend the Language Server Protocol and add one request and one
notification.

### Run worksheet request
The worksheet run request is sent from the client to the server to request that
the server runs a given worksheet and streams the result.

*Request:*

 - method: `worksheet/run`
 - params: `WorksheetRunParams` defined as follows:
   ```typescript
   interface WorksheetRunParams {
       /**
        * The worksheet to evaluate.
        */
       textDocument: VersionedTextDocumentIdentifier;
   }
   ```

*Response:*

 - result: `WorksheetRunResult` defined as follows:
   ```typescript
   interface WorksheetRunResult {
       /**
        * Indicates whether evaluation was successful.
        */
       success: boolean;
   }
   ```

### Worksheet output notification
The worksheet output notification is sent from the server to the client to
indicate that worksheet execution has produced some output.

*Notification:*

 - method: `worksheet/publishOutput`
 - params: `WorksheetRunOutput` defined as follows:
   ```typescript
   interface WorksheetRunOutput {
       /**
        * The worksheet that produced this output.
        */
       textDocument: VersionedTextDocumentIdentifier;

       /**
        * The range of the expression that produced this output.
        */
       range: Range;

       /**
        * The output that has been produced.
        */
       content: string;
   }
   ```
