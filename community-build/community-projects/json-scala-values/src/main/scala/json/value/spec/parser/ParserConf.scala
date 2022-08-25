package json.value.spec.parser

import com.github.plokhotnyuk.jsoniter_scala.core.{ReaderConfig,WriterConfig}

private[value] object ParserConf:
  
  val DEFAULT_READER_CONFIG = 
    ReaderConfig.withThrowReaderExceptionWithStackTrace(false)
                .withAppendHexDumpToParseException(false)
                .withPreferredBufSize(32768)
                .withPreferredCharBufSize(4096)
                .withCheckForEndOfInput(true)

  val DEFAULT_WRITER_CONFIG =
    WriterConfig.withThrowWriterExceptionWithStackTrace(false)
      .withIndentionStep(4)
      .withPreferredBufSize(32768)
      .withEscapeUnicode(false)

