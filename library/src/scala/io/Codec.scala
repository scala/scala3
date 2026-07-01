/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package io

import scala.language.`2.13`
import java.nio.charset.{CharacterCodingException, Charset, CharsetDecoder, CharsetEncoder, CodingErrorAction => Action}
import java.nio.charset.StandardCharsets.{ISO_8859_1, UTF_8}
import scala.annotation.migration
import scala.language.implicitConversions

// Some notes about encodings for use in refining this implementation.
//
// Emails: encoding recorded in header, e.g. Content-Type: charset= "iso-8859-1"
// HTML: optional content-type meta tag.
//   <meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">
// XML: optional encoding parameter.
//   <?xml version="1.0" encoding="ISO8859-1" ?>
//
// MacRoman vs. UTF-8: see https://groups.google.com/d/msg/jruby-developers/-qtwRhoE1WM/whSPVpTNV28J
// -Dfile.encoding: see https://bugs.java.com/view_bug.do?bug_id=4375816

/** A class for character encoding/decoding preferences.
 *
 *  @param charSet the character set used for encoding and decoding operations
 */
class Codec(val charSet: Charset) {
  type Configure[T] = (T => T, Boolean)
  type Handler      = CharacterCodingException => Int

  // these variables allow configuring the Codec object, and then
  // all decoders and encoders retrieved from it will use these settings.
  private var _onMalformedInput: Action | Null         = null
  private var _onUnmappableCharacter: Action | Null    = null
  private var _encodingReplacement: Array[Byte] | Null = null
  private var _decodingReplacement: String | Null      = null
  private var _onCodingException: Handler       = e => throw e

  /** The name of the Codec. */
  override def toString(): String = name

  // these methods can be chained to configure the variables above
  /** Configures the action taken when malformed input is encountered during encoding or decoding.
   *
   *  @param newAction the action to apply to malformed input
   *  @return this `Codec`, to allow configuration calls to be chained
   */
  def onMalformedInput(newAction: Action): this.type              = { _onMalformedInput = newAction ; this }
  /** Configures the action taken when an unmappable character is encountered during encoding or decoding.
   *
   *  @param newAction the action to apply to unmappable characters
   *  @return this `Codec`, to allow configuration calls to be chained
   */
  def onUnmappableCharacter(newAction: Action): this.type         = { _onUnmappableCharacter = newAction ; this }
  /** Configures the string substituted for input that cannot be decoded.
   *
   *  @param newReplacement the replacement string used by the decoder
   *  @return this `Codec`, to allow configuration calls to be chained
   */
  def decodingReplaceWith(newReplacement: String): this.type      = { _decodingReplacement = newReplacement ; this }
  /** Configures the bytes substituted for characters that cannot be encoded.
   *
   *  @param newReplacement the replacement bytes used by the encoder
   *  @return this `Codec`, to allow configuration calls to be chained
   */
  def encodingReplaceWith(newReplacement: Array[Byte]): this.type = { _encodingReplacement = newReplacement ; this }
  /** Configures the handler invoked by `wrap` when a `CharacterCodingException` is thrown.
   *
   *  @param handler the handler applied to a coding exception
   *  @return this `Codec`, to allow configuration calls to be chained
   */
  def onCodingException(handler: Handler): this.type              = { _onCodingException = handler ; this }

  /** Returns the name of the character set used by this `Codec`. */
  def name: String = charSet.name
  /** Returns a `CharsetEncoder` for this `Codec`'s character set, configured with this `Codec`'s
   *  malformed-input, unmappable-character, and replacement settings.
   */
  def encoder: CharsetEncoder = {
    val enc = charSet.newEncoder()
    if (_onMalformedInput ne null) enc.onMalformedInput(_onMalformedInput)
    if (_onUnmappableCharacter ne null) enc.onUnmappableCharacter(_onUnmappableCharacter)
    if (_encodingReplacement ne null) enc.replaceWith(_encodingReplacement)
    enc
  }
  /** Returns a `CharsetDecoder` for this `Codec`'s character set, configured with this `Codec`'s
   *  malformed-input, unmappable-character, and replacement settings.
   */
  def decoder: CharsetDecoder = {
    val dec = charSet.newDecoder()
    if (_onMalformedInput ne null) dec.onMalformedInput(_onMalformedInput)
    if (_onUnmappableCharacter ne null) dec.onUnmappableCharacter(_onUnmappableCharacter)
    if (_decodingReplacement ne null) dec.replaceWith(_decodingReplacement)
    dec
  }

  /** Evaluates `body`, routing any thrown `CharacterCodingException` to the configured exception handler.
   *
   *  @param body the encoding or decoding operation to evaluate
   *  @return the result of `body`, or the handler's result if a `CharacterCodingException` is thrown
   */
  def wrap(body: => Int): Int =
    try body catch { case e: CharacterCodingException => _onCodingException(e) }
}

/** Provides the lowest-priority implicit `Codec`, used as a fallback when no
 *  other `Codec` is in scope.
 */
trait LowPriorityCodecImplicits {
  self: Codec.type =>

  /** The Codec of Last Resort. */
  implicit lazy val fallbackSystemCodec: Codec = defaultCharsetCodec
}

object Codec extends LowPriorityCodecImplicits {
  final val ISO8859: Codec = Codec(ISO_8859_1)
  final val UTF8: Codec    = Codec(UTF_8)

  /** Optimistically these two possible defaults will be the same thing.
   *  In practice this is not necessarily true, and in fact Sun classifies
   *  the fact that you can influence anything at all via -Dfile.encoding
   *  as an accident, with any anomalies considered "not a bug".
   */
  def defaultCharsetCodec: Codec = apply(Charset.defaultCharset)
  /** Returns a `Codec` for the encoding named by the `file.encoding` system
   *  property.
   */
  def fileEncodingCodec: Codec = apply(scala.util.Properties.encodingString)
  /** Returns the default `Codec`, backed by the JVM's default charset. */
  def default: Codec = defaultCharsetCodec

  /** Creates a `Codec` for the named character set.
   *
   *  @param encoding the name of the character set, as understood by `Charset.forName`
   *  @return a `Codec` backed by the named character set
   */
  def apply(encoding: String): Codec        = new Codec(Charset.forName(encoding))
  /** Creates a `Codec` backed by the given character set.
   *
   *  @param charSet the character set to use for encoding and decoding
   *  @return a `Codec` backed by `charSet`
   */
  def apply(charSet: Charset): Codec        = new Codec(charSet)
  /** Creates a `Codec` that uses the given `CharsetDecoder` for decoding.
   *
   *  @param decoder the decoder to use; its `charset` becomes the `Codec`'s character set
   *  @return a `Codec` whose `decoder` is `decoder`
   */
  def apply(decoder: CharsetDecoder): Codec = {
    val _decoder = decoder
    new Codec(decoder.charset()) { override def decoder = _decoder }
  }

  @migration("This method was previously misnamed `toUTF8`. Converts from Array[Byte] to Array[Char].", "2.9.0")
  /** Decodes UTF-8 encoded bytes into an array of characters.
   *
   *  @param bytes the UTF-8 encoded bytes to decode
   *  @return the decoded characters
   */
  def fromUTF8(bytes: Array[Byte]): Array[Char] = fromUTF8(bytes, 0, bytes.length)
  /** Decodes a range of UTF-8 encoded bytes into an array of characters.
   *
   *  @param bytes the array containing the UTF-8 encoded bytes
   *  @param offset the index of the first byte to decode
   *  @param len the number of bytes to decode
   *  @return the decoded characters
   */
  def fromUTF8(bytes: Array[Byte], offset: Int, len: Int): Array[Char] = {
    val bbuffer = java.nio.ByteBuffer.wrap(bytes, offset, len)
    val cbuffer = UTF8.charSet.decode(bbuffer)
    val chars   = new Array[Char](cbuffer.remaining())
    cbuffer.get(chars)

    chars
  }

  @migration("This method was previously misnamed `fromUTF8`. Converts from character sequence to Array[Byte].", "2.9.0")
  /** Encodes a character sequence into UTF-8 bytes.
   *
   *  @param cs the characters to encode
   *  @return the UTF-8 encoded bytes
   */
  def toUTF8(cs: CharSequence): Array[Byte] = {
    val cbuffer = java.nio.CharBuffer.wrap(cs, 0, cs.length)
    val bbuffer = UTF8.charSet.encode(cbuffer)
    val bytes = new Array[Byte](bbuffer.remaining())
    bbuffer.get(bytes)

    bytes
  }
  /** Encodes a range of characters into UTF-8 bytes.
   *
   *  @param chars the array containing the characters to encode
   *  @param offset the index of the first character to encode
   *  @param len the number of characters to encode
   *  @return the UTF-8 encoded bytes
   */
  def toUTF8(chars: Array[Char], offset: Int, len: Int): Array[Byte] = {
    val cbuffer = java.nio.CharBuffer.wrap(chars, offset, len)
    val bbuffer = UTF8.charSet.encode(cbuffer)
    val bytes = new Array[Byte](bbuffer.remaining())
    bbuffer.get(bytes)

    bytes
  }

  /** Converts a character set name to a `Codec`.
   *
   *  @param s the name of the character set
   *  @return a `Codec` for the named character set
   */
  implicit def string2codec(s: String): Codec           = apply(s)
  /** Converts a `Charset` to a `Codec`.
   *
   *  @param c the character set to wrap
   *  @return a `Codec` backed by `c`
   */
  implicit def charset2codec(c: Charset): Codec         = apply(c)
  /** Converts a `CharsetDecoder` to a `Codec`.
   *
   *  @param cd the decoder to wrap
   *  @return a `Codec` that uses `cd` for decoding
   */
  implicit def decoder2codec(cd: CharsetDecoder): Codec = apply(cd)
}
