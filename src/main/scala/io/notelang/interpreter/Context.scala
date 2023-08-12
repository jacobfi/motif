package io.notelang.interpreter

import io.notelang.dsl._

case class Context(
  noteLength: Int,
  currentOctave: Int = 0,
  key: Note = Note('c', None),
  chords: Map[String, Note => Scale] = Map.empty,
)
