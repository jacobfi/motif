package io.notelang.interpreter

case class Context(
  noteLength: Int,
  currentOctave: Int = 0,
  key: Key = Key.CMajor,
)
