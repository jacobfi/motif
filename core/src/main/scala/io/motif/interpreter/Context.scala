package io.motif.interpreter

case class Context
(
  noteLength: Int,
  noteValue: Int,
  octave: Int = 0,
  key: Key = Key.CMajor,
)
