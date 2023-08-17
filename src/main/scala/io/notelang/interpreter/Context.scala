package io.notelang.interpreter

import io.notelang.dsl._

case class Context(
  noteLength: Int,
  currentOctave: Int = 0,
  key: Key = Key.CMajor,
)
