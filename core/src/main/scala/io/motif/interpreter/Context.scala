package io.motif.interpreter

case class Context
(
  noteLength: Int,
  noteValue: Int,
  channel: Int = 0,
  octave: Int = 0,
  key: Key = Key.CMajor,
  instruments: Map[Int, Int] = Map(0 -> 0),
)
