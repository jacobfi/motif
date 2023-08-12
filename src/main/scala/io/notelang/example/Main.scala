package io.notelang.example

import io.notelang.dsl.NoteParser
import io.notelang.interpreter.{MidiPlayer, MidiSequencer}

import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {
    val segment = NoteParser.read(Source.fromResource("test.n").mkString)
    val sequencer = new MidiSequencer()
    sequencer.addSegment(segment)
    val player = new MidiPlayer
    player.play(sequencer.getSequence)
  }

}
