package io.notelang.example

import io.notelang.dsl._
import io.notelang.interpreter._

import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {
    val program = NoteParser.read(Source.fromResource("test.n").mkString)
    val segment = Compiler.compile(program)
    val sequencer = new MidiSequencer()
    sequencer.add(segment)
    val player = new MidiPlayer
    player.play(sequencer.getSequence)
  }

}
