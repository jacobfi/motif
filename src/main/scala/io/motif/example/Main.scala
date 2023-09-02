package io.motif.example

import io.motif.dsl._
import io.motif.interpreter._

import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {
    val program = NoteParser.read(Source.fromResource(args.head).mkString)
    val segment = Compiler.compile(program)
    val sequencer = new MidiSequencer()
    sequencer.add(segment)
    val player = new MidiPlayer
    player.play(sequencer.getSequence)
  }

}
