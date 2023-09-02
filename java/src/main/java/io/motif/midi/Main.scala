package io.motif.midi

import io.motif.dsl._
import io.motif.interpreter.MidiSequencer

import scala.io.Source
import scala.util.Using

object Main {

  def main(args: Array[String]): Unit = {
    val input = Using(Source.fromFile(args.head))(_.getLines().mkString)
      .getOrElse(Source.fromResource(args.head).mkString)
    val program = NoteParser.read(input)
    val segment = Compiler.compile(program)

    val midi = new JavaMidiWriter
    val sequencer = new MidiSequencer(midi)
    sequencer.add(segment)

    val player = new JavaMidiPlayer
    player.play(midi.getSequence)
  }

}
