package io.notelang.example

import io.notelang.dsl.NoteParser
import io.notelang.interpreter.{MidiPlayer, MidiSequencer}

import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {
    val tokens = NoteParser.read(Source.fromResource("test.n").mkString)
    val sequence = new MidiSequencer().process(tokens)
    val player = new MidiPlayer
    player.play(sequence)
  }

}
