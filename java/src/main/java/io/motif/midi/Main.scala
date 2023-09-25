package io.motif.midi

import io.motif.dsl.*
import io.motif.interpreter.MidiSequencer

import scala.io.Source
import scala.util.Using

object Main:

  def main(args: Array[String]): Unit =
    if args.isEmpty then
      println("Please provide a filename!")
      System.exit(0)

    val filename = args.head
    val input = Using(Source.fromFile(filename))(_.getLines().mkString)
      .getOrElse(Source.fromResource(filename).mkString)

    println(s"Compiling $filename...")
    val program = NoteParser.read(input)
    val segment = Compiler.compile(program)

    println("Writing MIDI...")
    val midi = new JavaMidiWriter
    val sequencer = new MidiSequencer(midi)
    sequencer.add(segment)

    println("Begin playback")
    val player = new JavaMidiPlayer
    player.play(midi.getSequence)
