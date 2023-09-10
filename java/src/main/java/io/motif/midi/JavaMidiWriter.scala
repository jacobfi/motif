package io.motif.midi

import javax.sound.midi._

class JavaMidiWriter(override val ticksPerBeat: Int = 32) extends io.motif.interpreter.MidiWriter {

  private val sequence = new Sequence(Sequence.PPQ, ticksPerBeat)
  private val track = sequence.createTrack()

  def getSequence: Sequence = sequence

  override def noteOn(semitone: Int, channel: Int, ticks: Int): Unit =
    track.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_ON, channel, semitone, 127), ticks))

  override def noteOff(semitone: Int, channel: Int, ticks: Int): Unit =
    track.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_OFF, channel, semitone, 127), ticks))

  override def changeTempo(bpm: Int, ticks: Int): Unit = {
    // https://www.recordingblogs.com/wiki/midi-set-tempo-meta-message
    val microsecondsPerQuarterNote = 60_000_000 / bpm
    val bytes = BigInt(microsecondsPerQuarterNote).toByteArray
    track.add(new MidiEvent(new MetaMessage(0x51, bytes, bytes.length), ticks))
  }

  override def changeInstrument(program: Int, channel: Int, ticks: Int): Unit =
    track.add(new MidiEvent(new ShortMessage(ShortMessage.PROGRAM_CHANGE, channel, program, 0), ticks))

}
