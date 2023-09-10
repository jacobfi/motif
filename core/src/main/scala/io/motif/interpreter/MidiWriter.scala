package io.motif.interpreter

trait MidiWriter {

  val ticksPerBeat: Int

  def noteOn(semitone: Int, channel: Int, ticks: Int): Unit

  def noteOff(semitone: Int, channel: Int, ticks: Int): Unit

  def changeTempo(bpm: Int, ticks: Int): Unit

  def changeInstrument(program: Int, channel: Int, ticks: Int): Unit

}
