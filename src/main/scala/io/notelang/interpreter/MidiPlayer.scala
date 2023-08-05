package io.notelang.interpreter

import javax.sound.midi._

class MidiPlayer extends MetaEventListener {

  private val sequencer = MidiSystem.getSequencer
  sequencer.addMetaEventListener(this)

  def play(sequence: Sequence): Unit = {
    sequencer.setSequence(sequence)
    sequencer.open()
    sequencer.start()
  }

  override def meta(meta: MetaMessage): Unit = {
    println("EOT")
    sequencer.stop()
    sequencer.close()
  }

}
