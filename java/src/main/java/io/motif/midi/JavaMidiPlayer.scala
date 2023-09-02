package io.motif.midi

import javax.sound.midi._

class JavaMidiPlayer extends MetaEventListener {

  private val sequencer = MidiSystem.getSequencer
  sequencer.addMetaEventListener(this)

  def play(sequence: Sequence): Unit = {
    sequencer.setSequence(sequence)
    sequencer.open()
    sequencer.start()
  }

  override def meta(msg: MetaMessage): Unit = {
    msg.getType match {
      case 0x2F =>
        println("EOT")
        sequencer.stop()
        sequencer.close()
      case _ =>
    }
  }

}
