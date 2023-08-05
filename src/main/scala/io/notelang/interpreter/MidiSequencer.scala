package io.notelang.interpreter

import io.notelang.dsl._
import javax.sound.midi._

class MidiSequencer(ticksPerBeat: Int = 4) {

  private val noteIndex = Array('c', 'd', 'e', 'f', 'g', 'a', 'b')
  private val majorScale = Array(2, 2, 1, 2, 2, 2, 1)
  private val minorScale = Array(2, 1, 2, 2, 1, 2, 2)

  private val middleC = 60
  private val ticksPerNote = ticksPerBeat

  private val sequence = new Sequence(Sequence.PPQ, ticksPerBeat)
  private val track = sequence.createTrack()

  def process(tokens: Seq[Token]): Sequence = {
    val length = next(Group(tokens, 0))
    track.add(new MidiEvent(new ShortMessage(0xFF, 0x2F, 0x0), length + ticksPerBeat)) // EOT.
    sequence
  }

  private def next(token: Token, context: Context = Context(), ticks: Int = ticksPerBeat): Int = token match {
    case note: Note =>
      val tone = calculateSemitone(note)
      val duration = calculateNoteDuration(context.resolvePower(note.power), note.dots)
      track.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_ON, tone, 127), ticks))
      track.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_OFF, tone, 127), ticks + duration))
      ticks + duration
    case Rest(power, dots) =>
      ticks + calculateNoteDuration(context.resolvePower(power), dots)
    case Group(inner, power) =>
      inner.foldLeft(ticks) {
        case (current, token) => next(token, context.copy(power = power), current)
      }
    case Harmony(inner) =>
      val localTicks = context.resolveLastTicks(ticks)
      inner.foldLeft(localTicks) {
        case (current, token) => next(token, context.copy(lastTick = current), localTicks)
      }
    case _ => ticks
  }

  private def calculateSemitone(note: Note) = {
    val delta = majorScale.take(noteIndex.indexOf(note.letter)).sum
    val accidental = note.accidental.fold(0) {
      case Sharp => 1
      case DoubleSharp => 2
      case Flat => -1
      case DoubleFlat => -2
      case Natural => 0
    }
    middleC + delta + accidental + note.octave * 12
  }

  private def calculateNoteDuration(power: Int, dots: Int): Int = {
    val baseDuration =
      if (power > 0) ticksPerNote << power
      else ticksPerNote >> -power
    if (dots > 0) baseDuration + (baseDuration >> dots)
    else baseDuration
  }

}
