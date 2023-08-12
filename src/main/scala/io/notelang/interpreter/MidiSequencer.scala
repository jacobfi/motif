package io.notelang.interpreter

import io.notelang.dsl._
import javax.sound.midi._

class MidiSequencer(ticksPerBeat: Int = 32) {

  private val noteIndex = Array('c', 'd', 'e', 'f', 'g', 'a', 'b')
  private val majorScale = Array(2, 2, 1, 2, 2, 2, 1)
  private val minorScale = Array(2, 1, 2, 2, 1, 2, 2)

  private val middleC = 60
  private val cMajor = Note('c', None)

  private val sequence = new Sequence(Sequence.PPQ, ticksPerBeat)
  private val track = sequence.createTrack()

  def getSequence: Sequence = sequence

  def addSegment(segment: Segment): Unit = {
    val segmentLength = next(segment, ticksPerBeat, 0, cMajor, ticksPerBeat)
    next(Rest, ticksPerBeat, 0, cMajor, segmentLength)
  }

  private def next(token: Token, noteLength: Int, currentOctave: Int, key: Note, ticks: Int): Int = token match {
    case note: Note =>
      val semitone = pitch(note, currentOctave, key)
      track.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_ON, semitone, 127), ticks))
      track.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_OFF, semitone, 127), ticks + noteLength))
      ticks + noteLength
    case Rest => ticks + noteLength
    case Duration(token, power, dots) =>
      val length = modifiedLength(noteLength, power, dots).round.toInt
      next(token, length, currentOctave, key, ticks)
    case Octave(token, value) =>
      next(token, noteLength, currentOctave + value, key, ticks)
    case Fragment(tokens) =>
      val lengthFraction = (noteLength / tokens.map(unitLength).sum).toInt
      tokens.foldLeft(ticks) {
        case (tickSum, token) => next(token, lengthFraction, currentOctave, key, tickSum)
      }
      ticks + noteLength
    case Harmony(tokens) =>
      tokens.map(next(_, noteLength, currentOctave, key, ticks)).last
    case Segment(tokens) =>
      tokens.foldLeft(ticks) {
        case (tickSum, token) => next(token, noteLength, currentOctave, key, tickSum)
      }
    case Scale(root, token) =>
      next(token, noteLength, currentOctave, root, ticks)
  }

  private def pitch(note: Note, octave: Int, key: Note): Int = {
    val scaleIndex =
      if (note.symbol.isLetter) noteIndex.indexOf(note.symbol)
      else (noteIndex.indexOf(key.symbol) + note.symbol.asDigit - 1) % 7
    val shift = note.accidental.fold(0)(_.value) + key.accidental.fold(0)(_.value)
    val delta = majorScale.take(scaleIndex).sum
    middleC + delta + shift + octave * 12
  }

  private def modifiedLength(noteLength: Int, power: Int, dots: Int): Double = {
    val baseLength = noteLength * math.pow(2, power)
    if (dots > 0) baseLength + baseLength * math.pow(2, -dots)
    else baseLength
  }

  private def unitLength(token: Token): Double = token match {
    case _: Note => 1
    case Rest => 1
    case Duration(token, power, dots) => unitLength(token) * modifiedLength(1, power, dots)
    case Octave(token, _) => unitLength(token)
    case Fragment(_) => 1
    case Harmony(tokens) => unitLength(tokens.last)
    case Segment(tokens) => tokens.map(unitLength).sum
    case Scale(_, token) => unitLength(token)
  }

}
