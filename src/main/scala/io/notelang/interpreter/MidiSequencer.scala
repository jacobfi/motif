package io.notelang.interpreter

import io.notelang.dsl._
import javax.sound.midi._

class MidiSequencer(ticksPerBeat: Int = 32) {

  private val middleC = 60

  private val sequence = new Sequence(Sequence.PPQ, ticksPerBeat)
  private val track = sequence.createTrack()

  def getSequence: Sequence = sequence

  def add(expr: Expr, env: Map[String, Note => Scale]): Unit = {
    implicit val context: Context = Context(ticksPerBeat, chords = env)
    val trackLength = next(expr, ticksPerBeat)
    next(Rest, trackLength)
  }

  private def next(expr: Expr, ticks: Int)(implicit ctx: Context): Int = {
    import ctx._
    expr match {
      case note: Note =>
        val noteLetter = if (note.symbol.isLetter) note.symbol else key.toLetter(note.symbol.asDigit)
        val (base, delta) = key.baseAndDelta(noteLetter)
        val semitone = middleC + base + note.accidental.getOrElse(delta) + currentOctave * 12
        track.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_ON, semitone, 127), ticks))
        track.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_OFF, semitone, 127), ticks + noteLength))
        ticks + noteLength
      case Rest => ticks + noteLength
      case Duration(expr, power, dots) =>
        val length = modifiedLength(noteLength, power, dots).round.toInt
        next(expr, ticks)(ctx.copy(noteLength = length))
      case Octave(expr, value) =>
        next(expr, ticks)(ctx.copy(currentOctave = currentOctave + value))
      case Fragment(exprs) =>
        val lengthFraction = (noteLength / exprs.map(unitLength).sum).toInt
        val fragmentCtx = ctx.copy(noteLength = lengthFraction)
        exprs.foldLeft(ticks) { case (tickSum, expr) => next(expr, tickSum)(fragmentCtx) }
        ticks + noteLength
      case Harmony(exprs) =>
        exprs.map(next(_, ticks)).last
      case Segment(exprs) =>
        exprs.foldLeft(ticks) { case (tickSum, expr) => next(expr, tickSum) }
      case Scale(tonic, expr) =>
        next(expr, ticks)(ctx.copy(key = Key.majorOf(tonic.symbol, tonic.accidental.getOrElse(0))))
      case ChordRef(note, suffix) =>
        val expr = ctx.chords(suffix)(note)
        next(expr, ticks)
    }
  }

  private def modifiedLength(noteLength: Int, power: Int, dots: Int): Double = {
    val baseLength = noteLength * math.pow(2, power)
    if (dots > 0) baseLength + baseLength * math.pow(2, -dots)
    else baseLength
  }

  private def unitLength(expr: Expr)(implicit ctx: Context): Double = expr match {
    case _: Note => 1
    case Rest => 1
    case Duration(expr, power, dots) => unitLength(expr) * modifiedLength(1, power, dots)
    case Octave(expr, _) => unitLength(expr)
    case Fragment(_) => 1
    case Harmony(exprs) => unitLength(exprs.last)
    case Segment(exprs) => exprs.map(unitLength).sum
    case Scale(_, expr) => unitLength(expr)
    case ChordRef(note, suffix) => unitLength(ctx.chords(suffix)(note))
  }

}
