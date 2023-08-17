package io.notelang.interpreter

import io.notelang.dsl._
import javax.sound.midi._

class MidiSequencer(ticksPerBeat: Int = 32) {

  private val middleC = 60

  private val sequence = new Sequence(Sequence.PPQ, ticksPerBeat)
  private val track = sequence.createTrack()

  def getSequence: Sequence = sequence

  def add(segment: ILSegment): Unit = {
    implicit val context: Context = Context(ticksPerBeat)
    val trackLength = next(segment, ticksPerBeat)
    next(ILRest, trackLength)
  }

  private def next(expr: ILExpr, ticks: Int)(implicit ctx: Context): Int = {
    import ctx._
    expr match {
      case ILNote(symbol, accidental) =>
        val noteLetter = if (symbol.isLetter) symbol else key.toLetter(symbol.asDigit)
        val (base, delta) = key.baseAndDelta(noteLetter)
        val semitone = middleC + base + accidental.getOrElse(delta) + currentOctave * 12
        track.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_ON, semitone, 127), ticks))
        track.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_OFF, semitone, 127), ticks + noteLength))
        ticks + noteLength
      case ILRest => ticks + noteLength
      case ILDuration(expr, power, dots) =>
        val length = modifiedLength(noteLength, power, dots).round.toInt
        next(expr, ticks)(ctx.copy(noteLength = length))
      case ILOctave(expr, value) =>
        next(expr, ticks)(ctx.copy(currentOctave = currentOctave + value))
      case ILFragment(exprs) =>
        val lengthFraction = (noteLength / exprs.map(unitLength).sum).toInt
        val fragmentCtx = ctx.copy(noteLength = lengthFraction)
        exprs.foldLeft(ticks) { case (tickSum, expr) => next(expr, tickSum)(fragmentCtx) }
        ticks + noteLength
      case ILHarmony(exprs) =>
        exprs.map(next(_, ticks)).last
      case ILSegment(exprs) =>
        exprs.foldLeft(ticks) { case (tickSum, expr) => next(expr, tickSum) }
      case ILKey(tonic, expr) =>
        next(expr, ticks)(ctx.copy(key = Key.majorOf(tonic.symbol, tonic.accidental.getOrElse(0))))
    }
  }

  private def modifiedLength(noteLength: Int, power: Int, dots: Int): Double = {
    val baseLength = noteLength * math.pow(2, power)
    if (dots > 0) baseLength + baseLength * math.pow(2, -dots)
    else baseLength
  }

  private def unitLength(expr: ILExpr)(implicit ctx: Context): Double = expr match {
    case _: ILNote => 1
    case ILRest => 1
    case ILDuration(expr, power, dots) => unitLength(expr) * modifiedLength(1, power, dots)
    case ILOctave(expr, _) => unitLength(expr)
    case ILFragment(_) => 1
    case ILHarmony(exprs) => unitLength(exprs.last)
    case ILSegment(exprs) => exprs.map(unitLength).sum
    case ILKey(_, expr) => unitLength(expr)
  }

}
