package io.motif.interpreter

import io.motif.dsl._

class MidiSequencer(midi: MidiWriter) {

  private val middleC = 60

  def add(segment: ILSegment): Unit = {
    implicit val context: Context = Context(midi.ticksPerBeat, 4)
    val trackLength = next(segment, midi.ticksPerBeat)
    next(ILRest, trackLength)
  }

  private def next(expr: ILExpr, ticks: Int)(implicit ctx: Context): Int = {
    import ctx._
    expr match {
      case ILNote(symbol, accidental) =>
        val noteLetter = if (symbol.isLetter) symbol else key.toLetter(symbol.asDigit)
        val toneValue =
          if (symbol.isDigit) key.semitone(noteLetter) + accidental.getOrElse(0) // Accidental is additive.
          else {
            val (base, delta) = key.baseAndDelta(noteLetter)
            base + accidental.getOrElse(delta) // Overwrite key-delta with accidental.
          }
        val semitone = middleC + toneValue + octave * 12
        midi.noteOn(semitone, channel, ticks)
        midi.noteOff(semitone, channel, ticks + noteLength - 1)
        ticks + noteLength
      case ILRest => ticks + noteLength
      case ILDuration(expr, power, dots) =>
        val length = modifiedLength(noteLength, power, dots).round.toInt
        next(expr, ticks)(ctx.copy(noteLength = length))
      case ILOctave(expr, value) =>
        next(expr, ticks)(ctx.copy(octave = octave + value))
      case ILFragment(exprs) =>
        val lengthFraction = (noteLength / exprs.map(unitLength).sum).toInt
        val fragmentCtx = ctx.copy(noteLength = lengthFraction)
        exprs.foldLeft(ticks) { case (tickSum, expr) => next(expr, tickSum)(fragmentCtx) }
        ticks + noteLength
      case ILHarmony(exprs) =>
        exprs.map(next(_, ticks)).last
      case ILSegment(exprs) =>
        exprs.foldLeft(ticks) { case (tickSum, expr) => next(expr, tickSum) }
      case ILKey(tonic, mode, expr) =>
        next(expr, ticks)(ctx.copy(key = Key.ofMode(mode, tonic.symbol, tonic.accidental.getOrElse(0))))
      case ILTime(_, bpn, expr) =>
        val length = (noteLength * (noteValue / bpn.toDouble)).toInt
        next(expr, ticks)(ctx.copy(noteLength = length, noteValue = bpn))
      case ILTempo(bpm, expr) =>
        midi.changeTempo(bpm, ticks)
        next(expr, ticks)
      case ILInstrument(program, expr) =>
        val instrCtx = instruments.get(program) match {
          case Some(channel) => ctx.copy(channel = channel)
          case None =>
            val nextChannel = instruments.values.max + 1
            ctx.copy(channel = nextChannel, instruments = instruments + (program -> nextChannel))
        }
        midi.changeInstrument(program, instrCtx.channel, ticks)
        next(expr, ticks)(instrCtx)
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
    case ILKey(_, _, expr) => unitLength(expr)
    case ILTime(_, _, expr) => unitLength(expr)
    case ILTempo(_, expr) => unitLength(expr)
  }

}
