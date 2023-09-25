package io.motif.dsl

import io.motif.dsl.Expr.*
import io.motif.dsl.Statement.*

enum ILExpr:
  case ILNote(symbol: Char, accidental: Option[Int])
  case ILRest
  case ILDuration(expr: ILExpr, power: Int, dots: Int)
  case ILOctave(expr: ILExpr, value: Int)
  case ILFragment(exprs: Seq[ILExpr])
  case ILHarmony(exprs: Seq[ILExpr])
  case ILSegment(exprs: Seq[ILExpr])
  case ILKey(tonic: ILNote, mode: String, expr: ILExpr)
  case ILTime(beatsPerBar: Int, beatsPerNote: Int, expr: ILExpr)
  case ILTempo(bpm: Int, expr: ILExpr)
  case ILInstrument(program: Int, expr: ILExpr)

object Compiler:

  import ILExpr.*

  private case class Closure(f: Function)(using env: Environment)

  opaque type Environment = Map[String, ILExpr | Closure]
  opaque type Postprocess = List[ILExpr => ILExpr]

  def compile(program: Block): ILSegment =
    given Environment = Map.empty
    compile(program.statements, program.exprs) match
      case segment: ILSegment => segment
      case expr => ILSegment(Seq(expr))

  @scala.annotation.tailrec
  private def compile(statements: List[Statement], exprs: List[Expr], pp: Postprocess = Nil)
    (using env: Environment): ILExpr =
    statements match
      case head :: tail =>
        head match
          case KeyDirective(note, mode) =>
            val tonic: ILNote = ILNote(note.symbol, note.accidental)
            compile(tail, exprs, (ILKey(tonic, mode, _)) :: pp)
          case TimeDirective(bpb, bpn) => compile(tail, exprs, (ILTime(bpb, bpn, _)) :: pp)
          case TempoDirective(bpm) => compile(tail, exprs, (ILTempo(bpm, _)) :: pp)
          case InstrumentDirective(program) => compile(tail, exprs, (ILInstrument(program, _)) :: pp)
          case Assignment(name, value) =>
            compile(tail, exprs, pp)(using env + (name -> expand(value)))
      case Nil =>
        pp.foldLeft(ILSegment(exprs.map(eval)): ILExpr):
          case (expr, process) => process(expr)

  private def eval(expr: Expr)(using Environment): ILExpr = expr match
    case Note(symbol, accidental) => ILNote(symbol, accidental)
    case Rest => ILRest
    case Duration(expr, power, dots) => ILDuration(eval(expr), power, dots)
    case Octave(expr, value) => ILOctave(eval(expr), value)
    case Fragment(exprs) => ILFragment(exprs.map(eval))
    case Harmony(exprs) => ILHarmony(exprs.map(eval))
    case block: Block => compile(block.statements, block.exprs)
    case Reference(name) =>
      resolve(name).getOrElse:
        if NoteParser.chord.matches(name) then
          tryResolveChord(name.take(1), name.drop(1)).getOrElse:
            tryResolveChord(name.take(2), name.drop(2)).getOrElse:
              throw new IllegalArgumentException
        else
          throw new IllegalArgumentException

  private def expand(arg: Expr | Function)(using Environment) = arg match
    case expr: Expr => eval(expr)
    case func: Function => Closure(func)

  private def resolve(name: String)(using env: Environment) =
    env.get(name).flatMap:
      case expr: ILExpr => Some(expr)
      case _ => None

  private def tryResolveChord(prefix: String, suffix: String)(using Environment) =
    resolve(s"::$suffix").flatMap: chord =>
      val tonic: Option[ILNote] = prefix.toCharArray match
        case Array(symbol) => Some(ILNote(symbol.toLower, None))
        case Array(symbol, '#') => Some(ILNote(symbol.toLower, Some(1)))
        case Array(symbol, 'b') => Some(ILNote(symbol.toLower, Some(-1)))
        case _ => None
      tonic.map(ILKey(_, "major", chord))
