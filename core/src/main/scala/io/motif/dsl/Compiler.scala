package io.motif.dsl

sealed trait ILExpr

case class ILNote(symbol: Char, accidental: Option[Int]) extends ILExpr

case object ILRest extends ILExpr

case class ILDuration(expr: ILExpr, power: Int, dots: Int) extends ILExpr

case class ILOctave(expr: ILExpr, value: Int) extends ILExpr

case class ILFragment(exprs: Seq[ILExpr]) extends ILExpr

case class ILHarmony(exprs: Seq[ILExpr]) extends ILExpr

case class ILSegment(exprs: Seq[ILExpr]) extends ILExpr

case class ILKey(tonic: ILNote, mode: String, expr: ILExpr) extends ILExpr

case class ILTime(beatsPerBar: Int, beatsPerNote: Int, expr: ILExpr) extends ILExpr

case class ILTempo(bpm: Int, expr: ILExpr) extends ILExpr

case class ILInstrument(program: Int, expr: ILExpr) extends ILExpr

object Compiler {

  private case class Closure(env: Environment, f: Function)

  private type Environment = Map[String, Either[ILExpr, Closure]]
  private type Postprocess = List[ILExpr => ILExpr]

  def compile(program: Block): ILSegment = compile(program.statements, program.exprs)(Map.empty) match {
    case segment: ILSegment => segment
    case expr => ILSegment(Seq(expr))
  }

  @scala.annotation.tailrec
  private def compile(statements: List[Statement], exprs: List[Expr], pp: Postprocess = Nil)
    (implicit env: Environment): ILExpr =
    statements match {
      case head :: tail =>
        head match {
          case KeyDirective(note, mode) =>
            val tonic = ILNote(note.symbol, note.accidental)
            compile(tail, exprs, (ILKey(tonic, mode, _)) :: pp)
          case TimeDirective(bpb, bpn) => compile(tail, exprs, (ILTime(bpb, bpn, _)) :: pp)
          case TempoDirective(bpm) => compile(tail, exprs, (ILTempo(bpm, _)) :: pp)
          case InstrumentDirective(program) => compile(tail, exprs, (ILInstrument(program, _)) :: pp)
          case Assignment(name, value) => compile(tail, exprs, pp)(env + (name -> expand(value)))
        }
      case Nil =>
        pp.foldLeft(ILSegment(exprs.map(eval)): ILExpr) {
          case (expr, process) => process(expr)
        }
    }

  private def eval(expr: Expr)(implicit env: Environment): ILExpr =
    expr match {
      case Note(symbol, accidental) => ILNote(symbol, accidental)
      case Rest => ILRest
      case Duration(expr, power, dots) => ILDuration(eval(expr), power, dots)
      case Octave(expr, value) => ILOctave(eval(expr), value)
      case Fragment(exprs) => ILFragment(exprs.map(eval))
      case Harmony(exprs) => ILHarmony(exprs.map(eval))
      case block: Block => compile(block.statements, block.exprs)
      case Reference(name) =>
        resolve(name).getOrElse({
          if (NoteParser.chord.matches(name)) {
            tryResolveChord(name.take(1), name.drop(1)).getOrElse {
              tryResolveChord(name.take(2), name.drop(2)).getOrElse(throw new IllegalArgumentException)
            }
          } else throw new IllegalArgumentException
        })
    }

  private def expand(arg: Either[Expr, Function])(implicit env: Environment) = arg match {
    case Left(expr) => Left(eval(expr))
    case Right(func) => Right(Closure(env, func))
  }

  private def resolve(name: String)(implicit env: Environment) =
    env.get(name).flatMap(_.swap.toOption)

  private def tryResolveChord(prefix: String, suffix: String)(implicit env: Environment) =
    resolve(s"::$suffix").flatMap { chord =>
      val tonic = prefix.toCharArray match {
        case Array(symbol) => Some(ILNote(symbol.toLower, None))
        case Array(symbol, '#') => Some(ILNote(symbol.toLower, Some(1)))
        case Array(symbol, 'b') => Some(ILNote(symbol.toLower, Some(-1)))
        case _ => None
      }
      tonic.map(ILKey(_, "major", chord))
    }

}
