package io.notelang.dsl

sealed trait ILExpr

case class ILNote(symbol: Char, accidental: Option[Int]) extends ILExpr

case object ILRest extends ILExpr

case class ILDuration(expr: ILExpr, power: Int, dots: Int) extends ILExpr

case class ILOctave(expr: ILExpr, value: Int) extends ILExpr

case class ILFragment(exprs: Seq[ILExpr]) extends ILExpr

case class ILHarmony(exprs: Seq[ILExpr]) extends ILExpr

case class ILSegment(exprs: Seq[ILExpr]) extends ILExpr

case class ILKey(tonic: ILNote, expr: ILExpr) extends ILExpr

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
          case ChangeKey(expr) =>
            val tonic = eval(expr).asInstanceOf[ILNote]
            compile(tail, exprs, (ILKey(tonic, _)) :: pp)
          case Assignment(name, value) => compile(tail, exprs)(env + (name -> expand(value)))
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
      case VarRef(name) =>
        env.get(name).flatMap(_.swap.toOption) match {
          case Some(value) => value
          case _ if NoteParser.chord.matches(name) =>
            tryChord(name.take(1), name.drop(1)).getOrElse {
              tryChord(name.take(2), name.drop(2)).getOrElse(throw new IllegalArgumentException)
            }
          case _ => throw new IllegalArgumentException
        }
      case FuncRef(name, args) =>
        val Closure(localEnv, f) = env(name).getOrElse(throw new IllegalArgumentException)
        eval(f.body)(localEnv ++ f.argNames.zip(args.map(expand)))
    }

  private def expand(arg: Either[Expr, Function])(implicit env: Environment) = arg match {
    case Left(expr) => Left(eval(expr))
    case Right(func) => Right(Closure(env, func))
  }

  private def tryChord(prefix: String, suffix: String)(implicit env: Environment) =
    for {
      entry <- env.get(s"::$suffix")
      chord <- entry.toOption
      note <- prefix.toCharArray match {
        case Array(symbol) => Some(ILNote(symbol.toLower, None))
        case Array(symbol, '#') => Some(ILNote(symbol.toLower, Some(1)))
        case Array(symbol, 'b') => Some(ILNote(symbol.toLower, Some(-1)))
        case _ => None
      }
    } yield eval(chord.f.body)(chord.env + ("$" -> Left(note)))

}
