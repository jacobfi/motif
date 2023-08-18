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

  case class Closure(env: Environment, f: Function)

  type Environment = Map[String, Either[ILExpr, Closure]]

  def compile(program: Block): ILSegment = compile(program.statements, program.exprs)(Map.empty) match {
    case segment: ILSegment => segment
    case expr => ILSegment(Seq(expr))
  }

  private def compile(statements: List[Statement], exprs: List[Expr])(implicit env: Environment): ILExpr =
    statements match {
      case head :: tail =>
        head match {
          case ChangeKey(tonic) => ILKey(eval(tonic).asInstanceOf[ILNote], compile(tail, exprs))
          case Assignment(name, value) => compile(tail, exprs)(env + (name -> expand(value)))
        }
      case Nil => ILSegment(exprs.map(eval))
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
      case VarRef(name) => env(name).swap.getOrElse(throw new IllegalArgumentException)
      case FuncRef(name, args) =>
        val Closure(localEnv, f) = env(name).getOrElse(throw new IllegalArgumentException)
        eval(f.body)(localEnv ++ f.argNames.zip(args.map(expand)))
    }

  private def expand(arg: Either[Expr, Function])(implicit env: Environment) = arg match {
    case Left(expr) => Left(eval(expr))
    case Right(func) => Right(Closure(env, func))
  }

}
