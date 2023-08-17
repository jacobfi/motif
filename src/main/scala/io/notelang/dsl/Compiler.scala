package io.notelang.dsl

trait Function {
  def apply(args: Expr*): Expr
}

sealed trait ILExpr

case class ILNote(symbol: Char, accidental: Option[Int]) extends ILExpr

case object ILRest extends ILExpr

case class ILDuration(expr: ILExpr, power: Int, dots: Int) extends ILExpr

case class ILOctave(expr: ILExpr, value: Int) extends ILExpr

case class ILFragment(exprs: Seq[ILExpr]) extends ILExpr

case class ILHarmony(exprs: Seq[ILExpr]) extends ILExpr

case class ILSegment(exprs: Seq[ILExpr]) extends ILExpr

case class ILKey(tonic: Note, expr: ILExpr) extends ILExpr

object Compiler {

  type Environment = Map[String, Function]

  def compile(program: Block): ILSegment = { // FIXME: compile env
    val environment = program.statements.foldLeft(Map.empty[String, Function]) {
      case (env, statement) => eval(statement, env)
    }
    ILSegment(program.exprs.map(eval(_)(environment)))
  }

  private def eval(statement: Statement, env: Environment): Environment =
    statement match {
      case Chord(suffix, f) => env + (suffix -> {
        case note: Note => f(note)
        case _ => throw new UnsupportedOperationException
      })
    }

  private def eval(expr: Expr)(implicit env: Environment): ILExpr =
    expr match {
      case Note(symbol, accidental) => ILNote(symbol, accidental)
      case Rest => ILRest
      case Duration(expr, power, dots) => ILDuration(eval(expr), power, dots)
      case Octave(expr, value) => ILOctave(eval(expr), value)
      case Fragment(exprs) => ILFragment(exprs.map(eval))
      case Harmony(exprs) => ILHarmony(exprs.map(eval))
      case Scale(tonic, exprs) => ILKey(tonic, ILSegment(exprs.map(eval)))
      case block: Block => compile(block)
      case ChordRef(note, suffix) => eval(env(suffix)(note))
    }

}
