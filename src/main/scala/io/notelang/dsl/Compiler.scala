package io.notelang.dsl

sealed trait ILExpr

case class ILNote(symbol: Char, accidental: Option[Int]) extends ILExpr

case object ILRest extends ILExpr

case class ILDuration(expr: ILExpr, power: Int, dots: Int) extends ILExpr

case class ILOctave(expr: ILExpr, value: Int) extends ILExpr

case class ILFragment(exprs: Seq[ILExpr]) extends ILExpr

case class ILHarmony(exprs: Seq[ILExpr]) extends ILExpr

case class ILSegment(exprs: Seq[ILExpr]) extends ILExpr

case class ILKey(tonic: Note, expr: ILExpr) extends ILExpr

case class Function(argNames: List[String], body: Expr)

object Compiler {

  type Environment = Map[String, Either[ILExpr, Function]]

  def compile(program: Block, env: Environment = Map.empty): ILSegment = {
    val updatedEnv = program.statements.foldLeft(env) {
      case (env, statement) => statement match {
        case Chord(suffix, exprs) => env + (s"::$suffix" -> Right(Function(Nil, Block(Seq.empty, exprs))))
        case AssignVar(name, expr) => env + (name -> Left(eval(expr)(env)))
        case DeclareFunc(name, argNames, body) => env + (name -> Right(Function(argNames, body)))
      }
    }
    ILSegment(program.exprs.map(eval(_)(updatedEnv)))
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
      case block: Block => compile(block, env)
      case ChordRef(note, suffix) =>
        val exprs = env(s"::$suffix").getOrElse(throw new IllegalArgumentException).body.asInstanceOf[Block].exprs
        ILKey(note, ILSegment(exprs.map(eval)))
      case VarRef(name) => env(name).swap.getOrElse(throw new IllegalArgumentException)
      case FuncRef(name, args) =>
        val f = env(name).getOrElse(throw new IllegalArgumentException)
        eval(f.body)(env ++ f.argNames.zip(args).map {
          case (name, expr) => name -> Left(eval(expr))
        })
    }

}
