package io.notelang.dsl

import scala.util.matching.Regex
import scala.util.parsing.combinator._

sealed trait Statement

sealed trait Expr

case class Note(symbol: Char, accidental: Option[Int]) extends Expr

case object Rest extends Expr

case class Duration(expr: Expr, power: Int, dots: Int) extends Expr

case class Octave(expr: Expr, value: Int) extends Expr

case class Fragment(exprs: List[Expr]) extends Expr

case class Harmony(exprs: List[Expr]) extends Expr

case class Block(statements: List[Statement], exprs: List[Expr]) extends Expr

case class ChangeKey(expr: Expr) extends Statement

case class Assignment(name: String, value: Either[Expr, Function]) extends Statement

case class VarRef(name: String) extends Expr

case class FuncRef(name: String, args: List[Either[Expr, Function]]) extends Expr

case class Function(argNames: List[String], body: Expr)

// TODO: chords, keys, scopes, time signature

class Parser extends RegexParsers with PackratParsers {

  val name: Regex = """\w+""".r
  val chord: Regex = """[CDEFGAB]#?\w*""".r

  lazy val statement: PackratParser[Statement] =
    changeKey | declareChord | assignVar | assignFunc

  lazy val expr: PackratParser[Expr] =
    duration | octave | blockScope | fragment | harmony | scale | (note ||| varRef) | rest

  lazy val note: PackratParser[Note] =
    """[cdefgab1-7](#{1,2}|b{1,2}|!)?""".r ^^ { s =>
      Note(s.head, s.tail match {
        case "#" => Some(1)
        case "##" => Some(2)
        case "b" => Some(-1)
        case "bb" => Some(-2)
        case "!" => Some(0)
        case _ => None
      })
    }

  lazy val rest: PackratParser[Rest.type] = "~" ^^^ Rest

  lazy val number: PackratParser[Int] = """\d+""".r ^^ (_.toInt)

  lazy val duration: PackratParser[Duration] = expr ~ ("*" | "'" | ".") ~ opt(number) ^^ {
    case (duration: Duration) ~ "*" ~ num => duration.copy(power = duration.power + num.getOrElse(1))
    case (duration: Duration) ~ "'" ~ num => duration.copy(power = duration.power - num.getOrElse(1))
    case (duration: Duration) ~ "." ~ num => duration.copy(dots = duration.dots + num.getOrElse(1))
    case expr ~ "*" ~ num => Duration(expr, num.getOrElse(1), 0)
    case expr ~ "'" ~ num => Duration(expr, -num.getOrElse(1), 0)
    case expr ~ "." ~ num => Duration(expr, 0, num.getOrElse(1))
    case _ => throw new UnsupportedOperationException
  }

  lazy val octave: PackratParser[Octave] = ("+" | "-") ~ opt(number) ~ expr ^^ {
    case "+" ~ num ~ (octave: Octave) => octave.copy(value = octave.value + num.getOrElse(1))
    case "-" ~ num ~ (octave: Octave) => octave.copy(value = octave.value - num.getOrElse(1))
    case "+" ~ num ~ expr => Octave(expr, num.getOrElse(1))
    case "-" ~ num ~ expr => Octave(expr, -num.getOrElse(1))
    case _ => throw new UnsupportedOperationException
  }

  lazy val block: PackratParser[Block] = rep(statement) ~ rep1(expr) ^^ {
    case statements ~ exprs => Block(statements, exprs)
  }

  lazy val blockScope: PackratParser[Block] = "{" ~> block <~ "}"

  lazy val fragment: PackratParser[Fragment] = "(" ~> rep1(expr) <~ ")" ^^ Fragment

  lazy val harmony: PackratParser[Harmony] = "[" ~> rep1(expr) <~ "]" ^^ Harmony

  lazy val scale: PackratParser[Block] = "<" ~> expr ~ (":" ~> rep1(expr)) <~ ">" ^^ {
    case tonic ~ exprs => Block(ChangeKey(tonic) :: Nil, exprs)
  }

  lazy val changeKey: PackratParser[ChangeKey] = "key" ~> ":" ~> expr ^^ ChangeKey

  lazy val declareChord: PackratParser[Assignment] =
    """\$::\w*""".r ~ ("=" ~> scale) ^^ {
      case name ~ scale => Assignment(name.tail, Right(Function("$" :: Nil, scale)))
    }

  lazy val function: PackratParser[Function] = rep1(name <~ "->") ~ expr ^^ {
    case argNames ~ body => Function(argNames, body)
  }

  lazy val assignVar: PackratParser[Assignment] = name ~ ("=" ~> expr) ^^ {
    case name ~ expr => Assignment(name, Left(expr))
  }

  lazy val assignFunc: PackratParser[Assignment] = name ~ ("=" ~> function) ^^ {
    case name ~ func => Assignment(name, Right(func))
  }

  lazy val varRef: PackratParser[VarRef] = (chord | name | "$") ^^ VarRef

  lazy val funcRef: PackratParser[FuncRef] = ??? // TODO

}

object NoteParser extends Parser {

  def read(text: String): Block = {
    val program = parse(phrase(block), text)
    program.getOrElse {
      println(program)
      throw new RuntimeException("Parsing failed")
    }
  }

}
