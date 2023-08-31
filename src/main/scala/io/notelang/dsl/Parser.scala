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

case class KeyDirective(expr: Expr) extends Statement

case class TimeDirective(numerator: Int, denominator: Int) extends Statement

case class TempoDirective(bpm: Int) extends Statement

case class Assignment(name: String, value: Either[Expr, Function]) extends Statement

case class Reference(name: String) extends Expr

case class Function(argNames: List[String], body: Expr)

class Parser extends RegexParsers with PackratParsers {

  val name: Regex = """\w+""".r
  val chord: Regex = """[CDEFGAB]#?\w*""".r

  lazy val directive: PackratParser[Statement] =
    keyDir | timeDir | tempoDir

  lazy val statement: PackratParser[Statement] =
    directive | declareChord | assignFunc | assignVar

  lazy val expr: PackratParser[Expr] =
    duration | octave | blockScope | fragment | harmony | scale | (note ||| ref) | rest

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
    case tonic ~ exprs => Block(KeyDirective(tonic) :: Nil, exprs)
  }

  lazy val keyDir: PackratParser[KeyDirective] = "@key" ~> expr ^^ KeyDirective

  lazy val timeDir: PackratParser[TimeDirective] = "@time" ~> (number <~ "/") ~ number ^^ {
    case numerator ~ denominator => TimeDirective(numerator, denominator)
  }

  lazy val tempoDir: PackratParser[TempoDirective] = "@tempo" ~> number ^^ TempoDirective

  lazy val declareChord: PackratParser[Assignment] =
    """\$::\w*""".r ~ (("=" ~ "<" ~ "$" ~ ":") ~> rep1(expr) <~ ">") ^^ {
      case name ~ exprs => Assignment(name.tail, Left(Block(Nil, exprs)))
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

  lazy val ref: PackratParser[Reference] = (chord | name) ^^ Reference

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
