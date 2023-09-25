package io.motif.dsl

import scala.util.matching.Regex
import scala.util.parsing.combinator.*

enum Expr:
  case Note(symbol: Char, accidental: Option[Int])
  case Rest
  case Duration(expr: Expr, power: Int, dots: Int)
  case Octave(expr: Expr, value: Int)
  case Fragment(exprs: List[Expr])
  case Harmony(exprs: List[Expr])
  case Block(statements: List[Statement], exprs: List[Expr])
  case Reference(name: String)

enum Statement:
  case KeyDirective(note: Expr.Note, mode: String)
  case TimeDirective(numerator: Int, denominator: Int)
  case TempoDirective(bpm: Int)
  case InstrumentDirective(program: Int)
  case Assignment(name: String, value: Expr | Function)

case class Function(argNames: List[String], body: Expr)

class Parser extends RegexParsers with PackratParsers:

  import Expr.*
  import Statement.*

  val name: Regex = """\w+""".r
  val chord: Regex = """[A-G]#?\w*""".r

  lazy val directive: PackratParser[Statement] =
    keyDir | timeDir | tempoDir | instrumentDir

  lazy val statement: PackratParser[Statement] =
    directive | declareChord | assignFunc | assignVar

  lazy val expr: PackratParser[Expr] =
    duration | octave | blockScope | fragment | harmony | scale | (note ||| ref) | rest

  lazy val note: PackratParser[Note] =
    """[a-g1-7](#{1,2}|b{1,2}|!)?""".r ^^ { s => Note(s.head, accidental(s.tail)): Note }

  lazy val rest: PackratParser[Expr] = "~" ^^^ Rest

  lazy val number: PackratParser[Int] = """\d+""".r ^^ (_.toInt)

  lazy val duration: PackratParser[Expr] = expr ~ ("*" | "'" | ".") ~ opt(number) `^^` :
    case (duration: Duration) ~ "*" ~ num => duration.copy(power = duration.power + num.getOrElse(1))
    case (duration: Duration) ~ "'" ~ num => duration.copy(power = duration.power - num.getOrElse(1))
    case (duration: Duration) ~ "." ~ num => duration.copy(dots = duration.dots + num.getOrElse(1))
    case expr ~ "*" ~ num => Duration(expr, num.getOrElse(1), 0)
    case expr ~ "'" ~ num => Duration(expr, -num.getOrElse(1), 0)
    case expr ~ "." ~ num => Duration(expr, 0, num.getOrElse(1))
    case _ => throw new UnsupportedOperationException

  lazy val octave: PackratParser[Expr] = ("+" | "-") ~ opt(number) ~ expr `^^` :
    case "+" ~ num ~ (octave: Octave) => octave.copy(value = octave.value + num.getOrElse(1))
    case "-" ~ num ~ (octave: Octave) => octave.copy(value = octave.value - num.getOrElse(1))
    case "+" ~ num ~ expr => Octave(expr, num.getOrElse(1))
    case "-" ~ num ~ expr => Octave(expr, -num.getOrElse(1))
    case _ => throw new UnsupportedOperationException

  lazy val block: PackratParser[Block] = rep(statement) ~ rep1(expr) `^^` :
    case statements ~ exprs => Block(statements, exprs): Block

  lazy val blockScope: PackratParser[Block] = "{" ~> block <~ "}"

  lazy val fragment: PackratParser[Expr] = "(" ~> rep1(expr) <~ ")" ^^ Fragment.apply

  lazy val harmony: PackratParser[Expr] = "[" ~> rep1(expr) <~ "]" ^^ Harmony.apply

  lazy val scale: PackratParser[Expr] = "<" ~> note ~ (":" ~> rep1(expr)) <~ ">" `^^` :
    case tonic ~ exprs => Block(KeyDirective(tonic, "major") :: Nil, exprs)

  lazy val keyDir: PackratParser[Statement] = "@key" ~> "[A-G][#b]?".r ~ ("major" | "minor") `^^` :
    case s ~ mode => KeyDirective(Note(s.head.toLower, accidental(s.tail)), mode)

  lazy val timeDir: PackratParser[Statement] = "@time" ~> (number <~ "/") ~ number `^^` :
    case numerator ~ denominator => TimeDirective(numerator, denominator)

  lazy val tempoDir: PackratParser[Statement] = "@tempo" ~> number ^^ TempoDirective.apply

  lazy val instrumentDir: PackratParser[Statement] = "@instrument" ~> (number | name) `^^` :
    case num: Int => InstrumentDirective(num - 1)
    case s: String => InstrumentDirective(instrument(s) - 1)

  lazy val declareChord: PackratParser[Statement] =
    """\$::\w*""".r ~ (("=" ~ "<" ~ "$" ~ ":") ~> rep1(expr) <~ ">") `^^` :
      case name ~ exprs => Assignment(name.tail, Block(Nil, exprs))

  lazy val function: PackratParser[Function] = rep1(name <~ "->") ~ expr `^^` :
    case argNames ~ body => Function(argNames, body)

  lazy val assignVar: PackratParser[Statement] = name ~ ("=" ~> expr) `^^` :
    case name ~ expr => Assignment(name, expr)

  lazy val assignFunc: PackratParser[Statement] = name ~ ("=" ~> function) `^^` :
    case name ~ func => Assignment(name, func)

  lazy val ref: PackratParser[Expr] = (chord | name) ^^ Reference.apply

object NoteParser extends Parser:

  def read(text: String): Expr.Block =
    val program = parse(phrase(block), text)
    program.getOrElse:
      println(program)
      throw new RuntimeException("Parsing failed")
