package io.notelang.dsl

import scala.util.parsing.combinator._

sealed trait Statement

sealed trait Expr

case class Note(symbol: Char, accidental: Option[Int]) extends Expr

case object Rest extends Expr

case class Duration(expr: Expr, power: Int, dots: Int) extends Expr

case class Octave(expr: Expr, value: Int) extends Expr

case class Fragment(exprs: Seq[Expr]) extends Expr

case class Harmony(exprs: Seq[Expr]) extends Expr

case class Scale(tonic: Note, exprs: Seq[Expr]) extends Expr

case class Block(statements: Seq[Statement], exprs: Seq[Expr]) extends Expr

case class Chord(suffix: String, f: Note => Scale) extends Statement

case class ChordRef(note: Note, suffix: String) extends Expr

// TODO: chords, keys, scopes, time signature

class Parser extends RegexParsers with PackratParsers {

  lazy val statement: PackratParser[Statement] = chord

  lazy val expr: PackratParser[Expr] =
    duration | octave | blockScope | fragment | harmony | scale | chordRef | note | rest

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

  lazy val scale: PackratParser[Scale] = "<" ~> note ~ (":" ~> rep1(expr)) <~ ">" ^^ {
    case note ~ exprs => Scale(note, exprs)
  }

  lazy val chord: PackratParser[Chord] =
    """\$::\w*""".r ~ (("=" ~ "<" ~ "$" ~ ":") ~> rep1(expr) <~ ">") ^^ {
      case s"$$::$suffix" ~ exprs => Chord(suffix, Scale(_, exprs))
      case _ => throw new UnsupportedOperationException
    }

  lazy val chordRef: PackratParser[ChordRef] =
    """[CDEFGAB][#b]?\w*""".r ^^ {
      case s"$symbol#$suffix" => ChordRef(Note(symbol.head.toLower, Some(1)), suffix)
      case s"${symbol}b$suffix" => ChordRef(Note(symbol.head.toLower, Some(-1)), suffix)
      case s => ChordRef(Note(s.head.toLower, None), s.tail)
    }

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
