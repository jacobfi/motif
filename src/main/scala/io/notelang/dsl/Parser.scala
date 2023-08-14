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

case class Segment(exprs: Seq[Expr]) extends Expr

case class Scale(tonic: Note, expr: Expr) extends Expr

case class Chord(suffix: String, f: Note => Scale) extends Statement

case class ChordRef(note: Note, suffix: String) extends Expr

// TODO: chords, keys, scopes, time signature

class NoteParser extends RegexParsers with PackratParsers {

  lazy val statement: PackratParser[Statement] = chord

  lazy val expr: PackratParser[Expr] =
    duration | octave | fragment | harmony | segment | scale | chordRef | note | rest

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
    case token ~ "*" ~ num => Duration(token, num.getOrElse(1), 0)
    case token ~ "'" ~ num => Duration(token, -num.getOrElse(1), 0)
    case token ~ "." ~ num => Duration(token, 0, num.getOrElse(1))
    case _ => throw new UnsupportedOperationException
  }

  lazy val octave: PackratParser[Octave] = ("+" | "-") ~ opt(number) ~ expr ^^ {
    case "+" ~ num ~ (octave: Octave) => octave.copy(value = octave.value + num.getOrElse(1))
    case "-" ~ num ~ (octave: Octave) => octave.copy(value = octave.value - num.getOrElse(1))
    case "+" ~ num ~ token => Octave(token, num.getOrElse(1))
    case "-" ~ num ~ token => Octave(token, -num.getOrElse(1))
    case _ => throw new UnsupportedOperationException
  }

  lazy val fragment: PackratParser[Fragment] = "(" ~> rep1(expr) <~ ")" ^^ Fragment

  lazy val harmony: PackratParser[Harmony] = "[" ~> rep1(expr) <~ "]" ^^ Harmony

  lazy val segment: PackratParser[Segment] = "{" ~> rep1(expr) <~ "}" ^^ Segment

  lazy val scale: PackratParser[Scale] = "<" ~> note ~ (":" ~> expr) <~ ">" ^^ {
    case note ~ token => Scale(note, token)
  }

  lazy val chord: PackratParser[Chord] =
    """\$::\w*""".r ~ (("=" ~ "<" ~ "$" ~ ":") ~> expr <~ ">") ^^ {
      case s"$$::$suffix" ~ token => Chord(suffix, Scale(_, token))
      case _ => throw new UnsupportedOperationException
    }

  lazy val chordRef: PackratParser[ChordRef] =
    """[CDEFGAB][#b]?\w*""".r ^^ {
      case s"$symbol#$suffix" => ChordRef(Note(symbol.head.toLower, Some(1)), suffix)
      case s"${symbol}b$suffix" => ChordRef(Note(symbol.head.toLower, Some(-1)), suffix)
      case s => ChordRef(Note(s.head.toLower, None), s.tail)
    }

}

object NoteParser extends NoteParser {

  def read(text: String): (Expr, Map[String, Note => Scale]) = {
    val ast = parse(phrase(rep(statement) ~ expr), text)
    ast.getOrElse {
      println(ast)
      throw new RuntimeException("Parsing failed")
    } match {
      case statements ~ expr =>
        val env = statements.foldLeft(Map[String, Note => Scale]()) {
          case (map, Chord(suffix, f)) => map + (suffix -> f)
        }
        (expr, env)
    }
  }

}
