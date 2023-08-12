package io.notelang.dsl

import scala.util.parsing.combinator._

sealed trait Token

case class Note(symbol: Char, accidental: Option[Accidental]) extends Token

case object Rest extends Token

case class Duration(token: Token, power: Int, dots: Int) extends Token

case class Octave(token: Token, value: Int) extends Token

case class Fragment(tokens: Seq[Token]) extends Token

case class Harmony(tokens: Seq[Token]) extends Token

case class Segment(tokens: Seq[Token]) extends Token

case class Scale(root: Note, token: Token) extends Token

// TODO: chords, keys, scopes, time signature

class NoteParser extends RegexParsers with PackratParsers {

  lazy val token: PackratParser[Token] = duration | octave | fragment | harmony | segment | scale | note | rest

  lazy val note: PackratParser[Note] =
    """[cdefgab1-7](#{1,2}|b{1,2}|!)?""".r ^^ { s =>
      Note(s.head, s.tail match {
        case "#" => Some(Sharp)
        case "##" => Some(DoubleSharp)
        case "b" => Some(Flat)
        case "bb" => Some(DoubleFlat)
        case "!" => Some(Natural)
        case _ => None
      })
    }

  lazy val rest: PackratParser[Rest.type] = "~" ^^^ Rest

  lazy val number: PackratParser[Int] = """\d+""".r ^^ (_.toInt)

  lazy val duration: PackratParser[Duration] = token ~ ("*" | "'" | ".") ~ opt(number) ^^ {
    case (duration: Duration) ~ "*" ~ num => duration.copy(power = duration.power + num.getOrElse(1))
    case (duration: Duration) ~ "'" ~ num => duration.copy(power = duration.power - num.getOrElse(1))
    case (duration: Duration) ~ "." ~ num => duration.copy(dots = duration.dots + num.getOrElse(1))
    case token ~ "*" ~ num => Duration(token, num.getOrElse(1), 0)
    case token ~ "'" ~ num => Duration(token, -num.getOrElse(1), 0)
    case token ~ "." ~ num => Duration(token, 0, num.getOrElse(1))
    case _ => throw new UnsupportedOperationException
  }

  lazy val octave: PackratParser[Octave] = ("+" | "-") ~ opt(number) ~ token ^^ {
    case "+" ~ num ~ (octave: Octave) => octave.copy(value = octave.value + num.getOrElse(1))
    case "-" ~ num ~ (octave: Octave) => octave.copy(value = octave.value - num.getOrElse(1))
    case "+" ~ num ~ token => Octave(token, num.getOrElse(1))
    case "-" ~ num ~ token => Octave(token, -num.getOrElse(1))
    case _ => throw new UnsupportedOperationException
  }

  lazy val fragment: PackratParser[Fragment] = "(" ~> rep1(token) <~ ")" ^^ Fragment

  lazy val harmony: PackratParser[Harmony] = "[" ~> rep1(token) <~ "]" ^^ Harmony

  lazy val segment: PackratParser[Segment] = "{" ~> rep1(token) <~ "}" ^^ Segment

  lazy val scale: PackratParser[Scale] = "<" ~> note ~ (":" ~> token) <~ ">" ^^ {
    case note ~ token => Scale(note, token)
  }

}

object NoteParser extends NoteParser {

  def read(text: String): Segment = {
    val result = parse(phrase(segment), text)
    result.getOrElse {
      println(result)
      throw new RuntimeException("Parsing failed")
    }
  }

}
