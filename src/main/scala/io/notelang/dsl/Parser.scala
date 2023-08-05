package io.notelang.dsl

import scala.util.parsing.combinator._

sealed trait Token

case class Note(letter: Char, accidental: Option[Accidental], octave: Int, power: Int, dots: Int) extends Token

case class Rest(power: Int, dots: Int) extends Token

case class Group(inner: Seq[Token], power: Int) extends Token

case class Harmony(inner: Seq[Token]) extends Token

// TODO: chords, keys, scopes, time signature

case object Whitespace extends Token

class NoteParser extends RegexParsers {

  override def skipWhitespace: Boolean = false

  def accidental: Parser[Accidental] =
    """#{1,2}|b{1,2}|!""".r ^^ {
      case "#" => Sharp
      case "##" => DoubleSharp
      case "b" => Flat
      case "bb" => DoubleFlat
      case "!" => Natural
    }

  def octave: Parser[Int] =
    """[+-]\d*""".r ^^ {
      case s"+$num" => if (num.isEmpty) 1 else num.toInt
      case s"-$num" => if (num.isEmpty) -1 else -num.toInt
    }

  def duration: Parser[Int] =
    """[*']\d*""".r ^^ {
      case s"*$num" => if (num.isEmpty) 1 else num.toInt
      case s"'$num" => if (num.isEmpty) -1 else -num.toInt
    }

  def dots: Parser[Int] =
    """\.\d*""".r ^^ {
      case s".$num" => if (num.isEmpty) 1 else num.toInt
    }

  def note: Parser[Note] = opt(octave) ~ """[cdefgab]""".r ~ opt(accidental) ~ opt(duration) ~ opt(dots) ^^ {
    case maybeOctave ~ letter ~ maybeAccidental ~ maybeDuration ~ maybeDots =>
      Note(letter.head, maybeAccidental, maybeOctave.getOrElse(0), maybeDuration.getOrElse(0), maybeDots.getOrElse(0))
  }

  def rest: Parser[Rest] = "~" ~> opt(duration) ~ opt(dots) ^^ {
    case maybeDuration ~ maybeDots => Rest(maybeDuration.getOrElse(0), maybeDots.getOrElse(0))
  }

  def sequence: Parser[Seq[Token]] = repsep(note | rest | group | harmony, whiteSpace)

  def group: Parser[Group] = "(" ~> opt(duration <~ whiteSpace) ~ sequence <~ ")" ^^ {
    case maybeDuration ~ tokens => Group(tokens, maybeDuration.getOrElse(0))
  }

  def harmony: Parser[Harmony] = "[" ~> sequence <~ "]" ^^ Harmony

}

object NoteParser extends NoteParser {

  def read(text: String): Seq[Token] = {
    val result = parse(phrase(sequence <~ opt(whiteSpace)), text)
    result.getOrElse {
      println(result)
      throw new RuntimeException("Parsing failed")
    }
  }

}
