package io.notelang.dsl

object Accidental {

  def parse(s: String): Option[Accidental] = s match {
    case "#" => Some(Sharp)
    case "##" => Some(DoubleSharp)
    case "b" => Some(Flat)
    case "bb" => Some(DoubleFlat)
    case "!" => Some(Natural)
    case _ => None
  }

}

sealed trait Accidental {

  def value: Int = this match {
    case Sharp => 1
    case DoubleSharp => 2
    case Flat => -1
    case DoubleFlat => -2
    case Natural => 0
  }

}

case object Sharp extends Accidental

case object DoubleSharp extends Accidental

case object Flat extends Accidental

case object DoubleFlat extends Accidental

case object Natural extends Accidental
