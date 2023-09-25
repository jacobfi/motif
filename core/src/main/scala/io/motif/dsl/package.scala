package io.motif

package object dsl:

  def accidental(s: String): Option[Int] = s match
    case "#" => Some(1)
    case "##" => Some(2)
    case "b" => Some(-1)
    case "bb" => Some(-2)
    case "!" => Some(0)
    case _ => None

  def instrument(s: String): Int = s match
    case "piano" => 1
    case "violin" => 41
    case "flute" => 74
