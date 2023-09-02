package io.motif

package object dsl {

  def accidental(s: String): Option[Int] = s match {
    case "#" => Some(1)
    case "##" => Some(2)
    case "b" => Some(-1)
    case "bb" => Some(-2)
    case "!" => Some(0)
    case _ => None
  }

}
