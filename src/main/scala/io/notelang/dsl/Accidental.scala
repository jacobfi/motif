package io.notelang.dsl

sealed trait Accidental

case object Sharp extends Accidental

case object DoubleSharp extends Accidental

case object Flat extends Accidental

case object DoubleFlat extends Accidental

case object Natural extends Accidental
