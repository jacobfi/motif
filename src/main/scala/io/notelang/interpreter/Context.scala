package io.notelang.interpreter

case class Context(power: Int = 0, lastTick: Int = 0) {

  def resolvePower(givenPower: Int): Int =
    if (givenPower != 0) givenPower else power

  def resolveLastTicks(givenTicks: Int): Int =
    if (lastTick > 0) lastTick else givenTicks

}
