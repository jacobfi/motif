package io.notelang.interpreter

class Key private[Key](tonic: Char, keytone: Int, scale: Array[Int]) {

  private val tonicIndex = Key.noteIndex.indexOf(tonic)

  def toLetter(degree: Int): Char = Key.noteIndex((degree - 1 + tonicIndex) % 7)

  def semitone(letter: Char): Int = keytone + interval(letter)

  def baseAndDelta(letter: Char): (Int, Int) = {
    val tone = semitone(letter)
    val base = Key.CMajor.semitone(letter)
    (base, tone - base)
  }

  private def interval(letter: Char) = {
    val letterIndex = Key.noteIndex.indexOf(letter)
    val diff = letterIndex - tonicIndex
    if (diff >= 0) scale.take(diff).sum
    else -scale.takeRight(-diff).sum
  }

}

object Key {

  private val noteIndex = Array('c', 'd', 'e', 'f', 'g', 'a', 'b')
  private val majorScale = Array(2, 2, 1, 2, 2, 2, 1)
  private val minorScale = Array(2, 1, 2, 2, 1, 2, 2)

  val CMajor: Key = new Key('c', 0, majorScale)

  def majorOf(letter: Char, accidental: Int): Key =
    new Key(letter, CMajor.semitone(letter) + accidental, majorScale)

  def minorOf(letter: Char, accidental: Int): Key =
    new Key(letter, CMajor.semitone(letter) + accidental, minorScale)

}
