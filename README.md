# Motif

Welcome! Motif is a programming language for writing music.
It is still a work in progress, so unfortunately there is not much documentation yet!

Here is a small example that plays the first notes of Für Elise:

`+(e d#) +(e d# e -b d c)*. a`

You can find a full example of [Chopin's waltz in A minor (B150 Op. posth.)](core/src/main/resources/chopin.mtf)
in the resources folder.

## How to run

First make sure you have Java Runtime Environment installed.
Then [download the motifc.jar file](https://github.com/jacobfi/motif/releases/latest/download/motifc.jar)
and put it somewhere. Run it with:

`java -jar motifc.jar <filename>`

where `<filename>` is a plain-text file containing the Motif code of the score to play.
For example `java -jar motifc.jar chopin.mtf` to play the example of Chopin's waltz linked above.
