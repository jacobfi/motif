# Motif

Welcome! Motif is a programming language for writing music.
It is a work in progress, so the current state of the language is still very much subject to change.

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

# Documentation

The simplest thing to write is a single note, such as `a`. By default, this represents the A right above the middle C.
You can also write a number between 1 and 7, which represents a scale degree in the current key, where 1 is the tonic.
The default key is C major. Thus, in the default key, `6` and `a` are equivalent.
You can also make a rest (absence of sound) with the tilde `~` operator.

## Accidentals

To change the pitch of a note by one or two semitones from the current key, we can use the sharp `#`,
double sharp `##`, flat `b` or double flat `bb` operators. For example, this is a C sharp: `c#`, and this is a
double flat B: `bbb` (equivalent in pitch to a natural A).

Notes are always played relative to the current key, and so we also have the natural operator `!` to cancel any
sharps or flats from the active key. For example, this is a natural B in the key of D minor: `b!`.

Accidentals applied to a note indicated by a letter will overwrite the pitch of that note in the current key.
Accidentals applied to a note indicated by a number (scale degree) are *added* to the pitch of that note.

## Octave

The default octave ranges from the middle C to the B 7 scale degree above.
To make a note in a different octave, you can use the `+` or the `-` prefix operator, which adds or subtracts an octave
to the expression immediately following the operator. Thus, this is the A right below the middle C:
`-a`

## Duration

In the default time signature of 4/4, an unmodified expression (such as a single note-letter) lasts for one beat,
which is 1/4th of a whole note. Thus, the expression `c d e f g a b +c` is a C major scale made up of 8 quarter-notes.

We can obtain a half-note by using the postfix hollow-operator `*`, which doubles the current duration like so: `c*`.
This can be repeated to obtain even longer notes, such as a whole note like this: `c**` or `c*2` (both are equivalent).

Similarly, duration can be halved using the postfix flag-operator `'`.
Here we have an 8th (halved once), a 16th (halved twice) and a 32nd (halved thrice) note: `c' c'2 c'3`.

The postfix dot operator `.` adds an extra half of the current duration to the expression it is used on.
For example, this lasts for 3 quarter notes: `c*.` (double plus an extra half of that).

## Fragments

Regular parentheses `( )` are used to construct *fragments*.
A fragment is a sequence of notes played in succession, but where the duration of the whole fragment is measured
to take up exactly one unit of time. The standard unit is a quarter-note, but this is modifiable by the duration
operators described above.

Fragments can be used to make off-beat rhythms such as triplets. For example, this is a triplet with a duration
of one quarter-note: `(a b a)`.

We can also make a long-short syncopation, for example, by combining with the dot operator: `(c. e)`.
The C here lasts 3/16th and the E for 1/16th.

We can also use it to write evenly spaced out regular notes, such as this C major scale in 8th notes:
`(c d e f g a b +c)*2`. This is because the fragment is measured to take up a whole note by the `*2` operator,
and the fragment itself contain 8 notes.

Fragments can also be nested, so here we have a C and a D as 16th notes, followed by an 8th note E: `((c d) e)`.

## Harmonies

In order to play multiple notes simultaneously, we can use square parentheses `[ ]` to construct *harmonies*.
Each expression inside a harmony will be played beginning on the same beat.

For example, the following is a C major triad: `[c e g]`.

Harmonies can also be used to create voicings, for example the following is a C major scale played on both the middle
octave and the one below it: `[(c d e f g a b +c) -(c d e f g a b +c)]*2`. Here we have two fragments inside a harmony,
which means the fragments will *begin* simultaneously, but the notes inside the fragments are still played successively.

## Blocks and scope

A score can be structured using blocks. The code inside a Motif file is already one block, but blocks can be further
nested using curly parentheses `{ }`. A block is a list of *statements* followed by an *expression*.
A block evaluates to the expression at the end of the block, allowing the block to be used elsewhere as an
expression itself.

For example, the following is a block which defines a C major triad, and playes it simultaneously with a melody:

```
{
    Cmajor = [c e g]
    melody = { c d e } <-- this is also a block
    [Cmajor melody]
}
```

A *statement* can be either a variable declaration or a directive (see below). Variable declarations are only visible
inside the scope of that block, and shadow any names declared outside. Directives are only applied to the expression
at the end of the block (and transitively to any blocks inside that).

## Variables

Any expression can be assigned to a name using the equals sign `=` to make a *variable declaration*.
The left-hand side is the name of the variable, and the right-hand is the expression.
In the block example above, `Cmajor` and `melody` are both locally declared variables.
You can reference variables in subsequent expressions, and the contents of the variable will be inlined at the
place in the code where it is referenced.

## Directives

The other kind of statement is a *directive*, which is a property that will take effect on all expressions inside
the scope of the block in which the directive is declared.
There are currently three kinds of directives: key change, tempo and time signature.

### Key change

The `@key` directive sets the current musical key. For example, this will set the key to D minor: `@key D minor`.

### Tempo

The `@tempo` directive sets the tempo of playback to the indicated number in BPM. The tempo is 120 BPM by default.
This will set the tempo to 80 BPM: `@tempo 80`.

### Time signature

The `@time` directive sets the time signature to the indicated fraction. For example, this will set the time signature
to 3/8: `@time 3/8`, which will make the default duration of a note half as long as in 4/4 time.
