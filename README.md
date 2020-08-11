# Agador, voice response in toki pona

*Agador* combines several technologies to process spoken human language and generate responses.

* The [toki pona](tokipona.org) artificial language is extremely simple to learn and analyze, making it a prime candidate for computer input.  It's widely separated phonemes provide for a robustness in recognition not found in most languages, natural or [constructed](https://en.wikipedia.org/wiki/Constructed_language).

* The [Julius](https://github.com/julius-speech/julius) speech recognition system converts audio into a sequence of words in text.  It uses a simple grammar to tell it what sequences of sounds can form words in toki pona.

* An Adjacency Parser converts the sequence of words into a syntax tree, following the very simple grammar of the toki pona language.  The parser has the unique property of being able to handle local ambiguity.  Most parsers developed for computer programming languages can not deal with that.

* A tree database (implemented on top of the [LMDB key/value store](https://en.wikipedia.org/wiki/Lightning_Memory-Mapped_Database) remembers previous statements.  [Merkle keys](https://en.wikipedia.org/wiki/Merkle_signature_scheme) are used to provide pointers within the tree.

* All parts of *Agador*, including speech recognition, are Open Source.

* Everything runs locally with <u>no</u> cloud components.  Internet resources might be invoked to answer specific queries, such as about the weather.

Agador is not an artificial intelligence program; it employs straight forward signal processing and pattern matching techniques.  But nothing prevents AI technology being employed to service specific requests.

## Prerequisites

* The [Julius](https://github.com/julius-speech/julius)
speech recognition engine originally developed at Kyoto University and now continued at Nagoya Institute of Technology.  Julius is available in most Linux distributions.

* Agador is written in the [Common Lisp](https://en.wikipedia.org/wiki/Common_Lisp) language so the SBCL compiler for that is required.

### Speech Input

With Agador, Julius is operating with a _recognition grammar_.    Julius is faster and more accurate in this mode.  Julius _can_ operate in free-form dictation mode, but that makes the problem of matching up utterances with intended actions more difficult.  A grammar also reduces the possible number of interpretations on what has been said.

