# Agador, voice response for toki pona

*Agador* is an experiment in computer recognition of the toki pona
artificial language.  It combines several technologies to process spoken
human language and generate responses.

1. The [toki pona](tokipona.org) language is very simple to learn and analyze,
making it a prime candidate for computer input.  It's small dictionary
(only 124 words) and widely separated phonemes provide for a robustness
in recognition not found in most languages, natural or
[constructed](https://en.wikipedia.org/wiki/Constructed_language).

2. The [Kaldi](http://kaldi-asr.org/) speech recognition
system, fronted by the [VOSK](https://alphacephei.com/vosk/) API toolkit,
converts audio into a sequence of words in text.

* A Parser then converts the sequence of words into a syntax
tree, following the very simple grammar of the toki pona language.
The parser has the unique property of being able to handle local ambiguity,
something that most parsers developed for computer programming languages
can not deal with.

* A tree database (implemented on top of the
[LMDB key/value store](https://en.wikipedia.org/wiki/Lightning_Memory-Mapped_Database)
is used to remember statements for later search and analysis.
[Merkle keys](https://en.wikipedia.org/wiki/Merkle_signature_scheme) are
used to provide pointers within the tree.

* All parts of *Agador*, including speech recognition, are Open Source.

* Everything runs locally with <u>no</u> cloud components.
Internet resources might be invoked to answer specific queries,
such as about the weather.

Agador is not an artificial intelligence program; it employs straight
forward signal processing and pattern matching techniques.  But nothing
prevents AI technology being employed to service specific requests.

## Prerequisites

* Agador is written in the
[Common Lisp](https://en.wikipedia.org/wiki/Common_Lisp) language
and the SBCL compiler for that is required.

* The Lightning Memory Mapped Database (LMDB) is used for long-term memory.

* The McCLIM graphical user interface package is used for visual interaction.

## The Adjacency Parser

The speech recognition subsystem (or typed user input) delivers a
string of words to the parser.  The parser then delivers a
"syntax tree" to semantic processing and for display.

The parsing algorithm does not have a "state machine" like many parsers
used for programming languages.  Instead all applicable grammar rules
are applied holographically, wherever they happen to fit, to the
entire input.

It is possible that more than one overall parse tree will be found,
in which case either the input is truely ambiguous ("Time flies
like an arrow.") or there is an error in the grammar.  Toki Pona is a highly
ambiguous language with a lot depending on context, so this could
easily happen.  But the task of Agador is to only operate on clear
commands and to just "remember" the rest.

## Generating text
The way the parser rules are constructed allows them to be run in reverse
as a *generative grammar* to generate any number of syntactically valid
(though possibly meaningless) sentences in toki pona.  This can be
used for training the Kaldi speech recognizer.

