# Agador, voice response for toki pona

*Agador* is an experiment in computer recognition of the toki pona artificial language.  It combines several technologies to process spoken human language and generate responses.

* The [toki pona](tokipona.org) language is very simple to learn and analyze,
making it a prime candidate for computer input.  It's small dictionary
(only 124 words) and widely separated phonemes provide for a robustness
in recognition not found in most languages, natural or
[constructed](https://en.wikipedia.org/wiki/Constructed_language).

* The [Julius](https://github.com/julius-speech/julius) speech recognition
system converts audio into a sequence of words in text.  It uses a
very simple grammar to tell it what sequences of sounds can form
words in toki pona, and in what combinations those words can appear.

* An Adjacency Parser then converts the sequence of words into a syntax
tree, following the very simple grammar of the toki pona language.
The parser has the unique property of being able to handle local ambiguity,
something that most parsers developed for computer programming languages
can not deal with.

* A tree database (implemented on top of the [LMDB key/value store](https://en.wikipedia.org/wiki/Lightning_Memory-Mapped_Database) is used to remember statements for later search and analysis.  [Merkle keys](https://en.wikipedia.org/wiki/Merkle_signature_scheme) are used to provide pointers within the tree.

* All parts of *Agador*, including speech recognition, are Open Source.

* Everything runs locally with <u>no</u> cloud components.  Internet resources might be invoked to answer specific queries, such as about the weather.

Agador is not an artificial intelligence program; it employs straight forward signal processing and pattern matching techniques.  But nothing prevents AI technology being employed to service specific requests.

## Prerequisites

* The [Julius](https://github.com/julius-speech/julius)
speech recognition engine originally developed at Kyoto University and now continued at Nagoya Institute of Technology.  Julius is available in most Linux distributions.

* Agador is written in the [Common Lisp](https://en.wikipedia.org/wiki/Common_Lisp) language so the SBCL compiler for that is required.

### Speech Input

The Julius recognizer does no noise cancellation.  That will have to be done externally by choice of microphone and environment.
