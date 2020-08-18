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
The parser has the unique property of being able to handle local ambiguity, something that most parsers developed for computer programming languages can not deal with.

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

## Adjacency Parsers
### Parsing with minimal knowledge

Toki Pona is a very minimalist language, having only about 124 words and
most of those are *content words* (nouns, verbs, and modifiers).  One of the
unusual properties of Toki Pona grammar is that most nouns can also be
used as modifiers, and most of *those* can also be used as verbs.  So
a dictionary of these words is not particularly helpful in analyzing syntax.

The non-content words are *function words* [1] and Toki Pona only has
about a dozen of them.  In *uninflected languages* like Toki Pona (and
English in large part), is often possible to completely analyze a sentence
structure based on how the content words are placed in relation to the
function words.  This can be seen in the Lewis Carroll poem "Jabberwocky"
where the reader can immediately tell the parts of speech of all the
nonsense words, based on how they fit between the function words:

> Twas bryllyg, and the slythy toves  
> Did gyre and gymble in the wabe:  
> All mimsy were the borogoves;  
> And the mome raths outgrabe.  

It turns out that algorithms have been developed for finding patterns in
the usage of function words and thereby derive the function of all the
content words. [2]  The approach taken here is to merely treat all content
words the same (as nouns), apply all available rules involving nouns,
in combination with their adjacent function words, and see which choices
fit in with the context of the entire sentence.

In other words, when confronted with an ambiguous sequence of words, the
parser simply accepts *all* of the possible interpretations locally and
and waits until the end of the input sentence to see which one fits into
the rest of the sentence, and keeps that one in the final output.

The parser actually operates with a very small dictionary of only the
function words in toki pona.  Any word coimng from Julius that is not
in the internal dicitonary is just assigned the role of "noun".  (Julius
itself actually needs to have the entire dictionary, with pronunciation
information, in order to locate word boundaries.)

## References
1. [Definition and Examples of Function Words in English](https://www.thoughtco.com/function-word-grammar-1690876)

2. Young, Carol E. (1970), *Grammatical assignment based on function words*,
Ohio State University Department of Computer Science,
Master's thesis. THESIS1970MSYCEG



