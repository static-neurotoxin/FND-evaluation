# FND-evaluation

A series of Markdown documents to evaluate monospaced fonts and markdown parsers

## Purpose

Monospaced fonts are ubiquitous in programming. Markdown has become one of the defacto standards in documenting code and the meta data associated with projects. Yet, there is vast variations in monospaced fonts and markdown viewers. The goal of these documents is to provide sample data to evaluate fonts and tools for a particular task or general use.

## Pain points

A new trend in monospaced fonts for programming use is ligatures. On one hand, they can improve readability of code in a great many ways. However, operators in one language my have a different meaning in another. Also, if you move between languages, ligatures may obscure coding mistakes. Seeing `≠` is nice, but did you type `!=`, `<>`, `!==`, `~=`, `\=`, `/=` or something completely different?

## Documents

* [License](LICENSE.md) - Blue Oak Model License 1.0.0
* [Markdown features](Markdown.md) - Markdown feature exercises
* [Font Test](FontTest.md) - English text with an emphasis on difficult/awkward text for monospaced fonts
* [Top programming languages](TIOBE20.md) - Code samples in the top 20 [TIOBE index](https://www.tiobe.com/tiobe-index/) programming languages.
* [Common file formats](DataFormats.md) - File samples in common data exchange formats.
* [Historical programming languages](Historical.md) - Code samples in historically significant or interesting programming languages.
* [Esoteric programming languages](Esoteric.md) - Code samples in novelty, esoteric or just plain different languages.
* [Appendix](Appendix.md) - Samples of anything that I wanted to see that didn't quite fit anywhere else.

## Thanks

My motivation started with the amazing work done by [Programming Fonts](https://www.programmingfonts.org/)
Also [TIOBE](https://www.tiobe.com/) for collecting and sharing the interest of various programming languages.

## FYI

Inequality operators

| operator | language(s)                                                |
|----------|------------------------------------------------------------|
| `!=`     | C, C++, C#, Java, JavaScript, Python, Ruby, Go, Swift, PHP |
| `<>`     | SQL, Pascal, Basic                                         |
| `!==`    | JavaScript, PHP                                            |
| `~=`     | Lua, MATLAB                                                |
| `/=`     | Haskell, Fortran                                           |
| `\=`     | Prolog                                                     |
| `≠`      | APL                                                        |
