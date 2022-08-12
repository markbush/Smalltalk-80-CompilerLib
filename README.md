# Smalltalk-80-CompilerLib

A Smalltalk-80 compiler in Swift

## Introduction

This compiler is designed to work on the language specified in The Blue Book (["Smalltalk-80 The Language and it's Implementation"](https://rmod-files.lille.inria.fr/FreeBooks/BlueBook/Bluebook.pdf)).

### Omissions

The current scanner can recognise all Smalltalk-80 syntax.

### Extras

This implementation includes support for byte arrays, and dynamic arrays which are not referenced in the syntax diagrams of the Blue Book.

## Token Scanner

The token scanner is fairly standard.  It recognises identifiers, symbols, characters, strings, comments, block arguments, and numbers and stored the text in the token.  Otherwise, the token just uses the `TokenType` to identify it.  All tokens record the character position from the source that they were found at.

The only complication is negative numbers.  A `-` before a digit could be a negative number or a binary method.  The possible situations are checked to determine which is the appropriate way to tokenise this.  The method used seems to work so far.

## Parser

The parser is a hand written back-tracking parser.  It has been tested on the [Pharo](https://github.com/pharo-project/pharo) source (about 8,000 classes with over 90,000 methods) and doesn't generate any errors.  It has also now been tested on the original Smalltalk-80 sources file without errors.
