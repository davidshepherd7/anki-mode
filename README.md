[![travis][travis-badge]][travis-link] <!-- [![melpa][melpa-badge]][melpa-link] --> <!-- [![melpa stable badge][melpa-stable-badge]][melpa-stable-link] -->

[travis-link]: https://travis-ci.org/davidshepherd7/anki-mode
[travis-badge]: https://travis-ci.org/davidshepherd7/anki-mode.svg?branch=master
[melpa-link]: http://melpa.org/#/anki-mode
[melpa-badge]: http://melpa.org/packages/anki-mode-badge.svg
[melpa-stable-link]: https://stable.melpa.org/#/anki-mode
[melpa-stable-badge]: https://stable.melpa.org/packages/anki-mode-badge.svg

# Anki mode

A major mode for creating [Anki](https://en.wikipedia.org/wiki/Anki_(software)) cards.

Currently mostly works for me, but consider this alpha-quality software!

## Installation

1. Install [anki-connect](https://github.com/FooSoft/anki-connect) (anki addon 2055492159) and restart Anki.
2. Install this package (drop the `.el` file into your path and require it).

To use markdown you will also need a markdown parser installed. To not use
markdown you should be able to set `anki-mode-markdown-command` to `cat`.


## Usage

Call `anki-mode-new-card` to begin. Choose a deck and a card type, a buffer will
open into which you can write your card. Call `anki-mode-send-new-card` to send
the card to anki.

Currently the card types are hard coded so only the default ones can be used.

### Other features

* Press `$` to insert `[$] [/$]` for latex maths.
* Command `anki-mode-cloze-region`: wrap the selected region in a cloze deletion
  (with the number autodetected).

## File format

The `@` character at the beginning of a line is used to mark the name of a
field. Everything else on the line is used as the field name (with leading and
trailing whitespace removed), everything from the next line until the start of
the next field is used as the contents of the field.

For example a standard "Basic" card type would be written as:

```
@Front
What is a foo?
@Back
A particularly vicious variety of dandelion.
```

You can use markdown inside fields.
