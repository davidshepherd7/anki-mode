[![travis][travis-badge]][travis-link] [![melpa][melpa-badge]][melpa-link]<!-- [![melpa stable badge][melpa-stable-badge]][melpa-stable-link] -->

[travis-link]: https://travis-ci.org/davidshepherd7/anki-mode
[travis-badge]: https://travis-ci.org/davidshepherd7/anki-mode.svg?branch=master
[melpa-link]: http://melpa.org/#/anki-mode
[melpa-badge]: http://melpa.org/packages/anki-mode-badge.svg
[melpa-stable-link]: https://stable.melpa.org/#/anki-mode
[melpa-stable-badge]: https://stable.melpa.org/packages/anki-mode-badge.svg

# Anki mode

A major mode for creating [Anki](https://en.wikipedia.org/wiki/Anki_(software)) cards.

## Installation

1. Install [anki-connect](https://github.com/FooSoft/anki-connect) (anki addon 2055492159) and restart Anki.
2. Install this package (either by using melpa, or by dropping the `.el` file into your path and require-ing it).
3. Set `anki-mode-markdown-command` to your preferred markdown parser command, or install `pandoc` to use the
   default. `pandoc` with github flavoured markdown is the default because it handles `_`
   well. To disable markdown entirely set `anki-mode-markdown-command` to `cat`.


## Usage

Call `anki-mode-menu` to begin. Note that Anki must be running in order to use
`anki-mode`.


### Other features

* Press `$` to insert `[$] [/$]` for latex maths.
* Press `tab` to cycle through card fields.
* Command `anki-mode-cloze-region`: wrap the selected region in a cloze deletion
  (with the number autodetected).

### TODO

* Updating existing cards would be useful.
* Currently LaTeX can only be compiled within Anki itself (by previewing the card).


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


## See also

If you prefer org-mode to markdown, you might prefer to use [anki-editor](https://github.com/louietan/anki-editor).

[org-drill](https://orgmode.org/worg/org-contrib/org-drill.html) and
[pamparam](https://github.com/abo-abo/pamparam) are implementations of an entire
spaced repetition system within Emacs. You might prefer one of these if you want
to do the memorisation part of spaced repetition within Emacs, I prefer to use
the Anki mobile app.


## Changelog

### Unstable

* A magit-status-like menu screen
* Cycle through card fields
* Various minor quality improvements
* Handling of non-default card types
* Add an error message when card already exists

### 0.1

* Basic card creation
* Markdown rendering
