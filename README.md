**This repository is no longer actively updated**. The underlying Google Material Design Lite library is now on [limited support](https://github.com/google/material-design-lite#limited-support); as a consequence, this library will not receive additional updates. 

To use Material Design components with elm, try @aforemny's [elm-mdc](https://github.com/aforemny/elm-mdc).


# Material Design Components for Elm

Port of Google's
[Material Design Lite](https://www.getmdl.io/)
CSS/JS implementation of the
[Material Design Specification](https://www.google.com/design/spec/material-design/introduction.html).



[Live demo](https://debois.github.io/elm-mdl/) & [package documentation](http://package.elm-lang.org/packages/debois/elm-mdl/latest).

[![Build Status](https://travis-ci.org/debois/elm-mdl.svg?branch=v8)](https://travis-ci.org/debois/elm-mdl).


### Migration

If you are updating from 7.x.x, refer to the [Migration guide](https://github.com/debois/elm-mdl/blob/v8/MIGRATION.md).

### Get Started

Adapt
[examples/Counter.elm](https://github.com/debois/elm-mdl/tree/master/examples) to suit your needs.
The
[Live demo](https://debois.github.io/elm-mdl/) contains code samples for most components, which
you may find helpful.

Use one of the [templates](https://github.com/debois/elm-mdl/blob/v8/TEMPLATES.md) to get an easy starting point into elm-mdl.

For a long-form tutorial, you might like the excellent "Introduction to elm-mdl" by [@knewter](https://github.com/knewter), available as both a [daily drip video](https://www.dailydrip.com/topics/elm/drips/elm-mdl-introduction) and a very nice [writeup](https://medium.com/@dailydrip/introduction-to-using-material-design-in-elm-dc2320087410#.dodoot1wd).

### Get help

For more in-depth documentation, refer to the [extensive package
documentation](http://package.elm-lang.org/packages/debois/elm-mdl/latest/).

Ask any questions you may have on
[stackoverflow](https://stackoverflow.com/questions/ask?tags=elm+elm-mdl)
or on [#elm-mdl](https://elm.slack.com/messages/elm-mdl) in the elm-slack.


### Frequently asked questions

Please read the [FAQ here](https://github.com/debois/elm-mdl/blob/v8/FAQ.md).


### Other projects using Elm-mdl

Check out the [users page](https://github.com/debois/elm-mdl/blob/v8/USERS.md) for a list of projects using elm-mdl.

### Contribute

Contributions are warmly encouraged! Whether you are a newcomer to Elm or
an accomplished expert, the MDL port presents interesting challenges. Refer
to [this page](https://github.com/debois/elm-mdl/blob/master/CONTRIBUTING.md)
for a detailed list of possible contributions.

Most importantly: Do [report
bugs](https://github.com/debois/elm-mdl/issues/new). The elm-mdl library
aims to provide a completely smooth experience with Material Design for elm
developers. No bug is too small.

You may want to read the hints on how to get your issue [resolved
quickly](https://github.com/debois/elm-mdl/blob/master/CONTRIBUTING.md#can-i-speed-up-my-issue)
but you don't have to.

### Implementation

MDL is implemented primarily through CSS, with a little bit of JavaScript
adding and removing CSS classes in response to DOM events. This port
re-implements the JavaScript parts in Elm, but relies on the CSS of MDL
verbatim.
