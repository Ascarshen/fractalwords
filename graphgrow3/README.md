# GraphGrow3

graph-directed iterated function system fractals

node editor with audiovisualisation

## Requirements

At runtime you need Pure-data executables `pd` and `pdsend` which should be in
your `PATH`.  Configure your `~/.pdrc` so that the correct audio device and
sample rate is set automatically.  `pd` will listen on network port `6060` to
receive messages from `graphgrow3`.  To build `graphgrow3` you need `ghc-8.0.1`
(other `ghc` versions are untested) and recent `cabal-install` (supporting
sandboxes).  The visualisation uses OpenGL, hardware acceleration essential.

## Quick start

    git clone https://code.mathr.co.uk/toy-interface.git
    git clone https://code.mathr.co.uk/toy-diagrams.git
    git clone https://code.mathr.co.uk/toy-gtk.git
    git clone https://code.mathr.co.uk/toy-gtk-diagrams.git
    git clone https://code.mathr.co.uk/graphgrow.git
    cd graphgrow/graphgrow3
    cabal sandbox init
    cabal sandbox add-source ../../toy-*/
    cabal install
    .cabal-sandbox/bin/graphgrow3

## Usage

On startup the visualisation window "GG#V" is displayed, along with the "GG"
toolbar.  The "V" button reshows the visualisation if it is closed.  The "+"
button adds a new rule, which are colour coded.  Clicking the coloured rule
buttons opens the editor for each rule.  All windows apart from the "GG" toolbar
can be closed and re-opened later.

In the rule editor, left-click on empty space creates a new node.  Nodes can be
left-mouse dragged by their centers to move them, and nodes are deleted with a
right-click.  Left-mouse dragging between the outer rings of two nodes creates
a link.  Links are deleted by right-clicking on their center.  The direction of
a link can be reversed by left-clicking on the arrow.  Left-clicking on the
center of a link changes the target rule for that link, cycling through the
available rules.

The square nodes are fixed, and correspond to the identity transformation (if
there were a link between them, but see bugs below).

## Bugs

Links longer than a bit less than one unit (the length between the two fixed
square nodes) slow down rendering a lot and might cause a denial of service to
your desktop session.

Adding more than 4 rules may break visualisation.

Adding more than 8 rules may break sonification.

Untested on anything apart from GNU/Linux/Debian/Testing/amd64.
