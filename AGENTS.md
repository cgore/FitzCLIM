# AGENTS.md

This repository is FitzCLIM, a Common Lisp implementation of the CLIM II protocols, aimed at SBCL.  The product description is in `README.md`.  Do not restate it in source comments.

This is a clean-room implementation.  Do not copy source from McCLIM, from Franz CLIM, or from LispWorks CLIM.  The CLIM II specification may be consulted.  Its text is copyrighted; do not paste it into this tree.

Code written with the assistance of an LLM is welcome here.  The person who commits it is responsible for it.

## Shape

The frame manager realizes the standard gadgets as native controls.  On Windows those are Windows controls.  Layout positions them through the sheet protocol.  A native control does not hold CLIM output records.  Its items may still be Lisp objects, and a selection may run a command.

An application pane is drawn by FitzCLIM.  It records output and holds presentations.  A command loop reads commands, from typing or from a gesture on a presentation, and runs them.

A 3D pane is a native OpenGL child window, a sibling of the native controls.  Pointer events at its edge become a ray.  A hit returns a Lisp object and enters the same command and translator machinery as a 2D presentation.

The 2D graphics protocol stays two-dimensional.  Do not fold regions, transforms, clipping, formatted output, or output recording into the scene graph.  The OpenGL pane owns its redraw.  FitzCLIM may ask that pane to repaint.  The pane must not record its scene as output records.  A label in the scene is drawn and picked there.  Do not lay a transparent CLIM sheet over the OpenGL window.

## Provenance

McCLIM is LGPL-2.1-or-later.  Its interactive backend is CLX, its native Windows backend is abandoned, and it has no OpenGL pane.  The maintainer's working rule is that LLM-written code stays out of the McCLIM tree (Codeberg issue #1497).  FitzCLIM is a separate implementation so this tree can stay 3-clause BSD, so LLM-assisted code can land here, and so native controls and a 3D pane are part of the design.

Franz CLIM and LispWorks CLIM realize gadgets as native controls through the frame manager.  Follow that split.  Their source stays off limits, as does McCLIM's.  No other maintained CLIM runs on SBCL.

There is no CLIM III specification.  Implement CLIM II.  CLIM 2.2 is Franz's user guide, not a second formal specification.

## Scope

Build the CLIM II protocols this toolkit is for: frames, the command loop, presentations, the gadgets in use, one native frame manager, and one 3D pane.  The first frame manager targets Windows.

Drei, bundled applications, extra backends, and a port of every CLIM II corner come after that core.  Do not begin there.

Keep the 2D graphics protocol as specified.  A design that makes the graphics model a 3D scene graph, or that makes every presentation a native control, is a different toolkit.

## EDEFB

EDEFB, at `D:\Programming\Lisp\edefb`, is a sister repository (`https://github.com/cgore/edefb.git`).  FitzCLIM is a general toolkit.  Leave Elite Dangerous out of this tree.  Leave the EDEFB tree alone.  EDEFB has not chosen its interface.

## Copyright and license

The license is the 3-clause BSD notice in `LICENSE.md`.  Leave that text as it is written.

A new source file carries that notice at the top, each line prefixed with `;;;; `.  The copyright year is the year of the file.  When you edit a file in a later year, make the year a range from the first year on the notice through the year of the edit.

The notice names Christopher Mark Gore.  `Soli Deo Gloria` stays on the following line.

## Git

You may commit and push to GitHub as you deem the work ready.  The remote is `https://github.com/cgore/FitzCLIM.git` and the branch is `master`.  Do not rename the branch to `main`.  Do not force-push.

Make each commit a reasonable size.  One commit is one coherent change: large enough to stand on its own, and small enough to review and to revert.

In other repositories Christopher reviews the tree and lands the commits himself.  In this repository, commit finished work here.  Do not leave it uncommitted for him to land.

## Testing

Make all code well tested.  Cover the behavior you add or change, including the edges that change can break.  Run the tests before you commit.

Write those tests with Sigma's `sigma/behave` package whenever it can express them.  Group examples with `behavior` and `spec`.  Assert with `should`, `should-not`, `should-be-a`, `should-be-null`, `should-be-true`, `should-be-false`, and the generated comparators such as `should=` and `should-equal`.  Sigma is the ASDF system `:sigma` at <https://github.com/cgore/sigma>.  On this machine the checkout is `D:\Programming\Lisp\sigma`.

## Libraries

Use Sigma whenever it already provides the utility you need.  Load `:sigma` and use the specific package (`sigma/behave`, `sigma/sequence`, `sigma/string`, `sigma/control`, `sigma/numeric`, `sigma/hash`, and the others).  Call `sigma:use-all-sigma` only in a scratch listener.  Library and application code name the package they use.

Use another Common Lisp library when its license is compatible with the 3-clause BSD license in `LICENSE.md`.  A compatible license lets FitzCLIM depend on the library, or vendor it, while FitzCLIM's own code stays 3-clause BSD.  Permissive licenses meet that test, including BSD, MIT, ISC, the Apache License 2.0, and public-domain dedications.  A copyleft license does not.  Keep that library's copyright and license notice with any code you vendor.
