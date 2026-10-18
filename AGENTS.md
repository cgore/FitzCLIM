# AGENTS.md

This repository is FitzCLIM, a Common Lisp implementation of the CLIM II protocols, aimed at SBCL.  The product description is in `README.md`.  Do not restate it in source comments.

This is a clean-room implementation.  Do not copy source from McCLIM, from Franz CLIM, or from LispWorks CLIM.  The CLIM II specification may be consulted.  Its text is copyrighted; do not paste it into this tree.

Code written with the assistance of an LLM is welcome here.  The person who commits it is responsible for it.

## Shape

The frame manager realizes the standard gadgets as native controls.  On Windows those are Windows controls.  Layout positions them through the sheet protocol.

An application pane is drawn by FitzCLIM.  It records output and holds presentations.  A command loop reads commands, from typing or from a gesture on a presentation, and runs them.

A 3D pane is a native OpenGL child window, a sibling of the native controls.  Pointer events at its edge become a ray.  A hit returns a Lisp object and enters the same command and translator machinery as a 2D presentation.

The 2D graphics protocol stays two-dimensional.  Do not fold regions, transforms, clipping, formatted output, or output recording into the scene graph.  The OpenGL pane owns its redraw.  A label in the scene is drawn and picked there.  Do not lay a transparent CLIM sheet over the OpenGL window.

## Copyright and license

The license is the 3-clause BSD notice in `LICENSE.md`.  Leave that text as it is written.

A new source file carries that notice at the top, each line prefixed with `;;;; `.  The copyright year is the year of the file.  When you edit a file in a later year, make the year a range from the first year on the notice through the year of the edit.

The notice names Christopher Mark Gore.  `Soli Deo Gloria` stays on the following line.

## Git

Commit finished work and push it.  The remote is `https://github.com/cgore/FitzCLIM.git` and the branch is `master`.  Do not rename the branch to `main`.  Do not force-push.

In other repositories Christopher reviews the tree and lands the commits himself.  In this repository, do not leave a finished change uncommitted for him to land.
