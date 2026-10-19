#### Copyright (C) 2026, Christopher Mark Gore,
#### Soli Deo Gloria,
#### All rights reserved.
####
#### 22 Forest Glade Court, Saint Charles, Missouri 63304 USA.
#### Web: http://cgore.com
#### Email: cgore@cgore.com
####
#### Redistribution and use in source and binary forms, with or without
#### modification, are permitted provided that the following conditions are met:
####
####     * Redistributions of source code must retain the above copyright
####       notice, this list of conditions and the following disclaimer.
####
####     * Redistributions in binary form must reproduce the above copyright
####       notice, this list of conditions and the following disclaimer in the
####       documentation and/or other materials provided with the distribution.
####
####     * Neither the name of Christopher Mark Gore nor the names of other
####       contributors may be used to endorse or promote products derived from
####       this software without specific prior written permission.
####
#### THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
#### AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
#### IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
#### ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
#### LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
#### CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
#### SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
#### INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
#### CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
#### ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
#### POSSIBILITY OF SUCH DAMAGE.

LATEX_FILES=fitzclim.latex copyright.latex introduction.latex

all: fitzclim.dvi fitzclim.ps fitzclim.pdf

fitzclim.dvi: ${LATEX_FILES} fitzclim.bib
	latex fitzclim.latex
	bibtex fitzclim
	latex fitzclim.latex
	latex fitzclim.latex

fitzclim.ps: fitzclim.dvi
	dvips -o fitzclim.ps fitzclim.dvi

fitzclim.pdf: ${LATEX_FILES} fitzclim.bib
	pdflatex fitzclim.latex
	bibtex fitzclim
	pdflatex fitzclim.latex
	pdflatex fitzclim.latex

clean:
	rm -f fitzclim.pdf fitzclim.ps fitzclim.dvi \
	      fitzclim.aux fitzclim.bbl fitzclim.blg fitzclim.log \
	      fitzclim.toc fitzclim.out
