# DXF converter

## Background
DXF files represent computer aided data (CAD) in an easy to parse format, and this program generates 2D motion paths (in a proprietary format for industrial robots) from DXF files. 

You might be interested in it as a code example on how to structure GUI classes in Racket (badly) or how to implement some simple computer graphics routines like intersection queries (inefficiently). I know these code examples, no matter how unidiomatic they may be, would have been a gold mine for me when i was starting out.

My rationale for using Racket is it's cross-platform GUI in the standard library, and Racket was extremely easy to start using. Lisps also makes it very easy to manipulate data.

## Dependencies

stchang's graph library here: https://stchang.github.io/graph/graph.html
You can download it through DrRacket's package manager.

## Start-up

From DrRacket, run the program from main-window.rkt.

## Usage
Drag the canvas to pan, for selecting, hold down ctrl for windows or leave caps on.

## Some problems i've encountered:
1) Drawing 1000+ geometrical entities (lines/arcs/circles) can be laggy in the canvas. The underlying library for drawing used at this point of time can be found in this annoucement: http://blog.racket-lang.org/2010/12/racket-version-5.html
See for more details on the Racket mailing group: https://groups.google.com/forum/#!topic/racket-users/Z4Ozri3l-Dk

2) When selecting, the search function iterates over every single entity whether they are visible or near the selection box. This is mitigated somewhat by using the cheapest checks before going into the more computationally intensive checks, but it can be much further optimized by using specialized data structures like quad-tree or r-trees.

3) Tricky vicky coordinate systems. Computer coordinate systems' 0,0 point is at the top left corner which took a little effort to make it intuitive for the intended user/operator of the industrial robots to use. Also, dxf arc information were a pain for me to get right.