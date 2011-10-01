Long Tail Model
===============

`R` code to fit a heavy-tailed distribution using the Long Tail model

This code implements ``F(x)`` equation from http://firstmonday.org/htbin/cgiwrap/bin/ojs/index.php/fm/article/view/1832/1716
 
The Long Tail of a catalog is measured using the frequency distribution (e.g. purchases, downloads, etc.), ranked by item popularity.
The ``F(x)`` function models the cumulative distribution of the Long Tail data. 
It also splits the curve in three parts: ``Head``, ``Mid`` and ``Tail``.
