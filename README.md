Dear people potentially reading this because they googled some very specific things:

My intention with this was to be able to do arbitrary layouts independent of graphics libraries in Haskell. Because clay.h
looked quite useful I thought some bindings would do the trick.

I since then have come to think that bindings to imperative libraries are not actually a very good idea. The entire point of 
Haskell is to work with _algebraic_ abstractions. If the foundation is a bunch of side-effecting C code I don't believe you're
setting yourself up to use Haskell to it's greatest extent and at that point you may as well use an imperative language.

I don't know what a layout algebra might look like (though I have some thoughts) and I intend to circle back on that at a later date.
Anyway this code may contain interesting examples of fairly extensive C bindings in Haskell. 

-Liam
