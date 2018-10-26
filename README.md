# Scheme 9 From Empty Space (Reimagined), Martian Edition #

This is the source for [s9fes](http://www.t3x.org/s9fes) with patches to make it build on [9front](http://www.9front.org) plus some extras.

*last upstream sync* 20181025

### New Stuff ###
* now calling it 'Martian Edition' so people understand it's a tainted version of the real s9fes
* added several sys: help files for plan9
* added sys:userpasswd for accessing factotum (plan9 only)
* added a simple json module to the library
* added a (defer) function to allow atexit() hooks
* added a curl extension (for unix)
* worked quite a bit on 9p interaction [wip]

### Other Notes ###
* I've probably left development messages in the code, in case you see something weird
* Everything I've added is suspect.
* The plan9 "sys:" stuff isn't fully documented (yet).
* The "Reimagined" version has introduced some build warnings in plan9.
