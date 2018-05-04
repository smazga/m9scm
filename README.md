# Scheme 9 From Empty Space, Martian Edition #

This is the source for [s9fes](http://www.t3x.org/s9fes) with patches to make it build on [9front](http://www.9front.org) plus some extras.

*last upstream sync* 20171109

### New Stuff ###
* now calling it 'Martian Edition' so people understand it's a tainted version of the real s9fes
* added several sys: help files for plan9
* added sys:userpasswd for accessing factotum (plan9 only)
* added a simple json module to the library
* added a (defer) function to allow atexit() hooks

### Other Notes ###
* I've probably left development messages in the code, in case you see something weird
* Everything I've added is suspect.
* The plan9 "sys:" stuff isn't fully documented (yet).
