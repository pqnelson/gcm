# Overview

This is a literate Fortran program which studies a general circulation
model. Our focus is on pedagogy, reproducibility, and correctness.

The choice of Fortran is defensible on grounds of it being the industry
standard, and Fortran 90 resembles Pascal more than FORTAN 77. It's
readable, performant, and everyone else uses it (so using another
language would make reusing code convoluted).

# Dependencies

On Debian-based systems:

``` bash
$ sudo apt-get install gfortran libcoarrays-dev libcoarrays
```

# Directory Structure

```
├───nw      (for noweb literate programs)
├───src     (for extracted source code)
├───test    (for unit tests)
└───tex     (for extracted documentation)
```

# Code Conventions

Following Knuth's apparent conventions in the _TeXbook_, function and
procedure parameters are listed, and section names are used in the body
of functions as a sort of pseudocode. We're using noweb and Fortran 2018
(which resembles Pascal in many ways).

# License

Following Jake VanderPlas's
[argument](https://www.astrobetter.com/blog/2014/03/10/the-whys-and-hows-of-licensing-scientific-code/),
I'm inclined to use a BSD 3-clause license.
