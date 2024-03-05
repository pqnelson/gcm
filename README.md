# Overview

This is a literate Fortran program which studies a general circulation
model. Our focus is on pedagogy, reproducibility, and correctness.

The choice of Fortran is defensible on grounds of it being the industry
standard, and Fortran 90 resembles Pascal more than FORTAN 77. It's
readable, performant, and everyone else uses it (so using another
language would make reusing code convoluted).

Right now, it focuses on a simple 1-layer atmosphere using the
Bousinnesq approximation with no dissipation or forcing contributions,
discarding the Coriolis effect.

# Dependencies

On Debian-based systems:

``` bash
$ sudo apt-get install gfortran libcoarrays-dev libcoarrays
```

# Directory Structure

Right now, adhering to the
[FPM](https://github.com/fortran-lang/fpm/blob/master/PACKAGING.md)
packaging documentation, the current directory structure looks like:

```
├── app
│   └── main.f90 (the main program)
├── bin          (for binary programs)
│   └── tests    (for test suites)
├───nw           (for noweb literate programs)
├───obj          (for compiled object code)
├───src          (for extracted source code)
├───test         (for unit tests)
└───tex          (for extracted documentation)
```

**Remark.** This may change when/if we get to more complicated features. For
example, it may be easier to carve out a "module" for the ocean system,
and a "module" for the atmosphere. In this case, we would have a
subdirectory in `./src/` for each subsystem (e.g., `./src/ocean/` and
`./src/atmosphere/`, respectively). (End of Remark)

# Code Conventions

Following Knuth's apparent conventions in the _TeXbook_, function and
procedure parameters are listed, and section names are used in the body
of functions as a sort of pseudocode. We're using noweb and Fortran 2018
(which resembles Pascal in many ways).

# License

Following Jake VanderPlas's
[argument](https://www.astrobetter.com/blog/2014/03/10/the-whys-and-hows-of-licensing-scientific-code/),
I'm inclined to use a BSD 3-clause license.
