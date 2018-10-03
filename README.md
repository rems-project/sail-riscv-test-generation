# RISC-V instruction generation

This is a randomised instruction generator for RISC-V, based on the
[Sail model][sail].  A snapshot of that model is in the `model`
directory, and was generated from commit `f0d2765b` using the
`-ocaml_generators ast` option.

[sail]: https://github.com/rems-project/sail

Building this currently requires the lem, linksem, and zarith
libraries to be installed.  The easiest way to do this is to install
[Sail using opam][opam].

[opam]: https://github.com/rems-project/sail/wiki/OPAMInstall

## Licensing

See the `LICENCE` file for details; note that the references to PPrint
and ASL code refer to code in the Sail repository that is not present
here.
