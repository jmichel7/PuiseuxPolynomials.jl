# PuiseuxPolynomials
Multivariate (Puiseux) polynomials and rational fractions.

This  package implements  Puiseux polynomials,  that is linear combinations
with  coefficients  of  some  type  `T`  of monomials of the type `x₁^{a₁}…
xₙ^{aₙ}`  where  `xᵢ`  are  variables  and  `aᵢ` are exponents which can be
arbitrary  rational  numbers.  When  the  `aᵢ`  are  integers  we  speak of
"multivariate Laurent polynomials", and when the `aᵢ` are positive integers
we speak of "multivariate polynomials" (or true polynomials).

This  package also implements  multivariate rational fractions, constructed
as  the quotient of two Laurent  polynomials (which is then standardized to
be  the  quotient  of  two  true  polynomials  with  no  common factor). In
particular,  this package is a perfectly  usable (and hopefully quite good)
implementation   of  multivariate  polynomials  and  multivariate  rational
fractions, if that is what you are interested in.

See the docs for usage and more details.

[![Stable](https://img.shields.io/badge/docs-stable-blue.svg)](https://jmichel7.github.io/PuiseuxPolynomials.jl/stable/)
[![Dev](https://img.shields.io/badge/docs-dev-blue.svg)](https://jmichel7.github.io/PuiseuxPolynomials.jl/dev/)
[![Build Status](https://github.com/jmichel7/PuiseuxPolynomials.jl/actions/workflows/CI.yml/badge.svg?branch=main)](https://github.com/jmichel7/PuiseuxPolynomials.jl/actions/workflows/CI.yml?query=branch%3Amain)
