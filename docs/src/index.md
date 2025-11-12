```@meta
CurrentModule=PuiseuxPolynomials
```
# Puiseux polynomials
Documentation for [PuiseuxPolynomials](https://github.com/jmichel7/PuiseuxPolynomials)

```@docs
PuiseuxPolynomials
Mvp
@Mvp(::Any)
Mvp(p::Symbol)
valuation
degree
variables
LaurentPolynomials.coefficients(p::Mvp{T,N},v::Symbol) where {T, N}
LaurentPolynomials.coefficients(::Mvp)
coefficient
Monomial
monomials
powers
term
Base.pairs(::Monomial)
Base.isless(::Monomial,::Monomial)
lex
grlex
grevlex
Base.pairs(::Mvp)
Pol(x::Mvp{T, N} where N) where T
Pol(p::Mvp{T, N}, var::Symbol) where {T, N}
Mvp(x::Pol)
PuiseuxPolynomials.value
PuiseuxPolynomials.conj
PuiseuxPolynomials.derivative
laurent_denominator
gcd(::Mvp,::Mvp)
lcm(::Mvp,::Mvp)
scalar
Base.:^(::Mvp, ::AbstractMatrix)
LaurentPolynomials.Frac(::T) where T<:Mvp
grobner_basis
rename_variables
```
