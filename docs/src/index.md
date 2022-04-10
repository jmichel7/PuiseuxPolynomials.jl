# Puiseux polynomials
```@contents
Depth=3
```

```@docs
PuiseuxPolynomials
Mvp
@Mvp(::Any)
Mvp(p::Symbol)
valuation
degree
variables
LaurentPolynomials.coefficients(::Mvp,::Symbol)
LaurentPolynomials.coefficients(::Mvp)
coefficient
Monomial
monomials
powers
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
