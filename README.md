
<a id='Puiseux-polynomials'></a>

<a id='Puiseux-polynomials-1'></a>

# Puiseux polynomials

- [Puiseux polynomials](index.md#Puiseux-polynomials)

<a id='PuiseuxPolynomials' href='#PuiseuxPolynomials'>#</a>
**`PuiseuxPolynomials`** &mdash; *Module*.



This  package, which depends only  on the packages `LaurentPolynomials` and `ModuleElts`,  implements Puiseux polynomials,  that is linear combinations of  monomials of the  type `x₁^{a₁}… xₙ^{aₙ}`  where `xᵢ` are variables and `aᵢ`  are exponents which can be arbitrary rational numbers.

Recall  that  on  an  algebraically  closed coefficient field, the quotient field  of the Puiseux polynomials is  the algebraic closure of the quotient field  of  the  multivariate  polynomials.  We use Puiseux polynomials with cyclotomic coefficients as splitting fields of cyclotomic Hecke algebras.

This  package is in particular  a perfectly usable (and  quite good I hope) implementation   of  multivariate  polynomials  and  multivariate  rational fractions if you are only interested in that.

Some  functions described below work  only with polynomials where variables are  raised to integral powers;  we will refer to  such objects as "Laurent polynomials"; some functions require further that variables are raised only to  positive powers: we refer then to "true polynomials".

Puiseux  polynomials have the  parametric type `Mvp{C,E}`  where `C` is the type  of the coefficients and `E` is the type of the exponents (`E=Int` for Laurent   polynomials;   `E=Rational{Int}`   for   more   general   Puiseux polynomials).  When printing only `C` is  printed if `E==Int`. The rational fractions  have type  `Frac{Mvp{C,Int}}`(and in  addition the numerator and denominator should be true polynomials).

We first look at how to make Puiseux polynomials.

`@Mvp x₁,…,xₙ`

assigns  to each  Julia name  `xᵢ` an  `Mvp` representing  an indeterminate suitable   to  build   multivariate  polynomials   or  rational  fractions. `Mvp(:x₁)` creates the same `Mvp` without assigning it to variable `x₁`.

```julia-repl
julia> @Mvp x,y

julia> (x+y^-1)^3
Mvp{Int64}: x³+3x²y⁻¹+3xy⁻²+y⁻³

julia> x+Mvp(:z)
Mvp{Int64}: x+z

julia> x^(1//2)
Mvp{Int64,Rational{Int64}}: x½

julia> Mvp(3)
Mvp{Int64}: 3
```

`Mvp(x::Number)`  returns the multivariate polynomial  with only a constant term, equal to `x`.

It  is convenient to create `Mvp`s using  variables such as `x,y` above. To create  them more  directly, `Monomial(:x=>1,:y=>-2)`  creates the monomial `xy⁻²`, and then `Mvp(Monomial(:x=>1,:y=>-2)=>3,Monomial()=>4)` creates the `Mvp`  `3xy⁻²+4`. This is the way `Mvp` are printed in another context than the repl, IJulia or Pluto (where they display nicely as shown above).

```julia-rep1
julia> print(3x*y^-2+4)
Mvp(Monomial(:x,:y => -2) => 3,Monomial() => 4) # :x is shorthand for :x=>1

julia> print(x^(1//2))
Mvp(Monomial(:x => 1//2) => 1)
```

Only  monomials and one-term `Mvp`s can  be raised to a non-integral power; the  `Mvp` with one term constant `c`  times the monomial `m` can be raised to  a fractional  power of  denominator `d`  if and  only if `root(c,d)` is defined (this is equivalent to `c^{1//d}` for floats);

```julia-repl
julia> (4x)^(1//2)
Mvp{Int64,Rational{Int64}}: 2x½

julia> (2.0x)^(1//2)
Mvp{Float64,Rational{Int64}}: 1.4142135623730951x½

julia> root(2.0x)
Mvp{Float64,Rational{Int64}}: 1.4142135623730951x½
```

One  may  want  to  define  `root`  differently;  for instance, in my other package   `CylotomicNumbers`  I   define  square   roots  of  rationals  as cyclotomics;  I also have implemented in `CylotomicNumbers` arbitrary roots of roots of unity). Using `CylotomicNumbers`:

```julia-rep1
julia> (2x)^(1//2)
Mvp{Cyc{Int64},Rational{Int64}}: √2x½

julia> (E(3)*x)^(2//3)
Mvp{Cyc{Int64},Rational{Int64}}: ζ₉²x⅔
```

There  are various ways to take an  `Mvp` apart. Below are the most direct; look   also  at  the   functions  `coefficient`,  `coefficients`,  `pairs`, `monomials`, `variables` and `powers`.

```julia-repl
julia> p=3x*y^-2+4
Mvp{Int64}: 3xy⁻²+4

julia> term(p,1)
xy⁻² => 3

julia> term(p,2)
 => 4

julia> length(p) # the number of terms
2

julia> term.(p,1:length(p)) # same as pairs(p)
2-element Vector{Pair{Monomial{Int64}, Int64}}:
 xy⁻² => 3
      => 4

julia> m=first(term(p,1)) # same as first(monomials(p))
Monomial{Int64}:xy⁻²

julia> length(m) # how many variables in m
2

julia> m[:x] # power of x in m
1

julia> m[:y] # power of y in m
-2

julia> map((x,y)->x=>y,variables(m),powers(m)) # same as pairs(m)
2-element Vector{Pair{Symbol, Int64}}:
 :x => 1
 :y => -2
```

The valuation and degree of an Mvp can be inspected globally or variable by variable.

```julia-repl
julia> p
Mvp{Int64}: 3xy⁻²+4

julia> variables(p)
2-element Vector{Symbol}:
 :x
 :y

julia> degree(p),degree(p,:x),degree(p,:y)
(0, 1, 0)

julia> valuation(p),valuation(p,:x),valuation(p,:y)
(-1, 0, -2)
```

Terms  are totally ordered in an `Mvp`  by a monomial ordering (that is, an ordering  on  monomials  so  that  `x<y`  implies `xz<yz` for any monomials `x,y,z`).  By default, the  ordering is `lex`.  The terms are in decreasing order,  so that the  first term is  the highest. The  orderings `grlex` and `grevlex` are also implemented (see `grobner_basis` for how to use them).

An  `Mvp` is a *scalar*  if the valuation and  degree are `0`. The function `scalar`  returns the  constant coefficient  if the  `Mvp` is a scalar, and `nothing` otherwise.

Usual  arithmetic (`+`, `-`,  `*`, `^`, `/`,  `//`, `one`, `isone`, `zero`, `iszero`,  `==`)  works.  Elements  of  type  `<:Number`  are considered as scalars for scalar operations on the coefficients.

```julia-repl
julia> p
Mvp{Int64}: 3xy⁻²+4

julia> p^2
Mvp{Int64}: 9x²y⁻⁴+24xy⁻²+16

julia> p/2
Mvp{Float64}: 1.5xy⁻²+2.0

julia> p//2
Mvp{Rational{Int64}}: (3//2)xy⁻²+2//1
```

When  converting an `Mvp` one needs to specify the two type parameters (the type of the coefficients and the type of the exponents).

```julia-repl
julia> Mvp{Float64,Rational{Int}}(p)
Mvp{Float64,Rational{Int64}}: 3.0xy⁻²+4.0
```

One can evaluate an `Mvp` when setting the value of some variables by using the  function call syntax (actually, the keyword syntax for the object used as a function). There is also a more explicit `value` function.

```julia-repl
julia> p=x+y
Mvp{Int64}: x+y

julia> value(p,:x=>2)
Mvp{Int64}: y+2

julia> p(x=2)
Mvp{Int64}: y+2

julia> p(x=2,y=x)
Mvp{Int64}: x+2

julia> value(p,:x=>2,:y=>x)
Mvp{Int64}: x+2
```

Note  that  an  `Mvp`  always  evaluates  to an `Mvp`, for consistency. You should  use `scalar`  on the  result of  evaluating all  variables to get a number.

```julia-repl
julia> p(x=1,y=2)
Mvp{Int64}: 3

julia> scalar(p(x=1,y=2))
3

julia> v=(x^(1//2))(x=2.0)
Mvp{Float64,Rational{Int64}}: 1.4142135623730951

julia> scalar(v)
1.4142135623730951
```

One  can divide an `Mvp` by another when the division is exact, compute the `gcd` and `lcm` of two `Mvp`.

```julia-repl
julia> exactdiv(x^2-y^2,x-y)
Mvp{Int64}: x+y

julia> (x+y)/(2x^2)   # or by a monomial
Mvp{Float64}: 0.5x⁻¹+0.5x⁻²y

julia> (x+y)//(2x^2)
Mvp{Rational{Int64}}: (1//2)x⁻¹+(1//2)x⁻²y

julia> (x+y)/(x-y)    # otherwise one gets a rational fraction
Frac{Mvp{Int64, Int64}}: (x+y)/(x-y)
```

Raising  a non-monomial  Laurent polynomial  to a  negative power returns a rational   fraction.  Rational  fractions  are  normalized  such  that  the numerator  and denominators are true polynomials  prime to each other. They have  the  arithmetic  operations  `+`,  `-`  , `*`, `/`, `//`, `^`, `inv`, `one`,  `isone`, `zero`, `iszero` (these  operations can operate between an `Mvp` or a `Number` and a `Frac{<:Mvp}`).

```julia-repl
julia> (x+1)^-2
Frac{Mvp{Int64, Int64}}: 1/(x²+2x+1)

julia> x+1/(y+1)
Frac{Mvp{Int64, Int64}}: (xy+x+1)/(y+1)

julia> 1-1/(y+1)
Frac{Mvp{Int64, Int64}}: y/(y+1)
```

One  can evaluate a `Frac` when setting  the value of some variables by using the function call syntax or the `value` function:

```julia-repl
julia> ((x+y)/(x-y))(x=y+1)
Mvp{Float64}: 2.0y+1.0

julia> value((x+y)/(x-y),:x=>y+1;Rational=true) # use // for division
Mvp{Int64}: 2y+1
```

`Frac`  can  be  dissected  using  `numerator`  and `denominator`. They are scalars  for  broadcasting  and  can  be  sorted  (have  `cmp` and `isless` methods).

```julia-repl
julia> m=[x+y x-y;x+1 y+1]
2×2 Matrix{Mvp{Int64, Int64}}:
 x+y  x-y
 x+1  y+1

julia> n=inv(Frac.(m))
2×2 Matrix{Frac{Mvp{Int64, Int64}}}:
 (-y-1)/(x²-2xy-y²-2y)  (x-y)/(x²-2xy-y²-2y)
 (x+1)/(x²-2xy-y²-2y)   (-x-y)/(x²-2xy-y²-2y)

julia> lcm(denominator.(n))
Mvp{Int64}: x²-2xy-y²-2y
```

Finally,   `Mvp`s  have   methods  `conj`,   `adjoint`  which   operate  on coefficients,   a   `derivative`   method,   and  methods  `positive_part`, `negative_part` and `bar` (useful for Kazhdan-Lusztig theory).

```julia_repl
julia> @Mvp z
Mvp{Int64}: z

julia> hessian(p,vars)=[derivative(derivative(p,x),y) for x in vars, y in vars]
hessian (generic function with 1 method)

julia> hessian(x^2*y^2*z^2,[:x,:y,:z])
3×3 Matrix{Mvp{Int64, Int64}}:
 2y²z²  4xyz²  4xy²z
 4xyz²  2x²z²  4x²yz
 4xy²z  4x²yz  2x²y²

julia> jacobian(pols,vars)=[derivative(p,v) for p in pols, v in vars]
jacobian (generic function with 1 method)

julia> jacobian([x,y,z],[:x,:y,:z])
3×3 Matrix{Mvp{Int64, Int64}}:
 1  0  0
 0  1  0
 0  0  1

julia> p=(x+y^-1)^4
Mvp{Int64}: x⁴+4x³y⁻¹+6x²y⁻²+4xy⁻³+y⁻⁴

julia> positive_part(p)
Mvp{Int64}: x⁴

julia> negative_part(p)
Mvp{Int64}: y⁻⁴

julia> bar(p)
Mvp{Int64}: y⁴+4x⁻¹y³+6x⁻²y²+4x⁻³y+x⁻⁴
```

Despite  the degree of generality of our  polynomials, the speed is not too shabby. For the Fateman test f(f+1) where f=(1+x+y+z+t)^15, we take 3sec. According to the Nemo paper, Sagemath takes 10sec and Nemo takes 1.6sec.


<a target='_blank' href='https://github.com/jmichel7/PuiseuxPolynomials.jl/blob/a50a074146d6c6cc3525ddbe7386f0bda78d9c52/src/PuiseuxPolynomials.jl#L1-L339' class='documenter-source'>source</a><br>

<a id='PuiseuxPolynomials.Mvp' href='#PuiseuxPolynomials.Mvp'>#</a>
**`PuiseuxPolynomials.Mvp`** &mdash; *Type*.



`Mvp`s are implemented as a list of pairs `monomial=>coefficient` sorted by the monomial order `lex`.


<a target='_blank' href='https://github.com/jmichel7/PuiseuxPolynomials.jl/blob/a50a074146d6c6cc3525ddbe7386f0bda78d9c52/src/PuiseuxPolynomials.jl#L522-L525' class='documenter-source'>source</a><br>

<a id='PuiseuxPolynomials.@Mvp-Tuple{Any}' href='#PuiseuxPolynomials.@Mvp-Tuple{Any}'>#</a>
**`PuiseuxPolynomials.@Mvp`** &mdash; *Macro*.



`@Mvp x,y`

is  equivalent to `x=Mvp(:x);y=Mvp(:y)`  excepted it creates  `x,y` in the  global scope of the current module, since it uses `eval`.


<a target='_blank' href='https://github.com/jmichel7/PuiseuxPolynomials.jl/blob/a50a074146d6c6cc3525ddbe7386f0bda78d9c52/src/PuiseuxPolynomials.jl#L533-L538' class='documenter-source'>source</a><br>

<a id='PuiseuxPolynomials.Mvp-Tuple{Symbol}' href='#PuiseuxPolynomials.Mvp-Tuple{Symbol}'>#</a>
**`PuiseuxPolynomials.Mvp`** &mdash; *Method*.



`Mvp(x::Symbol)` creates the `Mvp` with one term of degree one and coefficient 1 with variable `x`


<a target='_blank' href='https://github.com/jmichel7/PuiseuxPolynomials.jl/blob/a50a074146d6c6cc3525ddbe7386f0bda78d9c52/src/PuiseuxPolynomials.jl#L598-L601' class='documenter-source'>source</a><br>

<a id='LaurentPolynomials.valuation' href='#LaurentPolynomials.valuation'>#</a>
**`LaurentPolynomials.valuation`** &mdash; *Function*.



`valuation(m::Mvp[,v::Symbol])`

The `valuation` of an `Mvp` is the minimal degree of a monomial.

With  second argument a variable name, `valuation` returns the valuation of the polynomial in that variable.

```julia-repl
julia> @Mvp x,y; a=x^2+x*y
Mvp{Int64}: x²+xy

julia> valuation(a), valuation(a,:y), valuation(a,:x)
(2, 0, 1)
```


<a target='_blank' href='https://github.com/jmichel7/PuiseuxPolynomials.jl/blob/a50a074146d6c6cc3525ddbe7386f0bda78d9c52/src/PuiseuxPolynomials.jl#L707-L723' class='documenter-source'>source</a><br>

<a id='LaurentPolynomials.degree' href='#LaurentPolynomials.degree'>#</a>
**`LaurentPolynomials.degree`** &mdash; *Function*.



`degree(m::Mvp[,v::Symbol])`

The `degree` of a monomial is the sum of  the exponents of the variables. The `degree` of an `Mvp` is the largest degree of a monomial.

With  second argument a  variable name, `degree`  returns the degree of the polynomial in that variable.

```julia-repl
julia> a=x^2+x*y
Mvp{Int64}: x²+xy

julia> degree(a), degree(a,:y), degree(a,:x)
(2, 1, 2)
```


<a target='_blank' href='https://github.com/jmichel7/PuiseuxPolynomials.jl/blob/a50a074146d6c6cc3525ddbe7386f0bda78d9c52/src/PuiseuxPolynomials.jl#L687-L703' class='documenter-source'>source</a><br>

<a id='PuiseuxPolynomials.variables' href='#PuiseuxPolynomials.variables'>#</a>
**`PuiseuxPolynomials.variables`** &mdash; *Function*.



`variables(a::Monomial)` iterator on the variables of `a` (a sorted list of `Symbol`s)


<a target='_blank' href='https://github.com/jmichel7/PuiseuxPolynomials.jl/blob/a50a074146d6c6cc3525ddbe7386f0bda78d9c52/src/PuiseuxPolynomials.jl#L379' class='documenter-source'>source</a><br>


`variables(p::Mvp)`

`variables(v::Array{Mvp})`

returns  the list of  variables of `p`  (resp. all `p`  in `v`) as a sorted list of `Symbol`s.

```julia-repl
julia> @Mvp x,y,z

julia> variables([x+y+1,z])
3-element Vector{Symbol}:
 :x
 :y
 :z
```


<a target='_blank' href='https://github.com/jmichel7/PuiseuxPolynomials.jl/blob/a50a074146d6c6cc3525ddbe7386f0bda78d9c52/src/PuiseuxPolynomials.jl#L924-L941' class='documenter-source'>source</a><br>

<a id='LaurentPolynomials.coefficients-Tuple{Mvp, Symbol}' href='#LaurentPolynomials.coefficients-Tuple{Mvp, Symbol}'>#</a>
**`LaurentPolynomials.coefficients`** &mdash; *Method*.



`coefficients(p::Mvp, var::Symbol)` 

returns  a Dict with keys the degree  in `var` and values the corresponding coefficient of `p` with respect to `var`.

```julia-repl
julia> p=(x+y+inv(y))^4
Mvp{Int64}: x⁴+4x³y+4x³y⁻¹+6x²y²+12x²+6x²y⁻²+4xy³+12xy+12xy⁻¹+4xy⁻³+y⁴+4y²+6+4y⁻²+y⁻⁴

julia> coefficients(p,:x)
Dict{Int64, Mvp{Int64, Int64}} with 5 entries:
  0 => y⁴+4y²+6+4y⁻²+y⁻⁴
  4 => 1
  2 => 6y²+12+6y⁻²
  3 => 4y+4y⁻¹
  1 => 4y³+12y+12y⁻¹+4y⁻³

julia> coefficients(p,:y)
Dict{Int64, Mvp{Int64, Int64}} with 9 entries:
  0  => x⁴+12x²+6
  4  => 1
  -1 => 4x³+12x
  2  => 6x²+4
  -3 => 4x
  -2 => 6x²+4
  -4 => 1
  3  => 4x
  1  => 4x³+12x
```

The  same  caveat  is  applicable  to  `coefficients` as to evaluating: the values  are always `Mvp`s. To get a list of scalars for the coefficients of a  univariate polynomial represented as a `Mvp`, one should use `scalar` on the values of `coefficients`.


<a target='_blank' href='https://github.com/jmichel7/PuiseuxPolynomials.jl/blob/a50a074146d6c6cc3525ddbe7386f0bda78d9c52/src/PuiseuxPolynomials.jl#L766-L801' class='documenter-source'>source</a><br>

<a id='LaurentPolynomials.coefficients-Tuple{Mvp}' href='#LaurentPolynomials.coefficients-Tuple{Mvp}'>#</a>
**`LaurentPolynomials.coefficients`** &mdash; *Method*.



`coefficients(p::Mvp)` 

is an efficient iterator over the coefficients of the monomials in `p`.


<a target='_blank' href='https://github.com/jmichel7/PuiseuxPolynomials.jl/blob/a50a074146d6c6cc3525ddbe7386f0bda78d9c52/src/PuiseuxPolynomials.jl#L759-L763' class='documenter-source'>source</a><br>

<a id='PuiseuxPolynomials.coefficient' href='#PuiseuxPolynomials.coefficient'>#</a>
**`PuiseuxPolynomials.coefficient`** &mdash; *Function*.



`coefficient(p::Mvp,m::Monomial)`

The coefficient of the polynomial `p` on the monomial `m`.

```julia-repl
julia> @Mvp x,y; p=(x-y)^3
Mvp{Int64}: x³-3x²y+3xy²-y³

julia> coefficient(p,Monomial(:x,:x,:y)) # coefficient on x²y
-3

julia> coefficient(p,Monomial()) # constant coefficient
0
```


<a target='_blank' href='https://github.com/jmichel7/PuiseuxPolynomials.jl/blob/a50a074146d6c6cc3525ddbe7386f0bda78d9c52/src/PuiseuxPolynomials.jl#L727-L742' class='documenter-source'>source</a><br>


`coefficient(p::Mvp, var::Symbol, d)` 

returns  the coefficient of degree `d` in the variable `var` in the `Mvp` `p`.

```julia-repl
julia> @Mvp x,y; p=(x+y^(1//2)+1)^3
Mvp{Int64,Rational{Int64}}: x³+3x²y½+3x²+3xy+6xy½+3x+y³⁄₂+3y+3y½+1

julia> coefficient(p,:y,1//2)
Mvp{Int64,Rational{Int64}}: 3x²+6x+3

julia> coefficient(p,:x,1)
Mvp{Int64,Rational{Int64}}: 3y+6y½+3
```


<a target='_blank' href='https://github.com/jmichel7/PuiseuxPolynomials.jl/blob/a50a074146d6c6cc3525ddbe7386f0bda78d9c52/src/PuiseuxPolynomials.jl#L819-L834' class='documenter-source'>source</a><br>

<a id='PuiseuxPolynomials.Monomial' href='#PuiseuxPolynomials.Monomial'>#</a>
**`PuiseuxPolynomials.Monomial`** &mdash; *Type*.



`Monomials` are implemented as a list of pairs `:variable=>degree` sorted in the alphabetic order of vraiables.


<a target='_blank' href='https://github.com/jmichel7/PuiseuxPolynomials.jl/blob/a50a074146d6c6cc3525ddbe7386f0bda78d9c52/src/PuiseuxPolynomials.jl#L347-L350' class='documenter-source'>source</a><br>

<a id='PuiseuxPolynomials.monomials' href='#PuiseuxPolynomials.monomials'>#</a>
**`PuiseuxPolynomials.monomials`** &mdash; *Function*.



`monomials(p::Mvp)` 

is an efficient iterator over the monomials of `p`.


<a target='_blank' href='https://github.com/jmichel7/PuiseuxPolynomials.jl/blob/a50a074146d6c6cc3525ddbe7386f0bda78d9c52/src/PuiseuxPolynomials.jl#L752-L756' class='documenter-source'>source</a><br>

<a id='PuiseuxPolynomials.powers' href='#PuiseuxPolynomials.powers'>#</a>
**`PuiseuxPolynomials.powers`** &mdash; *Function*.



`powers(a::Monomial)` iterator on the powers of variables in `a`


<a target='_blank' href='https://github.com/jmichel7/PuiseuxPolynomials.jl/blob/a50a074146d6c6cc3525ddbe7386f0bda78d9c52/src/PuiseuxPolynomials.jl#L381' class='documenter-source'>source</a><br>

<a id='Base.pairs-Tuple{Monomial}' href='#Base.pairs-Tuple{Monomial}'>#</a>
**`Base.pairs`** &mdash; *Method*.



`pairs(a::Monomial)` 

returns the pairs `:variable=>power` in `a`.


<a target='_blank' href='https://github.com/jmichel7/PuiseuxPolynomials.jl/blob/a50a074146d6c6cc3525ddbe7386f0bda78d9c52/src/PuiseuxPolynomials.jl#L383-L387' class='documenter-source'>source</a><br>

<a id='Base.isless-Tuple{Monomial, Monomial}' href='#Base.isless-Tuple{Monomial, Monomial}'>#</a>
**`Base.isless`** &mdash; *Method*.



`isless(a::Monomial,b::Monomial)`

For  our implementation of `Mvp`s to  work, `isless` must define a monomial order (that is, for monomials `m,a,b` we have `a<b => a*m<b*m`). By default we  use the  "lex" ordering.


<a target='_blank' href='https://github.com/jmichel7/PuiseuxPolynomials.jl/blob/a50a074146d6c6cc3525ddbe7386f0bda78d9c52/src/PuiseuxPolynomials.jl#L490-L496' class='documenter-source'>source</a><br>

<a id='PuiseuxPolynomials.lex' href='#PuiseuxPolynomials.lex'>#</a>
**`PuiseuxPolynomials.lex`** &mdash; *Function*.



`lex(a::Monomial, b::Monomial)` The  "lex" ordering,  where `a<b`  if the  first variable  in `a/b` occurs to a positive power.


<a target='_blank' href='https://github.com/jmichel7/PuiseuxPolynomials.jl/blob/a50a074146d6c6cc3525ddbe7386f0bda78d9c52/src/PuiseuxPolynomials.jl#L452-L456' class='documenter-source'>source</a><br>

<a id='PuiseuxPolynomials.grlex' href='#PuiseuxPolynomials.grlex'>#</a>
**`PuiseuxPolynomials.grlex`** &mdash; *Function*.



`grlex(a::Monomial, b::Monomial)` The "grlex" ordering, where `a<b̀` if `degree(a)>degree(b)` or the degrees are equal but `lex(a,b)`.


<a target='_blank' href='https://github.com/jmichel7/PuiseuxPolynomials.jl/blob/a50a074146d6c6cc3525ddbe7386f0bda78d9c52/src/PuiseuxPolynomials.jl#L467-L471' class='documenter-source'>source</a><br>

<a id='PuiseuxPolynomials.grevlex' href='#PuiseuxPolynomials.grevlex'>#</a>
**`PuiseuxPolynomials.grevlex`** &mdash; *Function*.



`grevlex(a::Monomial, b::Monomial)` The "grevlex" ordering, where `a<b̀` if `degree(a)>degree(b)` or the degrees are equal but the last variable in `a/b` occurs to a negative power.


<a target='_blank' href='https://github.com/jmichel7/PuiseuxPolynomials.jl/blob/a50a074146d6c6cc3525ddbe7386f0bda78d9c52/src/PuiseuxPolynomials.jl#L478-L482' class='documenter-source'>source</a><br>

<a id='Base.pairs-Tuple{Mvp}' href='#Base.pairs-Tuple{Mvp}'>#</a>
**`Base.pairs`** &mdash; *Method*.



`pairs(p::Mvp)` 

returns the pairs monomial=>coefficient in `p`.


<a target='_blank' href='https://github.com/jmichel7/PuiseuxPolynomials.jl/blob/a50a074146d6c6cc3525ddbe7386f0bda78d9c52/src/PuiseuxPolynomials.jl#L745-L749' class='documenter-source'>source</a><br>

<a id='LaurentPolynomials.Pol-Union{Tuple{Mvp{T}}, Tuple{T}} where T' href='#LaurentPolynomials.Pol-Union{Tuple{Mvp{T}}, Tuple{T}} where T'>#</a>
**`LaurentPolynomials.Pol`** &mdash; *Method*.



`Pol(p::Mvp{T}) where T`

converts the one-variable `Mvp{T}` `p` to a `Pol{T}`. It is an error if `p`   has more than one variable.

```julia-repl
julia> @Mvp x; @Pol q; Pol(x^2+x)
Pol{Int64}: q²+q
```


<a target='_blank' href='https://github.com/jmichel7/PuiseuxPolynomials.jl/blob/a50a074146d6c6cc3525ddbe7386f0bda78d9c52/src/PuiseuxPolynomials.jl#L852-L862' class='documenter-source'>source</a><br>

<a id='LaurentPolynomials.Pol-Union{Tuple{N}, Tuple{T}, Tuple{Mvp{T, N}, Symbol}} where {T, N}' href='#LaurentPolynomials.Pol-Union{Tuple{N}, Tuple{T}, Tuple{Mvp{T, N}, Symbol}} where {T, N}'>#</a>
**`LaurentPolynomials.Pol`** &mdash; *Method*.



`Pol(p::Mvp,v::Symbol)`

returns  a polynomial whose coefficients are  the coefficients of the `Mvp` `p`  with respect to the variable `v`.  The variable `v` should appear only with integral powers in `p`.

```julia-repl
julia> p=(x+y^(1//2))^3
Mvp{Int64,Rational{Int64}}: x³+3x²y½+3xy+y³⁄₂

julia> Pol(:q); Pol(p,:x)
Pol{Mvp{Int64, Rational{Int64}}}: q³+3y½q²+3yq+y³⁄₂
```


<a target='_blank' href='https://github.com/jmichel7/PuiseuxPolynomials.jl/blob/a50a074146d6c6cc3525ddbe7386f0bda78d9c52/src/PuiseuxPolynomials.jl#L873-L887' class='documenter-source'>source</a><br>

<a id='PuiseuxPolynomials.Mvp-Tuple{Pol}' href='#PuiseuxPolynomials.Mvp-Tuple{Pol}'>#</a>
**`PuiseuxPolynomials.Mvp`** &mdash; *Method*.



`Mvp(p::Pol)` converts `p` to  an  `Mvp` (with the same variable name). 

```julia-repl
julia> @Pol q
Pol{Int64}: q

julia> Mvp(q^2+q)
Mvp{Int64}: q²+q
```


<a target='_blank' href='https://github.com/jmichel7/PuiseuxPolynomials.jl/blob/a50a074146d6c6cc3525ddbe7386f0bda78d9c52/src/PuiseuxPolynomials.jl#L907-L917' class='documenter-source'>source</a><br>

<a id='PuiseuxPolynomials.value' href='#PuiseuxPolynomials.value'>#</a>
**`PuiseuxPolynomials.value`** &mdash; *Function*.



```julia
`value(p::Mvp,:x₁=>v₁,:x₂=>v₂,...)`
 ̀(p::Mvp)(x₁=v₁,…,xₙ=vₙ)`
```

returns  the value of  `p` when doing  the simultaneous substitution of the variable `:x1` by `v1`, of `x2` by `v2`, …

```julia-repl
julia> p=-2+7x^5*inv(y)
Mvp{Int64}: 7x⁵y⁻¹-2

julia> p(x=2)
Mvp{Int64}: -2+224y⁻¹

julia> p(y=1)
Mvp{Int64}: 7x⁵-2

julia> p(x=2,y=1)
Mvp{Int64}: 222
```

One should pay attention to the fact that the last value is not an integer, but  a constant `Mvp` (for consistency).  See the function `scalar` for how to convert such constants to their base ring.

```julia-repl
julia> p(x=y)
Mvp{Int64}: 7y⁴-2

julia> p(x=y,y=x)
Mvp{Int64}: -2+7x⁻¹y⁵
```

Evaluating an `Mvp` which is a Puiseux polynomial may cause calls to `root`

```julia-repl
julia> p=x^(1//2)*y^(1//3)
Mvp{Int64,Rational{Int64}}: x½y⅓

julia> p(;x=y)
Mvp{Int64,Rational{Int64}}: y⅚

julia> p(;x=4)
Mvp{Int64,Rational{Int64}}: 2y⅓

julia> p(;y=2.0)
Mvp{Float64,Rational{Int64}}: 1.2599210498948732x½
```


<a target='_blank' href='https://github.com/jmichel7/PuiseuxPolynomials.jl/blob/a50a074146d6c6cc3525ddbe7386f0bda78d9c52/src/PuiseuxPolynomials.jl#L980-L1027' class='documenter-source'>source</a><br>

<a id='Base.conj' href='#Base.conj'>#</a>
**`Base.conj`** &mdash; *Function*.



`conj(p::Mvp)` acts on the coefficients of `p`

```julia-repl
julia> @Mvp x;conj(im*x+1)
Mvp{Complex{Int64}}: (0 - 1im)x+1 + 0im
```


<a target='_blank' href='https://github.com/jmichel7/PuiseuxPolynomials.jl/blob/a50a074146d6c6cc3525ddbe7386f0bda78d9c52/src/PuiseuxPolynomials.jl#L665-L672' class='documenter-source'>source</a><br>

<a id='LaurentPolynomials.derivative' href='#LaurentPolynomials.derivative'>#</a>
**`LaurentPolynomials.derivative`** &mdash; *Function*.



The  function 'derivative(p,v₁,…,vₙ)' returns the  derivative of 'p' with  respect to  the variable given by the symbol 'v₁', then `v₂`, ...

```julia-repl
julia> @Mvp x,y;p=7x^5*y^-1-2
Mvp{Int64}: 7x⁵y⁻¹-2

julia> derivative(p,:x)
Mvp{Int64}: 35x⁴y⁻¹

julia> derivative(p,:y)
Mvp{Int64}: -7x⁵y⁻²

julia> derivative(p,:x,:y)
Mvp{Int64}: -35x⁴y⁻²

julia> p=x^(1//2)*y^(1//3)
Mvp{Int64,Rational{Int64}}: x½y⅓

julia> derivative(p,:x)
Mvp{Rational{Int64},Rational{Int64}}: (1//2)x⁻½y⅓

julia> derivative(p,:y)
Mvp{Rational{Int64},Rational{Int64}}: (1//3)x½y⁻⅔

julia> derivative(p,:z)
Mvp{Rational{Int64},Rational{Int64}}: 0
```


<a target='_blank' href='https://github.com/jmichel7/PuiseuxPolynomials.jl/blob/a50a074146d6c6cc3525ddbe7386f0bda78d9c52/src/PuiseuxPolynomials.jl#L1113-L1142' class='documenter-source'>source</a><br>

<a id='PuiseuxPolynomials.laurent_denominator' href='#PuiseuxPolynomials.laurent_denominator'>#</a>
**`PuiseuxPolynomials.laurent_denominator`** &mdash; *Function*.



`laurent_denominator(p1,p2,…)`

returns  the unique true monomial  `m` of minimal degree  such that for all the Laurent polynomials `p1,p2,…` the product `m*pᵢ` is a true polynomial.

```julia-repl
julia> laurent_denominator(x^-1,y^-2+x^4)
Monomial{Int64}:xy²
```


<a target='_blank' href='https://github.com/jmichel7/PuiseuxPolynomials.jl/blob/a50a074146d6c6cc3525ddbe7386f0bda78d9c52/src/PuiseuxPolynomials.jl#L1214-L1224' class='documenter-source'>source</a><br>

<a id='Base.gcd-Tuple{Mvp, Mvp}' href='#Base.gcd-Tuple{Mvp, Mvp}'>#</a>
**`Base.gcd`** &mdash; *Method*.



`gcd(p::Mvp,  q::Mvp)`  computes  the  `gcd`  of  the  'Mvp' arguments. The arguments must be true polynomials.

```julia-repl
julia> gcd(x^2-y^2,(x+y)^2)
Mvp{Int64}: -x-y
```


<a target='_blank' href='https://github.com/jmichel7/PuiseuxPolynomials.jl/blob/a50a074146d6c6cc3525ddbe7386f0bda78d9c52/src/PuiseuxPolynomials.jl#L1179-L1187' class='documenter-source'>source</a><br>

<a id='Base.lcm-Tuple{Mvp, Mvp}' href='#Base.lcm-Tuple{Mvp, Mvp}'>#</a>
**`Base.lcm`** &mdash; *Method*.



`lcm(p1,p2,...)`

Returns  the Lcm  of the  `Mvp` arguments.  The arguments  must be  true polynomials.

```julia-repl
julia> lcm(x^2-y^2,(x+y)^2)
Mvp{Int64}: -x³-x²y+xy²+y³
```


<a target='_blank' href='https://github.com/jmichel7/PuiseuxPolynomials.jl/blob/a50a074146d6c6cc3525ddbe7386f0bda78d9c52/src/PuiseuxPolynomials.jl#L1227-L1237' class='documenter-source'>source</a><br>

<a id='LaurentPolynomials.scalar' href='#LaurentPolynomials.scalar'>#</a>
**`LaurentPolynomials.scalar`** &mdash; *Function*.



`scalar(p::Mvp)`

If `p` is a scalar, return that scalar, otherwise return `nothing`.

```julia-repl
julia> p=Mvp(:x)+1
Mvp{Int64}: x+1

julia> w=p(x=4)
Mvp{Int64}: 5

julia> scalar(w)
5

julia> typeof(scalar(w))
Int64
```

if  `p` is an array, then apply `scalar` to its elements and return `nothing` if it contains any `Mvp` which is not a scalar.


<a target='_blank' href='https://github.com/jmichel7/PuiseuxPolynomials.jl/blob/a50a074146d6c6cc3525ddbe7386f0bda78d9c52/src/PuiseuxPolynomials.jl#L946-L966' class='documenter-source'>source</a><br>

<a id='Base.:^-Tuple{Mvp, AbstractMatrix}' href='#Base.:^-Tuple{Mvp, AbstractMatrix}'>#</a>
**`Base.:^`** &mdash; *Method*.



`Base.:^(p,m;vars=variables(p))`

Implements  the action of  a matrix on  `Mvp`s. `vars` should  be a list of symbols   representing  variables.   The  polynomial   `p`  is  changed  by simultaneous  substitution in it of  `vᵢ` by `(v×m)ᵢ` where  `v` is the row vector  of  the  `Mvp(vᵢ)`.  If  `vars`  is  omitted,  it  is  taken  to be `variables(p)`.

```julia-repl
julia> @Mvp x,y

julia> (x+y)^[1 2;3 1]
Mvp{Int64}: 3x+4y
```


<a target='_blank' href='https://github.com/jmichel7/PuiseuxPolynomials.jl/blob/a50a074146d6c6cc3525ddbe7386f0bda78d9c52/src/PuiseuxPolynomials.jl#L1086-L1101' class='documenter-source'>source</a><br>

<a id='LaurentPolynomials.Frac-Tuple{T} where T<:Mvp' href='#LaurentPolynomials.Frac-Tuple{T} where T<:Mvp'>#</a>
**`LaurentPolynomials.Frac`** &mdash; *Method*.



`Frac(a::Mvp,b::Mvp;pol=false,prime=false)`

`Mvp`s  `a` and `b` are promoted to  same coefficient type, and checked for being  true polynomials  without common  monomial factor (unless `pol=true` asserts  that this  is already  the case)  and unless `prime=true` they are made prime to each other by dividing by their gcd.


<a target='_blank' href='https://github.com/jmichel7/PuiseuxPolynomials.jl/blob/a50a074146d6c6cc3525ddbe7386f0bda78d9c52/src/PuiseuxPolynomials.jl#L1254-L1261' class='documenter-source'>source</a><br>

<a id='PuiseuxPolynomials.grobner_basis' href='#PuiseuxPolynomials.grobner_basis'>#</a>
**`PuiseuxPolynomials.grobner_basis`** &mdash; *Function*.



`grobner_basis(F;lt=lex)`

computes  a Gröbner basis  of the polynomial  ideal generated by the `Mvp`s given  by the vector `F`. The  keyword `lt` describes the monomial ordering to use.

```julia-repl
julia> @Mvp x,y,z; F=[x^2+y^2+z^2-1,x^2-y+z^2,x-z]
3-element Vector{Mvp{Int64, Int64}}:
 x²+y²+z²-1
 x²-y+z²
 x-z

julia> grobner_basis(F)
3-element Vector{Mvp{Int64, Int64}}:
 x-z
 -y+2z²
 4z⁴+2z²-1

julia> grobner_basis(F;lt=grlex)
3-element Vector{Mvp{Int64, Int64}}:
 x-z
 y²+y-1
 -y+2z²

julia> grobner_basis(F;lt=grevlex)
3-element Vector{Mvp{Int64, Int64}}:
 x-z
 y²+y-1
 2x²-y
```

There is no keyword to change the ordering of the variables. We suggest to use `rename_variables` for this purpose.


<a target='_blank' href='https://github.com/jmichel7/PuiseuxPolynomials.jl/blob/a50a074146d6c6cc3525ddbe7386f0bda78d9c52/src/PuiseuxPolynomials.jl#L1432-L1466' class='documenter-source'>source</a><br>

<a id='PuiseuxPolynomials.rename_variables' href='#PuiseuxPolynomials.rename_variables'>#</a>
**`PuiseuxPolynomials.rename_variables`** &mdash; *Function*.



`rename_variables(p,v)` renames `variables(p)` to `v`

`rename_variables(p,l::Pair{Symbol,Symbol}...)` renames the  variables in `p` as indicated by the pairs in `l`.

```julia-repl
julia> @Mvp x,y,z; p=x+y+z
Mvp{Int64}: x+y+z

julia> rename_variables(p,Symbol.('A':'Z'))
Mvp{Int64}: A+B+C

julia> rename_variables(p,[:U,:V])
Mvp{Int64}: U+V+z

julia> rename_variables(p,:x=>:U,:z=>:V) # faster than p(;x=Mvp(:U),z=Mvp(:V))
Mvp{Int64}: U+V+y
```


<a target='_blank' href='https://github.com/jmichel7/PuiseuxPolynomials.jl/blob/a50a074146d6c6cc3525ddbe7386f0bda78d9c52/src/PuiseuxPolynomials.jl#L1492-L1511' class='documenter-source'>source</a><br>

