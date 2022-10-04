"""
This package implements Puiseux polynomials, that is linear combinations of
monomials  of the type `x₁^{a₁}… xₙ^{aₙ}` where `xᵢ` are variables and `aᵢ`
are  exponents which can be arbitrary  rational numbers. It depends only on
the  packages `Reexport`, `LaurentPolynomials` and `ModuleElts`s; the names
defined by `LaurentPolynomials` are reexported by this package.

Recall  that  on  an  algebraically  closed coefficient field, the quotient
field  of the Puiseux polynomials is  the algebraic closure of the quotient
field  of the multivariate polynomials.  `Frac` of Puiseux polynomials with
cyclotomic   coefficients  are  a  splitting  field  for  cyclotomic  Hecke
algebras.

This  package is in particular  a perfectly usable (and  quite good I hope)
implementation   of  multivariate  polynomials  and  multivariate  rational
fractions if you are only interested in that.

Some  functions described below work  only with polynomials where variables
are   raised  to  integral  powers;  we  will  refer  to  such  objects  as
"multivariate  Laurent  polynomials";  some  functions require further that
variables   are  raised  only   to  positive  powers:   we  refer  then  to
"multivariate polynomials" (or true polynomials).

Puiseux  polynomials have the  parametric type `Mvp{C,E}`  where `C` is the
type  of the coefficients and `E` is the type of the exponents; `E=Int` for
Laurent   polynomials;   `E=Rational{Int}`   for   more   general   Puiseux
polynomials.  When  printing  only  `C`  is  printed  if `E==Int`. Rational
fractions  are only defined for  numerator and denominator true polynomials
and  have type  `Frac{Mvp{C,Int}}` ---  you can  build a  fraction from the
quotient  of two Laurent polynomials but the result will be normalized to a
quotient of two true polynomials.

We first look at how to make Puiseux polynomials.

`@Mvp x₁,…,xₙ`

assigns  to each  Julia name  `xᵢ` an  `Mvp` representing  an indeterminate
suitable   to  build   multivariate  polynomials   or  rational  fractions.
`Mvp(:x₁)` creates the same `Mvp` without assigning it to variable `x₁`.

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

`Mvp(x::Number)`  returns the multivariate polynomial  with only a constant
term, equal to `x`.

It  is convenient to create `Mvp`s using  variables such as `x,y` above. To
create  them more  directly, `Monomial(:x=>1,:y=>-2)`  creates the monomial
`xy⁻²`, and then `Mvp(Monomial(:x=>1,:y=>-2)=>3,Monomial()=>4)` creates the
`Mvp`  `3xy⁻²+4`. This is the way `Mvp` are printed in another context than
the repl, IJulia or Pluto (where they display nicely as shown above).

```julia-repl
julia> repr(3x*y^-2+4)
"Mvp(Monomial(:x, :y => -2) => 3, Monomial() => 4)" # :x is shorthand for :x=>1

julia> repr(x^(1//2))
"Mvp(Monomial(:x => 1//2))"
```

Only  monomials and one-term `Mvp`s can  be raised to a non-integral power;
the  `Mvp` with one term constant `c`  times the monomial `m` can be raised
to  a fractional  power of  denominator `d`  if and  only if `root(c,d)` is
defined (this is equivalent to `c^{1//d}` for floats);

```julia-repl
julia> (4x)^(1//2)
Mvp{Int64,Rational{Int64}}: 2x½

julia> (2.0x)^(1//2)
Mvp{Float64,Rational{Int64}}: 1.4142135623730951x½

julia> root(2.0x)
Mvp{Float64,Rational{Int64}}: 1.4142135623730951x½
```

One  may  want  to  define  `root`  differently;  for instance, in my other
package   `CylotomicNumbers`  I   define  square   roots  of  rationals  as
cyclotomics, and I also have implemented arbitrary roots of roots of unity.

```julia-rep1
julia> using CyclotomicNumbers

julia> (2x)^(1//2)
Mvp{Cyc{Int64},Rational{Int64}}: √2x½

julia> (E(3)*x)^(2//3)
Mvp{Cyc{Int64},Rational{Int64}}: ζ₉²x⅔
```
There  are various ways to take an  `Mvp` apart. Below are the most direct;
look   also  at  the   functions  `coefficient`,  `coefficients`,  `pairs`,
`monomials`, `variables` and `powers`.

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

julia> m[:x] # power of x in m, same as `degree(m,:x)`
1

julia> m[:y] # power of y in m, same as `degree(m,:y)`
-2

julia> map((x,y)->x=>y,variables(m),powers(m)) # same as pairs(m)
2-element Vector{Pair{Symbol, Int64}}:
 :x => 1
 :y => -2
```

The valuation and degree of an Mvp can be inspected globally or variable by
variable.

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

Terms  are totally ordered in an `Mvp`  by a monomial ordering (that is, an
ordering  on  monomials  so  that  `x<y`  implies `xz<yz` for any monomials
`x,y,z`).  The terms are in decreasing order, so that the first term is the
highest.  By  default,  the  ordering  is  `lex`. The orderings `grlex` and
`grevlex` are also implemented (see `grobner_basis` for how to use them).

An  `Mvp` is a *scalar*  if the valuation and  degree are `0`. The function
`scalar`  returns the  constant coefficient  if the  `Mvp` is a scalar, and
`nothing` otherwise.

Usual  arithmetic (`+`, `-`,  `*`, `^`, `/`,  `//`, `one`, `isone`, `zero`,
`iszero`,  `==`)  works.  Elements  of  type  `<:Number`  are considered as
scalars for scalar operations on the coefficients.

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

When  converting an `Mvp` one needs to specify the two type parameters (the
type of the coefficients and the type of the exponents).

```julia-repl
julia> Mvp{Float64,Rational{Int}}(p)
Mvp{Float64,Rational{Int64}}: 3.0xy⁻²+4.0
```

One can evaluate an `Mvp` when setting the value of some variables by using
the  function call syntax (actually, the keyword syntax for the object used
as a function). There is also a more explicit `value` function.

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

Note  that  an  `Mvp`  always  evaluates  to an `Mvp`, for consistency. You
should  use `scalar` on the  result of giving values  to all variables in a
`Mvp` to get a number.

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

One  can divide an `Mvp` by another when the division is exact, compute the
`gcd` and `lcm` of two `Mvp`.

```julia-repl
julia> exactdiv(x^2-y^2,x-y)
Mvp{Int64}: x+y

julia> (x+y)/(2x^2)   # or by a monomial
Mvp{Float64}: 0.5x⁻¹+0.5x⁻²y

julia> (x+y)//(2x^2)
Mvp{Rational{Int64}}: (1//2)x⁻¹+(1//2)x⁻²y

julia> (x+y)/(x-y)   # if the division is not exact one gets a rational fraction
Frac{Mvp{Int64, Int64}}: (x+y)/(x-y)
```

Raising  a non-monomial  Laurent polynomial  to a  negative power returns a
rational   fraction.  Rational  fractions  are  normalized  such  that  the
numerator  and denominators are true polynomials  prime to each other. They
have  the  arithmetic  operations  `+`,  `-`  , `*`, `/`, `//`, `^`, `inv`,
`one`,  `isone`, `zero`, `iszero` (these  operations can operate between an
`Mvp` or a `Number` and a `Frac{<:Mvp}`).

```julia-repl
julia> (x+1)^-2
Frac{Mvp{Int64, Int64}}: 1/(x²+2x+1)

julia> x+1/(y+1)
Frac{Mvp{Int64, Int64}}: (xy+x+1)/(y+1)

julia> 1-1/(y+1)
Frac{Mvp{Int64, Int64}}: y/(y+1)
```

One  can evaluate a `Frac` when setting  the value of some
variables by using the function call syntax or the `value` function:

```julia-repl
julia> ((x+y)/(x-y))(x=y+1)
Mvp{Float64}: 2.0y+1.0

julia> value((x+y)/(x-y),:x=>y+1;Rational=true) # use // for division
Mvp{Int64}: 2y+1
```

`Frac`  can  be  dissected  using  `numerator`  and `denominator`. They are
scalars  for  broadcasting  and  can  be  sorted  (have  `cmp` and `isless`
methods).

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

Finally,   `Mvp`s  have   methods  `conj`,   `adjoint`  which   operate  on
coefficients,   a   `derivative`   method,   and  methods  `positive_part`,
`negative_part` and `bar` (useful for Kazhdan-Lusztig theory).

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
Despite  the degree of generality of our  polynomials, the speed is not too
shabby.  For the Fateman test f(f+1)  where f=(1+x+y+z+t)^15, we take 3sec.
According to the Nemo paper, Sagemath takes 10sec and Nemo takes 1.6sec.
"""
module PuiseuxPolynomials
using ModuleElts, Reexport
@reexport using LaurentPolynomials
export coefficient, monomials, powers
export Mvp, Monomial, @Mvp, variables, value, laurent_denominator, term,
       lex, grlex, grevlex, grobner_basis, rename_variables
#------------------ Monomials ---------------------------------------------
"""
`Monomials` are implemented as a list of pairs `:variable=>degree` sorted
in the alphabetic order of vraiables.
"""
struct Monomial{T} # T is Int or Rational{Int}
  d::ModuleElt{Symbol,T}   
end

function Monomial(a::Union{Pair,Symbol}...)# convenient but slow
  a=promote(map(x->x isa Symbol ? x=>1 : x,a)...)
  Monomial(ModuleElt(collect(a)))
end

function Monomial(a::Pair{Symbol,T}...)where T
  Monomial(ModuleElt(collect(a)))
end

Monomial()=one(Monomial{Int})

Base.convert(::Type{Monomial{T}},v::Symbol) where T=Monomial(v=>T(1))
Base.convert(::Type{Monomial{T}},m::Monomial{N}) where {T,N}= 
  Monomial(convert(ModuleElt{Symbol,T},m.d))

# we need a promote rule to mix fractional momonials with others in an Mvp
Base.promote_rule(::Type{Monomial{N}},::Type{Monomial{M}}) where {N,M}=
  Monomial{promote_type(N,M)}

Base.:*(a::Monomial, b::Monomial)=Monomial(a.d+b.d)
Base.isone(a::Monomial)=iszero(a.d)
#Base.iszero(a::Monomial)=false
Base.one(::Type{Monomial{T}}) where T=Monomial(zero(ModuleElt{Symbol,T}))
Base.one(m::Monomial)=Monomial(zero(m.d))
Base.inv(a::Monomial)=Monomial(-a.d)
LaurentPolynomials.exactdiv(a::Monomial, b::Monomial)=a*inv(b)
Base.:/(a::Monomial, b::Monomial)=a*inv(b)
Base.://(a::Monomial, b::Monomial)=a*inv(b)
Base.:^(x::Monomial,p)=Monomial(x.d*p)
Base.getindex(a::Monomial,k)=getindex(a.d,k)
Base.length(a::Monomial)=length(a.d)
"`variables(a::Monomial)` iterator on the variables of `a` (a sorted list of `Symbol`s)"
variables(a::Monomial)=keys(a.d)
"`powers(a::Monomial)` iterator on the powers of variables in `a`"
powers(a::Monomial)=values(a.d)
"""
`pairs(a::Monomial)` 

returns the pairs `:variable=>power` in `a`.
"""
Base.pairs(a::Monomial)=pairs(a.d)
ispositive(a::Monomial)=all(>=(0),powers(a))

"copy of m with pairs at i (position or vector of positions) deleted"
deleteat(m::Monomial,i)=Monomial(ModuleElt(deleteat!(copy(pairs(m)),i);check=false))

const unicodeFrac=Dict((1,2)=>'½',(1,3)=>'⅓',(2,3)=>'⅔',
  (1,4)=>'¼',(3,4)=>'¾',(1,5)=>'⅕',(2,5)=>'⅖',(3,5)=>'⅗',
  (4,5)=>'⅘',(1,6)=>'⅙',(5,6)=>'⅚',(1,8)=>'⅛',(3,8)=>'⅜',
  (5,8)=>'⅝',(7,8)=>'⅞',(1,9)=>'⅑',(1,10)=>'⅒',(1,7)=>'⅐')

const subvec=['₀','₁','₂','₃','₄','₅','₆','₇','₈','₉']

function LaurentPolynomials.stringexp(io::IO,r::Rational{<:Integer})
  d=denominator(r); n=numerator(r)
  if isone(d) return LaurentPolynomials.stringexp(io,n) end
  if get(io,:TeX,false) return "^{\\frac{$n}{$d}}" end
  res=Char[]
  if n<0 push!(res,'⁻'); n=-n end
  if haskey(unicodeFrac,(n,d)) push!(res,unicodeFrac[(n,d)])
  else
   if isone(n) push!(res,'\U215F')
   else append!(res,map(x->LaurentPolynomials.supvec[x+1],reverse(digits(n))))
     push!(res,'⁄')
   end
   append!(res,map(x->subvec[x+1],reverse(digits(d))))
  end
  String(res)
end

function Base.show(io::IO,m::Monomial)
  replorTeX=get(io,:TeX,false) || get(io,:limit,false)
  if !replorTeX
    print(io,"Monomial(")
    join(io,map(pairs(m))do (s,c)
      if c==1 repr(s)
      elseif c==2 repr(s)*", "*repr(s)
      else repr(s=>c)
      end
    end,", ")
    print(io,")")
    return
  end
  if isone(m) return end
  start=true
  for (v,d) in pairs(m)
    if !(start || replorTeX) print(io,"*") end
    print(io,string(v))
    if !isone(d) 
      if isone(denominator(d)) d=numerator(d) end
      if replorTeX print(io,LaurentPolynomials.stringexp(io,d))
      elseif d isa Integer print(io,"^$d")
      else print(io,"^($d)")
      end
    end
    start=false
  end
end

function Base.show(io::IO, ::MIME"text/plain", m::Monomial)
  if !haskey(io,:typeinfo) 
    print(io,typeof(m),":") 
    io=IOContext(io,:typeinfo=>typeof(m))
  end
  show(io,m)
end

function lex(a::Vector{<:Pair}, b::Vector{<:Pair})
  for ((va,pa),(vb,pb)) in zip(a,b)
    if va!=vb return va<vb ? pa>0 : pb<0 end
    if pa!=pb return pb<pa end
  end
  la=length(a)
  lb=length(b)
  @inbounds la>lb ? last(a[lb+1])>0 : lb>la ? last(b[la+1])<0 : false
end

"""
`lex(a::Monomial, b::Monomial)`
The  "lex" ordering,  where `a<b`  if the  first variable  in `a/b`
occurs to a positive power.
"""
lex(a::Monomial, b::Monomial)=lex(pairs(a),pairs(b))

"""
`grlex(a::Monomial, b::Monomial)`
The "grlex" ordering, where `a<b̀` if `degree(a)>degree(b)` or the degrees
are equal but `lex(a,b)`.
"""
function grlex(a::Monomial, b::Monomial)
  da=degree(a);db=degree(b)
  if da!=db return db<da end
  lex(a,b)
end

"""
`grevlex(a::Monomial, b::Monomial)`
The "grevlex" ordering, where `a<b̀` if `degree(a)>degree(b)` or the degrees
are equal but the last variable in `a/b` occurs to a negative power.
"""
function grevlex(a::Monomial, b::Monomial)
  da=degree(a);db=degree(b)
  if da!=db return db<da end
  lex(reverse(pairs(b)),reverse(pairs(a)))
end

"""
`isless(a::Monomial,b::Monomial)`

For  our implementation of `Mvp`s to  work, `isless` must define a monomial
order (that is, for monomials `m,a,b` we have `a<b => a*m<b*m`). By default
we  use the  "lex" ordering.
"""
@inline Base.isless(a::Monomial, b::Monomial)=lex(a,b)

Base.:(==)(a::Monomial, b::Monomial)=pairs(a)==pairs(b)

#monomial d of greatest degree in each variable such that (m/d,n/d) positive
Base.gcd(m::Monomial,n::Monomial)=Monomial(ModuleElts.merge2(min,m.d,n.d))
Base.gcd(v::AbstractArray{<:Monomial})=reduce(gcd,v)

#monomial l of smallest degree in each variable such that (l/m,l/n) positive
Base.lcm(m::Monomial,n::Monomial)=Monomial(ModuleElts.merge2(max,m.d,n.d))
Base.lcm(v::AbstractArray{<:Monomial})=reduce(lcm,v)

Base.hash(a::Monomial, h::UInt)=hash(a.d,h)

LaurentPolynomials.degree(m::Monomial)=sum(powers(m);init=0)
LaurentPolynomials.degree(m::Monomial,var::Symbol)=m[var]

function LaurentPolynomials.root(m::Monomial,n::Integer=2)
  if all(x->iszero(x%n),powers(m)) 
    Monomial(ModuleElt(k=>div(v,n) for (k,v) in pairs(m)))
  else Monomial(ModuleElt(k=>v//n for (k,v) in pairs(m)))
  end
end

#------------------------------------------------------------------------------
"""
`Mvp`s are implemented as a list of pairs `monomial=>coefficient` sorted
by the monomial order `lex`.
"""
struct Mvp{T,N} # N=type of exponents T=type of coeffs
  d::ModuleElt{Monomial{N},T}
end

Mvp(a::Pair...;c...)=Mvp(ModuleElt(collect(promote(a...));c...))

function Mvp(a::Union{Pair,Monomial}...)# convenient but slow
  a=promote(map(x->x isa Monomial ? x=>1 : x,a)...)
  Mvp(ModuleElt(collect(a)))
end

Mvp(;c...)=zero(Mvp{Int,Int}) # for some calls to map() to work

"""
 `@Mvp x,y`

 is  equivalent to `x=Mvp(:x);y=Mvp(:y)`  excepted it creates  `x,y` in the
 global scope of the current module, since it uses `eval`.
"""
macro Mvp(t)
  if t isa Expr
    for v in t.args
      Base.eval(Main,:($v=Mvp($(Core.QuoteNode(Symbol(v))))))
    end
  elseif t isa Symbol
    Base.eval(Main,:($t=Mvp($(Core.QuoteNode(t)))))
  end
end

Base.broadcastable(p::Mvp)=Ref(p)
Base.cmp(a::Mvp,b::Mvp)=cmp(a.d,b.d)
Base.isless(a::Mvp,b::Mvp)=cmp(a,b)==-1
Base.hash(a::Mvp,h::UInt)=hash(a.d,h)

function Base.show(io::IO, ::MIME"text/html", a::Mvp)
  print(IOContext(io,:TeX=>true),"\$",a,"\$")
end

function Base.show(io::IO, ::MIME"text/plain", a::Mvp{T,N}) where{T,N}
  if !haskey(io,:typeinfo) 
    print(io,N==Int ? "Mvp{$T}" : "Mvp{$T,$N}",": ")
    io=IOContext(io,:typeinfo=>typeof(a))
  end
  show(io,a)
end

function Base.show(io::IO, x::Mvp)
  if get(io,:limit,false) || get(io,:TeX,false)
    show(IOContext(io,:showbasis=>nothing),x.d)
    # :showbasis=>nothing necessary if called when already showing a ModuleElt
  else
    print(io,"Mvp(")
    join(io,map(pairs(x))do (m,c)
      if c==1 repr(m)
      else repr(m=>c)
      end
    end,", ")
    print(io,")")
  end
end

Base.length(x::Mvp)=length(x.d)
term(x::Mvp,i)=pairs(x)[i]
ismonomial(x::Mvp)=length(x)==1 # x is a non-zero monomial
Base.zero(p::Mvp)=Mvp(zero(p.d))
Base.zero(::Type{Mvp{T,N}}) where {T,N}=Mvp(zero(ModuleElt{Monomial{N},T}))
Base.one(::Type{Mvp{T,N}}) where {T,N}=Mvp(one(Monomial{N})=>one(T);check=false)
Base.one(::Type{Mvp{T}}) where T=one(Mvp{T,Int})
Base.one(::Type{Mvp})=Mvp(1)
Base.one(p::Mvp{T,N}) where {T,N}=iszero(p) ? one(Mvp{T,N}) : Mvp(one(Monomial{N})=>one(first(coefficients(p)));check=false)
Base.isone(x::Mvp)=ismonomial(x) && isone(first(term(x,1)))&&isone(last(term(x,1)))
Base.copy(p::Mvp)=Mvp(ModuleElt(copy(pairs(p))))
Base.iszero(p::Mvp)=iszero(p.d)
Base.convert(::Type{Mvp},a::Number)=convert(Mvp{typeof(a),Int},a)
Base.convert(::Type{Mvp{T,N}},a::Number) where {T,N}=iszero(a) ? 
 zero(Mvp{T,N}) : Mvp(one(Monomial{N})=>convert(T,a);check=false)
(::Type{Mvp{T,N}})(a::Number) where {T,N}=convert(Mvp{T,N},a)
(::Type{Mvp{T,N}})(a::Mvp) where {T,N}=convert(Mvp{T,N},a)
Base.convert(::Type{Mvp{T,N}},a::Mvp{T1,N1}) where {T,T1,N,N1}=
  Mvp(convert(ModuleElt{Monomial{N},T},a.d))
Base.convert(::Type{Mvp},x::Mvp)=x
Base.convert(::Type{Mvp},v::Symbol)=Mvp(Monomial(v)=>1;check=false)
"""
`Mvp(x::Symbol)` creates the `Mvp` with one term of degree one and coefficient
1 with variable `x`
"""
Mvp(x::Symbol)=convert(Mvp,x)
Mvp(x::Number)=convert(Mvp,x)
Mvp(x::Mvp)=x
# stupid stuff to make LU work
Base.adjoint(a::Mvp)=conj(a)
Base.transpose(a::Mvp)=a
Base.abs(a::Mvp)=a

Base.:(==)(a::Mvp, b::Mvp)=a.d==b.d
Base.:(==)(a::Mvp,x::Number)=(iszero(a) && iszero(x)) || (!isnothing(x) && x==scalar(a))
Base.:(==)(x::Number,a::Mvp)= a==x

function Base.convert(::Type{T},a::Mvp) where T<:Number
  if iszero(a) return zero(T) end
  x=scalar(a)
  if isnothing(x) throw(InexactError(:convert,T,a)) end
  T(x)
end
(::Type{T})(a::Mvp) where T<: Number=convert(T,a)

Base.isinteger(p::Mvp)=scalar(p)!==nothing && isinteger(scalar(p))

# we need a promote rule to handle Vectors of Mvps of different types
Base.promote_rule(::Type{Mvp{T1,N1}},::Type{Mvp{T2,N2}}) where {T1,T2,N1,N2} =
  Mvp{promote_type(T1,T2),promote_type(N1,N2)}

# and this rule to handle Vectors mixing Mvps with numbers
Base.promote_rule(::Type{Mvp{T1,N}},::Type{T2}) where {T1,N,T2<:Number} =
  Mvp{promote_type(T1,T2),N}

Base.:+(a::Mvp, b::Mvp)=Mvp(a.d+b.d) # ModuleElts.+ takes care of promotion
Base.:+(a::Number, b::Mvp)=+(promote(a,b)...)
Base.:+(a::Mvp, b::Number)=+(promote(a,b)...)

Base.:-(a::Mvp)=Mvp(-a.d)
Base.:-(a::Mvp, b::Mvp)=Mvp(a.d-b.d)
Base.:-(a::Mvp, b::Number)=-(promote(a,b)...)
Base.:-(b::Number, a::Mvp)=-(promote(b,a)...)

Base.:*(a::Number, b::Mvp)=Mvp(b.d*a)
Base.:*(b::Mvp, a::Number)=a*b
# we have a monomial order so there is no ordering check in next line
Base.:*(a::Monomial, b::Mvp)=Mvp(ModuleElt(m*a=>c for (m,c) in pairs(b);check=false))
Base.:*(b::Mvp,a::Monomial)=a*b

function Base.:*(a::Mvp, b::Mvp)
  if length(a)>length(b) a,b=(b,a) end
  a,b=promote(a,b)
  if iszero(a) return a 
  elseif iszero(b) return b
  elseif isone(a) return b
  elseif isone(b) return a
  end
  let b=b # needed !!!!
    sum(b*m*c for (m,c) in pairs(a))
  end
end

Base.:/(p::Mvp,q::Number)=Mvp(p.d/q)
Base.://(p::Mvp,q::Number)=Mvp(p.d//q)
Base.div(a::Mvp,b::Number)=Mvp(merge(div,a.d,b))
LaurentPolynomials.exactdiv(a::Mvp,b::Number)=Mvp(merge(exactdiv,a.d,b;check=false))

"""
`conj(p::Mvp)` acts on the coefficients of `p`

```julia-repl
julia> @Mvp x;conj(im*x+1)
Mvp{Complex{Int64}}: (0 - 1im)x+1 + 0im
```
"""
Base.conj(a::Mvp)=Mvp(merge(conj,a.d;check=false))

function Base.:^(x::Mvp, p::Union{Integer,Rational})
  if isinteger(p) p=Int(p) end
  if iszero(p) return one(x)
  elseif iszero(x) || isone(p) return x
  elseif p isa Rational return root(x,denominator(p))^numerator(p) 
  elseif ismonomial(x) 
    (m,c)=term(x,1);return isone(c) ? Mvp(m^p=>c;check=false) : Mvp(m^p=>c^p;check=false)
  elseif p>=0 return Base.power_by_squaring(x,p) 
  else return Base.power_by_squaring(inv(x),-p)
  end
end

"""
`degree(m::Mvp[,v::Symbol])`

The `degree` of a monomial is the sum of  the exponents of the variables.
The `degree` of an `Mvp` is the largest degree of a monomial.

With  second argument a  variable name, `degree`  returns the degree of the
polynomial in that variable.

```julia-repl
julia> a=x^2+x*y
Mvp{Int64}: x²+xy

julia> degree(a), degree(a,:y), degree(a,:x)
(2, 1, 2)
```
"""
LaurentPolynomials.degree(p::Mvp)=iszero(p) ? 0 : maximum(degree,monomials(p))
LaurentPolynomials.degree(p::Mvp,v::Symbol)=iszero(p) ? 0 : maximum(x->degree(x,v),monomials(p))

"""
`valuation(m::Mvp[,v::Symbol])`

The `valuation` of an `Mvp` is the minimal degree of a monomial.

With  second argument a variable name, `valuation` returns the valuation of
the polynomial in that variable.

```julia-repl
julia> @Mvp x,y; a=x^2+x*y
Mvp{Int64}: x²+xy

julia> valuation(a), valuation(a,:y), valuation(a,:x)
(2, 0, 1)
```

"""
LaurentPolynomials.valuation(p::Mvp)=iszero(p) ? 0 : minimum(degree,monomials(p))
LaurentPolynomials.valuation(p::Mvp,v::Symbol)=iszero(p) ? 0 : minimum(x->degree(x,v),monomials(p))

"""
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
"""
coefficient(p::Mvp,m::Monomial)=p.d[m]

"""
`pairs(p::Mvp)` 

returns the pairs monomial=>coefficient in `p`.
"""
Base.pairs(p::Mvp)=pairs(p.d)

"""
`monomials(p::Mvp)` 

is an efficient iterator over the monomials of `p`.
"""
monomials(p::Mvp)=keys(p.d)

"""
`coefficients(p::Mvp)` 

is an efficient iterator over the coefficients of the monomials in `p`.
"""
LaurentPolynomials.coefficients(p::Mvp)=values(p.d)

"""
  `coefficients(p::Mvp, var::Symbol)` 

returns  a Dict with keys the degree  in `var` and values the corresponding
coefficient of `p` with respect to `var`.

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

The  same  caveat  is  applicable  to  `coefficients` as to evaluating: the
values  are always `Mvp`s. To get a list of scalars for the coefficients of
a  univariate polynomial represented as a `Mvp`, one should use `scalar` on
the values of `coefficients`.
"""
function LaurentPolynomials.coefficients(p::Mvp{T,N},v::Symbol)where {T,N}
  if iszero(p) return Dict{Int,typeof(p)}() end
  d=Dict{N,typeof(pairs(p))}()
  for (m,c) in pairs(p)
    found=false
    for (i,(v1,deg)) in enumerate(pairs(m))
      if v1==v 
        found=true
        d[deg]=push!(get!(d,deg,empty(pairs(p))),deleteat(m,i)=>c)
      end
    end
    if !found  d[0]=push!(get!(d,0,empty(pairs(p))),m=>c) end
  end
  Dict(dg=>Mvp(c...;check=false) for (dg,c) in d) # c... is sorted by defn of monomial order
end

"""
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
"""
function coefficient(p::Mvp,var::Symbol,d)
 res=empty(pairs(p))
  for (m,c) in pairs(p)
    found=false
    for (i,(v1,deg)) in enumerate(pairs(m))
      if v1==var && deg==d
        found=true
        push!(res,deleteat(m,i)=>c)
        break
      elseif v1>var break
      end
    end
    if d==0 && !found push!(res,m=>c) end
  end
  Mvp(ModuleElt(res;check=false))
end

"""
  `Pol(p::Mvp{T}) where T`

  converts the one-variable `Mvp{T}` `p` to a `Pol{T}`. It is an error if `p`
  has more than one variable.

```julia-repl
julia> @Mvp x; @Pol q; Pol(x^2+x)
Pol{Int64}: q²+q
```
"""
function LaurentPolynomials.Pol(x::Mvp{T})where T
  l=variables(x)
  if isempty(l) return Pol(scalar(x)) end
  if length(l)>1 error("cannot convert $(length(l))-variate Mvp to Pol") end
  val=Int(valuation(x))
  p=zeros(T,Int(degree(x))-val+1)
  for (mon,coeff) in pairs(x) p[Int(degree(mon))-val+1]=coeff end
  Pol(p,val)
end

"""
  `Pol(p::Mvp,v::Symbol)`

returns  a polynomial whose coefficients are  the coefficients of the `Mvp`
`p`  with respect to the variable `v`.  The variable `v` should appear only
with integral powers in `p`.

```julia-repl
julia> p=(x+y^(1//2))^3
Mvp{Int64,Rational{Int64}}: x³+3x²y½+3xy+y³⁄₂

julia> Pol(:q); Pol(p,:x)
Pol{Mvp{Int64, Rational{Int64}}}: q³+3y½q²+3yq+y³⁄₂
```
"""
function LaurentPolynomials.Pol(p::Mvp{T,N},var::Symbol)where{T,N}
  v=Int(valuation(p,var))
  res=[Pair{Monomial{N},T}[] for i in v:Int(degree(p,var))]
  for (m,c) in pairs(p)
    found=false
    for (i,(v1,deg)) in enumerate(pairs(m))
      if v1==var 
        found=true
        push!(res[Int(deg)-v+1],deleteat(m,i)=>c)
        break
      elseif v1>var break
      end
    end
    if !found push!(res[-v+1],m=>c) end
  end
  Pol(map(x->Mvp(ModuleElt(x;check=false)),res),v)
end

"""
`Mvp(p::Pol)` converts `p` to  an  `Mvp` (with the same variable name). 

```julia-repl
julia> @Pol q
Pol{Int64}: q

julia> Mvp(q^2+q)
Mvp{Int64}: q²+q
```
"""
Mvp(x::Pol)=convert(Mvp,x)

Base.convert(::Type{Mvp{T,N}},p::Pol) where{T,N}=
                     p(Mvp(convert(Monomial{N},LaurentPolynomials.varname[])=>one(T)))
Base.convert(::Type{Mvp},p::Pol)=p(Mvp(LaurentPolynomials.varname[]))

"""
`variables(p::Mvp)`

`variables(v::Array{Mvp})`

returns  the list of  variables of `p`  (resp. all `p`  in `v`) as a sorted
list of `Symbol`s.

```julia-repl
julia> @Mvp x,y,z

julia> variables([x+y+1,z])
3-element Vector{Symbol}:
 :x
 :y
 :z
```
"""
variables(pp::AbstractArray{<:Mvp})=sort(unique(k1 for p in pp for k in monomials(p) for k1 in variables(k)))

variables(p::Mvp)=sort(unique(k1 for k in monomials(p) for k1 in variables(k)))

"""
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
if  `p` is an array, then apply `scalar` to its elements and return `nothing`
if it contains any `Mvp` which is not a scalar.
"""
function LaurentPolynomials.scalar(p::Mvp{T})where T
  if iszero(p) return zero(T) end
  if ismonomial(p)
    (m,c)=term(p,1)
    if isone(m) return c end
  end
end

function LaurentPolynomials.scalar(m::AbstractArray{<:Mvp})
  p=scalar.(m)
  if !any(isnothing,p) return p end
end

"""
    `value(p::Mvp,:x₁=>v₁,:x₂=>v₂,...)`
     ̀(p::Mvp)(x₁=v₁,…,xₙ=vₙ)`

returns  the value of  `p` when doing  the simultaneous substitution of the
variable `:x1` by `v1`, of `x2` by `v2`, …

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

One should pay attention to the fact that the last value is not an integer,
but  a constant `Mvp` (for consistency).  See the function `scalar` for how
to convert such constants to their base ring.

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
"""
function value(p::Mvp,k::Pair...)
  res1=badi=nothing
  if length(k)==1 (key,val)=k[1] 
  # special code when length(k)==1 and !(key∈ variables(p)) is 10x faster
  elseif isempty(k) return p
  else vv=Dict(k) 
  end 
  for (i,(m,c1)) in enumerate(pairs(p))
    res=badj=nothing
    for (j,(v,c)) in enumerate(pairs(m))
      if length(k)==1 && key!=v continue end
      if length(k)>1 
        if !haskey(vv,v) continue 
        else val=vv[v] 
        end
      end
      if badj==nothing 
        badj=Int[]
        res=Mvp(c1) 
      end
      if isinteger(c) res*=val^Int(c)
      else res*=root(val,denominator(c))^numerator(c)
      end
      push!(badj,j)
    end
    if badj!==nothing
      res*=deleteat(m,badj)
      badj=nothing
      if badi===nothing res1=res
        badi=Int[]
      else res1+=res
      end
      push!(badi,i)
    end
 #  println("badi=$badi m=$m c=$c res1=$res1")
  end
  if badi===nothing return p end
  Mvp(deleteat!(copy(pairs(p)),badi)...;check=false)+res1
end

(p::Mvp)(;arg...)=value(p,arg...)

function LaurentPolynomials.root(a::Integer,n=2)
  if n!=2 return end
  r=isqrt(a)
  if r^2==a return r end
end

function LaurentPolynomials.root(p::Mvp,n=2)
  if iszero(p) return p end
  n=Int(n)
  if !ismonomial(p)
    throw(DomainError("root($p,$n) non-monomial not implemented")) 
  end
  (m,c)=term(p,1)
  isone(c) ? Mvp(root(m,n)=>c;check=false) : Mvp(root(m,n)=>root(c,n))
end

"""
`Base.:^(p,m;vars=variables(p))`

Implements  the action of  a matrix on  `Mvp`s. `vars` should  be a list of
symbols   representing  variables.   The  polynomial   `p`  is  changed  by
simultaneous  substitution in it of  `vᵢ` by `(v×m)ᵢ` where  `v` is the row
vector  of  the  `Mvp(vᵢ)`.  If  `vars`  is  omitted,  it  is  taken  to be
`variables(p)`.

```julia-repl
julia> @Mvp x,y

julia> (x+y)^[1 2;3 1]
Mvp{Int64}: 3x+4y
```
"""
Base.:^(p::Mvp,m::AbstractMatrix;vars=variables(p))=
  p(;map(Pair,vars,permutedims(Mvp.(vars))*m)...)

LaurentPolynomials.positive_part(p::Mvp)=
  Mvp(ModuleElt(m=>c for (m,c) in pairs(p) if ispositive(m);check=false))

LaurentPolynomials.negative_part(p::Mvp)=
  Mvp(ModuleElt(m=>c for (m,c) in pairs(p) if all(<(0),powers(m));check=false))

LaurentPolynomials.bar(p::Mvp)=Mvp(ModuleElt(inv(m)=>c for (m,c) in pairs(p)))

"""
The  function 'derivative(p,v₁,…,vₙ)' returns the  derivative of 'p' with 
respect to  the variable given by the symbol 'v₁', then `v₂`, ...

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
"""
function LaurentPolynomials.derivative(p::Mvp,vv...)
  # check needed because 0 could appear in coeffs
  for v in vv
    p=Mvp(ModuleElt(m*Monomial(v=>-1)=>c*degree(m,v) for (m,c) in pairs(p)))
  end
  p
end

# returns p/q when the division is exact, nothing otherwise
# Arguments must be true polynomials
function LaurentPolynomials.exactdiv(p::Mvp,q::Mvp)
  if isone(q) return p end
  if iszero(q) error("cannot divide by 0")
  elseif iszero(p) || isone(q) return p
  elseif ismonomial(q)
    m,c=term(q,1)
    return Mvp(ModuleElt(inv(m)*m1=>exactdiv(c1,c) for (m1,c1) in pairs(p);check=false))
   elseif ismonomial(p) error(q," does not exactly divide ",p)
  end 
  var=first(variables(first(monomials(p))))
  res=zero(p)
  mq=degree(q,var)
  cq=coefficient(q,var,mq)
  p1=p
  while !iszero(p1)
#   if length(p)<length(q) return nothing end
    mp=degree(p1,var)
    t=exactdiv(coefficient(p1,var,mp),cq)
    if mp<mq  error(q," does not exactly divide ",p)
    elseif mp!=mq t=Monomial(ModuleElt(var=>mp-mq))*t
    end
    res+=t
    p1-=t*q
  end
  res
end

Base.gcd(a::AbstractFloat,b::AbstractFloat)=one(a)*one(b)

"""
`gcd(p::Mvp,  q::Mvp)`  computes  the  `gcd`  of  the  'Mvp' arguments. The
arguments must be true polynomials.

```julia-repl
julia> gcd(x^2-y^2,(x+y)^2)
Mvp{Int64}: -x-y
```
"""
function Base.gcd(a::Mvp,b::Mvp)
  if iszero(a) return b
  elseif iszero(b) return a
  elseif ismonomial(a)
    (m,c)=term(a,1)
    return Mvp(gcd(m,reduce(gcd,monomials(b)))=>gcd(c,reduce(gcd,coefficients(b)));check=false)
  elseif ismonomial(b) return gcd(b,a)
  end
  va=variables(a)
  vb=variables(b)
  vars=intersect(va,vb)
  if isempty(vars) 
    if iszero(a) return b
    elseif iszero(b) return a
    else 
      return Mvp(gcd(reduce(gcd,coefficients(a)),reduce(gcd,coefficients(b))))
    end
  end
  v=first(vars)
  if length(union(va,vb))==1 srgcd(Pol(a),Pol(b))(Mvp(v))
  else              srgcd(Pol(a,v),Pol(b,v))(Mvp(v))
  end
end

Base.gcd(v::AbstractArray{<:Mvp})=reduce(gcd,v;init=Mvp(0))

"""
`laurent_denominator(p1,p2,…)`

returns  the unique true monomial  `m` of minimal degree  such that for all
the Laurent polynomials `p1,p2,…` the product `m*pᵢ` is a true polynomial.

```julia-repl
julia> laurent_denominator(x^-1,y^-2+x^4)
Monomial{Int64}:xy²
```
"""
laurent_denominator(pp::Mvp...)=inv(gcd([m for p in pp for (m,c) in pairs(p)]))

"""
`lcm(p1,p2,...)`

Returns  the Lcm  of the  `Mvp` arguments.  The arguments  must be  true
polynomials.

```julia-repl
julia> lcm(x^2-y^2,(x+y)^2)
Mvp{Int64}: -x³-x²y+xy²+y³
```
"""
Base.lcm(a::Mvp,b::Mvp)=exactdiv(a*b,gcd(a,b))
Base.lcm(a::AbstractArray{<:Mvp})=reduce(lcm,a)

Base.eltype(p::Mvp{T,N}) where{T,N} =T

Base.denominator(p::Mvp)=lcm(denominator.(coefficients(p)))
Base.numerator(p::Mvp{<:Rational{T},N}) where{T,N} =convert(Mvp{T,N},p*denominator(p))
#----------cohabitation of monomials and Mvps ------------------------------------

Base.promote_rule(::Type{Monomial{N}},::Type{T2}) where {N,T2<:Number}=Mvp{T2,N}

Base.promote_rule(::Type{Mvp{T,N}},::Type{Monomial{N1}}) where {T,N,N1} =
  Mvp{T,promote_type(N,N1)}

Base.convert(::Type{Mvp{T,N}},m::Monomial{N1}) where{T,N,N1}=Mvp(convert(Monomial{N},m)=>1)

Base.:+(a::Monomial,b::Monomial)=Mvp(a=>1,b=>1)
Base.:+(a::Union{Mvp,Number},b::Monomial)=+(promote(a,b)...)
Base.:+(b::Monomial,a::Union{Mvp,Number})=+(promote(a,b)...)
Base.:-(a::Monomial,b::Monomial)=Mvp(a=>1,b=>-1)
Base.:-(a::Union{Mvp,Number},b::Monomial)=-(promote(a,b)...)
Base.:-(b::Monomial,a::Union{Mvp,Number})=-(promote(a,b)...)
Base.:*(b::Monomial,a::Number)=Mvp(b=>a)
Base.:*(a::Number,b::Monomial)=Mvp(b=>a)
#----------------------------- Frac{Mvp{T,Int}} -------------------------------
# make both pols positive without common monomial factor
function make_positive(a::Mvp,b::Mvp)
  d=laurent_denominator(a,b)
  isone(d) ? (a,b) : (a*d,b*d)
end
  
LaurentPolynomials.Frac(a::Mvp,b::Mvp;k...)=Frac(promote(a,b)...;k...)
  
"""
`Frac(a::Mvp,b::Mvp;pol=false,prime=false)`

`Mvp`s  `a` and `b` are promoted to  same coefficient type, and checked for
being  true polynomials  without common  monomial factor (unless `pol=true`
asserts  that this  is already  the case)  and unless `prime=true` they are
made prime to each other by dividing by their gcd.
"""
function LaurentPolynomials.Frac(a::T,b::T;pol=false,prime=false)::Frac{T} where T<:Mvp
  if iszero(a) return LaurentPolynomials.Frac_(a,one(a))
  elseif iszero(b) error("division by 0")
  end
  if !pol
    (a,b)=make_positive(a,b)
  end
  if !prime && !ismonomial(b) && !ismonomial(a)
    d=gcd(a,b)
    a,b=exactdiv(a,d),exactdiv(b,d)
  end
  if scalar(b)==-1 a,b=(-a,-b) end
  return LaurentPolynomials.Frac_(a,b)
end

LaurentPolynomials.Frac(a::Mvp)=Frac(a,one(a);prime=true)

function LaurentPolynomials.Frac(a::Mvp{<:Rational,Int},b::Mvp{<:Rational,Int};k...)
  Frac(numerator(a)*denominator(b),numerator(b)*denominator(a);k...)
end

function Mvp(p::Frac{<:Mvp})
  if isone(p.den) return p.num
  elseif ismonomial(p.den) 
    (m,c)=term(p.den,1)
    return p.num*inv(m)*inv(c)
  end
  error("cannot convert ",p," to Mvp")
end

Base.convert(::Type{Mvp{T,Int}},p::Frac{<:Mvp}) where T=convert(Mvp{T,Int},Mvp(p))

function Base.convert(::Type{Frac{T}},p::Mvp) where T<:Mvp
  q=convert(T,p)
  Frac(q,one(q);prime=true)
end

function Base.convert(::Type{Frac{T}},p::Number) where T<:Mvp
  Frac(convert(T,p),convert(T,1);pol=true,prime=true)
end

function Base.promote_rule(a::Type{T1},b::Type{Frac{T2}})where {T1<:Mvp,T2<:Mvp}
  Frac{promote_type(T1,T2)}
end

function Base.promote_rule(a::Type{Frac{T1}},b::Type{Frac{T2}})where {T1<:Mvp,T2<:Mvp}
  Frac{promote_type(T1,T2)}
end

function Base.promote_rule(a::Type{T1},b::Type{Frac{T2}})where {T1<:Number,T2<:Mvp}
  Frac{promote_type(Mvp{T1,Int},T2)}
end

function Base.inv(p::Mvp)
  if ismonomial(p)
    (m,c)=term(p,1)
    return Mvp(inv(m)=>c^2==1 ? c : 1/c) 
  end
  Frac(Mvp(1),p;prime=true)
end

function Base.://(a::Mvp,b::Mvp)
  if iszero(a) return a end
  if ismonomial(b) 
    (m,c)=term(b,1)
    return Mvp(ModuleElt(m1/m=>c^2==1 ? c1*c : c1//c for (m1,c1) in pairs(a);check=false))
  end
  Frac(a,b)
end

function Base.:/(a::Mvp,b::Mvp)
  if iszero(a) return a end
  if ismonomial(b) 
    (m,c)=term(b,1)
    return Mvp(ModuleElt(m1/m=>c1/c for (m1,c1) in pairs(a);check=false))
  end
  Frac(a,b)
end

Base.://(a::Frac{<:Mvp},b::Number)=Frac(a.num,a.den*b;pol=true,prime=true)
Base.://(a::Frac{<:Mvp},b::Mvp)=Frac(a.num,a.den*b)
Base.:/(a::Frac{<:Mvp},b::Union{Frac{<:Mvp},Number,Mvp})=a//b
Base.:/(a::Union{Number,Mvp},b::Frac{<:Mvp})=a*inv(b)
Base.://(p::Number,q::Mvp)=Mvp(p)//q
Base.:/(p::Number,q::Mvp)=Mvp(p)/q

Base.:*(a::Frac{<:Mvp},b::Number)=Frac(a.num*b,a.den;pol=true,prime=true)
Base.:*(b::Number,a::Frac{<:Mvp})=a*b

value(p::Frac{<:Mvp},k::Pair...;Rational=false)=Rational ? 
  value(p.num,k...)//value(p.den,k...) : value(p.num,k...)/value(p.den,k...)

(p::Frac{<:Mvp})(;arg...)=value(p,arg...)

#--------------------  Grobner bases -------------------------------------
# the reference is Cox, Little, O'Shea chapter 2

# minimum term and its index for monomial order lt
# findmin does not have the keywords lt and by so I must spin my own
function fmin(l;lt=lex,by=first) 
  res=(first(l),1)
  for i in 2:length(l)
   if lt(by(l[i]),by(first(res))) res=(l[i],i) end
  end
  res
end

# Leading term for monomial order lt
LT(p;lt=lex)=lt==lex ? first(pairs(p)) : fmin(pairs(p);lt)[1]

# drop from p leading term for monomial order lt
function dropLT(p;lt=lex)
  if lt==lex return Mvp(ModuleElt(pairs(p)[2:end];check=false)) end
  Mvp(ModuleElt(deleteat!(pairs(p),fmin(pairs(p);lt)[2]);check=false))
end

# quotient of leading terms
function quotientLT(p,q;lt=lex)
  pm,pc=LT(p;lt)
  qm,qc=LT(q;lt)
  t=pm/qm
  if ispositive(t) Mvp(t=>exactdiv(pc,qc)) end
end

# remainder on division of p by list F
# Cox-Little-O'Shea Th. 3 §3 chap.2
function remainder(p,F;lt=lex)
  q=zero(F)
  r=zero(p)
  while !iszero(p)
    gotquotient=false
    for i in eachindex(F)
      t=quotientLT(p,F[i];lt)
      if t!==nothing 
        q[i]+=t
#       xprintln("p=",p," F[i]=",F[i]," t=",t)
        p-=t*F[i]
        gotquotient=true
        break
      end
    end
    if !gotquotient
      r+=Mvp(LT(p;lt))
      p=dropLT(p;lt)
    end
  end
  (q,r)
end

# Cox-Little-O'Shea def. 4.(ii) §6 chap.2
function S_polynomial(p,q;lt=lex)
  pm,pc=LT(p;lt)
  qm,qc=LT(q;lt)
  c=lcm(pm,qm)
  (c/pm)*exactdiv(p,pc)-(c/qm)*exactdiv(q,qc)
end
  
function reduce_basis(F;lt=lex)
# F=sort(F,by=length)
  F=copy(F)
  i=1
  while i<=length(F)
    if any(j->j!=i && quotientLT(F[i],F[j];lt)!==nothing,eachindex(F)) 
      deleteat!(F,i)
    else i+=1
    end
  end
  F
end

"""
`grobner_basis(F;lt=lex)`

computes  a Gröbner basis  of the polynomial  ideal generated by the `Mvp`s
given  by the vector `F`. The  keyword `lt` describes the monomial ordering
to use.
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

There is no keyword to change the ordering of the variables. We suggest
to use `rename_variables` for this purpose.
"""
function grobner_basis(F;lt=lex)
# Cox-Little-O'Shea Th. 9 §10 chap.2
  F=copy(F)
  B=[(i,j) for j in 1:length(F) for i in 1:j-1]
  t=length(F)
  while !isempty(B)
    i,j=popfirst!(B)
    li,_=LT(F[i];lt)
    lj,_=LT(F[j];lt)
    lij=lcm(li,lj)
    if lij==li*lj continue end
    ll=filter(l->l!=i && l!=j && !((i,l) in B) && !((j,l) in B)
               && !((l,i) in B) && !((l,j) in B),1:length(F))
    if any(l->ispositive(lij/first(LT(F[l];lt))),ll) continue end
    s=S_polynomial(F[i],F[j];lt)
    r=remainder(s,F;lt)[2]
    if !iszero(r) 
      t+=1
      push!(F,r)
      append!(B,tuple.(1:t-1,t))
    end
  end
  reduce_basis(F;lt)
end

"""
`rename_variables(p,v)` renames `variables(p)` to `v`

`rename_variables(p,l::Pair{Symbol,Symbol}...)` renames the 
variables in `p` as indicated by the pairs in `l`.

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
"""
function rename_variables(p::Mvp,l::Pair{Symbol,Symbol}...)
  d=Dict(l)
  Mvp(ModuleElt(map(pairs(p)) do (m,c)
    Monomial(ModuleElt(map(((v,i),)->(haskey(d,v) ? d[v] : v)=>i,pairs(m))))=>c
  end))
end

rename_variables(p::Mvp,l::AbstractVector{Symbol})=
     rename_variables(p,map((x,y)->x=>y,variables(p),l)...)
#--------------------  benchmarks -------------------------------------
#julia> @btime (x+y+z)^3;
#  2.292 μs (49 allocations: 5.42 KiB)

#julia1.8.0> @btime PuiseuxPolynomials.fateman(15)
# 2.888 s (15219387 allocations: 4.87 GiB)
function fateman(n)
  f=(1+sum(Mvp.((:x,:y,:z,:t))))*one(n)
  f=f^n
  length(f*(f+1))
end

#julia1.8.0> @btime inv(Frac.([x+y x-y;x+1 y+1])) setup=(x=Mvp(:x);y=Mvp(:y))
# 139.657 μs (3284 allocations: 235.44 KiB)
end
