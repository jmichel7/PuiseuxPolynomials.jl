using Documenter, PuiseuxPolynomials

DocMeta.setdocmeta!(PuiseuxPolynomials, :DocTestSetup, :(using PuiseuxPolynomials); recursive=true)

makedocs(;
    modules=[PuiseuxPolynomials],
    authors="Jean Michel <jean.michel@imj-prg.fr>",
    sitename="PuiseuxPolynomials.jl",
    format=Documenter.HTML(;
        canonical="https://jmichel7.github.io/PuiseuxPolynomials.jl",
        edit_link="main",
        assets=String[],
    ),
    pages=[
        "Home" => "index.md",
    ],
    warnonly=:missing_docs,
)

deploydocs(;
    repo="github.com/jmichel7/PuiseuxPolynomials.jl",
    devbranch="main",
)
