using Documenter, PermGroups

DocMeta.setdocmeta!(PermGroups, :DocTestSetup, :(using PermGroups); recursive=true)

makedocs(sitename="PermGroups documentation",
    modules=[PermGroups,Perms,Groups],
    authors="Jean Michel <jean.michel@imj-prg.fr> and contributors",
    format=Documenter.HTML(;
        canonical="https://jmichel7.github.io/PermGroups.jl",
        edit_link="main",
        assets=String[],
    ),
    pages=[
    "index.md",
    "perms.md",
    "groups.md",
    "permgroups.md",
    ],
    warnonly=:missing_docs,
)

deploydocs(;
    repo="github.com/jmichel7/PermGroups.jl",
    devbranch="main",
)
