#=
Computes the isomeric classes up to order 10 in about one second
The output for p<=7 is used in the preprint in a table.
=#
const pmax = 10

struct S
    p::Int
    v::Int
end

function isless(s1::S, s2::S)
    return (s1.p, s1.v)<(s2.p, s2.v)
end

struct Tree
    t::Tuple{Vararg{Tree}}
end

const tau = Tree(())

function chop(t :: Tree)
    if t == tau
        z = []
    else
        p = length(t.t)+1
        r = filter(u -> u!=tau, t.t)
        z = [S(p, length(r))]
        for u in r
            append!(z, chop(u))
        end
    end
    sort(z, lt=isless)
end

function Base.:*(a::Tree, b::Tree)::Tree
    if a.t == tau
        Tree(tuple(b))
    else
        Tree(tuple(vcat(a.t..., b)...))
    end
end

function gentrees()
    allTrees = Dict()
    trees = []
    no = Dict()

    allTrees[1] = [tau]
    trees = [tau]
    no[tau] = 1
    split = [(1, 0)]
    m = 1
    for p = 2:pmax
        allTrees[p] = []
        for i = 1:p-1
            for b in allTrees[i]
                for a in allTrees[p-i]
                    if a == tau || no[b] <= split[no[a]][2]
                        m = m + 1
                        t = a * b
                        push!(allTrees[p], t)
                        push!(trees, t)
                        no[t] = m
                        push!(split, (no[a], no[b]))
                    end
                end
            end
        end
    end
    return allTrees, trees, no
end

function tot(t :: Tree) :: String
    if t == tau
        return "child{node{}}"
     else
        return "child{node{}"*join(map(tot, t.t)," ")*"}"
     end
end

function tex(t::Tree) :: String
    if t == tau
        return "\\begin{tikzpicture}[treestyle]\\node{};\\end{tikzpicture}"
    else
        return "\\begin{tikzpicture}[treestyle]\\node{}" * join(map(tot, collect(t.t))) * ";\\end{tikzpicture}"
    end
end

function findIsomers(trees)
    d = Dict()
    for t in trees
        k = chop(t)
        if !haskey(d, k)
            d[k] = []
        end
        push!(d[k], t)
    end
    for (k,v) in d
        if length(v) > 1
            println(join(["\\S$(s.p)$(s.v)" for s in k], " "))
            println("\\[\n",join(["\\T{$(no[t])}" for t in v], "="),"\n\\]")
        end
    end
end

allTrees, trees, no = gentrees()


open("trees.tex","w") do io
    for (k,t) in enumerate(trees)
        println(io, "\\expandafter\\newcommand\\csname Tree$k\\endcsname{$(tex(t))}")
    end 
    for p in 1:5
        for v in 0:p-1
           W = "child{node[scale=1.2,fill=white]{}}"^v
           B = "child{node{}}"^(p-1-v)
           println(io, "\\expandafter\\newcommand\\csname Stump$p$v\\endcsname{\\begin{tikzpicture}[treestyle]\\node{}$W$B;\\end{tikzpicture}}")
        end
    end
end
println("trees.tex has been written")

for p in 5:10
    println("\\subsection{Isomers of Order $p}")
    findIsomers(allTrees[p])
end
  
