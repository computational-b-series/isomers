"
This Julia script counts the number n_p of order conditions
for scalar problems.

The number is equivalent to the number of sets of atomic 
stumps, that is

n_p = #{w is a set of stumps of total order p and valency 0}.

In order to be able to generate those sets recursively, 
we mark the lexicographically largest stump in the word.

The running time for p<= 24 is about ten seconds. 

Result:

[1, 2, 4, 8, 15, 28, 51, 91, 160, 278, 475, 803, 1342, 2218, 3629, 5885, 9455, 15068, 23824, 37389, 58265, 90198, 138738]

"

const pmax = 24

edges = [(q, w) for q = 2:pmax for w = 0:q-1]

function update(X)
    Y = Dict()
    for ((p, v, big), cc) in X
        for (q, w) in edges
            if big <= (q, w) && p + q - 1 <= pmax
                y = (p + q - 1, v + w - 1, max(big, (q, w)))
                if !haskey(Y, y)
                    Y[y] = 0
                end
                Y[y] += cc
            end
        end
    end
    return Y
end

function countStumps()
    X = Dict((p, v, (p, v)) => 1 for (p, v) in edges)
    Z = copy(X)
    while length(X) > 0
        X = update(X)
        for (z, cc) in X
            if !haskey(Z, z)
                Z[z] = 0
            end
            Z[z] += cc
        end
    end
    U = Dict()
    for ((p, v, big), cc) in Z
        if v >= 0
            if !haskey(U, (p, v))
                U[p, v] = 0
            end
            U[p, v] += cc
        end
    end
    return [U[p,0] for p in 2:pmax]
end

println(countStumps())