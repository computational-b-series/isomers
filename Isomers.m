px = 12 (* maximum order *)

Print["Generating all tress up to order ", px]
SetAttributes[L, Orderless];
o[L[t___], s_] := L[t, s]; (* Butcher product of two trees *)
tau = L[];
t[1] = tau;
ts[1] = {tau}; (* trees grouped by order *)
allts = {tau}; (* all trees *)
check[{p_, t_}] := If[Not[MemberQ[allts, t]], AppendTo[allts, t]; AppendTo[ts[p], t]]

For[p = 2, p <= px, p++,
    ts[p] = {};
    For[i = 1, i < p, i++, 
       check /@ Flatten[Table[{p, o[s, t]}, {s, ts[p - i]}, {t, ts[i]}], 1]
    ]
]
Print["How many are there?", Length /@ ts /@ Range[px]]

order[tau] = 1 
order[t_] := 1 + Plus@@(order/@(List@@t))
factorial[tau] = 1
factorial[t_] := order[t] * Times@@(factorial/@(List@@t))

phi[tau] := Plus@@b
phi[t_]  := b . Times@@phiA/@(List@@t)
phiA[tau]:= c
phiA[t_] := A . Times@@phiA/@(List@@t)


factor[tau] = {}
factor[t_] := With[{p1 = Length[t], m = Count[t, tau]}, Sort[{s[m, p1-m]} ~Join~ (Join@@Map[factor, t])]]

aa = Accumulate[Length /@ ts /@ ({1}~Join~Range[px-1])]    (* first index of a tree of order p *)
bb = Accumulate[Length /@ ts /@ Range[px]]                 (* last index of a tree of order p *)
psi[tn_] := factor[allts[[tn]]]

Print["Computing isomeric classes"]


For[p = 5, p<= px, p++,
    tt = Range[aa[[p]],bb[[p]]];
    isop = Select[GatherBy[tt,  psi], Length[#] > 1 &];
    Print["==== Isomers of order ==== ", p]
    Print[isop]
]    

Print["Checking the ambiguous method of page 185"]

A = {
{0,0,0,0,0,0},
{1/2, 0, 0, 0, 0, 0},
{-9/4, 13/4, 0, 0, 0, 0},
{9/64, 5/32, -3/64, 0, 0, 0},
{63/625, 259/2500, 231/2500, 252/625, 0, 0},
{-27/50, -139/50, -21/50, 56/25, 5/2, 0}}

c = Plus @@@ A;
b = {1/14, 0, 0, 32/81, 250/567, 5/54};

Print["Order residuals"]
Print[phi/@allts[[1;;17]] - 1/(factorial/@allts[[1;;17]])]

