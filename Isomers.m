(* Computations for 
Butcher, Podhaisky: Isomeric trees and the order of Runge--Kutta methods, arxiv 2021
*)

px = 8 (* maximum order *)

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
    Print["==== Isomers of order ", p , " ===="]
    Print[isop]
]    

check[p_]:= Module[{res, ii},
    res = FullSimplify[phi/@allts[[1;;bb[[p]]]] - 1/(factorial/@allts[[1;;bb[[p]]]])];
    ii = Flatten[Position[res, x_?(# != 0 &), 1]];
    Print["Non-zero residuals in the order conditions for trees ", ii]; 
    Print["are ", res[[ii]]]
]

Print["Checking the ambiguous method of page 185"]

A = { {0,0,0,0,0,0},
{1/2, 0, 0, 0, 0, 0},
{-9/4, 13/4, 0, 0, 0, 0},
{9/64, 5/32, -3/64, 0, 0, 0},
{63/625, 259/2500, 231/2500, 252/625, 0, 0},
{-27/50, -139/50, -21/50, 56/25, 5/2, 0}}
b = {1/14, 0, 0, 32/81, 250/567, 5/54};
c = Plus @@@ A;
check[5]

Print["John's new Method"]

A = {
{0,0,0,0,0,0},
{1/4,0,0,0,0,0},
{-1/2,1,0,0,0,0},
{3/16,0,9/16,0,0,0},
{291/2500,108/625,63/2500,-9/625,0,0},
{-146/135, 152/15, -7/15, 428/405, -700/81,0}}
b = {5/54, 0,0,32/81,250/567,1/14}
c = Plus @@@ A;

check[5]

Print["New sixth Order Method"]

w= Sqrt[415];

A = {{0, 0, 0, 0, 0, 0, 0, 0}, 
{1/2, 0, 0, 0, 0, 0, 0, 0}, 
{0, 1, 0, 0, 0, 0, 0, 0}, 
{16/125, 13/125, -4/125, 0, 0, 0, 0, 0}, 
{(-136 - 2*w)/2875, (-638 - 6*w)/2875, (821 + 7*w)/11500, (275 + w)/460, 0, 0, 0, 0}, 
{(2469 - 31*w)/40250, (-777 + 18*w)/2875, (18979 - 326*w)/241500, (895 + 2*w)/3220, (95 - w)/210, 0, 0, 0}, 
{(91295 - 6701*w)/45885, (-1415 + 12*w)/2185, (75035 - 3252*w)/91770, (-42285 + 3300*w)/6118, (4115 - 285*w)/399, (-260 + 20*w)/57, 0, 0}, 
{(-77534 + 6878*w)/181125, (1116 - 12*w)/2875, (-3216783 + 103963*w)/14852250, (3489 - 223*w)/1610, (-918 + 58*w)/315, (82 - 4*w)/45, (-285 + 38*w)/15375,0}}
b = {19/288, 0, 0, 25/96, 25/144, 25/144, 19/288, 25/96}
c = Plus @@@ A;

check[6]
