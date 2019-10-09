param H;

set T := 1 .. H;

set NA_S{T};

param NA_p{t in (T diff {1}), NA_S[t]} in NA_S[(t - 1)];

param NA_pt{t in T, s in NA_S[t], tp in T : (tp <= t)} in NA_S[tp], := (if (tp = t) then s else NA_pt[(t - 1), NA_p[t, s], tp]);

param NA_ptf{s in NA_S[H], t in T} in NA_S[t], := NA_pt[H, s, t];

param NA_d{t in T, NA_S[t]};

set S default NA_S[H];

param inv0;

set A;

param tau{A} in T;

set P;

set C := (A union P);

param q{C};

param a{t in T} := (sum{c in A : (tau[c] = t)} q[c]);

param d{t in T, s in S} default NA_d[t, NA_ptf[s, t]];

param invmin;

param invmax;

param gamma{P} in 0 .. (H - 1);

param pi{S};

param ca{C};

param cc{A};

param h{T};

var u{T, S} >= 0;

var w{T, S} >= 0;

var inv{T, S} >= 0;

var v{c in P, t in T, S : (t <= (H - gamma[c]))} binary;

var x{c in A, t in T, S : (t <= (tau[c] - 1))} binary;

s.t. balance0{s in S}: ((inv0 + a[1]) + u[1, s]) = ((d[1, s] + w[1, s]) + inv[1, s]);

s.t. balance{t in (T diff {1}), s in S}: ((inv[(t - 1), s] + a[t]) + u[t, s]) = ((d[t, s] + w[t, s]) + inv[t, s]);

s.t. inventory{t in T, s in S}: invmin <= inv[t, s] <= invmax;

s.t. singleAcquisition{c in P, s in S}: (sum{t in T : (t <= (H - gamma[c]))} v[c, t, s]) <= 1;

s.t. singleCancellation{c in A, s in S}: (sum{t in T : (t <= (tau[c] - 1))} x[c, t, s]) <= 1;

s.t. acquiredFuel{t in T, s in S}: u[t, s] = (sum{c in P : (gamma[c] <= (t - 1))} (q[c] * v[c, (t - gamma[c]), s]));

s.t. cancelledFuel{t in T, s in S}: w[t, s] = (sum{c in A : (tau[c] = t)} (q[c] * (sum{tp in T : (tp <= (tau[c] - 1))} x[c, tp, s])));

minimize cost: (sum{t in T, s in S} (pi[s] * (((sum{c in P : (t <= (H - gamma[c]))} ((ca[c] * q[c]) * v[c, t, s])) + (sum{c in A : (t <= (tau[c] - 1))} (((cc[c] - ca[c]) * q[c]) * x[c, t, s]))) + (h[t] * inv[t, s]))));

var NA_inv{t in T, NA_S[t]} >= 0;

s.t. NA_inv_ctr{t in T, s in S}: inv[t, s] = NA_inv[t, NA_ptf[s, t]];

var NA_v{c in P, t in T, NA_S[t] : (t <= (H - gamma[c]))} binary;

s.t. NA_v_ctr{c in P, t in T, s in S : (t <= (H - gamma[c]))}: v[c, t, s] = NA_v[c, t, NA_ptf[s, t]];

var NA_x{c in A, t in T, NA_S[t] : (t <= (tau[c] - 1))} binary;

s.t. NA_x_ctr{c in A, t in T, s in S : (t <= (tau[c] - 1))}: x[c, t, s] = NA_x[c, t, NA_ptf[s, t]];


data;


set A := "A1" "A2";

set P := "P1" "P2" "P3" "P4";

set NA_S[1] := 1;
set NA_S[2] := 1 2;
set NA_S[3] := 1 2 3 4;

param H := 3;

param inv0 := 20;

param invmin := 0;

param invmax := 80;

param tau := "A1" 1, "A2" 2;

param gamma := "P1" 1, "P2" 1, "P3" 1, "P4" 1;

param q := "A1" 38, "A2" 22, "P1" 10, "P2" 37, "P3" 39, "P4" 40;

param ca := "A1" 184, "A2" 210, "P1" 160, "P2" 231, "P3" 178, "P4" 152;

param cc := "A1" 32, "A2" 42;

param h := 1 1, 2 1, 3 1;

param NA_p :=
[2, *] := 1 1, 2 1
[3, *] := 1 1, 2 1, 3 2, 4 2;

param pi := 1 0.15624999999999994, 2 0.34375, 3 0.34375000000000006, 4 0.15625;

param NA_d :=
[1, *] := 1 35
[2, *] := 1 38, 2 17
[3, *] := 1 43, 2 24, 3 34, 4 44;


/*
STANDARD OUTPUT
---------------
$ glpsol --model fuelSupplyMinTSSep_NA.mod --output fuelSupplyMinTSSep_NA.out
GLPSOL: GLPK LP/MIP Solver, v4.55
Parameter(s) specified in the command line:
 --model fuelSupplyMinTSSep_NA.mod --output fuelSupplyMinTSSep_NA.out
Reading model section from fuelSupplyMinTSSep_NA.mod...
Reading data section from fuelSupplyMinTSSep_NA.mod...
fuelSupplyMinTSSep_NA.mod:133: warning: final NL missing before end of file
133 lines were read
Generating balance0...
Generating balance...
Generating inventory...
Generating singleAcquisition...
Generating singleCancellation...
Generating acquiredFuel...
Generating cancelledFuel...
Generating cost...
Generating NA_inv_ctr...
Generating NA_v_ctr...
Generating NA_x_ctr...
Model has been successfully generated
GLPK Integer Optimizer, v4.55
121 rows, 92 columns, 296 non-zeros
49 integer variables, all of which are binary
Preprocessing...
76 rows, 67 columns, 180 non-zeros
49 integer variables, all of which are binary
Scaling...
 A: min|aij| =  1.000e+00  max|aij| =  4.000e+01  ratio =  4.000e+01
GM: min|aij| =  7.376e-01  max|aij| =  1.356e+00  ratio =  1.838e+00
EQ: min|aij| =  5.905e-01  max|aij| =  1.000e+00  ratio =  1.694e+00
2N: min|aij| =  5.000e-01  max|aij| =  1.375e+00  ratio =  2.750e+00
Constructing initial basis...
Size of triangular part is 74
Solving LP relaxation...
GLPK Simplex Optimizer, v4.55
76 rows, 67 columns, 180 non-zeros
      0: obj =   2.446875000e+01  infeas =  1.875e+01 (2)
*     4: obj =   5.568468750e+03  infeas =  0.000e+00 (2)
*    10: obj =   3.787468750e+03  infeas =  1.615e-16 (2)
OPTIMAL LP SOLUTION FOUND
Integer optimization begins...
+    10: mip =     not found yet >=              -inf        (1; 0)
+    31: >>>>>   7.044468750e+03 >=   3.965468750e+03  43.7% (9; 0)
+    43: >>>>>   7.005468750e+03 >=   4.069468750e+03  41.9% (10; 1)
+    65: >>>>>   6.736468750e+03 >=   5.015968750e+03  25.5% (10; 7)
+    74: >>>>>   6.574968750e+03 >=   5.365468750e+03  18.4% (10; 10)
+    76: >>>>>   6.184468750e+03 >=   5.652968750e+03   8.6% (8; 13)
+    79: >>>>>   5.934968750e+03 >=   5.868468750e+03   1.1% (5; 19)
+    79: mip =   5.934968750e+03 >=     tree is empty   0.0% (0; 37)
INTEGER OPTIMAL SOLUTION FOUND
Time used:   0.0 secs
Memory used: 0.4 Mb (375739 bytes)
Writing MIP solution to 'fuelSupplyMinTSSep_NA.out'...

OUTPUT (RESUME)
---------------
Problem:    fuelSupplyMinTSSep_NA
Rows:       121
Columns:    92 (49 integer, 49 binary)
Non-zeros:  296
Status:     INTEGER OPTIMAL
Objective:  cost = 5934.96875 (MINimum)
(...)
    73 NA_inv[1,1]                23             0               
    74 NA_inv[2,1]                25             0               
    75 NA_inv[2,2]                46             0               
    76 NA_inv[3,1]                21             0               
    77 NA_inv[3,2]                40             0               
    78 NA_inv[3,3]                12             0               
    79 NA_inv[3,4]                 2             0               
    80 NA_v[P1,1,1] *              0             0             1 
    81 NA_v[P1,2,1] *              0             0             1 
    82 NA_v[P1,2,2] *              0             0             1 
    83 NA_v[P2,1,1] *              0             0             1 
    84 NA_v[P2,2,1] *              0             0             1 
    85 NA_v[P2,2,2] *              0             0             1 
    86 NA_v[P3,1,1] *              0             0             1 
    87 NA_v[P3,2,1] *              1             0             1 
    88 NA_v[P3,2,2] *              0             0             1 
    89 NA_v[P4,1,1] *              1             0             1 
    90 NA_v[P4,2,1] *              0             0             1 
    91 NA_v[P4,2,2] *              0             0             1 
    92 NA_x[A2,1,1] *              1             0             1 
(...)
*/

end;