set T;

set ST_ST{T};

param ST_H := (max{t in T} t);

param ST_pred{t in (T diff {1}), ST_ST[t]} in ST_ST[(t - 1)];

param ST_anc{t in T, s in ST_ST[t], tp in T : (tp <= t)} in ST_ST[tp], := (if (tp == t) then s else ST_anc[(t - 1), ST_pred[t, s], tp]);

param ST_ancf{s in ST_ST[ST_H], t in T} in ST_ST[t], := ST_anc[ST_H, s, t];

param ST_d{t in T, s in ST_ST[t]};

set S default ST_ST[ST_H];

param pi{S};

param y0;

set A;

param tau{A} in T;

set P;

set C := (A union P);

param q{C};

param a{t in T} := (sum{c in A : (tau[c] == t)} q[c]);

param d{t in T, s in S} default ST_d[t, ST_ancf[s, t]];

param ymin;

param ymax;

param H := (max{t in T} t);

param gamma{P} in 0 .. (H - 1);

param ca{C};

param cc{A};

param h{T};

var u{T, S} >= 0;

var w{T, S} >= 0;

var y{T, S} >= 0;

var v{c in P, t in T, S : (t <= (H - gamma[c]))} binary;

var x{c in A, t in T, S : (t <= (tau[c] - 1))} binary;

s.t. balance0{s in S}: ((y0 + a[1]) + u[1, s]) == ((d[1, s] + w[1, s]) + y[1, s]);

s.t. balance{t in T, s in S : (t > 1)}: ((y[(t - 1), s] + a[t]) + u[t, s]) == ((d[t, s] + w[t, s]) + y[t, s]);

s.t. inventory{t in T, s in S}: ymin <= y[t, s] <= ymax;

s.t. singleAcquisition{c in P, s in S}: (sum{t in T : (t <= (H - gamma[c]))} v[c, t, s]) <= 1;

s.t. singleCancellation{c in A, s in S}: (sum{t in T : (t <= (tau[c] - 1))} x[c, t, s]) <= 1;

s.t. acquiredFuel{t in T, s in S}: u[t, s] == (sum{c in P : (gamma[c] <= (t - 1))} (q[c] * v[c, (t - gamma[c]), s]));

s.t. cancelledFuel{t in T, s in S}: w[t, s] == (sum{c in A : (tau[c] == t)} (q[c] * (sum{tp in T : (tp <= (tau[c] - 1))} x[c, tp, s])));

minimize cost: (sum{t in T, s in S} (pi[s] * (((sum{c in P : (t <= (H - gamma[c]))} ((ca[c] * q[c]) * v[c, t, s])) + (sum{c in A : (t <= (tau[c] - 1))} (((cc[c] - ca[c]) * q[c]) * x[c, t, s]))) + (h[t] * y[t, s]))));

s.t. NA_u_ctr{t in T, s1 in S, s2 in S : (ST_ancf[s1, t] == ST_ancf[s2, t])}: u[t, s1] == u[t, s2];

s.t. NA_w_ctr{t in T, s1 in S, s2 in S : (ST_ancf[s1, t] == ST_ancf[s2, t])}: w[t, s1] == w[t, s2];

s.t. NA_y_ctr{t in T, s1 in S, s2 in S : (ST_ancf[s1, t] == ST_ancf[s2, t])}: y[t, s1] == y[t, s2];

s.t. NA_v_ctr{t in T, s1 in S, s2 in S, c in P : ((ST_ancf[s1, t] == ST_ancf[s2, t]) and (t <= (H - gamma[c])))}: v[c, t, s1] == v[c, t, s2];

s.t. NA_x_ctr{t in T, s1 in S, s2 in S, c in A : ((ST_ancf[s1, t] == ST_ancf[s2, t]) and (t <= (tau[c] - 1)))}: x[c, t, s1] == x[c, t, s2];

data;

set T := 1 2 3;

set A := "A1" "A2";

set P := "P1" "P2" "P3" "P4";

set ST_ST[1] := 1;
set ST_ST[2] := 1 2;
set ST_ST[3] := 1 2 3 4;

param y0 := 20;

param ymin := 0;

param ymax := 80;

param tau := "A1" 1, "A2" 2;

param gamma := "P1" 1, "P2" 1, "P3" 1, "P4" 1;

param q := "A1" 38, "A2" 22, "P1" 10, "P2" 37, "P3" 39, "P4" 40;

param ca := "A1" 184, "A2" 210, "P1" 160, "P2" 231, "P3" 178, "P4" 152;

param cc := "A1" 32, "A2" 42;

param h := 1 1, 2 1, 3 1;

param ST_pred :=
[2, *] := 1 1, 2 1
[3, *] := 1 1, 2 1, 3 2, 4 2;

param pi := 1 0.15624999999999994, 2 0.34375, 3 0.34375000000000006, 4 0.15625;

param ST_d :=
[1, *] := 1 35
[2, *] := 1 38, 2 17
[3, *] := 1 43, 2 24, 3 34, 4 44;

end;

/*
STANDARD OUTPUT
---------------
$ glpsol --model fuelSupplyMinTSSep_NA.mod --output fuelSupplyMinTSSep_NA.out
GLPSOL: GLPK LP/MIP Solver, v4.55
Parameter(s) specified in the command line:
 --model fuelSupplyMinTSSep_NA.mod --output fuelSupplyMinTSSep_NA.out
Reading model section from fuelSupplyMinTSSep_NA.mod...
Reading data section from fuelSupplyMinTSSep_NA.mod...
fuelSupplyMinTSSep_NA.mod:127: warning: final NL missing before end of file
127 lines were read
Generating balance0...
Generating balance...
Generating inventory...
Generating singleAcquisition...
Generating singleCancellation...
Generating acquiredFuel...
Generating cancelledFuel...
Generating cost...
Generating NA_u_ctr...
Generating NA_w_ctr...
Generating NA_y_ctr...
Generating NA_v_ctr...
Generating NA_x_ctr...
Model has been successfully generated
GLPK Integer Optimizer, v4.55
269 rows, 72 columns, 448 non-zeros
36 integer variables, all of which are binary
Preprocessing...
124 rows, 52 columns, 276 non-zeros
36 integer variables, all of which are binary
Scaling...
 A: min|aij| =  1.000e+00  max|aij| =  4.000e+01  ratio =  4.000e+01
GM: min|aij| =  8.842e-01  max|aij| =  1.131e+00  ratio =  1.279e+00
EQ: min|aij| =  8.314e-01  max|aij| =  1.000e+00  ratio =  1.203e+00
2N: min|aij| =  5.000e-01  max|aij| =  1.250e+00  ratio =  2.500e+00
Constructing initial basis...
Size of triangular part is 59
Solving LP relaxation...
GLPK Simplex Optimizer, v4.55
124 rows, 52 columns, 276 non-zeros
      0: obj =   2.446875000e+01  infeas =  1.875e+01 (65)
*     6: obj =   5.648468750e+03  infeas =  0.000e+00 (65)
*    11: obj =   3.787468750e+03  infeas =  2.220e-16 (65)
OPTIMAL LP SOLUTION FOUND
Integer optimization begins...
+    11: mip =     not found yet >=              -inf        (1; 0)
+    29: >>>>>   7.806968750e+03 >=   4.069468750e+03  47.9% (9; 0)
+    46: >>>>>   7.005468750e+03 >=   4.144468750e+03  40.8% (12; 4)
+    74: >>>>>   6.736468750e+03 >=   5.087968750e+03  24.5% (12; 11)
+    83: >>>>>   6.574968750e+03 >=   5.365468750e+03  18.4% (12; 14)
+    85: >>>>>   6.184468750e+03 >=   5.442968750e+03  12.0% (10; 17)
+    88: >>>>>   5.934968750e+03 >=   5.886968750e+03   0.8% (4; 32)
+    88: mip =   5.934968750e+03 >=     tree is empty   0.0% (0; 47)
INTEGER OPTIMAL SOLUTION FOUND
Time used:   0.0 secs
Memory used: 0.4 Mb (466433 bytes)
Writing MIP solution to 'fuelSupplyMinTSSep_NA.out'...

OUTPUT (RESUME)
---------------
Problem:    fuelSupplyMinTSSep_NA
Rows:       269
Columns:    72 (36 integer, 36 binary)
Non-zeros:  448
Status:     INTEGER OPTIMAL
Objective:  cost = 5934.96875 (MINimum)
(...)
    25 y[1,1]                     23             0               
    26 y[1,2]                     23             0               
    27 y[1,3]                     23             0               
    28 y[1,4]                     23             0               
    29 y[2,1]                     25             0               
    30 y[2,2]                     25             0               
    31 y[2,3]                     46             0               
    32 y[2,4]                     46             0               
    33 y[3,1]                     21             0               
    34 y[3,2]                     40             0               
    35 y[3,3]                     12             0               
    36 y[3,4]                      2             0               
    37 v[P1,1,1]    *              0             0             1 
    38 v[P1,2,1]    *              0             0             1 
    39 v[P1,1,2]    *              0             0             1 
    40 v[P1,2,2]    *              0             0             1 
    41 v[P1,1,3]    *              0             0             1 
    42 v[P1,2,3]    *              0             0             1 
    43 v[P1,1,4]    *              0             0             1 
    44 v[P1,2,4]    *              0             0             1 
    45 v[P2,1,1]    *              0             0             1 
    46 v[P2,2,1]    *              0             0             1 
    47 v[P2,1,2]    *              0             0             1 
    48 v[P2,2,2]    *              0             0             1 
    49 v[P2,1,3]    *              0             0             1 
    50 v[P2,2,3]    *              0             0             1 
    51 v[P2,1,4]    *              0             0             1 
    52 v[P2,2,4]    *              0             0             1 
    53 v[P3,1,1]    *              0             0             1 
    54 v[P3,2,1]    *              1             0             1 
    55 v[P3,1,2]    *              0             0             1 
    56 v[P3,2,2]    *              1             0             1 
    57 v[P3,1,3]    *              0             0             1 
    58 v[P3,2,3]    *              0             0             1 
    59 v[P3,1,4]    *              0             0             1 
    60 v[P3,2,4]    *              0             0             1 
    61 v[P4,1,1]    *              1             0             1 
    62 v[P4,2,1]    *              0             0             1 
    63 v[P4,1,2]    *              1             0             1 
    64 v[P4,2,2]    *              0             0             1 
    65 v[P4,1,3]    *              1             0             1 
    66 v[P4,2,3]    *              0             0             1 
    67 v[P4,1,4]    *              1             0             1 
    68 v[P4,2,4]    *              0             0             1 
    69 x[A2,1,1]    *              1             0             1 
    70 x[A2,1,2]    *              1             0             1 
    71 x[A2,1,3]    *              1             0             1 
    72 x[A2,1,4]    *              1             0             1 
(...)
*/
