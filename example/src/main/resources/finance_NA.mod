set T;

set ST_ST{T};

param ST_H := (max{t in T} t);

param ST_pred{t in (T diff {1}), ST_ST[t]} in ST_ST[(t - 1)];

param ST_anc{t in T, s in ST_ST[t], tp in T : (tp <= t)} in ST_ST[tp], := (if (tp == t) then s else ST_anc[(t - 1), ST_pred[t, s], tp]);

param ST_ancf{s in ST_ST[ST_H], t in T} in ST_ST[t], := ST_anc[ST_H, s, t];

set I;

param ST_xi{t in T, s in ST_ST[t], i_ in I : (t > 1)};

set S default ST_ST[ST_H];

param pi{S};

param H := (max{t in T} t);

param b;

param xi{t in T, s in S, i_ in I : (t > 1)} default ST_xi[t, ST_ancf[s, t], i_];

param G;

param q;

param r;

var x{t in T, S, I : (t < H)} >= 0;

var y{S} >= 0;

var w{S} >= 0;

s.t. budget{s in S}: (sum{i in I} x[1, s, i]) == b;

s.t. balance{t in 2 .. (H - 1), s in S}: (sum{i in I} (xi[t, s, i] * x[(t - 1), s, i])) == (sum{i in I} x[t, s, i]);

s.t. goal{s in S}: (((sum{i in I} (xi[H, s, i] * x[(H - 1), s, i])) - y[s]) + w[s]) == G;

maximize utility: (sum{s in S} (pi[s] * ((q * y[s]) - (r * w[s]))));

s.t. NA_x_ctr{t in T, s1 in S, s2 in S, i_ in I : ((ST_ancf[s1, t] == ST_ancf[s2, t]) and (t < H))}: x[t, s1, i_] == x[t, s2, i_];

data;

set I := "stock" "bonds";

set T := 1 2 3 4;

set ST_ST[1] := 1;
set ST_ST[2] := 1 2;
set ST_ST[3] := 1 2 3 4;
set ST_ST[4] := 1 2 3 4 5 6 7 8;

param G := 80000;

param b := 55000;

param q := 1;

param r := 4;

param ST_pred :=
[2, *] := 1 1, 2 1
[3, *] := 1 1, 2 1, 3 2, 4 2
[4, *] := 1 1, 2 1, 3 2, 4 2, 5 3, 6 3, 7 4, 8 4;

param pi := 1 0.125, 2 0.125, 3 0.125, 4 0.125, 5 0.125, 6 0.125, 7 0.125, 8 0.125;

param ST_xi :=
[2, 1, *] := "stock" 1.25, "bonds" 1.14
[2, 2, *] := "stock" 1.06, "bonds" 1.12
[3, 1, *] := "stock" 1.25, "bonds" 1.14
[3, 2, *] := "stock" 1.06, "bonds" 1.12
[3, 3, *] := "stock" 1.25, "bonds" 1.14
[3, 4, *] := "stock" 1.06, "bonds" 1.12
[4, 1, *] := "stock" 1.25, "bonds" 1.14
[4, 2, *] := "stock" 1.06, "bonds" 1.12
[4, 3, *] := "stock" 1.25, "bonds" 1.14
[4, 4, *] := "stock" 1.06, "bonds" 1.12
[4, 5, *] := "stock" 1.25, "bonds" 1.14
[4, 6, *] := "stock" 1.06, "bonds" 1.12
[4, 7, *] := "stock" 1.25, "bonds" 1.14
[4, 8, *] := "stock" 1.06, "bonds" 1.12;

end;

/*
STANDARD OUTPUT
---------------
$ glpsol --model finance_NA.mod --output finance_NA.out
GLPSOL: GLPK LP/MIP Solver, v4.55
Parameter(s) specified in the command line:
 --model finance_NA.mod --output finance_NA.out
Reading model section from finance_NA.mod...
Reading data section from finance_NA.mod...
98 lines were read
Generating budget...
Generating balance...
Generating goal...
Generating utility...
Generating NA_x_ctr...
Model has been successfully generated
GLPK Simplex Optimizer, v4.55
257 rows, 64 columns, 480 non-zeros
Preprocessing...
208 rows, 56 columns, 456 non-zeros
Scaling...
 A: min|aij| =  1.000e+00  max|aij| =  1.250e+00  ratio =  1.250e+00
Problem data seem to be well scaled
Constructing initial basis...
Size of triangular part is 49
      0: obj =  -6.406650000e+02  infeas =  6.776e+03 (159)
*     4: obj =  -3.181785000e+03  infeas =  0.000e+00 (159)
*    10: obj =  -1.514084643e+03  infeas =  2.183e-11 (159)
OPTIMAL LP SOLUTION FOUND
Time used:   0.0 secs
Memory used: 0.5 Mb (502331 bytes)
Writing basic solution to 'finance_NA.out'...

OUTPUT (RESUME)
---------------
Problem:    finance_NA
Rows:       257
Columns:    64
Non-zeros:  480
Status:     OPTIMAL
Objective:  utility = -1514.084643 (MAXimum)
(...)
     1 x[1,1,stock] B        41479.3             0               
     2 x[1,1,bonds] B        13520.7             0               
     3 x[1,2,stock] B        41479.3             0               
     4 x[1,2,bonds] B        13520.7             0               
     5 x[1,3,stock] B        41479.3             0               
     6 x[1,3,bonds] B        13520.7             0               
     7 x[1,4,stock] B        41479.3             0               
     8 x[1,4,bonds] B        13520.7             0               
     9 x[1,5,stock] B        41479.3             0               
    10 x[1,5,bonds] B        13520.7             0               
    11 x[1,6,stock] B        41479.3             0               
    12 x[1,6,bonds] B        13520.7             0               
    13 x[1,7,stock] B        41479.3             0               
    14 x[1,7,bonds] B        13520.7             0               
    15 x[1,8,stock] B        41479.3             0               
    16 x[1,8,bonds] B        13520.7             0               
    17 x[2,1,stock] B        65094.6             0               
    18 x[2,1,bonds] B        2168.14             0               
    19 x[2,2,stock] B        65094.6             0               
    20 x[2,2,bonds] B        2168.14             0               
    21 x[2,3,stock] B        65094.6             0               
    22 x[2,3,bonds] B        2168.14             0               
    23 x[2,4,stock] B        65094.6             0               
    24 x[2,4,bonds] B        2168.14             0               
    25 x[2,5,stock] B        36743.2             0               
    26 x[2,5,bonds] B          22368             0               
    27 x[2,6,stock] B        36743.2             0               
    28 x[2,6,bonds] B          22368             0               
    29 x[2,7,stock] B        36743.2             0               
    30 x[2,7,bonds] B          22368             0               
    31 x[2,8,stock] B        36743.2             0               
    32 x[2,8,bonds] B          22368             0               
    33 x[3,1,stock] B        83839.9             0               
    34 x[3,1,bonds] NL             0             0                    -0.00625 
    35 x[3,2,stock] B        83839.9             0               
    36 x[3,2,bonds] B              0             0               
    37 x[3,3,stock] NL             0             0                 -0.00697545 
    38 x[3,3,bonds] B        71428.6             0               
    39 x[3,4,stock] B              0             0               
    40 x[3,4,bonds] B        71428.6             0               
    41 x[3,5,stock] NL             0             0                 -0.00697545 
    42 x[3,5,bonds] B        71428.6             0               
    43 x[3,6,stock] B              0             0               
    44 x[3,6,bonds] B        71428.6             0               
    45 x[3,7,stock] B          64000             0               
    46 x[3,7,bonds] B              0             0               
    47 x[3,8,stock] B          64000             0               
    48 x[3,8,bonds] NL             0             0                 -0.00876583 
    49 y[1]         B        24799.9             0               
    50 y[2]         B         8870.3             0               
    51 y[3]         B        1428.57             0               
    52 y[4]         NL             0             0                   -0.220424 
    53 y[5]         B        1428.57             0               
    54 y[6]         NL             0             0                   -0.220424 
    55 y[7]         NL             0             0                   -0.227417 
    56 y[8]         NL             0             0                      -0.375 
    57 w[1]         NL             0             0                      -0.375 
    58 w[2]         NL             0             0                      -0.375 
    59 w[3]         NL             0             0                      -0.375 
    60 w[4]         NL             0             0                   -0.154576 
    61 w[5]         NL             0             0                      -0.375 
    62 w[6]         NL             0             0                   -0.154576 
    63 w[7]         NL             0             0                   -0.147583 
    64 w[8]         B          12160             0               
(...)
*/
