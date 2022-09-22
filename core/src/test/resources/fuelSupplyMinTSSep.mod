# Sets
param H;
set T := 1..H;
set A;
set P;
set C := A union P;
set S;

# Parameters
param d{T, S};
param y0;
param ymin;
param ymax;

param tau{A}   in T;
param gamma{P} in 0..H-1;
param q{C};

param ca{C};
param cc{A};
param h{T};

param a{t in T} := sum{c in A : tau[c] == t} q[c];

param pi{S};
param ancf{S, T};

# Variables
var y{T, S} >= 0;
var v{c in P, t in T, S : t <= H - gamma[c]} binary;
var x{c in A, t in T, S : t <= tau[c] - 1  } binary;

var u{T, S} >= 0;
var w{T, S} >= 0;

# Objective function
minimize cost: 
  sum{t in T, s in S} pi[s] * (
    sum{c in P : t <= H - gamma[c]}          ca[c]  * q[c] * v[c,t,s] +
    sum{c in A : t <= tau[c] - 1  } (cc[c] - ca[c]) * q[c] * x[c,t,s] +
    h[t] * y[t,s]
  );

# Constraints
s.t. balance0{        s in S        }: 
  y0        + a[1] + u[1,s] = d[1,s] + w[1,s] + y[1,s];
s.t. balance {t in T, s in S : t > 1}: 
  y[t-1, s] + a[t] + u[t,s] = d[t,s] + w[t,s] + y[t,s];

s.t. inventory{t in T, s in S}: ymin <= y[t,s] <= ymax;

s.t. single_acquisition {c in P, s in S}: 
  sum{t in 1 .. H - gamma[c]} v[c,t,s] <= 1;

s.t. single_cancellation{c in A, s in S}: 
  sum{t in 1 .. tau[c] - 1  } x[c,t,s] <= 1;

s.t. acquired_fuel{t in T, s in S}: 
  u[t,s] = sum{c in P : gamma[c] <= t-1} q[c] * v[c, t-gamma[c], s];

s.t. cancelled_fuel{t in T, s in S}:
  w[t,s] = sum{c in A : tau[c] == t} (
            q[c] * sum{tp in T : tp <= tau[c] - 1} x[c,tp,s]
           );

# Non anticipativity 
s.t. na_v{
      c in P, t in T, s1 in S, s2 in S : 
        t <= H - gamma[c] and ancf[s1,t] == ancf[s2,t]
    }: v[c,t,s1] == v[c,t,s2];

s.t. na_x{
      c in A, t in T, s1 in S, s2 in S : 
        t <= tau[c] - 1 and ancf[s1,t] == ancf[s2,t]
    }: x[c,t,s1] == x[c,t,s2];

s.t. na_y{       
      t in T, s1 in S, s2 in S : ancf[s1,t] == ancf[s2,t]
    }: y[t,s1] == y[t,s2];

end;
