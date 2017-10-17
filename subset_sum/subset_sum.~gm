SETS     i /i1*i10/;

PARAMETERS
         nums(i) /i1 1, i2 2, i3 3, i4 4, i5 5, i6 6, i7 7, i8 8, i9 9, i10 10/;

BINARY VARIABLES
         indices(i);

VARIABLES
         obj;

EQUATIONS
         Objective, Cons1, Cons2;

Objective..      obj =E= sum(i, indices(i) * nums(i));
Cons1..          sum(i, indices(i)) =E= 3;
Cons2..          obj =L= 9;

MODEL    subset_sum /Objective, Cons2, Cons1/;

SOLVE    subset_sum using MIP maximising obj;

DISPLAY  indices.l

