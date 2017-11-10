SETS     i               USP suites
                         /i1*i10/
         p               products
                         /p1*p15/
         t               time periods
                         /t1*t4/
         PI(i, p)        set of products produced by i
         TI(i, t)        set of time periods in which i is available for use
                         /i1*i10.t1*t4/;

*        facility i6 unavailable until t5
         TI(i, t)$(ord(i)=6 and ord(t)<5) = no;
*         TI(i, t)$(ord(i)=6) = no;
*        facility i9 unavailable until t41
         TI(i, t)$(ord(i)=9 and ord(t)<40) = no;
*         TI(i, t)$(ord(i)=9) = no;

ALIAS    (t, tt);

* to be imported from input_data.xslx
PARAMETER        Demand(p,t)
                 r(i, p)
                 yd(i, p)
                 eta(i, p);

SET              PI(i, p);

$Call 'Gdxxrw input_data.xlsx output=input.gdx skipempty=0 trace=2 index=Index!A1:F6'
$gdxin input.gdx
$Load Demand PI r yd eta
$gdxin

SCALARS          H               production time horizon [days]
                                 /87/
                 alpha           production lead time [days]
                                 /14/
                 zeta            product life time [time periods]
                                 /8/
                 nu              sales price [rmu per batch]
                                 /2.5/
                 rho             storage cost [rmu per batch]
                                 /.01/
                 d               lateness penalty [rmu per batch]
                                 /.1/
                 psi             changeover cost [rmu per batch]
                                 /2/
                 pie             backlog decay [unitless]
                                 /1/
                 Tmin            min campaign duration [days]
                                 /0/;

BINARY VARIABLEs Z(i, p, t)      enforces incorporation of lead time
                 Y(i, p, t)      denotes whether p is manufactured in suite i at time t. Also activates Z(ipt)

INTEGER VARIABLES
*                for production constraints
                 B(i, p, t)     number of batches;
POSITIVE VARIABLES
                 K(i, p, t)     kg of product produced
*                for timing constraints
                 Time(i, p, t)  campaign duration
                 Tftot(i, t)    total campaign duration
*                for storage consraints
                 Storage(p, t)  amount of  p stored at time t
                 S(p, t)        sales
                 W(p, t)        amount of p wasted
*                for backlog constraint
                 delta(p, t)    penalty for the late delivery of p at time t;

VARIABLE
*                PROFIT
                 profit          profit;

EQUATIONS
*                OBJECTIVE
                 Objective,
*                production constraints
                 Batches(i, p, t),
                 Conversion(i, p, t),
                 Zfunction(i, p, t),
                 Yfunction(i, t),
*                timing constraints
                 MinTiming(i, p, t),
                 MaxTiming(i, p, t),
                 TotalTime(i, t),
*                storage constraints
                 StorageFunction(p, t),
                 LifetimeLimit(p, t),
*                sales and penalty constraint
                 Backlog(p, t);
$ontext
                 equations definitions
$offtext
*                        OBJECTIVE function
Objective..              profit          =E=     SUM((p,t), nu*S(p, t) - rho*Storage(p, t) - d*delta(p, t)
                                                 -SUM(i$(PI(i, p) and TI(i, t)), (eta(i, p)*B(i, p, t)) + psi*Z(i, p, t)));
*                        production constraints
Batches(i, p, t)$(PI(i, p) and TI(i, t))..              B(i, p, t)     =E=     Z(i, p, t) + r(i, p) * (Time(i, p, t) - alpha * Z(i, p, t));
Conversion(i, p, t)$(PI(i, p) and TI(i, t))..           K(i, p, t)     =E=     B(i, p, t)*yd(i, p);
Zfunction(i, p, t)$(PI(i, p) and TI(i, t))..            Z(i, p, t)     =G=     Y(i, p, t) - Y(i, p, t - 1);
*Zfunction(i, p, t)$(PI(i, p) and TI(i, t))..            Z(i, p, t)     =G=     Y(i, p, t) - Y(i, p, t - 1)$(TI(i, t- 1) and PI(i, p));
Yfunction(i, t)$TI(i, t)..                             SUM(p$PI(i, p), Y(i, p, t))     =L=     1;
*                        timing constraints
MinTiming(i, p, t)$(PI(i, p) and TI(i, t))..            Tmin*Y(i, p, t)       =L=     Time(i, p, t);
MaxTiming(i, p, t)$(PI(i, p) and TI(i, t))..            H*Y(i, p, t)          =G=     Time(i, p, t);
TotalTime(i, t)$TI(i, t)..                              Tftot(i, t)           =E=     SUM(p, Time(i, p, t));
*                        storage constraints
StorageFunction(p, t)..                                  Storage(p, t) =E= Storage(p, t - 1) + SUM(i$(PI(i, p) and TI(i, t)), K(i, p, t)) - S(p, t) - W(p, t);
LifetimeLimit(p, t)..                                    Storage(p, t) =L= SUM((tt)$(ORD(tt)>ORD(t) and ORD(tt)<=(ORD(t) + zeta)), S(p, tt) );
*                        backlog constraints
Backlog(p, tt)..                                 delta(p, tt) =E= pie*delta(p, tt - 1) + Demand(p, tt) - S(p, tt);

MODEL                    single_objective /All/;

                         option optcr = 0.0;
                         option reslim = 50000;
                         option threads = 4;

delta.fx(p,t)$(ord(t)>0) = 0;

$ontext 
B.fx(i,p,t)$(ord(i)=1 and ord(p)=9 and ord(t)=1) = 9;
B.fx(i,p,t)$(ord(i)=1 and ord(p)=10 and ord(t)=2) = 9;
B.fx(i,p,t)$(ord(i)=1 and ord(p)=1 and ord(t)=3) = 4;
B.fx(i,p,t)$(ord(i)=1 and ord(p)=6 and ord(t)=4) = 7;

B.fx(i,p,t)$(ord(i)=2 and ord(p)=4 and ord(t)=1) = 25;
B.fx(i,p,t)$(ord(i)=2 and ord(p)=11 and ord(t)=2) = 22;
B.fx(i,p,t)$(ord(i)=2 and ord(p)=11 and ord(t)=3) = 34;
B.fx(i,p,t)$(ord(i)=2 and ord(p)=6 and ord(t)=4) = 29;

B.fx(i,p,t)$(ord(i)=3 and ord(p)=13 and ord(t)=1) = 15;
B.fx(i,p,t)$(ord(i)=3 and ord(p)=13 and ord(t)=2) = 4;
B.fx(i,p,t)$(ord(i)=3 and ord(p)=13 and ord(t)=3) = 5;
B.fx(i,p,t)$(ord(i)=3 and ord(t)=4) = 0;
$offtext

SOLVE                    single_objective USING MIP MAXIMIZING profit;

SCALARS
         Stotal
         Dtotal
         InventoryCost
         ChangeOverCost
         BatchCost
         CostTotal
         Revenue
         TotalProduced;

Stotal = sum((p,t), S.l(p,t));
Dtotal = sum((p,t), Demand(p,t));
InventoryCost = sum((p,t), rho*Storage.l(p,t));
ChangeOverCost = sum((i,p,t), psi*Z.l(i,p,t));
BatchCost = sum((i,p,t), eta(i,p)*B.l(i,p,t));
CostTotal = InventoryCost + ChangeOverCost + BatchCost;
Revenue = sum((p,t), nu*S.l(p,t));
TotalProduced = sum((i,p,t), K.l(i,p,t));

display Stotal;
display Dtotal;
display InventoryCost, ChangeOverCost, BatchCost, CostTotal;
display Revenue, profit.l;
display K.l;
display S.l;
display B.l;
display Storage.l, S.l, delta.l, Time.l, TotalProduced;
display Z.l;
display Y.l;
display Time.l;



