SETs     i               USP suites
                         /i1*i3/
         j               DSP suites
                         /j1*j2/
         p               products
                         /p1*p4/
         t               time periods
                         /t1*t9/
         map(i, p)        i1 serves p1*p2. i2*i3 serve p3*p4
                         /i1.p1*p2, i2*i3.p3*p4/

ALIAS(t, tt);

PARAMETERs
$ontext
                 parameter data for upstream production i
$offtext
                 CR(p)           production rate [batches a day]
                                 /p1 .05, p2 .045, p3 .08, p4 .08/
                 alpha(p)        lead time [days]
                                 /p1 30, p2 32, p3 22.5, p4 22.5/
                 zeta(p)         product lifetime [time periods]
                                 /p1*p4 1/
                 C(p)            storage capacity [batch a time period]
                                 /p1*p4 10/
                 CTmin(p)        min campaign length [days]
                                 /p1 20, p2 21, p3 12.5, p4 12.5/
                 CTmax(p)        max capaign length [days]
                                 /p1*p4 60/
                 rho(p)          storage cost [rmu a batch]
                                 /p1*p4 5/
                 nu(p)           sales price [rmu a batch]
                                 /p1 25, p2 20, p3 17, p4 17/
                 eta(p)          mfg cost [rmu a batch]
                                 /p1 5, p2 2, p3 1, p4 1/
                 tau(p)          waste disposal cost [rmu a batch]
                                 /p1*p4 5/
$ontext
                 parameter data for downstream production j
$offtext
                 FR(p)           production rate [batches a day]
                                 /p1*p4 .1/
                 beta(p)         lead time [days]
                                 /p1 40, p2 42, p3 44.5, p4 44.5/
                 sigma(p)        product lifetime [time periods]
                                 /p1*p4 3/
                 F(p)            storage capacity [batch a time period]
                                 /p1*p4 40/
                 FTmin(p)        min campaign length [days]
                                 /p1*p4 10/
                 FTmax(p)        max campaign length [days]
                                 /p1*p4 60/
                 omega(p)        storage cost [rmu a batch]
                                 /p1*p4 1/
                 d(p)            lateness penalty [rmu a batch]
                                 /p1*p4 20/
                 psi(p)          changeover cost [rmu a batch]
                                 /p1*p4 1/
                 lambda(p)       production factor
                                 /p1 1, p2 1, p3 0.5, p4 0.5/;

TABLE            Demand(p, t)         demand profile [number of batches]
                 t1      t2      t3      t4      t5      t6      t7      t8      t9
         p1                               6               4                       4
         p2               4                                       4
         p3                                      10                              10
         p4               6               8;

SCALAR           H               production time horizon
                                 /60/;

BINARY VARIABLEs Z(i, p, t)      includes upstream lead time alpha(p)
                 W(j, p, t)      includes downstream lead time beta(p)
                 X(j, p, t)      activates W(jpt)
                 Y(i, p, t)      denotes whether p is manufactured in suite i at time t. Also activates Z(ipt)
                 U(j, p, t)      denotes whether p is manufactured in suite j at time t. Also activates X(jpt)

INTEGER VARIABLES
$ontext
                 for production constraints
$offtext
                 CP(i, p, t)     upstream production
                 FP(j, p, t)     downstream production;

POSITIVE VARIABLES
$ontext
                 for production constraints
$offtext
                 CT(i, p, t)     upstream production time
                 FT(j, p, t)     downstream production time
$ontext
                 for timing constraints
$offtext
                 CTtot(i, t)     total upstream time
                 FTtot(j, t)     total downstream time
$ontext
                 for storage consraints
$offtext
                 CI(p, t)        amount of crude p stored at time t
                 CW(p, t)        amount of crude p wasted
                 FI(p, t)        amount of final p stored at time t
                 S(p, t)         sales
                 FW(p, t)        amount of final p wasted
$ontext
                 for sales and penalty constraint
$offtext
                 delta(p, t)     penalty for the late delivery of p at time t;

VARIABLE
*                PROFIT
                 profit          profit
                 Sold
                 TC
                 PC
                 CC
                 SC
                 BC
                 WC;

EQUATIONs
*                OBJECTIVE
                 Objective,
                 Sales,
                 TotalCost,
                 ProductionCost,
                 ChangeoverCost,
                 StorageCost,
                 BacklogCost,
                 WasteCost,
*                production constraints
                 UpstreamProduction(i, p, t),
                 DownstreamProduction(j, p, t),
                 Zfunction(i, p, t),
                 Xfunction(j, p, t),
                 Wfunction(j, p, t),
                 Yfunction(i, t),
                 Ufunction(j, t),
*                timing constraints
                 USP_minTiming(i, p, t),
                 USP_maxTiming(i, p, t),
                 DSP_minTiming(j, p, t),
                 DSP_maxTiming(j, p, t),
                 USP_TotalTime(i, t),
                 DSP_TotalTime(j, t),
                 USP_H(i, t),
                 DSP_H(j, t),
*                storage constraints
                 CrudeStorage(p, t),
                 FinalStorage(p, t),
                 CrudeCapacity(p, t),
                 FinalCapacity(p, t),
                 CrudeLimit(p, t),
                 FinalLimit(p, t),
*                sales and penalty constraint
                 del(p, t);
$ontext
                 equations definitions
$offtext
*                        OBJECTIVE function
Objective..              profit          =E=      Sold - TC;
Sales..                  Sold            =E=      SUM( (p, t), nu(p) * S(p, t));
ProductionCost..         PC              =E=      SUM( (i, p, t), eta(p) * CP(i, p, t) ) + SUM( (j, p, t), eta(p) * FP(j, p, t) );
ChangeoverCost..         CC              =E=      SUM( (i, p, t), psi(p) * Z(i, p, t) ) + SUM( (j, p, t), psi(p) * X(j, p, t) );
StorageCost..            SC              =E=      SUM( (p, t), rho(p) * CI(p, t) + omega(p) * FI(p, t));
BacklogCost..            BC              =E=      SUM( (p, t), d(p) * delta(p, t));
WasteCost..              WC              =E=      SUM( (p, t), tau(p) * (CW(p, t) + FW(p, t)) );
TotalCost..              TC              =E=      PC + CC + SC + BC + WC;
*                        production constraints
UpstreamProduction(map(i, p), t)..               CP(i, p, t)     =E=     Z(i, p, t) + CR(p) * ( CT(i, p, t) - alpha(p) * Z(i, p, t) );
* the constaint above can be written in two ways:
*                        UpstreamProduction(map(i, p), t)
*                        OR
*                        UpstreamProduction(i, p, t)$map(i, p)
* the constraint will not be evaluated unless the conditional is true
DownstreamProduction(j, p, t)..                  FP(j, p, t)     =E=     X(j, p, t) + FR(p) * ( FT(j, p, t) - beta(p) * X(j, p, t) );
Zfunction(map(i, p), t)..                        Z(i, p, t)      =G=     Y(i, p, t) - Y(i, p, t - 1);
Xfunction(j, p, t)..                             X(j, p, t)      =G=     U(j, p, t) - U(j, p, t - 1);
Wfunction(j, p, t)..                             W(j, p, t)      =G=     SUM(i, Z(i, p, t)$map(i, p))/card(i) + X(j, p, t) - 1;
Yfunction(i, t)..                                SUM(p, Y(i, p, t)$map(i, p)) =L=   1;
Ufunction(j, t)..                                SUM(p, U(j, p, t)) =L=   1;
*                        timing constraints
USP_minTiming(i, p, t)$map(i, p)..               CTmin(p) * Y(i, p, t) =L= CT(i, p, t);
USP_maxTiming(i, p, t)$map(i, p)..               CTmax(p) * Y(i, p, t) =G= CT(i, p, t);
DSP_minTiming(j, p, t)..                         FTmin(p) * U(j, p, t) =L= FT(j, p, t);
DSP_maxTiming(j, p, t)..                         FTmax(p) * U(j, p, t) =G= FT(j, p, t);
USP_TotalTime(i, t)..                            CTtot(i, t)           =E= SUM(p, CT(i, p, t)$map(i, p) );
DSP_TotalTime(j, t)..                            FTtot(j, t)           =E= SUM(p, FT(j, p, t) );
USP_H(i, t)..                                    CTtot(i, t)           =L= H;
DSP_H(j, t)..                                    FTtot(j, t)           =L= H;
*                        storage constraints
CrudeStorage(p, t)..           CI(p, t)        =E=     CI(p, t - 1) + SUM(i, CP(i, p, t)$map(i, p) )
                                                 - ((1/lambda(p)) * SUM(j, FP(j, p, t)))
                                                 - CW(p, t);
FinalStorage(p, t)..           FI(p, t)        =E=     FI(p, t - 1) + SUM(j, FP(j, p, t) )
                                                 - S(p, t) - FW(p, t);
CrudeCapacity(p, t)..          CI(p, t)        =L=     C(p);
FinalCapacity(p, t)..          FI(p, t)        =L=     F(p);
CrudeLimit(p, t)..             CI(p, t)        =L=     SUM((j, tt)$(ORD(tt)>ORD(t) and ORD(tt)<=(ORD(t) + (zeta(p)))), FP(j, p, tt));
FinalLimit(p, t)..             FI(p, t)        =L=     SUM((tt)$(ORD(tt)>ORD(t) and ORD(tt)<=(ORD(t) + (sigma(p)))), S(p, tt) );
*                        backlog constraints
del(p, t)..                    delta(p, t)     =E=     delta(p, t - 1) + Demand(p, t) - S(p, t);

MODEL                    lakhdar2005example2 /All/;

                         option MIP = CPLEX;
                         option optcr = 0.0;
                         option threads = 4;

SOLVE                    lakhdar2005example2 USING MIP MAXIMIZING profit;

DISPLAY profit.l, Sold.l, TC.l, PC.l, BC.l, SC.l, CC.l, WC.l;

