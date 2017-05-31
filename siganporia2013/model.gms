SETS     i               facilities (2 owned 1 CMO 1 future)
                         /BRKL-60A, BRKL-60B, CMO, Future/
         p               products (3 perfusion 1 mAb)
                         /KG-PF, KG-N, rFVII, Mab/
         t               time periods (8 years each t = 1 month or 30 days)
                         /t1*t96/
         y               years
                         /1*8/
         TY(y,t)         time periods which are in year y
                         /1 . (t1*t12)
                          2 . (t13*t24)
                          3 . (t25*t36)
                          4 . (t37*t48)
                          5 . (t49*t60)
                          6 . (t61*t72)
                          7 . (t73*t84)
                          8 . (t85*t96)/
         IO(i)           owned facilities
                         /BRKL-60A, BRKL-60B, Future/
         PP(p)           perfusion products
                         /KG-PF, KG-N, rFVII/
         IT(i,t)         facilities which are available in period t
         IP(i,p)         facilities which can produce product p
                         /BRKL-60A  .     (KG-N, KG-PF, rFVII, Mab)
                          BRKL-60B  .     (KG-PF, KG-N)
                          CMO       .     (rFVII, Mab)
                          Future    .     (KG-PF, KG-N, Mab)/;

*        Control when certain facilities are available
         IT(i,t) = YES;
         IT(i,t)$(ORD(i) = 1 and ORD(t) < 25) = NO;
         IT(i,t)$(ORD(i) = 4 and ORD(t) < 49) = NO;

DISPLAY IT

ALIAS
*        j represents the DSP suite
         (i,j);
ALIAS
*        pp is used for changeover constraints
         (p,p0);
ALIAS
*        theta is used for perfusion3 constraint,
*        which ensures that the perfusion campaign is run for its entire length
         (t,theta);

SCALARS
         H               available production time horizon over time period t [days]
                         /30/
         R               rejection coefficient [%]
                         /0/
         wcost           waste cost
                         /1/
         FcostStart      cost per new startup
                         /0/
         tCap            number of days in a year that production is allowed
                         /270/;

PARAMETERS
         etaU(i,p)       USP batch cost
         etaD(i,p)       DSP batch cost
         xU(i,p)         USP batch output
         xD(i,p)         DSP batch output
         xlD(p)          DSP lot sizes
         alpha(p,p0)     changeover time
         rho(i,p)        storage cost
         rho2(i,p)       cost of carrying inventory USP
         rho3(i,p)       cost of carrying inventory DSP
         C(i,p)          storage capacity
         lambda(i,p)     licence fees
         lambdaU(i,p)    retrofitting cost for USP
         lambdaD(i,p)    retrofitting cost for DSP
         kappa(i)        facility investment cost
         delta(p)        backlog penalty
         upsilon(p)      sales price in Cyrus original model (will be used as to penalise backlog)
         beta(p)         ramp-up time
         tau(p)          perfusion culture duration [days]
         taut(p)         perfusion culture duration [time periods]
         tFirstBatch(p)  duration of first batch of a fed-batch process
                         /Mab 11/
         TminU(p)
         TmaxU(p)        maximum production time available within time period for USP
         TminD(p)
         TmaxD(p)        maximum production time available within time period for DSP
         epsilon(t)      discount factor
         epsilonY(y)      discount factor in years
         zetaU(p)        USP product shelf-life
         zetaD(p)        DSP product shelf-life
         rU(i,p)         USP batch rate [batches a day]
         rD(i,p)         DSP batch rate [batches a day]
         InfR            inflation rate
         IntR            interest rate
         fcostU(i)       fixed USP cost
         fcostD(i)       fixed DSP cost
         zeta2(i)        time taken to build a facility
         retrofitTime    time it takes to retrofit a facility in months
         IUmin(p)        strategic USP inventory level
         IDmin(p)        strategic DSP inventory level
         Ipenalty(p)     penlaty for not meeting the strategic inventory levels
         D(p,t)          demand
         tlag(p)         time lag in time periods for QCQA
                         /KG-PF   1
                          KG-N    1
                          rFVII   0/
         firstDemand(p);

TABLE    qc(i,j)        cost to transport intermediate material from facility i to j
                 BRKL-60A BRKL-60B  CMO     Future
BRKL-60A         0        0.005     0.1     0.01
BRKL-60B         0.005    0         0.1     0.01
CMO              0.1      0.1       0       0.1
Future           0.01     0.01      0.1     0;

TABLE    etaBias(i,p)    bias factor: multiple of manufacturing cost
                 KG-PF    KG-N    rFVII   Mab
BRKL-60A         1.1      1.1     1       1
BRKL-60B         1        1       1       1
CMO              1        1       1       1
Future           1        1       1       1;

****************************
*   INPUT DATA FROM EXCEL  *
****************************

$Call 'Gdxxrw input_data.xlsx output=input.gdx skipempty=0 trace=2 index=Index!A3:E39'
$gdxin input.gdx
$Load etaU etaD xU xD xlD alpha rho C lambdaU lambdaD kappa beta tau TminU TmaxU TminD TmaxD rU rD D zetaU zetaD InfR IntR zeta2 retrofitTime fcostU fcostD IUmin IDmin lambda upsilon
$gdxin

*        The following loop sets up the firstDemand parameter, which keeps the value of the time period
*        when each product is first due
         SCALAR flag;
         LOOP(p,
                 flag = 0;
                 LOOP(t,
                         IF(D(p,t) > 0 and flag = 0,
                                 firstDemand(p) = ORD(t);
                                 flag = 1;
                                 );
                         );
                 );

*        Create discount factors epsilon(t) and epsilon(y)
         epsilon(t) = ((1 + InfR / 12) / (1 + IntR / 12)) ** (ORD(t) - 1);
         epsilonY(y) = ((1 + InfR) / (1 + IntR)) ** (ORD(y) - 1);

*        costs of carrying USP and DSP inventory are calculated from the production costs
         rho2(i,p) = 0.012 * etaU(i,p) / xU(i,p);
         rho3(i,p) = 0.012 * etaD(i,p) / xD(i,p);

*        set up taut(p) parameter
         taut(p) = ceil(tau(p) / H);

*        penalty parameter is taken from the Excel file inputs
         delta(p) = upsilon(p) * 2;

*        set up Ipenalty parameter
         Ipenalty(p) = etaU('BRKL-60A',p) / xU('BRKL-60A',p) * 2;

DISPLAY  epsilon, rho2, rho3, taut, Ipenalty, firstDemand;

BINARY VARIABLES
         YU(i,p,t)       1 if product is produced in USP over period t
         YD(j,p,t)       1 if product is produced in DSP over period t
         F(i,p,t)        1 if there is a new perfusion culture;

INTEGER VARIABLES
         BU(i,p,t)       number of USP batches produced in facility i over time t
         BD(j,p,t)       number of DSP batches produced in facility i over time t;

POSITIVE VARIABLES
         IU(i,p,t)       USP inventory level amount of intermediate product p stored over period t
         ID(i,p,t)       DSP inventory level
         TU(i,p,t)       production time used in USP
         TD(i,p,t)       production time used in DSP
         S(i,p,t)        amount of product p which is sold in period t at facility i
         W(i,p,t)        amount of product p which is wasted in period t at facility i
         Q(i,j,p,t)      flow of material from i (USP) to j (DSP)
         IUunder(p,t)    amount of USP material under the strategic target
         IDunder(p,t)    amount of DSP material under the strategic target
         DeltaVar(p,t)   demand not met
         CFU(i,y)        fixed cost for the year (USP)
         CFD(i,y)        fixed cost for the year (DSP);

BINARY VARIABLES
         K(i,t)          1 if facility is invested in period t
         LU(i,p,t)       1 if retrofitting for product p in facility i is carried out
         LD(i,p,t)       1 if retrofitting for product p in facility i is carried out
         YY(i,p,t)       1 if a new campaign is started
         AU(i,p,t)       1 if USP is available
         AD(i,p,t)       1 if DSP is available
         AF(i,t)         1 if facility i has been built
         ARU(i,p,t)      1 if retrofitting is complete
         ARD(i,p,t)
         ZU(i,p0,p,t)    1 if USP changeover occurs from pp -> p in facility i over period t
         ZD(i,p0,p,t)    1 if DSP changeover occurs from pp -> p in facility i over period t
         L(i,p,t)        1 if facility has been used for the product (used for licence fee)
         YyearU(i,p,y)   1 if p was produced in i in year y (USP)
         YyearD(i,p,y)   1 if p was produced in i in year y (DSP)
         fU(i)           1 if facility has been used
         fD(i)           1 if facility has been used;

VARIABLES
*        variables that make up the objective function
         IC              inventory cost
         IPC             inventory penalty cost
         VC              variable cost
         FC              fixed cost
         TC              transport cost
         WC              waste cost
         BPC             backlog penalty cost
         FI              investment cost
         RC              retrofitting cost
         LC              licence cost
*        objective
         Total           total costs (objective function);

EQUATIONS
         uspFedBatch(i,p,t)
         uspPerfusion(i,p,t)
         uspY(i,t)
         dspY(i,t)
         campaignY(i,p,t)
         dspFedBatch(i,p,t)
         dspPerfusion(i,p,t)
*        Constraints for checking the availability of the facility
         uspAvailability(i,p,t)
         dspAvailability(i,p,t)
         uspFacilityBuilt(i,p,t) constraint to check if the facility has been built
         dspFacilityBuilt(i,p,t)
         uspRetrofitted(i,p,t)   constraint to check if the USP suite has been retrofitted
         dspRetrofitted(i,p,t)   constraint to check if the DSP suite has been retrofitted
*        Investment constraints
         investment(i,t)         capital investment constraint
         uspRetrofit(i,p,t)      investment constraint for USP suite retrofitting
         dspRetrofit(i,p,t)      investment constraint for DSP suite retrofitting
*        Constraints for license fees
*        Technically speaking, there is no differentiation between USP and DSP
         uspLicence(i,p,t)
         dspLicence(i,p,t)
*        Fixed Cost constraints
         uspFixed(i,p,t)
         dspFixed(i,p,t)
*        Timing Constraints
         uspTime1(i,p,t)
         uspTime2(i,p,t)
         dspTime(i,p,t)
         uspChangeover1(i,p0,p,t)
         uspChangeover2(i,p0,p,t)
         dspChangeover1(i,p0,p,t)
*         dspChangeover2(i,p0,p,t)
         uspChangeover3(i,t)
         dspChangeover3(i,t)
*        Perfusion Constraints
         perfusion1(i,p,t)       new campaign constraint
         perfusion2(i,p,t)       ensures that a new perfusion culture is started once the previous one has finished
         perfusion3(i,p,t)       ensures that the perfusion campaign is run for its entire length and that each day in the month is used
         perfusion4(i,p,t)       prevents the perfusion campaigns being started near the end of the planning horizon
*        Inventory Constraints
         uspInventory(i,p,t)
         materialFlow(j,p,t)
         dspInventory(i,p,t)
         uspStrategic(p,t)
         dspStrategic(p,t)
*        Utilisation Constraints
         uspUtil(i,y)            maximum utilisation constraint for in-house facilities
         dspUtil(i,y)
*        Shelf-Life Constraints
         uspLife(i,p,t)
         dspLife(i,p,t)
*        Sales
         sales(p,t)
*        OBJECTIVE FUNCTIONS
         minCost
         inventory
         invPenalty
         varCost
         fixed
         transport
         waste
         backlog
         investmentOb
         retrofitting
         licence;

****************************
*   OBJECTIVE FUNCTIONS    *
****************************

inventory..              IC =E= SUM((i,p,t)$(IP(i,p)),
                         epsilon(t) * (rho(i,p) * (IU(i,p,t) + ID(i,p,t))
                         + rho2(i,p) * IU(i,p,t) + rho3(i,p) * ID(i,p,t)));

invPenalty..             IPC =E= SUM((p,t)$(ORD(t) >= firstDemand(p)),
                         epsilon(t) * (Ipenalty(p) * (IUunder(p,t) + IDunder(p,t))));

varCost..                VC =E= SUM((i,p,t)$(IP(i,p)),
                         epsilon(t) * (etaBias(i,p) * (etaU(i,p) * BU(i,p,t) + etaD(i,p) * BD(i,p,t))));

fixed..                  FC =E= SUM((i,y)$(IO(i)),
                         epsilonY(y) * (fcostU(i) * fU(i) + fcostD(i) * fD(i)));

transport..              TC =E= SUM((i,j,p,t)$(IP(i,p) and IP(j,p)),
                         epsilon(t) * (qc(i,j) * Q(i,j,p,t)));

waste..                  WC =E= SUM((i,p,t)$(IP(i,p)),
                         epsilon(t) * (wcost * W(i,p,t)));

backlog..                BPC =E= SUM((p,t)$(ORD(t) >= firstDemand(p)),
                         epsilon(t) * (delta(p) * DeltaVar(p,t)));

investmentOb..           FI =E= SUM((i,t),
                         epsilon(t) * (kappa(i) * K(i,t)));

retrofitting..           RC =E= SUM((i,t,p)$(IP(i,p)),
                         epsilon(t) * (lambdaU(i,p) * LU(i,p,t) + lambdaD(i,p) * LD(i,p,t)));

licence..                LC =E= SUM((i,t,p)$(IP(i,p)),
                         epsilon(t) * (lambda(i,p) * L(i,p,t)));

minCost..                Total =E= IC + IPC + VC + FC + TC + WC + BPC + FI + RC + LC;

****************************
*        CONSTRAINTS       *
****************************

*USP Fed-Batch production. $(not PP(p)) excludes the perfusion products from the equation
uspFedBatch(i,p,t)$(IP(i,p) and not PP(p))..                     BU(i,p,t) =E= YY(i,p,t) + rU(i,p)
                                                                 * (TU(i,p,t) - tFirstBatch(p) * YY(i,p,t)
                                                                 - SUM(p0$(IP(i,p0) and ORD(p0) <> ORD(p)), alpha(p0,p) * ZU(i,p0,p,t)));

*USP Perfusion production. $(PP(p)) includes the perfusion products
uspPerfusion(i,p,t)$(IP(i,p) and PP(p))..                        BU(i,p,t) =E= rU(i,p)
                                                                 * (TU(i,p,t) - beta(p) * F(i,p,t)
                                                                 - SUM(p0$(IP(i,p0) and ORD(p0) <> ORD(p)), alpha(p0,p) * ZU(i,p0,p,t)));

*The following equations ensure that only one p is manufactured at any given t
uspY(i,t)..                                                      SUM(p, YU(i,p,t)) =L= 1;
dspY(i,t)..                                                      SUM(p, YU(i,p,t)) =L= 1;

*YY indicates new upstream campaigns. New campaign can only occur if there
*was no production of that product in the previous time period
campaignY(i,p,t)$(IP(i,p))..                                     YY(i,p,t) =G= YU(i,p,t) - YU(i,p,t-1);

*DSP Fed-Batch production
dspFedBatch(i,p,t)$(IP(i,p) and not PP(p))..                     BD(i,p,t) =E= BU(i,p,t);

*DSP Perfusion production
dspPerfusion(i,p,t)$(IP(i,p) and PP(p))..                        BD(i,p,t) =E= rD(i,p)*TD(i,p,t);

*Availability Constraints (linked with the Investment constraints):
*in order for production to take place in a facility,it must
*be first be available for use: built or retrofitted
uspAvailability(i,p,t)$(IP(i,p))..                               YU(i,p,t) =L= AU(i,p,t);
dspAvailability(i,p,t)$(IP(i,p))..                               YD(i,p,t) =L= AD(i,p,t);
uspFacilityBuilt(i,p,t)$(IP(i,p))..                              AU(i,p,t) =L= AF(i,t);
dspFacilityBuilt(i,p,t)$(IP(i,p))..                              AD(i,p,t) =L= AF(i,t);
uspRetrofitted(i,p,t)$(IP(i,p))..                                AU(i,p,t) =L= ARU(i,p,t);
dspRetrofitted(i,p,t)$(IP(i,p))..                                AD(i,p,t) =L= ARD(i,p,t);

*Investment Constraints: before a facility can be used, there must first be investment into the construction
investment(i,t)$(ORD(t) > zeta2(i))..                            AF(i,t) =L= AF(i,t-1) + K(i,t-zeta2(i));
uspRetrofit(i,p,t)$(IP(i,p) and lambdaU(i,p)>0 and ORD(t)>6)..
                                                                 ARU(i,p,t) =L= ARU(i,p,t-1) + LU(i,p,t-retrofitTime);
dspRetrofit(i,p,t)$(IP(i,p) and lambdaD(i,p)>0 and ORD(t)>6)..
                                                                 ARD(i,p,t) =L= ARD(i,p,t-1) + LD(i,p,t-retrofitTime);
uspLicence(i,p,t)$(IP(i,p))..                                    AU(i,p,t) =L= AU(i,p,t-1) + L(i,p,t);
dspLicence(i,p,t)$(IP(i,p))..                                    AD(i,p,t) =L= AD(i,p,t-1) + L(i,p,t);

*Fixed Cost Constraints (for the owned facilities only, i.e. IO(i))
uspFixed(i,p,t)$(IO(i) and IP(i,p))..                            fU(i) =G= YU(i,p,t);
dspFixed(i,p,t)$(IO(i) and IP(i,p))..                            fD(i) =G= YD(i,p,t);

*Timing Constraints
uspTime1(i,p,t)$(IP(i,p))..                                      TU(i,p,t) =L= TmaxU(p) * YU(i,p,t);
uspTime2(i,p,t)$(IP(i,p))..                                      TU(i,p,t) =G= TminU(p) * YU(i,p,t);
dspTime(i,p,t)$(IP(i,p))..                                       TD(i,p,t) =L= TmaxD(p) * YD(i,p,t)
                                                                 - SUM(p0$(IP(i,p0) and ORD(p0) <> ORD(p)), alpha(p0,p) * ZD(i,p0,p,t));
*Changeover Constraints
uspChangeover1(i,p0,p,t)$(IP(i,p) and IP(i,p0))..
                                                                 ZU(i,p0,p,t) =G= YU(i,p,t) + YU(i,p0,t-1)$(ORD(p0) <> ORD(p)) - 1;
uspChangeover2(i,p0,p,t)$(IP(i,p) and IP(i,p0))..
                                                                 ZU(i,p0,p,t) =L= 1 - YU(i,p,t) + YU(i,p0,t-1)$(ORD(p0) <> ORD(p));
dspChangeover1(i,p0,p,t)$(IP(i,p) and IP(i,p0))..
                                                                 ZD(i,p0,p,t) =G= YD(i,p,t) + YD(i,p0,t-1)$(ORD(p0) <> ORD(p)) - 1;
*dspChangeover2(i,p0,p,t)$(IP(i,p) and IP(i,p0))..
*                                                                 ZD(i,p0,p,t) =G= 1 - YD(i,p,t) + YD(i,p0,t-1)$(ORD(p0) <> ORD(p));

uspChangeover3(i,t)..                                            SUM((p0,p), ZU(i,p0,p,t)) =L= 1;
dspChangeover3(i,t)..                                            SUM((p0,p), ZD(i,p0,p,t)) =L= 1;

*Perfusion Constraints
perfusion1(i,p,t)$(IP(i,p) and PP(p))..                          F(i,p,t) =G= YU(i,p,t) - YU(i,p,t-1);
perfusion2(i,p,t)$(IP(i,p) and PP(p))..                          F(i,p,t) =G= YU(i,p,t) + F(i,p,t-taut(p)) - 1;

perfusion3(i,p,t)$(IP(i,p) and PP(p))..                          tau(p) * F(i,p,t) =L= SUM(theta$(ORD(theta) >= ORD(t) and ORD(theta) < (ORD(t) + taut(p))),
                                                                 TU(i,p,theta));

perfusion4(i,p,t)$(PP(p) and ORD(t) > (card(t) - taut(p) + 1)).. F(i,p,t) =E= 0;

*Inventory Constraints
uspInventory(i,p,t)$(IP(i,p))..                                  IU(i,p,t) =E= xU(i,p) * (1 - R) * BU(i,p,t-tlag(p))
                                                                 + IU(i,p,t-1)
                                                                 - SUM(j$(IP(j,p)), Q(i,j,p,t));

materialFlow(j,p,t)$(IP(j,p))..                                  SUM(i$(IP(i,p)), Q(i,j,p,t)) =E= xlD(p) * BD(j,p,t);
dspInventory(i,p,t)$(IP(i,p))..                                  ID(i,p,t) =E= xD(i,p) * BD(i,p,t) + ID(i,p,t-1) - S(i,p,t) - W(i,p,t);
uspStrategic(p,t)$(ORD(t) > 0)..                                 SUM(i$(IP(i,p)), IU(i,p,t)) =G= IUmin(p) - IUunder(p,t);
dspStrategic(p,t)$(ORD(t) > 0)..                                 SUM(i$(IP(i,p)), ID(i,p,t)) =G= IDmin(p) - IDunder(p,t);

*Utilisation Constraints
uspUtil(i,y)$(IO(i))..                                           SUM((p,t)$(TY(y,t) and IT(i,t) and IP(i,p)), TU(i,p,t)) =L= tCap;
dspUtil(i,y)$(IO(i))..                                           SUM((p,t)$(TY(y,t) and IT(i,t) and IP(i,p)), TD(i,p,t)) =L= tCap;

*Shelf-Life Constraints
uspLife(i,p,t)$(IP(i,p) and ORD(t) < (card(t) - zetaU(p)))..
                                                                 IU(i,p,t) =L= SUM((theta,j)$(ORD(theta) > ORD(t) and ORD(theta) <= (ORD(t) + zetaU(p))),
                                                                 Q(i,j,p,theta));

dspLife(i,p,t)$(IP(i,p) and ORD(t) < (card(t) - zetaD(p)))..
                                                                 ID(i,p,t) =L= SUM(theta$(ORD(theta) > ORD(t) and ORD(theta) <= (ORD(t) + zetaD(p))),
                                                                 S(i,p,theta));

*Sales
sales(p,t)..                                                     SUM(i$(IP(i,p)), S(i,p,t)) =E= D(p,t) - DeltaVar(p,t) + DeltaVar(p,t-1);

*Disallow transfer of material from CMO to in-house (and vice-versa)
Q.fx("CMO",j,p,t)$(ORD(j) <> 3) = 0;
Q.fx(i,"CMO",p,t)$(ORD(i) <> 3) = 0;

*******************************************************************
AF.fx(i,t)$(ORD(t) <= zeta2(i)) = 0;
ARU.fx(i,p,t)$(ORD(t) <= 6 and lambdaU(i,p) > 0) = 0;
ARD.fx(i,p,t)$(ORD(t) <= 6 and lambdaD(i,p) > 0) = 0;
IU.up(i,p,t)$(not IP(i,p)) = inf;
IU.fx(i,p,t)$(not IP(i,p)) = 0;
BU.lo(i,p,t) = 0;
BD.lo(i,p,t) = 0;
BU.up(i,p,t) = 30;
BD.up(i,p,t) = 30;
S.up(i,p,t)=inf;
S.fx(i,p,t)$(not D(p,t)) = 0;
AU.fx(i,p,t)$(not IT(i,t)) = no;
AD.fx(i,p,t)$(not IT(i,t)) = no;

MODEL                    siganporia2013 /All/;

                         option threads = 4;
                         option optcr = 0.14;
                         option reslim = 2000;

SOLVE                    siganporia2013 USING MIP MINIMIZING Total;

SCALARS optimality, time;

         time = siganporia2013.etsolve;
         optimality = abs(siganporia2013.objval - siganporia2013.objest) / (1E-10 + abs(siganporia2013.objval));

DISPLAY Total.l, IC.l, IPC.l, VC.l, FC.l, TC.l, WC.l, BPC.l, FI.l, RC.l, LC.l, time, optimality;

