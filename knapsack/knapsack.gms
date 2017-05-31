SETS     i       objects
                 /i1*i20/;

PARAMETERS
         weight(i)
         utility(i);

$Call 'Gdxxrw input_data.xlsx output=input.gdx skipempty=0 trace=2 index=Index!A1:E6'
$gdxin input.gdx
$Load weight utility
$gdxin

SCALARS
         maxWeight /10/
         maxItems /2/;

VARIABLES
         totalUtility
         totalWeight
         totalItems;

BINARY VARIABLES
         objects(i);

EQUATIONS
         totalU
         totalW
         totalI
         weightConstraint
         itemsConstraint;

totalU..                 totalUtility =E= SUM(i,utility(i)*objects(i));
totalW..                 totalWeight  =E= SUM(i,weight(i)*objects(i));
totalI..                 totalItems   =E= SUM(i,objects(i));
weightConstraint..       totalWeight  =L= maxWeight;
itemsConstraint..        totalItems   =L= maxItems;

MODEL            KnapsackProb    /all/;

                 option optcr    = 0.0;

SOLVE            KnapsackProb    USING MIP MAXIMISING    totalUtility;
