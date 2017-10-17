SETS
         i               tasks

         i_st            storage

         s               states

         j               units
                         /j1*j4/
         n               events

         n_d             sub events for intermediate due dates
                         /n_d1*n_d6/;

ALIAS    (i, ii);
ALIAS    (s, ss);
ALIAS    (n, nn);
ALIAS    (n, nnn);

PARAMETERS
         R_j_s           processing rate for state s in unit j
         T_min_s         min processing duration for state s
         lambda_ss_s     conversion from state ss to s at a given stage
         M_ss_s          binary param to map the formation of state s after processing of state ss at a given stage
         tau_j_ss_s      sequence dependent changeover time from processing of state ss to state in unit j

         theta_s         selling price for state s

         rho_s           storage cost for state s
         delta_s         backlog_cost for state s
         eta_s           manufacturing cost for state s
         sigma_s         shelf life of state s
         phi_s           waste disposal cost for state s
         mu_s            changeover cost for state s
         mu_ss           changeover cost from state s to state ss

         Cap_max_s       maximum storage capacity for state s
         H               horizon;

BINARY VARIABLES
         w(i, s, n)      assign the beginning of a task i that processes state s at event n
         y(i_st, s, n)   assign the beginning of storage task i_st that stores state s at event n
         z(i_st, s, n)   if there is nonzero amount stored for state s by storage task i_st at event n
         vt(i_st, s, n)  if amount stored for state s by storage task i_st at event n is wasted

VARIABLES
        b(i, s, n)       amount of state s processed by task i at event n
        T_s(i, s, n)     start time of processing state s by task i at event n
        T_f(i, s, n)     finish time of processing state s by task i at event n
        S(s, n)          amount of state s delivered at event n
        ST(i_st, s, n)   amount of state s stored by storage task i_st at event n
        ST_0(s, n)       amount of raw material state s as and when required from external sources at event n
        TT_s(i_st, s, n) start time of storage of state s by storage task i_st at event n
        TT_f(i_st, s, n) finish time of storage of state s by storage task i_st at even n
        sdur(i_st, s, n) storage duration of state s by storage task i_st at event n
        X(i_st, s, n)    amount of state s by storage task i_st that is wasted at event n
        P(s, n_d)        total unfulfilled demand amount for state s at the end of sub event n_d
        wc(i, ss, s, nn) [0 1] continuous variable for changeover from state ss processed by task i at event nn to state s at later event n
        Profit
        Obj;

EQUATIONS



