// Generated with CoinSyDe : Code Synthesizer for System Design //

/*** Your Includes Go Here ***/

typedef enum { true = 1, false = 0 } bool_t;

int mainProcess_moore_st;

int mainProcess (bool_t i);
int mainProcess_moore (int i1);

int mainProcess (bool_t i) {
    int o;
    int fromBool;
    fromBool = ( i ==  true ) ? ( int ) 1 : ( int ) 0;
                
    o = mainProcess_moore (fromBool);
    return o;
}
int mainProcess_moore (int i1) {
    int o1;
    mainProcess_moore_st =  mainProcess_moore_st +  i1 ;
            
    o1 =  mainProcess_moore_st ;
            
    return o1;
}

int main(int argc, char ** argv) {
    mainProcess_moore_st = 0;
    int o;
    bool_t i;
    
    /***  Here You Write Your Testbench ***/
    
    o = mainProcess (i);
}