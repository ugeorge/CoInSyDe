// Generated with CoinSyDe : Code Synthesizer for System Design //

/*** Your Includes Go Here ***/

// Custom Types
typedef enum { true = 1, false = 0 } bool_t;

// State variables
int mainProcess_moore_st;

// Native C function header files


// Function declarations
int bool2int (bool_t arg);
int mainProcess (bool_t i);
int mainProcess_moore (int i1);

// Function definitions
#include "./int2bool.c";
int mainProcess (bool_t i) {
    int o;
    int fromBool;
    fromBool = bool2int (i);
    o = mainProcess_moore (fromBool);
    return o;
}
int mainProcess_moore (int i1) {
    int o1;
    mainProcess_moore_st =  mainProcess_moore_st +  i1 ;
            
    o1 =  mainProcess_moore_st ;
            
    return o1;
}

// Main Function
int main(int argc, char ** argv) {
    mainProcess_moore_st = 0;
    int o;
    bool_t i;
    
    /***  Here You Write Your Testbench ***/
    
    o = mainProcess (i);
}

