// Generated with CoInSyDe : Code Synthesizer for System Design //

// Included libraries
#include "stdio.h"
#include "./bool2int.c"

// Custom types
typedef enum { false=0, true=1 } bool_t;

// State variables
int mainProcess_moore_st;

// Function declarations
int mainProcess_moore (int i1);

// Function definitions
int mainProcess_moore (int i1) {
    int o1;
    mainProcess_moore_st =mainProcess_moore_st +i1;
    o1 =mainProcess_moore_st;
    
    
    return o1;
}

// Main function
int main(int argc, char ** argv) {
    mainProcess_moore_st = 0;
    int fromBool;
    bool_t i;
    int o;
    while (1) {
        scanf("%d", &i);
        fromBool = bool2int (i);
        o = mainProcess_moore (fromBool);printf("%d",o);
        
    }
}
