// Generated with CoInSyDe : Code Synthesizer for System Design //

// Included libraries
#include "stdio.h"
#include "./bool2int.c"

// Custom types
typedef enum { false=0, true=1 } bool_t;

// State variables
int st0;

// Function declarations
int mainProcess_moore (int i1);

// Function definitions
int mainProcess_moore (int i1) {
    int o1;
    st0=st0+i1;
    o1=st0;
    return o1;
}

// Main function
int main(int argc, char ** argv) {
    st0 = 0;
    int fromBool;
    bool_t i;
    int o;
    while (1) {
        scanf("%d", &i);
        fromBool = bool2int (i);
        o = mainProcess_moore (fromBool);
        printf("%d",o);
    }
}
