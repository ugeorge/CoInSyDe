// Generated with CoInSyDe : Code Synthesizer for System Design //

// Included libraries
#include "stdio.h"

// Custom types


// State variables


// Function declarations
int mulacc (int in1, int in2, int acc);

// Function definitions
int mulacc (int in1, int in2, int acc) {
  return acc + in1 * in2;
}

// Main function
int main(int argc, char ** argv) {
  int input[10];
  int output[10];
  while (1) {
    {int __io_it;
      for (__io_it = 0; __io_it < 10; __io_it++){
        scanf("%d", input[__io_it]);
      }
    }
    unsigned _it;
    int COEF[5] = {1,2,4,3,2};
    unsigned _range;
    for (_it = 0; _it < min(10) ; _it++){
      _range = min(10) - _it;
        unsigned _it0;
      int _acc = 0;
       for (_it0 = 0; _it0 < min(10,5) ; _it0++){
          output = mulacc(input, COEF, _acc);
      }
    }
    {int __io_it;
      for (__io_it = 0; __io_it < 10; __io_it++){
        printf("%d", output[__io_it]);
      }
    }
  }
  
  return 0;
  
}
