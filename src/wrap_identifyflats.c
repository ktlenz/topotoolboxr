#include <stddef.h>
#include <stdint.h>

#include "topotoolbox.h"

#include "topotoolboxr.h"//    This is a working version to run identifyflats
//

void wrap_identifyflats(int  *outputR, float *demR, int *dimsR){ //might be usefull to not ask for a ptrdiff_t in the first place
                                 //and let the user input a long array

   ptrdiff_t dims [2]= {dimsR[0], dimsR[1]};

   identifyflats(outputR, demR, dims);
}