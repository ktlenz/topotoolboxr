#include <stddef.h>
#include <stdint.h>

#include <R.h>

#include "topotoolbox.h"
#include "topotoolboxr.h"

void wrap_gwdt_computecosts(float *costsR, int *conncompsR, int *flatsR,
                            float *original_demR, float *filled_demR, int *dimsR){
  
  ptrdiff_t dims [2]= {dimsR[0], dimsR[1]};
  // Allocate memory for conncomps
  ptrdiff_t *conncomps = R_Calloc(dims[0] * dims[1], ptrdiff_t);
  
  // Fill conncomps from the int-valued conncompsR array
  for (ptrdiff_t j = 0; j < dims[1]; j++) {
    for (ptrdiff_t i = 0; i < dims[0]; i++) {
      conncomps[j * dims[0] + i] = conncompsR[j * dims[0] + i];
    }
  }
  
  gwdt_computecosts(costsR, conncomps,
                    flatsR, original_demR, filled_demR,
                    dims);
  
  // Free the allocated memory
  R_Free(conncomps);
}
