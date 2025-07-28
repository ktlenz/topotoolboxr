#include <stddef.h>
#include <stdint.h>
#include <R.h>

#include "topotoolbox.h"
#include "topotoolboxr.h"

void wrap_gwdt(float *distR,
               float *costsR,
               int *flatsR,
               int *dimsR){
  
  // Transform integers (R) to ptrdiff_t
  ptrdiff_t dims [2]= {dimsR[0], dimsR[1]};
  ptrdiff_t *prev = R_Calloc(dims[0] * dims[1], ptrdiff_t);
  ptrdiff_t *heap = R_Calloc(dims[0] * dims[1], ptrdiff_t);
  ptrdiff_t *back = R_Calloc(dims[0] * dims[1], ptrdiff_t);
  
  // Cost computation using libtopotoolbox
  gwdt(distR, prev, costsR, flatsR, heap, back, dims);
  
  // Free memory
  R_Free(prev);
  R_Free(heap);
  R_Free(back);
}