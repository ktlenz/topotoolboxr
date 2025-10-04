#include <stddef.h>
#include <stdint.h>
#include <R.h>

#include "topotoolbox.h"
#include "topotoolboxr.h"

void wrap_flow_accumulation_edgelist(
    float *accR, // output
    int   *sourceR, // ptrdiff_t
    int   *targetR, // ptrdiff_t
    float *fractionR,
    float *weightsR,
    int   *edge_countR, // ptrdiff_t
    int   *dimsR){ // ptrdiff_t
        // Transformation of integers and array allocation
        ptrdiff_t dims [2]= {dimsR[0], dimsR[1]};
        ptrdiff_t edge_count = (ptrdiff_t)edge_countR[0];
        ptrdiff_t *source = R_Calloc(edge_count, ptrdiff_t);
        ptrdiff_t *target = R_Calloc(edge_count, ptrdiff_t);

        // Convert sourceR and targetR to ptrdiff_t
        for (ptrdiff_t idx = 0; idx < edge_count; idx++) {
            source[idx] = (ptrdiff_t)sourceR[idx];
            target[idx] = (ptrdiff_t)targetR[idx];
        }

        // Flow accumulation computation using libtopotoolbox
        flow_accumulation_edgelist(accR, source, target, fractionR, weightsR, edge_count, dims);

        // Free memory
        R_Free(source);
        R_Free(target);
    }