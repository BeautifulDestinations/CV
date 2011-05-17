//@@language c
#include <opencv/cv.h>
#include <opencv/cxcore.h>
#include <opencv/highgui.h>
#include <complex.h>

struct haralick_values {
  // Angular second moments
  double asm_0;
  double asm_45;
  double asm_90;
  double asm_135;
  // Contrasts
  double contrast_0;
  double contrast_45;
  double contrast_90;
  double contrast_135;
  // Correlations
  double correlation_0;
  double correlation_45;
  double correlation_90;
  double correlation_135;
  // Entropies
  double entropy_0;
  double entropy_45;
  double entropy_90;
  double entropy_135;
};

/* 
 * Calculates texture features for given image.
 *
 * @param  image IplImage to be analyzed 
 * @return haralick_values struct containing calculated texture features
 */
struct haralick_values *calculate_values(IplImage *image);

int get_color(IplImage *image, int x, int y);

