#ifndef __SAMPLE_DEF_H
#define __SAMPLE_DEF_H

#ifdef __INCUDINE_USE_64_BIT_SAMPLE__
#define SAMPLE  double
#else
#define SAMPLE  float
#endif

#endif  /* __SAMPLE_DEF_H */
