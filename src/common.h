#ifndef __SAMPLE_DEF_H
#define __SAMPLE_DEF_H

#ifdef __INCUDINE_USE_64_BIT_SAMPLE__
#define SAMPLE  double
#else
#define SAMPLE  float
#endif

#ifdef __INCUDINE_SCHED_FIFO__
#define INCUDINE_SCHED_POLICY  SCHED_FIFO
#else
#define INCUDINE_SCHED_POLICY  SCHED_RR
#endif

#define FALSE  0
#define TRUE   1

#endif  /* __SAMPLE_DEF_H */
