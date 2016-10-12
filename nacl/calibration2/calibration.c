#define _GNU_SOURCE
#include <sched.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/mman.h>

#ifndef HIDEMINMAX
#define MAX(X,Y) (((X) > (Y)) ? (X) : (Y))
#define MIN(X,Y) (((X) < (Y)) ? (X) : (Y))
#endif

#define maccess(X) (*(volatile size_t*)X)

#define HIST_LEN (250)
#define TRIES (10*1024)
#define NB_PREFETCH (10)

char* array;

inline __attribute__((always_inline)) uint64_t rdtsc_begin() 
{
  uint64_t a, d;
  asm volatile ("mfence\n\t"
    "rdtsc\n\t" // rdtscp not supported by NaCl, revert to rdtsc
    "mov %%rdx, %0\n\t"
    "mov %%rax, %1\n\t"
    "xor %%rax, %%rax\n\t"
    "cpuid\n\t"
    : "=r" (d), "=r" (a)
    :
    : "%rax", "%rbx", "%rcx", "%rdx");
  a = (d<<32) | a;
  return a;
}

inline __attribute__((always_inline)) uint64_t rdtsc_end() 
{
  uint64_t a, d;
  asm volatile(
    "xor %%rax, %%rax\n\t"
    "cpuid\n\t"
    "rdtsc\n\t" // rdtscp not supported by NaCl, revert to rdtsc
    "mov %%rdx, %0\n\t"
    "mov %%rax, %1\n\t"
    "mfence\n\t"
    : "=r" (d), "=r" (a)
    :
    : "%rax", "%rbx", "%rcx", "%rdx");
  a = (d<<32) | a;
  return a;
}

inline __attribute__((always_inline)) void evict_all()
{
  size_t evict_size = 1024*1024;
  size_t array[evict_size];
  int i;
  for(i=0; i<evict_size; i++)
  {
    array[i] = 0;
  }
}


int main()
{
/*
  volatile uint32_t reg[4];
  __asm__ __volatile__(
      "cpuid"
      : "=a"(reg[3]), "=b"(reg[0]), "=c"(reg[2]), "=d"(reg[1])
      : "a"(0)
      : "cc");
  printf("%s\n", (char *)reg);
 */
 


  //memset(array,1,5*1024*sizeof(char));
  
  array = mmap(NULL, 1024*1024*1024, PROT_READ | PROT_WRITE, MAP_POPULATE | MAP_ANONYMOUS | MAP_PRIVATE, -1, 0);
  memset(array,-1,4*1024*1024);
  char* p = (void*)(((size_t)array+2*1024*1024) >> 21 << 21);
  
  size_t hit_histogram[HIST_LEN];
  size_t miss_histogram[HIST_LEN];
  size_t prefetch_histogram[HIST_LEN];
  memset(hit_histogram,0,HIST_LEN*sizeof(size_t));
  memset(miss_histogram,0,HIST_LEN*sizeof(size_t));
  memset(prefetch_histogram,0,HIST_LEN*sizeof(size_t));
  int i,j;
  
  char* line;
  char* end;
  size_t n;
  printf("what is the address of %p?\n",p);
  fflush(stdout);
  getline(&line,&n,stdin);
  
  uint64_t phys = strtoull(line,&end,16);
  printf("%llx\n", phys);
  
  memset(array,-1,1024*1024*1024);
  /*
  // Histogram: maccess
  for(i=0; i<TRIES; i++)
  {
    size_t time = rdtsc_begin();
    maccess(array);
    size_t delta = rdtsc_end() - time;
    hit_histogram[MIN(HIST_LEN-1,delta)]++;
  }
  */
  
  // Histogram: maccess, evict
  for(i=0; i<TRIES; i++)
  {
    size_t time = rdtsc_begin();
    maccess(p);
    size_t delta = rdtsc_end() - time;
    miss_histogram[MIN(HIST_LEN-1,delta)]++;
    evict_all();
  }

  // Histogram: prefetch, maccess, evict
  for(i=0; i<TRIES; i++)
  {
    for(j=0; j<3; j++)
    {
      sched_yield();
      //asm volatile ("prefetchnta (%0)" : : "r" ((uint64_t)(uint32_t)array));
      //asm volatile ("prefetcht2 (%0)" : : "r" ((uint64_t)(uint32_t)array));
      __builtin_prefetch((void*)p, 0, 0); //prefetchnta 
      __builtin_prefetch((void*)p, 0, 1); //prefetcht2   
    }
    size_t time = rdtsc_begin();
    maccess(p);
    size_t delta = rdtsc_end() - time;
    prefetch_histogram[MIN(HIST_LEN-1,delta)]++;
    evict_all();
  }
  
  // Histogram: prefetch, maccess, evict
  for(i=0; i<TRIES; i++)
  {
    for(j=0; j<3; j++)
    {
      sched_yield();
      
      asm volatile ("prefetchnta (%%rax)" : : "a" (phys));
      //asm volatile ("prefetcht2 (%)" : : "a" (phys));
      //__builtin_prefetch(phys, 0, 0); //prefetchnta 
      //__builtin_prefetch(phys, 0, 1); //prefetcht2   
    }
    size_t time = rdtsc_begin();
    maccess(p);
    size_t delta = rdtsc_end() - time;
    hit_histogram[MIN(HIST_LEN-1,delta)]++;
    evict_all();
  }
  
  // Pretty print histograms
  for(i=150; i<HIST_LEN; i++)
  {
    printf("%3d: %10zu %10zu %10zu\n", i, hit_histogram[i], miss_histogram[i], prefetch_histogram[i]);
  }


  return 0;  
}
