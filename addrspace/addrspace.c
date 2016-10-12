#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <sched.h>
#include <unistd.h>
#include <stdlib.h>
#include <fcntl.h>
#include "../cacheutils.h"

size_t array[5*1024];

size_t hit_histogram[4000];
size_t miss_histogram[4000];

int g_pagemap_fd = -1;

#define assert(X) do { if (!(X)) { fprintf(stderr,"assertion '" #X "' failed\n"); exit(-1); } } while (0)

void prefetch(void* p)
{
  asm volatile ("prefetchnta (%0)" : : "r" (p));
  asm volatile ("prefetcht2 (%0)" : : "r" (p));
}

void init_pagemap() {
  g_pagemap_fd = open("/proc/self/pagemap", O_RDONLY);
  assert(g_pagemap_fd >= 0);
}

size_t get_physical_addr(size_t virtual_addr_p) {
  prefetch((void*)virtual_addr_p);
  rdtsc_begin();
  uint64_t virtual_addr = (uint64_t)virtual_addr_p;
  size_t value;
  off_t offset = (virtual_addr / 4096) * sizeof(value);
  int got = pread(g_pagemap_fd, &value, sizeof(value), offset);
  return value;
}

size_t onlyreload(void* addr) // row hit
{
  size_t time = rdtsc_begin();
  prefetch(addr);
  size_t delta = rdtsc_end() - time;
  //maccess((void*)0x500000);
  return delta;
}

size_t flushandreload(void* addr) // row miss
{
  flush(addr);
  size_t time = rdtsc_begin();
  prefetch(addr);
  size_t delta = rdtsc_end() - time;
  return delta;
}

const char* SPACES[] = {"","--->","--- --->","--- --- --->"};
#define TRIES (1*128*1024)
void loopover(size_t start, size_t end, size_t step, size_t depth, size_t print)
{
  size_t addr = start;
  const char* spaces = SPACES[depth];
  do
  {
    sched_yield();
    memset(hit_histogram,0,4000*sizeof(size_t));
    memset(miss_histogram,0,4000*sizeof(size_t));
    for (int i = 0; i < TRIES; ++i)
    {
      size_t d = onlyreload((void*)addr);
      hit_histogram[MIN(3999,d)]++;
    }
    if ((get_physical_addr(addr) & (0x100ull << 55)))
    {
      for (int i = 0; i < TRIES; ++i)
      {
        size_t d = flushandreload((void*)addr);
        miss_histogram[MIN(3999,d)]++;
      }
    }
    size_t sum_hit = 0;
    size_t hit_max = 0;
    size_t hit_max_i = 0;
    size_t sum_miss = 0;
    size_t miss_max = 0;
    size_t miss_max_i = 0;
    for (int i = 0; i < 4000; ++i)
    {
      if (hit_max < hit_histogram[i])
      {
        hit_max = hit_histogram[i];
        hit_max_i = i;
      }
      if (miss_max < miss_histogram[i])
      {
        miss_max = miss_histogram[i];
        miss_max_i = i;
      }
      sum_hit += hit_histogram[i] * i;
      sum_miss += miss_histogram[i] * i;
    }
    sum_hit /= TRIES;
    sum_miss /= TRIES;
/*    if (depth >= 1 && hit_max_i == 250)
      return;
    if (depth >= 2 && hit_max_i == 229)
      return;
    if (depth >= 3 && hit_max_i == 217)
      return;
    if ((depth == 3 && hit_max_i != 202) || ((addr == 0 && step > (1ull << 21)) || (hit_max_i != 217 && hit_max_i != 220 && hit_max_i != 202)))*/
    {
      if (print)
        printf("%s%16zx: %4zu %4zu %1s%1s%1s%1s%1s (%16zx)\n",spaces,addr,sum_hit,sum_miss,(get_physical_addr(addr) & (0x100ull << 55)) ? "P" : "",(get_physical_addr(addr) & (0x40ull << 55)) ? "M" : "",(get_physical_addr(addr) & (0x2ull << 55)) ? "E" : "",(get_physical_addr(addr) & (0x1ull << 55)) ? "D" : "",((get_physical_addr(addr) & ((0x1ull << 55)-1ull))  == 0)?"Z":"",get_physical_addr(addr));
      if ((end == 0 || end >= addr+step) && step > 4096ULL)
        loopover(addr,addr+step,step >> 9,depth+1,print);
    }
    addr += step;
    if ((addr >> 47) > 0)
      addr |= 0xFFFF000000000000ull;
  }
  while (addr != end);
}

int main(int argc, char** argv)
{
  init_pagemap();
  loopover(0,0,(1ULL << 39),0,1);
  return 0;
}
