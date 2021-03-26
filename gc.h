#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#define GC_HEAP_SIZE (1 << 20)
#define WORD_BITS (8 * sizeof(u32))
#define FREE_MAP_SIZE (GC_HEAP_SIZE / sizeof(u32))
#define GC_HDR_SIZE sizeof(struct gc_header)
#define GC_CANARY 0x4e542be6 // just a random value

typedef unsigned char u8;
typedef uint32_t u32;
typedef uint64_t u64;

struct gc_header {
  u32 canary;
  u32 size;
};

struct garbage_collector {
  u32 *free_map;
  u8 *heap;
  u64 top_stack_ptr;
};

struct garbage_collector gc;

void gc_init(u8 *top_stack_ptr) {
  gc.top_stack_ptr = (u64)top_stack_ptr;
  gc.heap = calloc(GC_HEAP_SIZE, sizeof(u8));
  gc.free_map = calloc(FREE_MAP_SIZE, sizeof(u8));
}

int gc_byte_free(u32 i) {
  u32 bit = 1 << (i % WORD_BITS);
  return (gc.free_map[i / WORD_BITS] & bit) == 0;
}

void gc_mark_bytes(u32 start, u32 size) {
  for (int i = start; i < start + size; ++i)
    gc.free_map[i / WORD_BITS] |= 1 << (i % WORD_BITS);
}

u32 gc_find_first_fit(u32 nbytes) {
  for (u32 heap_offset = 0; heap_offset < GC_HEAP_SIZE - nbytes; ++heap_offset) {
    for (u32 offset = 0; offset < nbytes; ++offset) {
      if (gc_byte_free(heap_offset + offset))
        continue;
      heap_offset += offset; // cannot fit our block here, so jump forward
      goto mem_filled;
    }
    return heap_offset;
    mem_filled:;
  }
  printf("ERROR: GC out of memory.\n");
  exit(1);
}

void* gc_malloc(u32 bytes) {
  u32 nbytes = bytes + GC_HDR_SIZE;
  u32 heap_offset = gc_find_first_fit(nbytes);
  gc_mark_bytes(heap_offset, nbytes);

  // write gc header information
  struct gc_header *hdr = (struct gc_header*) (gc.heap + heap_offset);
  hdr->canary = GC_CANARY;
  hdr->size = nbytes;

  // clear the memory and return a ptr to it
  memset(hdr + 1, 0, bytes);
  return hdr + 1;
}

void gc_mark_range(u8 *ptr, u32 size);

void gc_mark_ptr(u8 *ptr) {
  if (ptr < (gc.heap + GC_HDR_SIZE) || gc.heap + GC_HEAP_SIZE < ptr)
    return; // not a gc heap ptr

  struct gc_header *hdr = (struct gc_header*) (ptr - GC_HDR_SIZE);
  if (hdr->canary != GC_CANARY)
    return; // value not a gc ptr, just looked like one

  u32 start_byte_offset = ptr - gc.heap - GC_HDR_SIZE;
  if (!gc_byte_free(start_byte_offset))
    return; // already marked, cyclical reference

  gc_mark_bytes(start_byte_offset, hdr->size);
  gc_mark_range((u8*)(hdr + 1), hdr->size - GC_HDR_SIZE);
}

void gc_mark_range(u8 *ptr, u32 size) {
  for (u32 i = 0; i + sizeof(u8*) <= size; ++i)
    gc_mark_ptr(*(u8**) (ptr + i));
}

// Pointers to gc-allocated objects can also be found in registers instead
// of on the stack. This is very common when compiling with optimizations.
void gc_mark_from_registers(void) {
  u64 registers[12];
  asm("mov %%rax, %0;" : "=m" (registers[0]));
  asm("mov %%rbx, %0;" : "=m" (registers[1]));
  asm("mov %%rcx, %0;" : "=m" (registers[2]));
  asm("mov %%rdx, %0;" : "=m" (registers[3]));
  asm("mov %%r8,  %0;" : "=m" (registers[4]));
  asm("mov %%r9,  %0;" : "=m" (registers[5]));
  asm("mov %%r10, %0;" : "=m" (registers[6]));
  asm("mov %%r11, %0;" : "=m" (registers[7]));
  asm("mov %%r12, %0;" : "=m" (registers[8]));
  asm("mov %%r13, %0;" : "=m" (registers[9]));
  asm("mov %%r14, %0;" : "=m" (registers[10]));
  asm("mov %%r15, %0;" : "=m" (registers[11]));
  gc_mark_range((u8*)registers, sizeof(registers));
}

void gc_collect(void) {
  u8 dummy;
  u32 stack_size = gc.top_stack_ptr - (u64)&dummy;
  memset(gc.free_map, 0, FREE_MAP_SIZE); // remove all markings
  gc_mark_from_registers();              // mark things contained in registers
  gc_mark_range(&dummy, stack_size);     // mark things on the stack
}

// utility functions for the overloaded addition operator
char* str_add(char* s1, char *s2) {
  char *str = gc_malloc(strlen(s1) + strlen(s2) + 1);
  sprintf(str, "%s%s", s1, s2);
  return str;
}
char* str_add_is(int a, char* s) {
  char num_str[12]; // max size of a str int
  sprintf(num_str, "%d", a);
  return str_add(num_str, s);
}
char* str_add_si(char* s, int a) {
  char num_str[12]; // max size of a str int
  sprintf(num_str, "%d", a);
  return str_add(s, num_str);
}
