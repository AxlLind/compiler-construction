#include <stdlib.h>
#include <string.h>

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

void* gc_malloc(u32 bytes) {
  u32 nbytes = bytes + GC_HDR_SIZE;
  for (u32 start_byte = 0; start_byte < GC_HEAP_SIZE; ++start_byte) {
    for (u32 offset = 0; offset < nbytes; ++offset) {
      if (gc_byte_free(start_byte + offset))
        continue;
      start_byte += offset; // cannot fit our block here, so jump forward
      goto mem_filled;
    }

    // found a fit, mark it!
    gc_mark_bytes(start_byte, nbytes);

    // write gc header information
    struct gc_header *hdr = (struct gc_header*) (gc.heap + start_byte);
    hdr->canary = GC_CANARY;
    hdr->size = nbytes;

    // clear the memory and return a ptr to it
    memset(hdr + 1, 0, bytes);
    return hdr + 1;

    mem_filled:;
  }

  printf("ERROR: GC out of memory.");
  exit(1);
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
  gc_mark_range((u8*)hdr, hdr->size);
}

void gc_mark_range(u8 *ptr, u32 size) {
  if (size < sizeof(u8*))
    return; // range not big enough to contain a ptr
  for (u32 i = 0; i < size; ++i)
    gc_mark_ptr(*(u8**) (ptr + i));
}

// Marked as 'noinline' to force a stack-frame to be allocated.
// This means stack_size will always be positive even when collecting from main,
// When compiling with -O2/3 this becomes an issue since this gets inlined.
void __attribute__((noinline)) gc_collect() {
  u8 dummy;
  u32 stack_size = gc.top_stack_ptr - (u64)&dummy;
  memset(gc.free_map, 0, FREE_MAP_SIZE); // remove all markings
  gc_mark_range(&dummy, stack_size);  // mark everything we can reach
}

char* str_add_is(int a, char* s) {
  u32 slen = strlen(s);
  char *new_s = gc_malloc(slen + 12); // max size of an int string
  sprintf(new_s, "%d%s", a, s);
  return new_s;
}
char* str_add_si(char* s, int a) {
  u32 slen = strlen(s);
  char *new_s = gc_malloc(slen + 12); // max size of an int string
  sprintf(new_s, "%s%d", s, a);
  return new_s;
}
char* str_add_ss(char* s1, char *s2) {
  u32 slen1 = strlen(s1);
  u32 slen2 = strlen(s2);
  char *new_s = gc_malloc(slen1 + slen2 + 1);
  sprintf(new_s, "%s%s", s1, s2);
  return new_s;
}
