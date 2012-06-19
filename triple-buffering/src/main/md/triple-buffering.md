# Triple Buffering as a Concurrency Mechanism #

[Triple Buffering](http://en.wikipedia.org/wiki/Multiple_buffering#Triple_buffering) is a way of passing data between a producer and a consumer running at different rates. It ensures that the consumer only sees complete data with minimal lag, but the consumer doesn't expect to see every piece of data. The technique is most commonly used between images produced by a graphics card and a monitor; the graphics card is free to render hundreds of frames per second while the monitor consumes a fixed 60 fps.

This technique is also applicable as a small-scale lock-free concurrency mechanism; many applications consume real-time data but want to operate on fixed snapshots, or alternatively the data-processing operation performed takes longer than the time between each new piece of input data (and missing input data is acceptable). The following C macros declare a generic triple-buffer data structure:

~~~~
@TripleBuffer.h:*@ +=
#include <inttypes.h>
/* bit flags are (unused) (new write) (2x dirty) (2x clean) (2x snap) */
#define TRIPLE_BUFFER_TYPE(TYPENAME, TYPE) \
  struct TYPENAME { \
    TYPE buffer[3]; \
    volatile uint_fast8_t flags; \
  }
/* initially dirty = 0, clean = 1 and snap = 2 */
#define TRIPLE_BUFFER_NEW(NAME,TYPENAME) \
  struct TYPENAME NAME; \
  NAME.flags = 0x6;
~~~~

I've made the type declaration a macro so the buffer member of the struct will be the correct size - C++ implementations should use templates. If the producer and consumer are on different threads it makes it less obvious whether I should put the three buffers next to each other in memory. If the three buffers are on the same cache line when the writer writes to one, then this may invalidate the buffers in the cache of the reader's core unnecessarily. The writer needs to access the "dirty" buffer and the reader needs to access the "snap" buffer:

~~~~
@TripleBuffer.h:*@ +=
#define TRIPLE_BUFFER_SNAP_PTR(BUFFER) &BUFFER.buffer[BUFFER.flags & 0x3]
#define TRIPLE_BUFFER_WRITE_PTR(BUFFER) &BUFFER.buffer[(BUFFER.flags & 0x30) >> 4]
~~~~

The "dirty" pointer points to the buffer that the writer is currently writing to, and the "clean" pointer is the buffer with the most recent complete set of data. When the writer has finished modifying the dirty buffer it flips the two pointers with the following macro:

~~~~
@TripleBuffer.h:*@ +=
#ifdef _GLIBCXX_ATOMIC_BUILTINS
#define TRIPLE_BUFFER_FLIP_WRITER(BUFFER) \
  do { \
    uint_fast8_t flagsNow; \
    uint_fast8_t newFlags; \
    do { \
      flagsNow = BUFFER.flags; \
      newFlags = 0x40 | ((flagsNow & 0xC) << 2) | ((flagsNow & 0x30) >> 2) | (flagsNow & 0x3); \
    } while(!__sync_bool_compare_and_swap(&BUFFER.flags, flagsNow, newFlags)); \
  } while(0)
#else
  #error no atomic operations available!
#endif
~~~~

This macro is lock-free (it uses GCC built-ins - it should be changed to use C11 atomic operations when compiler support for them is mature enough). The order of the two operations is important as it guarantees that the clean and snap points are complete, immutable sets of data. The clean pointer is set to the dirty pointer first (as this macro has been called it is assumed that the dirty pointer points to a now-complete set of data). The dirty pointer is then updated - but as the clean pointer is the one that is snapped by the reader only its consistency is important. Note that this macro doesn't memset or otherwise clear the new dirty buffer before returning, so writers should do this themselves if they only intend to write to parts of the buffer.

~~~~
@TripleBuffer.h:*@ +=
#ifdef _GLIBCXX_ATOMIC_BUILTINS
#define TRIPLE_BUFFER_NEW_SNAP(BUFFER) \
  do { \
    uint_fast8_t flagsNow; \
    uint_fast8_t newFlags; \
    do { \
      flagsNow = BUFFER.flags; \
      if((flagsNow & 0x40)==0) break; \
      newFlags = (flagsNow & 0x30) | ((flagsNow & 0x3) << 2) | ((flagsNow & 0xC) >> 2); \
    } while(!__sync_bool_compare_and_swap(&BUFFER.flags, flagsNow, newFlags)); \
  } while(0)
#else
  #error no atomic operations available!
#endif
~~~~

The reader should call this macro when they want to take a snapshot of the current clean buffer. As above, this is lock-free and uses GCC built-ins. The order of the two atomic operations is important; switching the clean one first effectively isolates the current clean buffer from the writer, preserving its contents even if the writer continues to write and flip. I assume this method is called by the reader, as between the two atomic operations the snap pointer points to a changing set of data. However, the next atomic operation points the snap to the isolated buffer, and when control flow returns from the macro the snap pointer is pointing at what was the clean buffer when it was called. After calling this macro the clean buffer contains the last snap, so snapping twice without an intervening write-and-flip will just return old data. The triple buffer (in its current form) is therefore only useful if the writer always has higher frequency of writes than the reader has of reads. Also, there's no intrinsic mechanism to detect if a snap has changed. If the reader must know this, then the data must have a sequence number or timestamp (either from its producer or inserted by the writer) which the reader can compare with that of the previous snap.