# Lab 7 - Initial idea: A garbage collected C-backend
#### Axel Lindeberg (alindeb)
I read through the proposed ideas and the C-backend with a garbage collector sounded the most fun to me. This will require two main things I think:

1. A new optional phase of the compile, `CCodeGeneration`, enabled with a compiler flag. This phase would act like the pretty-printer implemented previously, expect it would instead output C-code. The three main complications here I think is:
  - How to handle inheritance. I would have to research a bit further but perhaps casting a `struct A*` to `struct B*` is valid if the fields of B occur as the first fields of A. However there might be alignment constraints there. If that is fine then implementing this is probably not to difficult. We can just cast A to it's parent class when necessary. Another way might be that if A inherits from B we could have A just hold an instance of B as a field. The most difficult part of inheritance is maybe implementing overridden functions. It would require some kind of vtable implementation, i.e storing function pointers to the correct version we want to use and these vtables would have to be initialized correctly.
  - How to handle identifier collisions. I have to be careful to make sure that all punkt0 identifiers are still valid and product C-code that compiles. For example, `char` is a valid identifier in punkt0 but a keyword in C. Perhaps you can perpend a prefix to all identifiers from punkt0, and make sure the supplied code (like the GC) never contains that prefix.
  - How to handle memory allocations in C, see next point.

2. A standalone garbage collector implemented in pure C. I think I would want to implement it myself, otherwise this project will be done too fast and not as fun. All memory allocations made by the punkt0 program would have to go through this GC instead of the regular `malloc`. A GC is definitely not trivial to implement but one main thing that simplifies this process is the fact that in punkt0 root nodes in the reference-tree can only be found on the stack. We have no global/static variables so we just have to scan through the stack when marking, and then all children of the pointers we find. This would require some scoping so as to be doable within the time constraints of this course:
  - For one, I think an arena-allocator is more feasible to implement, e.g the GC would allocate some large area of memory at the beginning of the program, keep track of what is free/used with in this region, and hand out bits of memory as requested.
  - Perhaps, use a simple bit-map to keep track of which individual bytes are free or used. This is relatively easy to implement but perhaps not very fast.
  - A simple, first fit algorithm to find a slot for a new allocation. This of course leads to fragmentation, but all allocations will be of similar sizes (the size of the classes defined in the punkt0 program) so I think it will not be a huge issue, except for generated strings which can have any size.
  - When to call garbage collection. As a first implementation we could add a `gc collect` call before every allocation, that is however needlessly aggressive GC collecting but it would stress-test the GC a bit.

## Extensions
If I have time I might do one of these, however this project already seems quite a handful.
- Dynamically grow the size of the GC heap. This is not trivial since we probably give out pointers to the heap directly. If we reallocate, these pointers are potentially invalidated. This could be done by having a linked-list of heap-arenas. So when one is full we allocate a completely separate one, so that the past ones are still valid.
- Implement a more complicated allocator, with a free-list, buddy allocator, or some other well-known algorithm.
- Implement a more sophisticated way of deciding when to garbage collect, perhaps keep track of the total memory allocated since collecting last, and only collect if that reaches some threshold.
