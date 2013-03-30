#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>
#include <setjmp.h>
#include <dlfcn.h>
#include <errno.h>

#include "ool.h"
#include "scanner.h"

/* TODO

- Metaclass class methods copied from Object instance
- Object contains: method
- Set and Dictionary equals
- Complete Set and Dict methods
- Complete tests
- More elegant string-dicts
- Primitive regular expressions for string index, rindex and split
- More indirection
  - Method-Call.eval should use Metaclass.{class,instance}-method:
  - Direct calls to new, dict ops, etc?
*/

/* DESIGN NOTE

All objects must be accessible to the memory management subsystem, i.e. the garbage collector.
The garbage collector needs to know which objects are active.  The basis of active objects is
the root set.

The root set consists of:
- the VM registers,
- the VM stack, and
- the main module.

Whenever a new object is created, it must be held in something accessible from the root set.
By convention, all object constructors return the new object in R0.

Objects can be passed around as C function arguments, as long as they are objects already
accessible from the root set.

Any function that uses a VM register must save and restore the register's value.
This includes R0, even if a new value will be returned in R0, because an input parameter
may be an object held only in R0.
*/



#ifndef NDEBUG

struct {
  struct {
    unsigned stack_depth;
    unsigned stack_depth_max;
  } vm[1];
  struct {
    unsigned alloc_cnt;
    unsigned bytes_alloced;
    unsigned free_cnt;
    unsigned bytes_freed;
    unsigned bytes_in_use;
    unsigned bytes_in_use_max;
    unsigned bytes_in_use_collect;
    unsigned collect_cnt;
    unsigned collected_cnt;
    unsigned bytes_collected;
  } mem[1];
} stats[1];

void
stats_print(void)
{
#define PRINT_STAT(x)  printf("%s = %u\n", #x, x)

  PRINT_STAT(stats->vm->stack_depth);
  PRINT_STAT(stats->vm->stack_depth_max);
  PRINT_STAT(stats->mem->alloc_cnt);
  PRINT_STAT(stats->mem->bytes_alloced);
  PRINT_STAT(stats->mem->free_cnt);
  PRINT_STAT(stats->mem->bytes_freed);
  PRINT_STAT(stats->mem->bytes_in_use);
  PRINT_STAT(stats->mem->bytes_in_use_max);
  PRINT_STAT(stats->mem->bytes_in_use_collect);
  PRINT_STAT(stats->mem->collect_cnt);
  PRINT_STAT(stats->mem->collected_cnt);
  PRINT_STAT(stats->mem->bytes_collected);
}

#endif

/***************************************************************************/

extern int yyparse();

/***************************************************************************/

#define LIST_FIRST(li)  ((li)->next)
#define LIST_LAST(li)   ((li)->prev)
#define LIST_END(li)    (li)
#define LIST_PREV(nd)   ((nd)->prev)
#define LIST_NEXT(nd)   ((nd)->next)

struct list *
list_init(struct list *li)
{
  return (li->prev = li->next = li);
}

struct list *
list_insert(struct list *nd, struct list *before)
{
  struct list *p = LIST_PREV(before);

  nd->prev = p;
  nd->next = before;

  return (p->next = before->prev = nd);
}

struct list *
list_erase(struct list *nd)
{
  struct list *p = LIST_PREV(nd), *q = LIST_NEXT(nd);

  LIST_NEXT(p) = q;
  LIST_PREV(q) = p;

  return (nd);
}

/***************************************************************************/

const unsigned crc32_tbl[] = {
	0x00000000, 0x77073096, 0xee0e612c, 0x990951ba, 0x076dc419, 0x706af48f,
	0xe963a535, 0x9e6495a3,	0x0edb8832, 0x79dcb8a4, 0xe0d5e91e, 0x97d2d988,
	0x09b64c2b, 0x7eb17cbd, 0xe7b82d07, 0x90bf1d91, 0x1db71064, 0x6ab020f2,
	0xf3b97148, 0x84be41de,	0x1adad47d, 0x6ddde4eb, 0xf4d4b551, 0x83d385c7,
	0x136c9856, 0x646ba8c0, 0xfd62f97a, 0x8a65c9ec,	0x14015c4f, 0x63066cd9,
	0xfa0f3d63, 0x8d080df5,	0x3b6e20c8, 0x4c69105e, 0xd56041e4, 0xa2677172,
	0x3c03e4d1, 0x4b04d447, 0xd20d85fd, 0xa50ab56b,	0x35b5a8fa, 0x42b2986c,
	0xdbbbc9d6, 0xacbcf940,	0x32d86ce3, 0x45df5c75, 0xdcd60dcf, 0xabd13d59,
	0x26d930ac, 0x51de003a, 0xc8d75180, 0xbfd06116, 0x21b4f4b5, 0x56b3c423,
	0xcfba9599, 0xb8bda50f, 0x2802b89e, 0x5f058808, 0xc60cd9b2, 0xb10be924,
	0x2f6f7c87, 0x58684c11, 0xc1611dab, 0xb6662d3d,	0x76dc4190, 0x01db7106,
	0x98d220bc, 0xefd5102a, 0x71b18589, 0x06b6b51f, 0x9fbfe4a5, 0xe8b8d433,
	0x7807c9a2, 0x0f00f934, 0x9609a88e, 0xe10e9818, 0x7f6a0dbb, 0x086d3d2d,
	0x91646c97, 0xe6635c01, 0x6b6b51f4, 0x1c6c6162, 0x856530d8, 0xf262004e,
	0x6c0695ed, 0x1b01a57b, 0x8208f4c1, 0xf50fc457, 0x65b0d9c6, 0x12b7e950,
	0x8bbeb8ea, 0xfcb9887c, 0x62dd1ddf, 0x15da2d49, 0x8cd37cf3, 0xfbd44c65,
	0x4db26158, 0x3ab551ce, 0xa3bc0074, 0xd4bb30e2, 0x4adfa541, 0x3dd895d7,
	0xa4d1c46d, 0xd3d6f4fb, 0x4369e96a, 0x346ed9fc, 0xad678846, 0xda60b8d0,
	0x44042d73, 0x33031de5, 0xaa0a4c5f, 0xdd0d7cc9, 0x5005713c, 0x270241aa,
	0xbe0b1010, 0xc90c2086, 0x5768b525, 0x206f85b3, 0xb966d409, 0xce61e49f,
	0x5edef90e, 0x29d9c998, 0xb0d09822, 0xc7d7a8b4, 0x59b33d17, 0x2eb40d81,
	0xb7bd5c3b, 0xc0ba6cad, 0xedb88320, 0x9abfb3b6, 0x03b6e20c, 0x74b1d29a,
	0xead54739, 0x9dd277af, 0x04db2615, 0x73dc1683, 0xe3630b12, 0x94643b84,
	0x0d6d6a3e, 0x7a6a5aa8, 0xe40ecf0b, 0x9309ff9d, 0x0a00ae27, 0x7d079eb1,
	0xf00f9344, 0x8708a3d2, 0x1e01f268, 0x6906c2fe, 0xf762575d, 0x806567cb,
	0x196c3671, 0x6e6b06e7, 0xfed41b76, 0x89d32be0, 0x10da7a5a, 0x67dd4acc,
	0xf9b9df6f, 0x8ebeeff9, 0x17b7be43, 0x60b08ed5, 0xd6d6a3e8, 0xa1d1937e,
	0x38d8c2c4, 0x4fdff252, 0xd1bb67f1, 0xa6bc5767, 0x3fb506dd, 0x48b2364b,
	0xd80d2bda, 0xaf0a1b4c, 0x36034af6, 0x41047a60, 0xdf60efc3, 0xa867df55,
	0x316e8eef, 0x4669be79, 0xcb61b38c, 0xbc66831a, 0x256fd2a0, 0x5268e236,
	0xcc0c7795, 0xbb0b4703, 0x220216b9, 0x5505262f, 0xc5ba3bbe, 0xb2bd0b28,
	0x2bb45a92, 0x5cb36a04, 0xc2d7ffa7, 0xb5d0cf31, 0x2cd99e8b, 0x5bdeae1d,
	0x9b64c2b0, 0xec63f226, 0x756aa39c, 0x026d930a, 0x9c0906a9, 0xeb0e363f,
	0x72076785, 0x05005713, 0x95bf4a82, 0xe2b87a14, 0x7bb12bae, 0x0cb61b38,
	0x92d28e9b, 0xe5d5be0d, 0x7cdcefb7, 0x0bdbdf21, 0x86d3d2d4, 0xf1d4e242,
	0x68ddb3f8, 0x1fda836e, 0x81be16cd, 0xf6b9265b, 0x6fb077e1, 0x18b74777,
	0x88085ae6, 0xff0f6a70, 0x66063bca, 0x11010b5c, 0x8f659eff, 0xf862ae69,
	0x616bffd3, 0x166ccf45, 0xa00ae278, 0xd70dd2ee, 0x4e048354, 0x3903b3c2,
	0xa7672661, 0xd06016f7, 0x4969474d, 0x3e6e77db, 0xaed16a4a, 0xd9d65adc,
	0x40df0b66, 0x37d83bf0, 0xa9bcae53, 0xdebb9ec5, 0x47b2cf7f, 0x30b5ffe9,
	0xbdbdf21c, 0xcabac28a, 0x53b39330, 0x24b4a3a6, 0xbad03605, 0xcdd70693,
	0x54de5729, 0x23d967bf, 0xb3667a2e, 0xc4614ab8, 0x5d681b02, 0x2a6f2b94,
	0xb40bbe37, 0xc30c8ea1, 0x5a05df1b, 0x2d02ef8d
};

unsigned
obj_hash(obj_t obj, void *buf, unsigned n)
{
  unsigned      r;
  unsigned char *p;

  if (obj->hash_cache->valid)  return (obj->hash_cache->h);

  for (r = ~0, p= (unsigned char *) buf; n; --n, ++p) {
    r = crc32_tbl[(r ^ *p) & 0xFF] ^ (r >> 8);
  }
  r = ~r;

  obj->hash_cache->h     = r;
  obj->hash_cache->valid = 1;

  return (r);
}

/***************************************************************************/

const char * const fatal_msgs[] = {
  "",
  "Double error",
  "Out of memory",
  "Stack underflow"
};

void
fatal(unsigned errcode)
{
  HARD_ASSERT(errcode < ARRAY_SIZE(fatal_msgs));

  fprintf(stderr, "\n*** FATAL ERROR: %s\n", fatal_msgs[errcode]);

  abort();
}

/***************************************************************************/

unsigned initf;			/* TRUE <=> Initialization in progress */
unsigned collectingf;		/* TRUE <=> Collection in progress */

void collect(void);

void *
_cmalloc(unsigned size)
{
  void *result = 0;

  if (!initf) {
    result = malloc(size);
  
    if (result == 0) {
      collect();
    }
  }

  if (result == 0) {
    if ((result = malloc(size)) == 0)  fatal(FATAL_NO_MEM);
  }

#ifndef NDEBUG
  ++stats->mem->alloc_cnt;
  stats->mem->bytes_alloced += size;
  stats->mem->bytes_in_use  += size;
  if (stats->mem->bytes_in_use > stats->mem->bytes_in_use_max) {
    stats->mem->bytes_in_use_max = stats->mem->bytes_in_use;
  }
#endif

  return (result);
}

void *
_zcmalloc(unsigned size)
{
  void *result = _cmalloc(size);

  memset(result, 0, size);

  return (result);
}

void
_cfree(unsigned size, void *ptr)
{
#ifndef NDEBUG
  ++stats->mem->free_cnt;
  stats->mem->bytes_freed += size;
  stats->mem->bytes_in_use -= size;
#endif

  free(ptr);
}

/* Object lists

Objects are kept on the active list.
Collection consists of:
(1) zeroing reference counts for all objects in the active list,
(2) walking all objects, starting from the root set,
    moving all referenced objects from the ective to the
    marked list,
(3) freeing all objects left in the active list, and
(4) swapping the acive and marked lists.
*/
struct list obj_list[2];
unsigned obj_list_idx_active, obj_list_idx_marked = 1;
#define OBJ_LIST_ACTIVE  (&obj_list[obj_list_idx_active])
#define OBJ_LIST_MARKED  (&obj_list[obj_list_idx_marked])

void
obj_list_swap(void)
{
  unsigned temp;

  temp = obj_list_idx_active;
  obj_list_idx_active = obj_list_idx_marked;
  obj_list_idx_marked = temp;
}

/* Functions to access the inst_init, inst_walk and inst_free
   functions associated with classes.

The idea is that each class has 3 functions for handling instances:
- inst_init, which takes a varargs argument list pointer to parameters
  for initializing an instance,
- inst_walkm which takes a pointer to a function to be run on
  each object the given instance refers to, and
- inst_free, which frees any non-object resources associated with
  the instance.

Each of these functions should end in a call to the respective function
for the parent class.
*/

/* Call the class' instance initialization function for the given
   instance
*/

void
inst_init(obj_t inst, ...)
{
  obj_t   cl = inst_of(inst);
  va_list ap;

  va_start(ap, inst);
  (*CLASS(cl)->inst_init)(cl, inst, ap);
  va_end(ap);
}

/* Call the parent class' instance initialization function */

void
inst_init_parent(obj_t cl, obj_t inst, va_list ap)
{
  obj_t parent = CLASS(cl)->parent;

  (*CLASS(parent)->inst_init)(parent, inst, ap);
}

/* Call the class' instance walk function for the given instance */

void
inst_walk(obj_t inst, void (*func)(obj_t))
{
  obj_t cl = inst_of(inst);

  if (cl == NIL) {
    inst_walk_metaclass(inst, func);

    return;
  }

  (*CLASS(cl)->inst_walk)(cl, inst, func);
}

/* Call the parent class' instance walk function */

void
inst_walk_parent(obj_t cl, obj_t inst, void (*func)(obj_t))
{
  obj_t parent = CLASS(cl)->parent;

  (*CLASS(parent)->inst_walk)(parent, inst, func);
}

/* Call the class' instance free function for the given instance */

void
inst_free(obj_t inst)
{
  obj_t cl = inst_of(inst);

  (*CLASS(cl)->inst_free)(cl, inst);
}

/* Call the parent class' instance free function */

void
inst_free_parent(obj_t cl, obj_t inst)
{
  obj_t parent = CLASS(cl)->parent;

  (*CLASS(parent)->inst_free)(parent, inst);
}

void obj_release(obj_t obj);

/* Need to be careful in assigning object references...
   need to handle the case of an idempotent assignment.
*/

void
_obj_assign(obj_t *dst, obj_t src)
{
  obj_t temp = *dst;

  *dst = src;
  obj_release(temp);
}
#define OBJ_ASSIGN(dst, src)  (_obj_assign(&(dst), obj_retain(src)))

/* Allocate storage for an object, and hook it into the
   active object list
*/

obj_t
obj_alloc(unsigned size)
{
  obj_t result = OBJ(_zcmalloc(size));

  list_insert(result->list_node, LIST_END(OBJ_LIST_ACTIVE));

  return (result);
}

/* Free storage used by given object */

void
obj_free(obj_t obj)
{
  /* Call up the class hierarchy, to free any non-object
     resources
  */
  inst_free(obj);
  
  /* Remove from active object list */
  list_erase(obj->list_node);

  /* Add to instance cache */
  list_insert(obj->list_node, LIST_END(CLASS(obj->inst_of)->inst_cache));
}

/* Bump the given object's reference count */

obj_t
obj_retain(obj_t obj)
{
  if (obj != NIL) {
    ++obj->ref_cnt;
    
    HARD_ASSERT(obj->ref_cnt != 0);
  }
  
  return (obj);
}

/* Decrement the given object's reference count; 
   if the reference count goes to 0, free the object
*/

void
obj_release(obj_t obj)
{
  if (obj == NIL
      || obj->ref_cnt == 0  /* Because of reference cycles */
      || --obj->ref_cnt != 0
      )  return;

  /* Call up the class hierarchy, to release all objects referenced
     by this one
  */
  inst_walk(obj, obj_release);

  obj_free(obj);
}

/* Mark the given object, for garbage collection  */

void
obj_mark(obj_t obj)
{
  if (obj == NIL)  return;

  /* Bump its reference count */
  obj_retain(obj);

  /* If marked before, done */
  if (obj->ref_cnt > 1)  return;

  /* Marked for first time => add it to the marked list ... */
  list_erase(obj->list_node);
  list_insert(obj->list_node, LIST_END(OBJ_LIST_MARKED));

  /* ... and mark everything this object refers to */
  inst_walk(obj, obj_mark);
}

/* Walk all objects referred to by the root set */

void
root_walk(void (*func)(obj_t))
{
  obj_t    *q;
  unsigned n;

  /* All VM registers */

  for (q = regs, n = ARRAY_SIZE(regs); n; --n, ++q)  (*func)(*q);

  /* Everything on the VM stack */
  for (q = sp; q < stack_end; ++q)  (*func)(*q);

  /* The main module */
  (*func)(module_main);
}

/* Perform garbage collection */

void
collect(void)
{
  struct list *e, *p;
  obj_t       r;

  collectingf = 1;

#ifndef NDEBUG
  ++stats->mem->collect_cnt;
#endif

  for (e = LIST_END(OBJ_LIST_ACTIVE), p = LIST_FIRST(OBJ_LIST_ACTIVE);
       p != e;
       p = LIST_NEXT(p)
       ) {
    r = FIELD_PTR_TO_STRUCT_PTR(p, struct obj, list_node[0]);

#ifndef NDEBUG
    ASSERT(r->ref_cnt != 0);
#endif

    r->ref_cnt = 0;
  }

  root_walk(obj_mark);

  for (e = LIST_END(OBJ_LIST_ACTIVE); (p = LIST_FIRST(OBJ_LIST_ACTIVE)) != e; ) {
    r = FIELD_PTR_TO_STRUCT_PTR(p, struct obj, list_node[0]);
    
#ifndef NDEBUG
    ++stats->mem->collected_cnt;
    stats->mem->bytes_collected += CLASS(inst_of(r))->inst_size;
#endif

    obj_free(r);
  }
  
  obj_list_swap();

  collectingf = 0;
}

#ifndef NDEBUG

void
mem_chk(void)
{
  struct list *e, *p;

  for (e = LIST_END(OBJ_LIST_ACTIVE), p = LIST_FIRST(OBJ_LIST_ACTIVE); p != e; p = LIST_NEXT(p)) {
    obj_t    q = FIELD_PTR_TO_STRUCT_PTR(p, struct obj, list_node[0]);

    if (q->ref_cnt == 0) {
      printf("Consistency error: obj @ %p active, ref_cnt == 0\n", q);
      return;
    }

    if (is_class(q)) {
      struct list *ec, *pc;

      for (ec = LIST_END(CLASS(q)->inst_cache), pc = LIST_FIRST(CLASS(q)->inst_cache); pc != ec; pc = LIST_NEXT(pc)) {
	obj_t    qc = FIELD_PTR_TO_STRUCT_PTR(pc, struct obj, list_node[0]), rc = inst_of(qc);

	if (rc != q) {
	  printf("Consistency error: obj @ %p in inst cache for class %p, s/b %p\n", qc, rc, q);
	  return;
	}
	
	if (qc->ref_cnt != 0) {
	  printf("Consistency error: obj @ %p in inst cache, ref_cnt != 0\n", qc);
	  return;
	}
      }
    }
  }
}

void
mem_dump(void)
{
  unsigned    n, bytes;
  struct list *e, *p;

  for (bytes = n = 0, e = LIST_END(OBJ_LIST_ACTIVE), p = LIST_FIRST(OBJ_LIST_ACTIVE); p != e; p = LIST_NEXT(p)) {
    obj_t    q = FIELD_PTR_TO_STRUCT_PTR(p, struct obj, list_node[0]), r = inst_of(q);
    char     *s;
    unsigned i;

    if (r) {
      s = STRING(CLASS(r)->name)->data;
      i = CLASS(r)->inst_size;
    } else {
      s = "#Metaclass";
      i = sizeof(struct inst_metaclass);
    }

    printf("obj=%p\tref_cnt=%u\tinst_of=%s\tbytes=%u\n", q, q->ref_cnt, s, i);
    ++n;
    bytes += i;
  }
  printf("Number of objects: %u\n", n);
  printf("Bytes used: %u\n", bytes);
}

#endif

/***************************************************************************/

#ifndef NDEBUG
#define ASSERT_REG_VALID(r)  ASSERT((r) < ARRAY_SIZE(regs))
#else
#define ASSERT_REG_VALID(r)
#endif

void
vm_assign(unsigned dst, obj_t val)
{
  ASSERT_REG_VALID(dst);

  OBJ_ASSIGN(REG(dst), val);
}

#define VM_STACK_CHECK_PUSH(n) \
  do { if ((sp - (n)) < stack_fence)  error(ERR_STACK_OVF); } while (0)
#define VM_STACK_CHECK_POP(n) \
  do { if ((sp + (n)) > stack_end)  fatal(FATAL_STACK_UNF); } while (0)

#ifndef NDEBUG
#define VM_STATS_UPDATE_PUSH(n)						\
  do {									\
    if ((stats->vm->stack_depth += (n)) > stats->vm->stack_depth_max) {	\
      stats->vm->stack_depth_max = stats->vm->stack_depth;			\
    }									\
  } while (0)
#define VM_STATS_UPDATE_POP(n) \
  (stats->vm->stack_depth -= (n))
#else
#define VM_STATS_UPDATE_PUSH(n)
#define VM_STATS_UPDATE_POP(n)
#endif

void
vm_push(unsigned r)
{
  ASSERT_REG_VALID(r);
  VM_STACK_CHECK_PUSH(1);

  VM_STATS_UPDATE_PUSH(1);

  *--sp = obj_retain(REG(r));
}

void
vm_pop(unsigned r)
{
  ASSERT_REG_VALID(r);
  VM_STACK_CHECK_POP(1);

  VM_STATS_UPDATE_POP(1);

  _obj_assign(&REG(r), *sp++);
}

void
vm_drop(void)
{
  VM_STACK_CHECK_POP(1);

  VM_STATS_UPDATE_POP(1);

  obj_release(*sp++);
}

void
vm_dropn(unsigned n)
{
  VM_STACK_CHECK_POP(n);

  VM_STATS_UPDATE_POP(n);

  for ( ; n; --n)  obj_release(*sp++);
}

void
vm_enter(unsigned r)
{
  unsigned n = r + 1;
  obj_t    *p;

  ASSERT_REG_VALID(r);
  VM_STACK_CHECK_PUSH(n);

  VM_STATS_UPDATE_PUSH(n);
  
  for (p = &REG(0); n; --n, ++p)  *--sp = obj_retain(*p);
}

void
vm_leave(unsigned r)
{
  unsigned n = r + 1;
  obj_t    *p;

  ASSERT_REG_VALID(r);
  VM_STACK_CHECK_POP(n);

  VM_STATS_UPDATE_POP(n);

  for (p = &REG(r); r; --r, --p)  _obj_assign(p, *sp++);
  obj_release(*sp++);
}

void
m_inst_alloc(obj_t cl)
{
  struct list *li, *p;
  unsigned    sz = CLASS(cl)->inst_size;
  obj_t       obj;

  if (sz == 0)  error(ERR_CANNOT_INST, cl);

  vm_push(0);

  li = CLASS(cl)->inst_cache;
  if ((p = LIST_FIRST(li)) != LIST_END(li)){
    list_erase(p);

    obj = FIELD_PTR_TO_STRUCT_PTR(p, struct obj, list_node);
    
    memset(obj, 0, sz);

    list_insert(obj->list_node, LIST_END(OBJ_LIST_ACTIVE));
  } else {
    obj = obj_alloc(sz);
  }

  OBJ_ASSIGN(R0, obj);
  OBJ_ASSIGN(R0->inst_of, cl);

  vm_drop();
}

/***************************************************************************/

struct frame {
  struct frame *prev;
  unsigned     type;
  obj_t        *sp;
  struct frame *envp_prev;
  obj_t        env_dict;
};

struct frame_jmp {
  struct frame base;
  jmp_buf      jmp_buf;
};

struct frame *frp, *envp;

#define FRAME_POP \
  do { vm_dropn(frp->sp - sp);  frp = frp->prev; } while (0)

#define FRAME_RESTART_BEGIN			\
  {						\
  struct frame_jmp __frame[1];			\
  int              frame_jmp_code;		\
  __frame->base.prev = frp;			\
  __frame->base.type = FRAME_TYPE_RESTART;	\
  __frame->base.sp   = sp;			\
  frp = &__frame->base;				\
  frame_jmp_code = setjmp(__frame->jmp_buf);	\
  stack_fence = stack + STACK_HEADROOM;

#define FRAME_RESTART_POP  FRAME_POP

#define FRAME_RESTART_END			\
  FRAME_RESTART_POP;				\
  }

struct frame_input {
  struct frame    base;
  struct inp_desc inp_desc;
};

#define FRAME_INPUT_BEGIN(_fp, _fn, _s)		\
  {						\
  struct frame_input __frame[1];		\
  __frame->base.prev = frp;			\
  __frame->base.type = FRAME_TYPE_INPUT;	\
  __frame->base.sp   = sp;			\
  __frame->inp_desc.fp = (_fp);			\
  __frame->inp_desc.filename = (_fn);		\
  __frame->inp_desc.str = (_s);			\
  yy_input_push(&__frame->inp_desc);		\
  frp = &__frame->base;				

#define FRAME_INPUT_POP \
  do { yy_input_pop(&((struct frame_input *) frp)->inp_desc);  FRAME_POP; } while (0)

#define FRAME_INPUT_END					\
  FRAME_INPUT_POP;					\
  }

struct frame_module {
  struct frame base;
  obj_t        module_prev, module;
};

#define FRAME_MODULE_BEGIN(m)			\
  {						\
  struct frame_module __frame[1];		\
  __frame->base.prev = frp;			\
  __frame->base.type = FRAME_TYPE_MODULE;	\
  __frame->base.sp   = sp;			\
  __frame->base.envp_prev = envp;		\
  __frame->module_prev = module_cur;		\
  __frame->base.env_dict = module_cur = (m);	\
  frp = envp = &__frame->base;

#define FRAME_MODULE_POP				     \
  do {							     \
    module_cur = ((struct frame_module *) frp)->module_prev; \
    envp = frp->envp_prev;				     \
    FRAME_POP;						     \
  } while (0)

#define FRAME_MODULE_END			\
  FRAME_MODULE_POP;				\
  }

struct frame_while {
  struct frame_jmp base;
};

#define FRAME_WHILE_BEGIN					\
  {								\
    struct frame_while __frame[1];				\
    int                frame_jmp_code;				\
    __frame->base.base.prev = frp;				\
    __frame->base.base.type = FRAME_TYPE_WHILE;			\
    __frame->base.base.sp   = sp;				\
    frp = &__frame->base.base;					\
    frame_jmp_code = setjmp(__frame->base.jmp_buf);

#define FRAME_WHILE_POP  FRAME_POP

#define FRAME_WHILE_END		\
  FRAME_WHILE_POP;		\
  }

struct frame_block {
  struct frame_jmp base;
};

#define FRAME_BLOCK_BEGIN(d)				\
  {							\
  struct frame_block __frame[1];			\
  int                frame_jmp_code;			\
  __frame->base.base.prev = frp;			\
  __frame->base.base.type = FRAME_TYPE_BLOCK;		\
  __frame->base.base.sp   = sp;				\
  if (__frame->base.base.env_dict = (d)) {		\
    __frame->base.base.envp_prev = envp;		\
    envp = &__frame->base.base;				\
  }							\
  frp = &__frame->base.base;				\
  frame_jmp_code = setjmp(__frame->base.jmp_buf);

#define FRAME_BLOCK_POP							\
  do {									\
    if (frp->env_dict)  envp = frp->envp_prev;				\
    FRAME_POP;								\
  } while (0)

#define FRAME_BLOCK_END					\
  FRAME_BLOCK_POP;					\
  }

struct frame_method_call {
  struct frame base;
  obj_t        cl, sel, args;
};

#define FRAME_METHOD_CALL_BEGIN(_sel, _args)	\
  {						\
  struct frame_method_call __frame[1];		\
  __frame->base.prev = frp;			\
  __frame->base.type = FRAME_TYPE_METHOD_CALL;	\
  __frame->base.sp   = sp;			\
  __frame->cl   = NIL;				\
  __frame->sel  = (_sel);			\
  __frame->args = (_args);			\
  frp = &__frame->base;			      

#define FRAME_METHOD_CALL_POP  FRAME_POP

#define FRAME_METHOD_CALL_END \
  FRAME_METHOD_CALL_POP;	      \
  }

void
frame_jmp(unsigned type, int frame_jmp_code)
{
  struct frame *p;

  switch (type) {
  case FRAME_TYPE_RESTART:
  case FRAME_TYPE_WHILE:
  case FRAME_TYPE_BLOCK:
    break;
  default:
    HARD_ASSERT(0);
  }

  for (p = frp; p; p = p->prev) {
    if (p->type == type)  break;
  }

  if (p == 0)  return;

  while (frp != p) {
    switch (frp->type) {
    case FRAME_TYPE_RESTART:
      FRAME_RESTART_POP;
      break;
    case FRAME_TYPE_INPUT:
      FRAME_INPUT_POP;
      break;
    case FRAME_TYPE_MODULE:
      FRAME_MODULE_POP;
      break;
    case FRAME_TYPE_WHILE:
      FRAME_WHILE_POP;
      break;
    case FRAME_TYPE_BLOCK:
      FRAME_BLOCK_POP;
      break;
    case FRAME_TYPE_METHOD_CALL:
      FRAME_METHOD_CALL_POP;
      break;
    default:
      HARD_ASSERT(0);
    }
  }

  longjmp(((struct frame_jmp *) frp)->jmp_buf, frame_jmp_code);
}

/***************************************************************************/

unsigned
is_list(obj_t obj)
{
  return (obj == NIL || is_kind_of(obj, consts.cl.list));
}

unsigned
is_class(obj_t obj)
{
  obj_t cl;

  return ((cl = inst_of(obj)) == NIL || cl == consts.cl.metaclass);
}

/***************************************************************************/

void m_method_call(obj_t sel, unsigned argc, obj_t args);
void m_method_call_1(obj_t sel, obj_t recvr);
void m_method_call_2(obj_t sel, obj_t recvr, obj_t arg);
void m_method_call_3(obj_t sel, obj_t recvr, obj_t arg1, obj_t arg2);

obj_t
method_find(obj_t cl, int dict_ofs, obj_t sel, obj_t *cl_fnd)
{
  obj_t dn;
  
  for ( ; cl; cl = CLASS(cl)->parent) {
    dn = dict_at(*(obj_t *)((char *) cl + dict_ofs), sel);
    if (dn != NIL) {
      *cl_fnd = cl;
      return (CDR(dn));
    }
  }

  return (NIL);
}

obj_t
cl_method_find(obj_t cl, obj_t sel, obj_t *cl_fnd)
{
  return (method_find(cl, FIELD_OFS(struct inst_metaclass, cl_methods), sel, cl_fnd));
}

obj_t
inst_method_find(obj_t cl, obj_t sel, obj_t *cl_fnd)
{
  return (method_find(cl, FIELD_OFS(struct inst_metaclass, inst_methods), sel, cl_fnd));
}

void
m_method_call(obj_t sel, unsigned argc, obj_t args)
{
  obj_t recvr = CAR(args);
  obj_t m;

  FRAME_METHOD_CALL_BEGIN(sel, args) {
    struct frame_method_call *mcfrp = (struct frame_method_call *) frp;
    obj_t                    cl;

    if (is_class(recvr)
	&& (m = cl_method_find(recvr, sel, &cl))
	|| (m = inst_method_find(recvr ? inst_of(recvr) : consts.cl.list, sel, &cl))
	|| inst_of(recvr) == NIL
	&& (m = inst_method_find(consts.cl.object, sel, &cl))
	) {
      mcfrp->cl = cl;

      if (inst_of(m) == consts.cl.code_method) {
	/* Break recursion */
	
	(*CODE_METHOD(m)->func)(argc, args);
      } else {
	obj_t module = CLASS(cl)->module;

	if (module != module_cur) {
	  FRAME_MODULE_BEGIN(module) {
	    m_method_call_2(consts.str.evalc, m, args);
	  } FRAME_MODULE_END;
	} else {
	  m_method_call_2(consts.str.evalc, m, args);
	}
      }
    } else {
      mcfrp->cl = is_class(recvr) ? recvr : inst_of(recvr);
      
      error(ERR_NO_METHOD, sel);
    }
  } FRAME_METHOD_CALL_END;
}

void
m_method_call_1(obj_t sel, obj_t recvr)
{
  vm_push(0);

  m_dptr_new(consts.cl.list, recvr, NIL);

  m_method_call(sel, 1, R0);

  vm_drop();
}

void
m_method_call_2(obj_t sel, obj_t recvr, obj_t arg)
{
  vm_push(0);

  m_dptr_new(consts.cl.list, arg, NIL);
  m_dptr_new(consts.cl.list, recvr, R0);

  m_method_call(sel, 2, R0);

  vm_drop();
}

void
m_method_call_3(obj_t sel, obj_t recvr, obj_t arg1, obj_t arg2)
{
  vm_push(0);

  m_dptr_new(consts.cl.list, arg2, NIL);
  m_dptr_new(consts.cl.list, arg1, R0);
  m_dptr_new(consts.cl.list, recvr, R0);

  m_method_call(sel, 3, R0);

  vm_drop();
}

/***************************************************************************/

/* Metaclass */

void
_inst_walk_metaclass(obj_t inst, void (*func)(obj_t))
{
  (*func)(CLASS(inst)->name);
  (*func)(CLASS(inst)->parent);
  (*func)(CLASS(inst)->module);
  (*func)(CLASS(inst)->cl_vars);
  (*func)(CLASS(inst)->cl_methods);
  (*func)(CLASS(inst)->inst_vars);
  (*func)(CLASS(inst)->inst_methods);
}

void
inst_walk_metaclass(obj_t inst, void (*func)(obj_t))
{
  _inst_walk_metaclass(inst, func);

  (*func)(inst_of(inst));
}

void
cm_metaclass_name(unsigned argc, obj_t args)
{
  if (argc != 1)  error(ERR_NUM_ARGS);

  vm_assign(0, CLASS(consts.cl.metaclass)->name);
}

void
cm_metaclass_parent(unsigned argc, obj_t args)
{
  if (argc != 1)  error(ERR_NUM_ARGS);

  vm_assign(0, CLASS(consts.cl.metaclass)->parent);
}

void
cm_metaclass_cl_methods(unsigned argc, obj_t args)
{
  if (argc != 1)  error(ERR_NUM_ARGS);

  vm_assign(0, CLASS(consts.cl.metaclass)->cl_methods);
}

void
cm_metaclass_cl_vars(unsigned argc, obj_t args)
{
  if (argc != 1)  error(ERR_NUM_ARGS);
  
  vm_assign(0, CLASS(consts.cl.metaclass)->cl_vars);
}

void
cm_metaclass_inst_methods(unsigned argc, obj_t args)
{
  if (argc != 1)  error(ERR_NUM_ARGS);
  
  vm_assign(0, CLASS(consts.cl.metaclass)->inst_methods);
}

void
cm_metaclass_inst_vars(unsigned argc, obj_t args)
{
  if (argc != 1)  error(ERR_NUM_ARGS);

  vm_assign(0, CLASS(consts.cl.metaclass)->inst_vars);
}

void
cm_metaclass_hash(unsigned argc, obj_t args)
{
  if (argc != 1)  error(ERR_NUM_ARGS);

  m_integer_new(consts.cl.integer, string_hash(CLASS(consts.cl.metaclass)->name));
}

void
inst_init_class(obj_t cl, obj_t inst, va_list ap)
{
  vm_push(0);

  OBJ_ASSIGN(CLASS(inst)->name,   va_arg(ap, obj_t));
  OBJ_ASSIGN(CLASS(inst)->parent, va_arg(ap, obj_t));
  OBJ_ASSIGN(CLASS(inst)->module, va_arg(ap, obj_t));
  CLASS(inst)->inst_size = va_arg(ap, unsigned);
  CLASS(inst)->inst_init = va_arg(ap, void (*)(obj_t cl, obj_t inst, va_list ap));
  CLASS(inst)->inst_walk = va_arg(ap, void (*)(obj_t cl, obj_t inst, void (*func)(obj_t)));
  CLASS(inst)->inst_free = va_arg(ap, void (*)(obj_t cl, obj_t inst));

  m_string_dict_new(consts.cl.dict, 16);
  OBJ_ASSIGN(CLASS(inst)->cl_vars, R0);
  m_string_dict_new(consts.cl.dict, 16);
  OBJ_ASSIGN(CLASS(inst)->cl_methods, R0);
  m_string_dict_new(consts.cl.dict, 16);
  OBJ_ASSIGN(CLASS(inst)->inst_vars, R0);
  m_string_dict_new(consts.cl.dict, 16);
  OBJ_ASSIGN(CLASS(inst)->inst_methods, R0);
  
  dict_at_put(OBJ(MODULE(CLASS(inst)->module)->base), CLASS(inst)->name, inst);

  list_init(CLASS(inst)->inst_cache);

  vm_pop(0);

  inst_init_parent(cl, inst, ap);
}

void
cl_inst_cache_free(obj_t cl)
{
  struct list *li, *p;

  /* Free all cached instances */

  li = CLASS(cl)->inst_cache;
  while ((p = LIST_FIRST(li)) != LIST_END(li)) {
    list_erase(p);
    
    /* The actual free */
    _cfree(CLASS(cl)->inst_size, FIELD_PTR_TO_STRUCT_PTR(p, struct obj, list_node));
  }
}

void
inst_walk_class(obj_t cl, obj_t inst, void (*func)(obj_t))
{
  _inst_walk_metaclass(inst, func);

  if (collectingf)  cl_inst_cache_free(cl);

  inst_walk_parent(cl, inst, func);
}

void
inst_free_class(obj_t cl, obj_t inst)
{
  cl_inst_cache_free(cl);
  
  inst_free_parent(cl, inst);
}

void
m_class_new(obj_t    name,
	    obj_t    parent,
	    obj_t    module, 
	    unsigned inst_size,
	    void     (*_inst_init)(obj_t cl, obj_t inst, va_list ap),
	    void     (*_inst_walk)(obj_t cl, obj_t inst, void (*func)(obj_t)),
	    void     (*_inst_free)(obj_t cl, obj_t inst)
	    )
{
  vm_push(0);

  m_inst_alloc(consts.cl.metaclass);
  inst_init(R0, name, parent, module, inst_size, _inst_init, _inst_walk, _inst_free);

  vm_drop();
}

unsigned
is_subclass_of(obj_t cl1, obj_t cl2)
{
  for (; cl1; cl1 = CLASS(cl1)->parent) {
    if (cl1 == cl2)  return (1);
  }

  return (0);
}

void
inst_init_user(obj_t cl, obj_t inst, va_list ap)
{
  inst_init_parent(cl, inst, ap);
}

void
inst_walk_user(obj_t cl, obj_t inst, void (*func)(obj_t))
{
  unsigned ofs;
  obj_t    *p;

  ofs = CLASS(CLASS(cl)->parent)->inst_size;
  for (p = (obj_t *)((unsigned char *) inst + ofs); ofs < CLASS(cl)->inst_size; ofs += sizeof(obj_t), ++p) {
    (*func)(*p);
  }

  inst_walk_parent(cl, inst, func);
}

void
inst_free_user(obj_t cl, obj_t inst)
{
  inst_free_parent(cl, inst);
}

void cm_object_new(unsigned argc, obj_t args);

void
cm_class_new(unsigned argc, obj_t args)
{
  obj_t    name, parent, inst_vars;
  unsigned ofs;

  if (argc != 4)  error(ERR_NUM_ARGS);
  args = CDR(args);
  name = CAR(args);    args = CDR(args);
  parent = CAR(args);  args = CDR(args);
  inst_vars = CAR(args);

  vm_enter(1);

  m_class_new(name,
	      parent,
	      module_cur,
	      CLASS(parent)->inst_size + list_len(inst_vars) * sizeof(obj_t),
	      inst_init_user,
	      inst_walk_user,
	      inst_free_user
	      );
  vm_assign(1, R0);

  for (ofs = CLASS(parent)->inst_size; inst_vars; inst_vars = CDR(inst_vars), ofs += sizeof(obj_t)) {
    m_integer_new(consts.cl.integer, ofs);
    dict_at_put(CLASS(R1)->inst_vars, CAR(inst_vars), R0);
  }

  vm_assign(0, R1);

  vm_leave(1);
}

void
m_fqclname(obj_t cl)
{
  obj_t s = CLASS(cl)->name;

  vm_push(0);
  
  m_fqmodname(CLASS(cl)->module);
  if (string_len(R0) == 0) {
    vm_assign(0, s);
  } else {
    m_string_newc(consts.cl.string, 3, string_len(R0), STRING(R0)->data,
		                       1, ".",
		                       string_len(s), STRING(s)->data
		  );
  }

  vm_drop();
}

void
cm_class_name(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.metaclass))  error(ERR_INVALID_ARG, recvr);
  
  m_fqclname(recvr);
}

void
cm_class_parent(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.metaclass))  error(ERR_INVALID_ARG, recvr);
  
  vm_assign(0, CLASS(recvr)->parent);
}

void
cm_class_module(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.metaclass))  error(ERR_INVALID_ARG, recvr);
  
  vm_assign(0, CLASS(recvr)->module);
}

void
cm_class_cl_methods(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.metaclass))  error(ERR_INVALID_ARG, recvr);
  
  vm_assign(0, CLASS(recvr)->cl_methods);
}

void
cm_class_cl_vars(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.metaclass))  error(ERR_INVALID_ARG, recvr);
  
  vm_assign(0, CLASS(recvr)->cl_vars);
}

void
cm_class_inst_methods(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.metaclass))  error(ERR_INVALID_ARG, recvr);
  
  vm_assign(0, CLASS(recvr)->inst_methods);
}

void
cm_class_inst_vars(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.metaclass))  error(ERR_INVALID_ARG, recvr);
  
  m_dict_keys(CLASS(recvr)->inst_vars);
}

void
cm_class_hash(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.metaclass))  error(ERR_INVALID_ARG, recvr);
  
  m_integer_new(consts.cl.integer, string_hash(CLASS(recvr)->name));
}

void
cm_class_cl_method(unsigned argc, obj_t args)
{
  obj_t recvr, arg, cl;

  if (argc != 2)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.metaclass))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  
  vm_assign(0, cl_method_find(recvr, arg, &cl));
}

void
cm_class_inst_method(unsigned argc, obj_t args)
{
  obj_t recvr, arg, cl;

  if (argc != 2)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.metaclass))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  
  vm_assign(0, inst_method_find(recvr, arg, &cl));
}

/***************************************************************************/

/* Class: Object */

obj_t
inst_of(obj_t obj)
{
  return (obj ? obj->inst_of : consts.cl.object);
}

unsigned
is_kind_of(obj_t inst, obj_t cl)
{
  return (is_subclass_of(inst_of(inst), cl));
}

void
inst_init_object(obj_t cl, obj_t inst, va_list ap)
{
}

void
inst_walk_object(obj_t cl, obj_t inst, void (*func)(obj_t))
{
  (*func)(inst_of(inst));
}

void
inst_free_object(obj_t cl, obj_t inst)
{
}

void
cm_object_new(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)                                error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.metaclass))  error(ERR_INVALID_ARG, recvr);
  
  m_inst_alloc(recvr);
}

void
cm_object_quote(unsigned argc, obj_t args)
{
  if (argc != 1)  error(ERR_NUM_ARGS);
  
  vm_assign(0, CAR(args));
}

void
cm_object_eval(unsigned argc, obj_t args)
{
  cm_object_quote(argc, args);
}

void
cm_object_instof(unsigned argc, obj_t args)
{
  if (argc != 1)  error(ERR_NUM_ARGS);
  
  vm_assign(0, inst_of(CAR(args)));
}

void
cm_object_eq(unsigned argc, obj_t args)
{
  if (argc != 2)  error(ERR_NUM_ARGS);
  
  m_boolean_new(consts.cl.boolean, CAR(args) == CAR(CDR(args)));
}

void
m_object_tostring(obj_t obj)
{
  if (obj == NIL) {
    vm_assign(0, consts.str.nil);
    return;
  }
  
  vm_push(0);

  m_fqclname(inst_of(obj));

  {
    unsigned bufsize = string_len(R0) + 16 + 18;
    char     buf[bufsize];	/* Potentially non-portable */
    
    m_string_newc(consts.cl.string,
		  1,
		  snprintf(buf, bufsize, "<instance of %s @ %p>",
			   STRING(R0)->data,
			   obj
			   ),
		  buf
		  );
  }

  vm_drop();
}

void
cm_object_tostring(unsigned argc, obj_t args)
{
  if (argc != 1)  error(ERR_NUM_ARGS);
  
  m_object_tostring(CAR(args));
}

unsigned
atou(unsigned n, char *s, unsigned *pval)
{
  unsigned result = 0, val = 0;
  char     c;
  
  for ( ; n; --n, ++s, ++result) {
    c = *s;
    if (c < '0' || c > '9')  break;

    val = 10 * val + (c - '0');
  }

  *pval = val;

  return (result);
}

static int
_abs(int i)
{
  return (i < 0 ? -i : i);
}

struct printf_fmt_info {
  struct {
    unsigned alt   : 1;
    unsigned zero  : 1;
    unsigned left  : 1;
    unsigned blank : 1;
    unsigned plus  : 1;
    unsigned width_valid : 1;
    unsigned prec_valid  : 1;
  } flags;
  unsigned width, prec;
  char mode;
};

int
printf_fmt_parse(unsigned n, char *s, struct printf_fmt_info *fi)
{
  int      result = 0;
  unsigned k;

  memset(fi, 0, sizeof(*fi));

  if (n < 1 || *s != '%')  return (-1);

  ++s;  --n;  ++result;

  for ( ; n; --n, ++s, ++result) {
    switch (*s) {
    case '#':
      fi->flags.alt = 1;
      continue;
    case '0':
      fi->flags.zero = 1;
      continue;
    case '-':
      fi->flags.left = 1;
      continue;
    case ' ':
      fi->flags.blank = 1;
      continue;
    case '+':
      fi->flags.plus = 1;
      continue;
    default:
      ;
    }

    break;
  }

  if (n == 0)  return (-1);

  if (fi->flags.zero && fi->flags.left)  fi->flags.zero = 0;

  if ((k = atou(n, s, &fi->width)) > 0)  fi->flags.width_valid = 1;
  s += k;
  n -= k;
  result += (int) k;
  
  if (n == 0)  return (-1);

  if (*s == '.') {
    ++s;  --n;  ++result;
    
    if ((k = atou(n, s, &fi->prec)) > 0)  fi->flags.prec_valid = 1;
    s += k;
    n -= k;
    result += (int) k;

    if (n == 0 || fi->flags.prec_valid >= 0 && fi->flags.width_valid && fi->prec > fi->width)  return (-1);
  }

  fi->mode = *s;

  ++s;  --n;  ++result;
  
  return (result);
}
  
void
cm_object_tostring_fmt(unsigned argc, obj_t args)
{
  obj_t           recvr, arg;
  unsigned        n;
  struct printf_fmt_info fi[1];

  if (argc != 2)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  arg   = CAR(CDR(args));
  if (!is_kind_of(arg, consts.cl.string))     error(ERR_INVALID_ARG, arg);
  n = string_len(arg);
  if (printf_fmt_parse(n, STRING(arg)->data, fi) != (int) n
      || fi->mode != 'O'
      ) {
    error(ERR_INVALID_ARG, arg);
  }

  vm_push(0);

  m_object_tostring(recvr);

  if (m_string_pad(R0, fi) < 0)  error(ERR_INVALID_ARG, arg);

  vm_drop();
}

void
cm_object_print(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)  error(ERR_NUM_ARGS);
  recvr = CAR(args);

  m_method_call_1(consts.str.tostring, recvr);
  m_string_print(R0);
  
  vm_assign(0, recvr);
}

void
m_obj_printc(obj_t obj, obj_t outf)
{
  vm_push(0);

  m_method_call_1(consts.str.tostring, obj);
  m_string_fprint(R0, outf);

  vm_drop();
}

void
cm_object_printc(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 2)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  
  m_obj_printc(recvr, CAR(CDR(args)));

  vm_assign(0, recvr);
}

obj_t *
obj_attr_find(obj_t inst, obj_t s)
{
  obj_t cl, p;

  if (is_class(inst)) {
    for (cl = inst; cl; cl = CLASS(cl)->parent) {
      if (p = dict_at(CLASS(cl)->cl_vars, s)) {
	return (&CDR(p));
      }
    }
  }

  for (cl = inst_of(inst); cl; cl = CLASS(cl)->parent) {
    if (p = dict_at(CLASS(cl)->inst_vars, s)) {
      return ((obj_t *)((char *) inst + INTEGER(CDR(p))->val));
    }
  }

  error(ERR_NO_ATTR, s);

  return (0);
}

void
m_obj_at(obj_t inst, obj_t k)
{
  vm_assign(0, *obj_attr_find(inst, k));
}

void
cm_object_at(unsigned argc, obj_t args)
{
  if (argc != 2)  error(ERR_NUM_ARGS);
  
  m_obj_at(CAR(args), CAR(CDR(args)));
}

void
cm_object_at_put(unsigned argc, obj_t args)
{
  obj_t recvr, k, val;;

  if (argc != 3)  error(ERR_NUM_ARGS);
  recvr = CAR(args);  args  = CDR(args);
  k     = CAR(args);  args  = CDR(args);
  val   = CAR(args);
  
  OBJ_ASSIGN(*obj_attr_find(recvr, k), val);
  vm_assign(0, val);
}

void
cm_object_if(unsigned argc, obj_t args)
{
  obj_t recvr;
  
  if (argc != 2)                              error(ERR_NUM_ARGS);
  recvr = CAR(args);

  vm_push(0);

  m_method_call_1(consts.str.eval, recvr);
  if (!is_kind_of(R0, consts.cl.boolean))     error(ERR_INVALID_ARG, recvr);
  
  if (BOOLEAN(R0)->val) {
    m_method_call_1(consts.str.eval, CAR(CDR(args)));
  }

  vm_drop();
}

void
cm_object_if_else(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 3)                              error(ERR_NUM_ARGS);
  recvr = CAR(args);

  vm_push(0);

  m_method_call_1(consts.str.eval, recvr);
  if (!is_kind_of(R0, consts.cl.boolean))     error(ERR_INVALID_ARG, recvr);
  args = CDR(args);
  
  m_method_call_1(consts.str.eval,
		  BOOLEAN(R0)->val ? CAR(args) : CAR(CDR(args))
		  );

  vm_drop();
}

enum { WHILE_CONT = 1, WHILE_BREAK };

void
cm_object_while(unsigned argc, obj_t args)
{
  obj_t recvr, arg;

  if (argc != 2)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  arg   = CAR(CDR(args));

  FRAME_WHILE_BEGIN {
    vm_push(0);
    
    switch (frame_jmp_code) {
    case 0:
    case WHILE_CONT:
      for (;;) {
	m_method_call_1(consts.str.eval, recvr);
	if (!is_kind_of(R0, consts.cl.boolean))  error(ERR_INVALID_VALUE_2, recvr, R0);
	if (!BOOLEAN(R0)->val)  break;
	m_method_call_1(consts.str.eval, arg);
      }
      
    case WHILE_BREAK:
      break;
      
    default:
      HARD_ASSERT(0);
    }
    
  } FRAME_WHILE_END;
}

void
cm_object_for_loop(unsigned argc, obj_t args)
{
  obj_t recvr, body, incr;
  
  if (argc != 3)  error(ERR_NUM_ARGS);
  recvr = CAR(args);  args  = CDR(args);
  body  = CAR(args);  args  = CDR(args);
  incr  = CAR(args);
  
  FRAME_WHILE_BEGIN {
    vm_push(0);
    
    switch (frame_jmp_code) {
    case 0:
      for (;;) {
	m_method_call_1(consts.str.eval, recvr);
	if (!is_kind_of(R0, consts.cl.boolean))  error(ERR_INVALID_VALUE_2, recvr, R0);
	if (!BOOLEAN(R0)->val)  break;
	m_method_call_1(consts.str.eval, body);
      case WHILE_CONT:
	m_method_call_1(consts.str.eval, incr);
      }
      
    case WHILE_BREAK:
      break;
      
    default:
      HARD_ASSERT(0);
    }
    
  } FRAME_WHILE_END;
}

void
cm_object_break(unsigned argc, obj_t args)
{
    if (argc != 1)  error(ERR_NUM_ARGS);

    vm_assign(0, CAR(args));

    frame_jmp(FRAME_TYPE_WHILE, WHILE_BREAK);

    error(ERR_WHILE);
}

void
cm_object_cont(unsigned argc, obj_t args)
{
    if (argc != 1)  error(ERR_NUM_ARGS);

    frame_jmp(FRAME_TYPE_WHILE, WHILE_CONT);

    error(ERR_WHILE);
}

void
cm_object_return(unsigned argc, obj_t args)
{
    if (argc != 1)  error(ERR_NUM_ARGS);

    vm_assign(0, CAR(args));

    frame_jmp(FRAME_TYPE_BLOCK, 1);

    error(ERR_BLOCK);
}

void
cm_object_and(unsigned argc, obj_t args)
{
  obj_t recvr, arg;

  if (argc != 2)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  arg   = CAR(CDR(args));

  vm_push(0);

  m_method_call_1(consts.str.eval, recvr);
  if (R0)  m_method_call_1(consts.str.eval, arg);

  vm_drop();
}

void
cm_object_or(unsigned argc, obj_t args)
{
  obj_t recvr, arg;

  if (argc != 2)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  arg   = CAR(CDR(args));

  vm_push(0);

  m_method_call_1(consts.str.eval, recvr);
  if (R0 == 0)  m_method_call_1(consts.str.eval, arg);

  vm_drop();
}

void
cm_object_cons(unsigned argc, obj_t args)
{
  obj_t arg;

  if (argc != 2)  error(ERR_NUM_ARGS);

  arg = CAR(CDR(args));
  m_dptr_new(is_list(arg) ? consts.cl.list : consts.cl.pair, CAR(args), arg);
}

void
cm_object_copy(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)  error(ERR_NUM_ARGS);

  recvr = CAR(args);
  m_method_call_2(consts.str.newc, inst_of(recvr), recvr);
}

void
cm_object_ge(unsigned argc, obj_t args)
{
  if (argc != 2)  error(ERR_NUM_ARGS);

  m_method_call_2(consts.str.comparec, CAR(args), CAR(CDR(args)));
  if (!is_kind_of(R0, consts.cl.integer))  error(ERR_INVALID_VALUE, R0);

  m_boolean_new(consts.cl.boolean, INTEGER(R0)->val >= 0);
}

void
cm_object_gt(unsigned argc, obj_t args)
{
  if (argc != 2)  error(ERR_NUM_ARGS);

  m_method_call_2(consts.str.comparec, CAR(args), CAR(CDR(args)));
  if (!is_kind_of(R0, consts.cl.integer))  error(ERR_INVALID_VALUE, R0);

  m_boolean_new(consts.cl.boolean, INTEGER(R0)->val > 0);
}

void
cm_object_le(unsigned argc, obj_t args)
{
  if (argc != 2)  error(ERR_NUM_ARGS);

  m_method_call_2(consts.str.comparec, CAR(args), CAR(CDR(args)));
  if (!is_kind_of(R0, consts.cl.integer))  error(ERR_INVALID_VALUE, R0);

  m_boolean_new(consts.cl.boolean, INTEGER(R0)->val <= 0);
}

void
cm_object_lt(unsigned argc, obj_t args)
{
  if (argc != 2)  error(ERR_NUM_ARGS);

  m_method_call_2(consts.str.comparec, CAR(args), CAR(CDR(args)));
  if (!is_kind_of(R0, consts.cl.integer))  error(ERR_INVALID_VALUE, R0);

  m_boolean_new(consts.cl.boolean, INTEGER(R0)->val < 0);
}

/***************************************************************************/

/* Class: Code_method */

void
inst_init_code_method(obj_t cl, obj_t inst, va_list ap)
{
  CODE_METHOD(inst)->func = va_arg(ap, void (*)(unsigned, obj_t));

  inst_init_parent(cl, inst, ap);
}

void
inst_walk_code_method(obj_t cl, obj_t inst, void (*func)(obj_t))
{
  inst_walk_parent(cl, inst, func);
}

void
inst_free_code_method(obj_t cl, obj_t inst)
{
  inst_free_parent(cl, inst);
}

void
m_code_method_new(obj_t cl, void (*func)(unsigned, obj_t))
{
  m_inst_alloc(cl);
  inst_init(R0, func);
}

void
cm_code_method_eval(unsigned argc, obj_t args)
{
  obj_t recvr, nargs;
  
  if (argc != 2)                                  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.code_method))  error(ERR_INVALID_ARG, recvr);
  nargs = CAR(CDR(args));
  if (!is_list(nargs))  error(ERR_INVALID_ARG, nargs);

  (*CODE_METHOD(recvr)->func)(list_len(nargs), nargs);
}

/***************************************************************************/

/* Class: Boolean */

void
inst_init_boolean(obj_t cl, obj_t inst, va_list ap)
{
  BOOLEAN(inst)->val = va_arg(ap, unsigned);

  inst_init_parent(cl, inst, ap);
}

void
inst_walk_boolean(obj_t cl, obj_t inst, void (*func)(obj_t))
{
  inst_walk_parent(cl, inst, func);
}

void
inst_free_boolean(obj_t cl, obj_t inst)
{
  inst_free_parent(cl, inst);
}

void
m_boolean_new(obj_t cl, unsigned val)
{
  vm_assign(0, val ? consts._bool._true : consts._bool._false);
}

void
cm_boolean_new(unsigned argc, obj_t args)
{
  obj_t    recvr, arg;
  unsigned bval = 0;
  
  if (argc < 1 || argc > 2)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (inst_of(recvr) != consts.cl.metaclass)  error(ERR_INVALID_ARG, recvr);
  if (argc < 2) {
    bval = 0;
  } else {
    arg = CAR(CDR(args));

    if (is_kind_of(arg, consts.cl.boolean)) {
      bval = BOOLEAN(arg)->val;
    } else if (is_kind_of(arg, consts.cl.integer)) {
      bval = INTEGER(arg)->val != 0;
    } else if (is_kind_of(arg, consts.cl._float)) {
      bval = FLOAT(arg)->val != 0.0;
    } else if (is_kind_of(arg, consts.cl.string)) {
      bval = string_len(arg) != 0;
    } else {
      error(ERR_INVALID_ARG, arg);
    }
  }

  m_boolean_new(recvr, bval);
}

void
cm_boolean_and(unsigned argc, obj_t args)
{
  obj_t recvr, arg;

  if (argc != 2)                              error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.boolean))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  if (!is_kind_of(arg, consts.cl.boolean))    error(ERR_INVALID_ARG, arg);
  
  m_boolean_new(consts.cl.boolean, BOOLEAN(recvr)->val && BOOLEAN(arg)->val);
}

void
cm_boolean_or(unsigned argc, obj_t args)
{
  obj_t recvr, arg;

  if (argc != 2)                              error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.boolean))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  if (!is_kind_of(arg, consts.cl.boolean))    error(ERR_INVALID_ARG, arg);
  
  m_boolean_new(consts.cl.boolean, BOOLEAN(recvr)->val || BOOLEAN(arg)->val);
}

void
cm_boolean_xor(unsigned argc, obj_t args)
{
  obj_t recvr, arg;

  if (argc != 2)                              error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.boolean))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  if (!is_kind_of(arg, consts.cl.boolean))    error(ERR_INVALID_ARG, arg);
  
  m_boolean_new(consts.cl.boolean, (BOOLEAN(recvr)->val != 0) ^ (BOOLEAN(arg)->val != 0));
}

void
cm_boolean_not(unsigned argc, obj_t args)
{
  obj_t recvr;
  
  if (argc != 1)                              error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.boolean))  error(ERR_INVALID_ARG, recvr);
  
  m_boolean_new(consts.cl.boolean, !BOOLEAN(recvr)->val);
}

void
m_boolean_tostring(obj_t obj)
{
  vm_assign(0, BOOLEAN(obj)->val ? consts.str._true : consts.str._false);
}

void
cm_boolean_tostring(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)                              error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.boolean))  error(ERR_INVALID_ARG, recvr);
  
  m_boolean_tostring(recvr);
}

void
cm_boolean_tostring_fmt(unsigned argc, obj_t args)
{
  obj_t           recvr, arg;
  unsigned        n;
  struct printf_fmt_info fi[1];

  if (argc != 2)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.boolean))  error(ERR_INVALID_ARG, recvr);
  arg   = CAR(CDR(args));
  if (!is_kind_of(arg, consts.cl.string))     error(ERR_INVALID_ARG, arg);
  n = string_len(arg);
  if (printf_fmt_parse(n, STRING(arg)->data, fi) != (int) n
      || fi->mode != 'O'
      ) {
    error(ERR_INVALID_ARG, arg);
  }

  vm_push(0);

  m_boolean_tostring(recvr);

  if (m_string_pad(R0, fi) < 0)  error(ERR_INVALID_ARG, arg);

  vm_drop();
}

void
cm_boolean_equals(unsigned argc, obj_t args)
{
  obj_t recvr, arg;

  if (argc != 2)                              error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.boolean))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  
  m_boolean_new(consts.cl.boolean,
		inst_of(arg) == inst_of(recvr)
		&& (BOOLEAN(arg)->val != 0) == (BOOLEAN(recvr)->val != 0)
		);
}

void
cm_boolean_assert(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)                              error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.boolean))  error(ERR_INVALID_ARG, recvr);
  
  if (BOOLEAN(recvr)->val == 0)  error(ERR_ASSERT_FAIL);
  
  vm_assign(0, recvr);
}

/***************************************************************************/

/* Class: Integer */

void
inst_init_integer(obj_t cl, obj_t inst, va_list ap)
{
  INTEGER(inst)->val = va_arg(ap, integer_val_t);

  inst_init_parent(cl, inst, ap);
}

void
inst_walk_integer(obj_t cl, obj_t inst, void (*func)(obj_t))
{
  inst_walk_parent(cl, inst, func);
}

void
inst_free_integer(obj_t cl, obj_t inst)
{
  inst_free_parent(cl, inst);
}

void
m_integer_new(obj_t cl, integer_val_t val)
{
  m_inst_alloc(cl);
  inst_init(R0, val);
}

void
cm_integer_new(unsigned argc, obj_t args)
{
  obj_t         recvr, arg;
  integer_val_t ival;

  if (argc < 1 || argc > 2)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (inst_of(recvr) != consts.cl.metaclass)  error(ERR_INVALID_ARG, recvr);
  if (argc < 2) {
    ival = 0;
  } else {
    arg = CAR(CDR(args));

    if (is_kind_of(arg, consts.cl.boolean)) {
      ival = BOOLEAN(arg)->val != 0;
    } else if (is_kind_of(arg, consts.cl.integer)) {
      ival = INTEGER(arg)->val;
    } else if (is_kind_of(arg, consts.cl._float)) {
      ival = (integer_val_t) FLOAT(arg)->val;
    } else if (is_kind_of(arg, consts.cl.string)) {
      unsigned n = string_len(arg);
      char     *fmt;
      
      if (n < 1) {
	error(ERR_INVALID_ARG, arg);
      } else if (n >= 3
	  && STRING(arg)->data[0] == '0'
	  && (STRING(arg)->data[1] | 0x20) == 'x'
	  ) {
	fmt = INTEGER_SCANF_FMT_HEX;
      } else if (n >= 2
		 && STRING(arg)->data[0] == '0'
		 ) {
	fmt = INTEGER_SCANF_FMT_OCT;
      } else {
	fmt = INTEGER_SCANF_FMT_DEC;
      }
      
      if (sscanf(STRING(arg)->data, fmt, &ival) != 1) {
	error(ERR_INVALID_ARG, arg);
      }
    } else {
      error(ERR_INVALID_ARG, arg);
    }
  }

  m_integer_new(recvr, ival);
}

void
m_integer_tostring(obj_t obj, char *fmt, struct printf_fmt_info *fi)
{
  unsigned buf_size = 32;

  if (fi != 0 && fi->flags.width_valid)  buf_size = fi->width + 1;

  {
    char buf[buf_size];		/* Potentially non-portable */

    m_string_newc(consts.cl.string, 1, snprintf(buf, buf_size, fmt, INTEGER(obj)->val), buf);
  }
}

void
cm_integer_tostring(unsigned argc, obj_t args)
{
  obj_t recvr;
  
  if (argc != 1)                              error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.integer))  error(ERR_INVALID_ARG, recvr);
  
  m_integer_tostring(recvr, INTEGER_PRINTF_FMT, 0);
}

void
cm_integer_tostring_fmt(unsigned argc, obj_t args)
{
  obj_t           recvr, arg;
  unsigned        n;
  struct printf_fmt_info fi[1];
  
  if (argc != 2)                              error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.integer))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  if (!is_kind_of(arg, consts.cl.string))  error(ERR_INVALID_ARG, arg);
  n = string_len(arg);
  if (printf_fmt_parse(n, STRING(arg)->data, fi) != (int) n)  error(ERR_INVALID_ARG, arg);
  switch (fi->mode) {
  case 'd':
  case 'i':
  case 'o':
  case 'u':
  case 'x':
  case 'X':
  case 'O':
    break;
  default:
    error(ERR_INVALID_ARG, arg);
  }

  if (fi->mode == 'O')  STRING(arg)->data[n - 1] = 'd';

  vm_push(0);

  m_string_newc(consts.cl.string, 3, n - 1, STRING(arg)->data,
	                             2, "ll",	/* FIXME - Geralize */
	                             1, &STRING(arg)->data[n - 1]
		);

  m_integer_tostring(recvr, STRING(R0)->data, fi);

  vm_drop();
}

void
cm_integer_hash(unsigned argc, obj_t args)
{
  obj_t recvr;
  
  if (argc != 1)                              error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.integer))  error(ERR_INVALID_ARG, recvr);
  
  m_integer_new(consts.cl.integer, obj_hash(recvr, &INTEGER(recvr)->val, sizeof(INTEGER(recvr)->val)));
}

void
cm_integer_equals(unsigned argc, obj_t args)
{
  obj_t recvr, arg;

  if (argc != 2)                              error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.integer))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  
  m_boolean_new(consts.cl.boolean,
		is_kind_of(arg, consts.cl.integer)
		&& INTEGER(arg)->val == INTEGER(recvr)->val
		);
}

void
cm_integer_minus(unsigned argc, obj_t args)
{
  obj_t recvr;
  
  if (argc != 1)                              error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.integer))  error(ERR_INVALID_ARG, recvr);
  
  m_integer_new(consts.cl.integer, -INTEGER(recvr)->val);
}

int
integer_sgn(integer_val_t ival)
{
  if (ival < 0)  return (-1);
  if (ival > 0)  return (1);
  return (0);
}

integer_val_t
integer_abs(integer_val_t ival)
{
  return (ival < 0 ? -ival : ival);
}

void
cm_integer_abs(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)                              error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.integer))  error(ERR_INVALID_ARG, recvr);

  m_integer_new(consts.cl.integer, integer_abs(INTEGER(recvr)->val));
}

void
cm_integer_add(unsigned argc, obj_t args)
{
  obj_t         recvr, arg;
  integer_val_t ival1, ival2, iresult;
  int           s1, s2;

  if (argc != 2)                              error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.integer))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  if (!is_kind_of(arg, consts.cl.integer))    error(ERR_INVALID_ARG, arg);

  ival1 = INTEGER(recvr)->val;
  ival2 = INTEGER(arg)->val;
  iresult = ival1 + ival2;
  s1 = integer_sgn(ival1);
  s2 = integer_sgn(ival2);

  if (s1 > 0 && s2 > 0) {
    if (!(iresult > ival1 && iresult > ival2))  error(ERR_OVF);
  } else if (s1 < 0 && s2 < 0) {
    if (!(iresult < ival1 && iresult < ival2))  error(ERR_OVF);
  }
  
  m_integer_new(consts.cl.integer, iresult);
}

void
cm_integer_sub(unsigned argc, obj_t args)
{
  obj_t         recvr, arg;
  integer_val_t ival1, ival2, iresult;
  int           s1, s2;

  if (argc != 2)                              error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.integer))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  if (!is_kind_of(arg, consts.cl.integer))    error(ERR_INVALID_ARG, arg);
  
  ival1 = INTEGER(recvr)->val;
  ival2 = INTEGER(arg)->val;
  iresult = ival1 - ival2;
  s1 = integer_sgn(ival1);
  s2 = integer_sgn(ival2);

  if (s1 > 0 && s2 < 0) {
    if (!(iresult > ival1 && iresult > ival2))  error(ERR_OVF);
  } else if (s1 < 0 && s2 > 0) {
    if (!(iresult < ival1 && iresult < ival2))  error(ERR_OVF);
  }
  
  m_integer_new(consts.cl.integer, iresult);
}

void
cm_integer_mult(unsigned argc, obj_t args)
{
  obj_t         recvr, arg;
  integer_val_t ival1, ival2, iresult, uval1, uval2, uresult;
  int           s1, s2, sresult;

  if (argc != 2)                              error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.integer))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  if (!is_kind_of(arg, consts.cl.integer))    error(ERR_INVALID_ARG, arg);
  
  ival1 = INTEGER(recvr)->val;
  ival2 = INTEGER(arg)->val;
  iresult = ival1 * ival2;
  uval1 = integer_abs(ival1);
  uval2 = integer_abs(ival2);
  uresult = integer_abs(iresult);
  s1 = integer_sgn(ival1);
  s2 = integer_sgn(ival2);
  sresult = integer_sgn(iresult);

  if (ival1 != 0 && ival2 != 0) {
    if (!(uresult >= uval1 && uresult >= uval2
	  && (s1 == s2 ? sresult > 0 : sresult < 0)
	  )
	) error(ERR_OVF);
  }
  
  m_integer_new(consts.cl.integer, iresult);
}

void
cm_integer_div(unsigned argc, obj_t args)
{
  obj_t         recvr, arg;
  integer_val_t ival1, ival2;

  if (argc != 2)                              error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.integer))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  if (!is_kind_of(arg, consts.cl.integer))    error(ERR_INVALID_ARG, arg);

  ival1 = INTEGER(recvr)->val;
  ival2 = INTEGER(arg)->val;
  
  if (ival2 == 0)  error(ERR_OVF);

  m_integer_new(consts.cl.integer, ival1 / ival2);
}

void
cm_integer_mod(unsigned argc, obj_t args)
{
  obj_t         recvr, arg;
  integer_val_t ival1, ival2;

  if (argc != 2)                              error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.integer))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  if (!is_kind_of(arg, consts.cl.integer))    error(ERR_INVALID_ARG, arg);

  ival1 = INTEGER(recvr)->val;
  ival2 = INTEGER(arg)->val;
  
  if (ival2 == 0)  error(ERR_OVF);

  m_integer_new(consts.cl.integer, ival1 % ival2);
}

void
m_integer_range(integer_val_t init, integer_val_t lim, integer_val_t step)
{
  integer_val_t val;
  obj_t         *p;
  
  vm_push(1);
  
  vm_assign(1, NIL);
  for (p = &R1, val = init; val < lim; val += step) {
    m_integer_new(consts.cl.integer, val);
    m_dptr_new(consts.cl.list, R0, NIL);
    OBJ_ASSIGN(*p, R0);
    p = &CDR(R0);
  }
  vm_assign(0, R1);
  
  vm_pop(1);
}

void
cm_integer_range(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)                              error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.integer))  error(ERR_INVALID_ARG, recvr);
  
  m_integer_range(0, INTEGER(recvr)->val, 1);
}

void
cm_integer_range_init(unsigned argc, obj_t args)
{
  obj_t recvr, arg;

  if (argc != 2)                              error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.integer))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  if (!is_kind_of(arg, consts.cl.integer))    error(ERR_INVALID_ARG, arg);
  
  m_integer_range(INTEGER(arg)->val, INTEGER(recvr)->val, 1);
}

void
cm_integer_range_init_step(unsigned argc, obj_t args)
{
  obj_t recvr, arg0, arg1;
  
  if (argc != 3)                              error(ERR_NUM_ARGS);
  recvr = CAR(args);  args = CDR(args);
  if (!is_kind_of(recvr, consts.cl.integer))  error(ERR_INVALID_ARG, recvr);
  arg0 = CAR(args);  args = CDR(args);
  if (!is_kind_of(arg0, consts.cl.integer))   error(ERR_INVALID_ARG, arg0);
  arg1 = CAR(args);
  if (!is_kind_of(arg1, consts.cl.integer))   error(ERR_INVALID_ARG, arg1);
  
  m_integer_range(INTEGER(arg0)->val, INTEGER(recvr)->val, INTEGER(arg1)->val);
}

void
cm_integer_chr(unsigned argc, obj_t args)
{
  obj_t         recvr;
  integer_val_t ival;
  char          c;
  
  if (argc != 1)                              error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.integer))  error(ERR_INVALID_ARG, recvr);
  ival = INTEGER(recvr)->val;
  if (ival < 0 || ival > 255)                 error(ERR_INVALID_ARG, recvr);
  
  c = ival;
  m_string_newc(consts.cl.string, 1, 1, &c);
}

void
cm_integer_and(unsigned argc, obj_t args)
{
  obj_t recvr, arg;

  if (argc != 2)                              error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.integer))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  if (!is_kind_of(arg, consts.cl.integer))    error(ERR_INVALID_ARG, arg);

  m_integer_new(consts.cl.integer, INTEGER(recvr)->val & INTEGER(arg)->val);
}

void
cm_integer_or(unsigned argc, obj_t args)
{
  obj_t recvr, arg;

  if (argc != 2)                              error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.integer))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  if (!is_kind_of(arg, consts.cl.integer))    error(ERR_INVALID_ARG, arg);

  m_integer_new(consts.cl.integer, INTEGER(recvr)->val | INTEGER(arg)->val);
}

void
cm_integer_xor(unsigned argc, obj_t args)
{
  obj_t recvr, arg;

  if (argc != 2)                              error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.integer))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  if (!is_kind_of(arg, consts.cl.integer))    error(ERR_INVALID_ARG, arg);

  m_integer_new(consts.cl.integer, INTEGER(recvr)->val ^ INTEGER(arg)->val);
}

void
cm_integer_not(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)                              error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.integer))  error(ERR_INVALID_ARG, recvr);

  m_integer_new(consts.cl.integer, ~INTEGER(recvr)->val);
}

void
cm_integer_compare(unsigned argc, obj_t args)
{
  obj_t recvr, arg;

  if (argc != 2)                              error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.integer))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  if (!is_kind_of(arg, consts.cl.integer))    error(ERR_INVALID_ARG, arg);

  m_integer_new(consts.cl.integer, integer_sgn(INTEGER(recvr)->val - INTEGER(arg)->val));
}

/***************************************************************************/

/* Class: Float */

void
inst_init_float(obj_t cl, obj_t inst, va_list ap)
{
  FLOAT(inst)->val = va_arg(ap, float_val_t);

  inst_init_parent(cl, inst, ap);
}

void
inst_walk_float(obj_t cl, obj_t inst, void (*func)(obj_t))
{
  inst_walk_parent(cl, inst, func);
}

void
inst_free_float(obj_t cl, obj_t inst)
{
  inst_free_parent(cl, inst);
}

void
m_float_new(obj_t cl, float_val_t val)
{
  m_inst_alloc(cl);
  inst_init(R0, val);
}

void
cm_float_new(unsigned argc, obj_t args)
{
  obj_t       recvr, arg;
  float_val_t fval;

  if (argc < 1 || argc > 2)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (inst_of(recvr) != consts.cl.metaclass)  error(ERR_INVALID_ARG, recvr);
  if (argc < 2) {
    fval = 0.0;
  } else {
    arg = CAR(CDR(args));
  
    if (is_kind_of(arg, consts.cl.boolean)) {
      fval = BOOLEAN(arg)->val ? 1.0 : 0.0;
    } else if (is_kind_of(arg, consts.cl.integer)) {
      fval = (float_val_t) INTEGER(arg)->val;
    } else if (is_kind_of(arg, consts.cl._float)) {
      fval = FLOAT(arg)->val;
    } else if (is_kind_of(arg, consts.cl.string)) {
      if (string_len(arg) < 1 || sscanf(STRING(arg)->data, FLOAT_SCANF_FMT, &fval) != 1) {
	error(ERR_INVALID_ARG, arg);
      }
    } else {
      error(ERR_INVALID_ARG, arg);
    }
  }

  m_float_new(recvr, fval);
}

int
float_sgn(float_val_t fval)
{
  if (fval < 0.0)  return (-1);
  if (fval > 0.0)  return (1);
  return (0);
}

float_val_t
float_abs(float_val_t fval)
{
  return (fval < 0.0 ? -fval : fval);
}

void
cm_float_abs(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)                             error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl._float))  error(ERR_INVALID_ARG, recvr);

  m_float_new(consts.cl._float, float_abs(FLOAT(recvr)->val));
}

void
cm_float_add(unsigned argc, obj_t args)
{
  obj_t recvr, arg;

  if (argc != 2)                             error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl._float))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  if (!is_kind_of(arg, consts.cl._float))    error(ERR_INVALID_ARG, arg);

  m_float_new(consts.cl._float, FLOAT(recvr)->val + FLOAT(arg)->val);
}

void
cm_float_sub(unsigned argc, obj_t args)
{
  obj_t recvr, arg;

  if (argc != 2)                             error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl._float))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  if (!is_kind_of(arg, consts.cl._float))    error(ERR_INVALID_ARG, arg);

  m_float_new(consts.cl._float, FLOAT(recvr)->val - FLOAT(arg)->val);
}

void
cm_float_mult(unsigned argc, obj_t args)
{
  obj_t recvr, arg;

  if (argc != 2)                             error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl._float))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  if (!is_kind_of(arg, consts.cl._float))    error(ERR_INVALID_ARG, arg);

  m_float_new(consts.cl._float, FLOAT(recvr)->val * FLOAT(arg)->val);
}

void
cm_float_div(unsigned argc, obj_t args)
{
  obj_t recvr, arg;

  if (argc != 2)                             error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl._float))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  if (!is_kind_of(arg, consts.cl._float))    error(ERR_INVALID_ARG, arg);

  m_float_new(consts.cl._float, FLOAT(recvr)->val / FLOAT(arg)->val);
}

void
cm_float_minus(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)                             error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl._float))  error(ERR_INVALID_ARG, recvr);

  m_float_new(consts.cl._float, -FLOAT(recvr)->val);
}

void
cm_float_hash(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)                             error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl._float))  error(ERR_INVALID_ARG, recvr);

  m_integer_new(consts.cl.integer, obj_hash(recvr, &FLOAT(recvr)->val, sizeof(FLOAT(recvr)->val)));
}

void
cm_float_equals(unsigned argc, obj_t args)
{
  obj_t recvr, arg;

  if (argc != 2)                             error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl._float))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));

  m_boolean_new(consts.cl.boolean,
		inst_of(arg) == inst_of(recvr)
		&& FLOAT(arg)->val == FLOAT(recvr)->val
		);
}

void
m_float_tostring(obj_t obj, char *fmt, struct printf_fmt_info *fi)
{
  unsigned buf_size = 64;

  if (fi && fi->flags.width_valid)  buf_size = fi->width + 1;

  {
    char buf[buf_size];  /* Potentially non-portable */
    
    m_string_newc(consts.cl.string, 1, snprintf(buf, buf_size, fmt, FLOAT(obj)->val), buf);
  }
}

void
cm_float_tostring(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)                             error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl._float))  error(ERR_INVALID_ARG, recvr);

  m_float_tostring(recvr, FLOAT_PRINTF_FMT, 0);
}

void
cm_float_tostring_fmt(unsigned argc, obj_t args)
{
  obj_t           recvr, arg;
  unsigned        n;
  struct printf_fmt_info fi[1];
  
  if (argc != 2)                             error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl._float))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  if (!is_kind_of(arg, consts.cl.string))  error(ERR_INVALID_ARG, arg);
  n = string_len(arg);
  if (printf_fmt_parse(n, STRING(arg)->data, fi) != (int) n)  error(ERR_INVALID_ARG, arg);
  switch (fi->mode) {
  case 'e':
  case 'f':
  case 'g':
  case 'O':
    break;
  default:
    error(ERR_INVALID_ARG, arg);
  }

  if (fi->mode == 'O')  STRING(arg)->data[n - 1] = 'g';

  vm_push(0);

  m_string_newc(consts.cl.string, 3, n -1, STRING(arg)->data,
	                             1, "L",	/* FIXME - Generalize */
	                             1, &STRING(arg)->data[n - 1]
		);

  m_float_tostring(recvr, STRING(R0)->data, fi);

  vm_drop();
}

void
cm_float_compare(unsigned argc, obj_t args)
{
  obj_t recvr, arg;

  if (argc != 2)                             error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl._float))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  if (!is_kind_of(arg, consts.cl._float))    error(ERR_INVALID_ARG, arg);

  m_integer_new(consts.cl.integer, float_sgn(FLOAT(recvr)->val - FLOAT(arg)->val));
}

/***************************************************************************/

/* Class: String */

void
inst_init_string(obj_t cl, obj_t inst, va_list ap)
{
  if ((STRING(inst)->size = va_arg(ap, unsigned)) > 0) {
    STRING(inst)->data = _cmalloc(STRING(inst)->size);
  }

  inst_init_parent(cl, inst, ap);
}

void
inst_walk_string(obj_t cl, obj_t inst, void (*func)(obj_t))
{
  inst_walk_parent(cl, inst, func);  
}

void
inst_free_string(obj_t cl, obj_t inst)
{
  if (STRING(inst)->data) {
    _cfree(STRING(inst)->size, STRING(inst)->data);
  }

  inst_free_parent(cl, inst);  
}

void
m_string_newc(obj_t cl, unsigned n, ...)
{
  va_list  ap;
  unsigned j, k;
  unsigned size;
  char     *p, *q;

  va_start(ap, n);
  for (size = 0, k = n; k; --k) {
    j = va_arg(ap, unsigned);
    p = va_arg(ap, char *);
    size += j;
  }
  if (size > 0)  ++size;
  va_end(ap);

  vm_push(0);
  
  m_inst_alloc(cl);
  inst_init(R0, size);

  va_start(ap, n);
  for (q = STRING(R0)->data, k = n; k; --k) {
    j = va_arg(ap, unsigned);
    p = va_arg(ap, char *);

    memcpy(q, p, j);
    q += j;
  }
  if (size > 0)  *q = 0;
  va_end(ap);

  vm_drop();
}

void
m_string_newv(obj_t cl, obj_t a)
{
  unsigned k, n, size;
  obj_t    *p;
  char     *q;

  for (size = 0, p = ARRAY(a)->data, n = ARRAY(a)->size; n; --n, ++p) {
    size += string_len(*p);
  }
  if (size > 0)  ++size;

  vm_push(0);
  
  m_inst_alloc(cl);
  inst_init(R0, size);

  for (q = STRING(R0)->data, p = ARRAY(a)->data, n = ARRAY(a)->size; n; --n, ++p) {
    k = string_len(*p);
    memcpy(q, STRING(*p)->data, k);
    q += k;
  }
  if (size > 0)  *q = 0;

  vm_drop();
}

void
m_string_resize(obj_t str, unsigned new_size)
{
  unsigned n;

  vm_push(0);

  m_inst_alloc(consts.cl.string);
  inst_init(R0, new_size);

  n = string_len(str);
  if (n != 0) {
    ++n;

    HARD_ASSERT(new_size >= n);

    memcpy(STRING(R0)->data, STRING(str)->data, n);
  }

  vm_drop();
}

void
m_string_append_char(obj_t str, unsigned idx, char c)
{
  unsigned n = string_len(str);

  if (idx >= n) {
    m_string_resize(str, n << 1);
  } else {
    vm_assign(0, str);
  }

  STRING(R0)->data[idx] = c;
}

void
cm_string_new(unsigned argc, obj_t args)
{
  obj_t recvr, arg;

  if (argc < 1 || argc > 2)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (inst_of(recvr) != consts.cl.metaclass)  error(ERR_INVALID_ARG, recvr);
  if (argc == 1) {
    m_string_newc(recvr, 0);
    return;
  }
  arg = CAR(CDR(args));

  if (is_kind_of(arg, consts.cl.string)) {
    m_string_newc(recvr, 1, string_len(arg), STRING(arg)->data);

    return;
  }

  m_method_call_1(arg, consts.str.tostring);
}

unsigned
string_len(obj_t s)
{
  unsigned result = STRING(s)->size;

  if (result > 1)  --result;

  return (result);
}

unsigned
string_hash(obj_t s)
{
  return (obj_hash(s, STRING(s)->data, string_len(s)));
}

unsigned
string_equal(obj_t s1, obj_t s2)
{
  unsigned n1 = string_len(s1);
  unsigned n2 = string_len(s2);

  return (n1 == n2
	  && strncmp(STRING(s1)->data, STRING(s2)->data, n1) == 0
	  );
}

void
cm_string_hash(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)                             error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.string))  error(ERR_INVALID_ARG, recvr);
  
  m_integer_new(consts.cl.integer, string_hash(recvr));
}

void
cm_string_equal(unsigned argc, obj_t args)
{
  obj_t recvr, arg;

  if (argc != 2)                             error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.string))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  
  m_boolean_new(consts.cl.boolean,
		inst_of(arg) == inst_of(recvr)
		&& string_equal(arg, recvr)
		);
}

int
string_cmp(obj_t s1, obj_t s2)
{
  unsigned n1 = string_len(s1), n2 = string_len(s2);
  char     *p1 = STRING(s1)->data, *p2 = STRING(s2)->data;

  for ( ; n1 > 0 && n2 > 0; --n1, --n2, ++p1, ++p2) {
    char c1 = *p1, c2 = *p2;

    if (c1 < c2)  return (-1);
    if (c1 > c2)  return (1);
  }
  
  if (n1 < n2)  return (-1);
  if (n1 > n2)  return (1);
  return (0);
}

void
_m_string_pad(obj_t s, struct printf_fmt_info *fi)
{
  unsigned n;

  vm_enter(1);

  vm_assign(0, s);
  n = string_len(R0);

  if (fi) {
    if (fi->flags.prec_valid && fi->prec < n) {
      m_string_newc(consts.cl.string, 1, fi->prec, STRING(R0)->data);
      n = string_len(R0);
    }
    
    if (fi->flags.width_valid && fi->width > n) {
      unsigned d = fi->width - n;
      
      vm_assign(1, R0);
      
      m_inst_alloc(consts.cl.string);
      inst_init(R0, fi->width + 1);
      
      if (fi->flags.left) {
	memcpy(STRING(R0)->data, STRING(R1)->data, n);
	memset(STRING(R0)->data + n, ' ', d);
	STRING(R0)->data[fi->width] = 0;
      } else {
	memset(STRING(R0)->data, ' ', d);
	strcpy(STRING(R0)->data + d, STRING(R1)->data);
      }
    }
  }

  vm_leave(1);
}

int
m_string_pad(obj_t s, struct printf_fmt_info *fi)
{
  if (fi->flags.prec_valid && string_len(s) > (unsigned) fi->prec)  return (-1);

  _m_string_pad(s, fi);

  return (0);
}

unsigned
string_quotify_len(unsigned n, char *s)
{
  unsigned k, np, nq, sf;
  char     c;

  for (sf = np = nq = 0, k = n; k; --k, ++s) {
    c = *s;
    if (!isprint(c)) {
      ++np;
      continue;
    }
    switch (c) {
    case '"':
      ++nq;
      break;
    case ' ':
      sf = 1;
    }
  }
  
  return (n == 0 || np != 0 || nq != 0 || sf
	  ? 1 + n + 3 * np + nq + 1
	  : n
	  );
}

void
m_string_quotify(unsigned n, char *s, unsigned newn)
{
  char *q;

  vm_push(0);

  m_inst_alloc(consts.cl.string);
  inst_init(R0, newn + 1);
  q = STRING(R0)->data;
  *q++ = '"';
  for (; n; --n, ++s) {
    char c = *s;

    if (!isprint(c)) {
      sprintf(q, "\\x%02x", c);
      q += 4;
      
      continue;
    }
    if (c == '"')  *q++ = '\\';
    *q++ = c;
  }
  *q++ = '"';

  vm_drop();
}

void
m_string_tostring(obj_t str, struct printf_fmt_info *fi)
{
  unsigned n, nn;

  vm_push(0);

  n  = string_len(str);
  nn = string_quotify_len(n, STRING(str)->data);

  if (nn != n) {
    if (fi && fi->flags.prec_valid) {
      while (nn > (unsigned) fi->prec) {
	--n;
	nn = string_quotify_len(n, STRING(str)->data);
      }
    }
    
    if (nn != n) {
      m_string_quotify(n, STRING(str)->data, nn);
    } else {
      vm_assign(0, str);
    }
  } else {
    vm_assign(0, str);
  }

  _m_string_pad(R0, fi);

  vm_drop();
}

void
cm_string_tostring(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)                             error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.string))  error(ERR_INVALID_ARG, recvr);

  m_string_tostring(recvr, 0);
}

void
cm_string_tostring_fmt(unsigned argc, obj_t args)
{
  obj_t                  recvr, arg;
  unsigned               n;
  struct printf_fmt_info fi[1];

  if (argc != 2)                             error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.string))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  if (!is_kind_of(arg, consts.cl.string))  error(ERR_INVALID_ARG, arg);
  n = string_len(arg);
  if (printf_fmt_parse(n, STRING(arg)->data, fi) < 0)  error(ERR_INVALID_ARG, arg);
  switch (fi->mode) {
  case 's':
  case 'O':
    break;
  default:
    error(ERR_INVALID_ARG, arg);
  }

  m_string_tostring(recvr, fi);
}

void
cm_string_append(unsigned argc, obj_t args)
{
  obj_t recvr, arg;

  if (argc != 2)                             error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.string))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  if (!is_kind_of(arg, consts.cl.string))    error(ERR_INVALID_ARG, arg);

  m_string_newc(inst_of(recvr), 2, string_len(recvr), STRING(recvr)->data,
	                           string_len(arg), STRING(arg)->data
		);
}

void
cm_string_eval(unsigned argc, obj_t args)
{
  if (argc != 1) error(ERR_NUM_ARGS);

  m_method_call_2(consts.str.atc, consts.cl.env, CAR(args));
}

void
m_string_fprint(obj_t s, obj_t fi)
{
  if (string_len(s) == 0)  return;

  fputs(STRING(s)->data, _FILE(fi)->fp);
}

void
m_string_print(obj_t s)
{
  vm_push(0);
  
  m_file_stdout();
  m_string_fprint(s, R0);
  
  vm_drop();
}

void
cm_string_print(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)                             error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.string))  error(ERR_INVALID_ARG, recvr);
  
  m_string_print(recvr);

  vm_assign(0, recvr);
}

void
cm_string_printc(unsigned argc, obj_t args)
{
  obj_t recvr, arg;

  if (argc != 2)                             error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.string))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  if (!is_kind_of(arg, consts.cl.file))      error(ERR_INVALID_ARG, arg);
  
  m_string_fprint(recvr, arg);
  
  vm_assign(0, recvr);
}

void
cm_string_len(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)                             error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.string))  error(ERR_INVALID_ARG, recvr);
  
  m_integer_new(consts.cl.integer, string_len(recvr));
}

void
slice_idxs(int size, int *start, int *len)
{
  int end;

  if (*start < 0)  *start = size + *start;
  end = *start + *len;

  if (end < *start) {
    int temp;

    temp   = *start;
    *start = end + 1;
    end    = temp + 1;
  }

  if (end < 0 || *start >= size) {
    *start = *len = 0;

    return;
  }

  if (*start < 0)  *start = 0;
  if (end > size)  end = size;
  *len = end - *start;
}

int
m_string_substr(obj_t s, int ofs, int len)
{
  slice_idxs(string_len(s), &ofs, &len);

  if (len == 0)  return (-1);

  m_string_newc(inst_of(s), 1, len, &STRING(s)->data[ofs]);

  return (0);
}

void
cm_string_at(unsigned argc, obj_t args)
{
  obj_t recvr, arg;

  if (argc != 2)                             error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.string))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  if (!is_kind_of(arg, consts.cl.integer))   error(ERR_INVALID_ARG, arg);

  if (m_string_substr(recvr, INTEGER(arg)->val, 1) < 0)  error(ERR_IDX_RANGE, arg);
}

void
cm_string_at_len(unsigned argc, obj_t args)
{
  obj_t         recvr, arg1, arg2;
  integer_val_t len;

  if (argc != 3)                              error(ERR_NUM_ARGS);
  recvr = CAR(args);  args = CDR(args);
  if (!is_kind_of(recvr, consts.cl.string))   error(ERR_INVALID_ARG, recvr);
  arg1 = CAR(args);  args = CDR(args);
  if (!is_kind_of(arg1, consts.cl.integer))   error(ERR_INVALID_ARG, arg1);
  arg2 = CAR(args);
  if (!is_kind_of(arg2, consts.cl.integer))   error(ERR_INVALID_ARG, arg2);
  len = INTEGER(arg2)->val;
  if (len < 0)                                error(ERR_INVALID_ARG, arg2);

  if (m_string_substr(recvr, INTEGER(arg1)->val, len) < 0)  error(ERR_IDX_RANGE_2, arg1, arg2);
}

void
cm_string_asc(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!(is_kind_of(recvr, consts.cl.string) && string_len(recvr) > 0))  error(ERR_INVALID_ARG, recvr);

  m_integer_new(consts.cl.integer, STRING(recvr)->data[0]);
}

void
cm_string_foreach(unsigned argc, obj_t args)
{
  obj_t    recvr, cl, arg, *p;
  char     *s;
  unsigned n;

  if (argc != 2)                             error(ERR_NUM_ARGS);
  recvr = CAR(args);
  cl = inst_of(recvr);
  if (!is_subclass_of(cl, consts.cl.string))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));

  vm_enter(1);
  
  vm_assign(1, NIL);
  for (p = &R1, s = STRING(recvr)->data, n = string_len(recvr); n; --n, ++s) {
    m_string_newc(cl, 1, 1, s);
    m_dptr_new(consts.cl.list, R0, NIL);
    m_method_call_2(consts.str.evalc, arg, R0);
    m_dptr_new(consts.cl.list, R0, NIL);
    OBJ_ASSIGN(*p, R0);
    p = &CDR(R0);
  }
  vm_assign(0, R1);
  
  vm_leave(1);
}

int
string_index(obj_t s1, obj_t s2, unsigned ofs, int dir)
{
    char     *p, c = STRING(s2)->data[0];
    unsigned n = string_len(s1), i;

    i = (dir < 0) ? n - 1 - ofs : ofs;
    n -= i;
    for (p = STRING(s1)->data + i; n; --n, p += dir, i += dir) {
	if (*p == c)  return ((int) i);
    }
    return (-1);
}

void
cm_string_index(unsigned argc, obj_t args)
{
  obj_t recvr, arg;

  if (argc != 2)                             error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.string))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  if (!is_kind_of(arg, consts.cl.string))    error(ERR_INVALID_ARG, arg);

  m_integer_new(consts.cl.integer, string_index(recvr, arg, 0, 1));
}

void
cm_string_rindex(unsigned argc, obj_t args)
{
  obj_t recvr, arg;

  if (argc != 2)                             error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.string))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  if (!is_kind_of(arg, consts.cl.string))    error(ERR_INVALID_ARG, arg);

  m_integer_new(consts.cl.integer, string_index(recvr, arg, 0, -1));
}

void
cm_string_split(unsigned argc, obj_t args)
{
  obj_t    recvr, cl, arg, *p;
  unsigned ofs, recvr_len;
    
  if (argc != 2)                             error(ERR_NUM_ARGS);
  recvr = CAR(args);
  cl = inst_of(recvr);
  if (!is_subclass_of(cl, consts.cl.string))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  if (!is_kind_of(arg, consts.cl.string))    error(ERR_INVALID_ARG, arg);

  vm_enter(1);
  
  vm_assign(1, NIL);
  recvr_len = string_len(recvr);
  for (p = &R1, ofs = 0; ofs < recvr_len; ) {
    int      i = string_index(recvr, arg, ofs, 1);
    unsigned n = (i < 0) ? recvr_len - ofs : (unsigned) i - ofs;
    
    m_string_newc(cl, 1, n, STRING(recvr)->data + ofs);
    m_dptr_new(consts.cl.list, R0, NIL);
    OBJ_ASSIGN(*p, R0);
    p = &CDR(R0);
    ofs += n + 1;
  }
  vm_assign(0, R1);
  
  vm_leave(1);
}

void
cm_string_compare(unsigned argc, obj_t args)
{
  obj_t recvr, arg;

  if (argc != 2)                             error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.string))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  if (!is_kind_of(arg, consts.cl.string))    error(ERR_INVALID_ARG, arg);
  
  m_integer_new(consts.cl.integer, string_cmp(recvr, arg));
}

void
cm_string_load(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)                             error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.string))  error(ERR_INVALID_ARG, recvr);

  vm_push(0);

  FRAME_INPUT_BEGIN(0, 0, STRING(recvr)->data) {
    for (;;) {
      yyparse();
      
      if (yy_input_eof())  break;

      vm_dropn(frp->sp - sp); /* Discard all objs pushed during parsing */
      
      m_method_call_1(consts.str.eval, R0);
    }
  } FRAME_INPUT_END;

  vm_drop();
}

void
cm_string_member(unsigned argc, obj_t args)
{
  obj_t recvr, arg;

  if (argc != 2)                             error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.string))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  if (!(is_kind_of(arg, consts.cl.string) && string_len(arg) == 1)) {
    error(ERR_INVALID_ARG, arg);
  }

  m_boolean_new(consts.cl.boolean, string_index(recvr, arg, 0, 1) != -1);
}

/***************************************************************************/

/* Class: Dptr */

void
inst_init_dptr(obj_t cl, obj_t inst, va_list ap)
{
  OBJ_ASSIGN(CAR(inst), va_arg(ap, obj_t));
  OBJ_ASSIGN(CDR(inst), va_arg(ap, obj_t));

  inst_init_parent(cl, inst, ap);
}

void
inst_walk_dptr(obj_t cl, obj_t inst, void (*func)(obj_t))
{
  (*func)(CAR(inst));
  (*func)(CDR(inst));

  inst_walk_parent(cl, inst, func);  
}

void
inst_free_dptr(obj_t cl, obj_t inst)
{
  inst_free_parent(cl, inst);  
}

void
cm_dptr_car(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)                           error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.dptr))  error(ERR_INVALID_ARG, recvr);

  vm_assign(0, CAR(recvr));
}

void
cm_dptr_cdr(unsigned argc, obj_t args)
{
  obj_t recvr, result;

  if (argc != 1)                           error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (recvr == NIL) {
    result = NIL;
  } else {
    if (!is_kind_of(recvr, consts.cl.dptr))  error(ERR_INVALID_ARG, recvr);
    result = CDR(recvr);
  }

  vm_assign(0, result);
}

void
cm_dptr_hash(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)                           error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.dptr))  error(ERR_INVALID_ARG, recvr);
  
  vm_enter(1);
  
  m_method_call_1(consts.str.hash, CAR(recvr));
  vm_assign(1, R0);
  m_method_call_1(consts.str.hash, CDR(recvr));
  m_integer_new(consts.cl.integer, INTEGER(R1)->val + INTEGER(R0)->val);
  
  vm_leave(1);
}

void
cm_dptr_equals(unsigned argc, obj_t args)
{
  obj_t recvr, arg;

  if (argc != 2)                           error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.dptr))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  if (!is_kind_of(arg, consts.cl.dptr)) {
    m_boolean_new(consts.cl.boolean, 0);
    return;
  }

  vm_enter(1);
  
  m_method_call_2(consts.str.equalsc, CAR(recvr), CAR(arg));
  vm_assign(1, R0);
  m_method_call_2(consts.str.equalsc, CDR(recvr), CDR(arg));
  m_boolean_new(consts.cl.boolean, BOOLEAN(R1)->val && BOOLEAN(R0)->val);

  vm_leave(1);
}

/***************************************************************************/

/* Class: Pair */

void
inst_init_pair(obj_t cl, obj_t inst, va_list ap)
{
  inst_init_parent(cl, inst, ap);
}

void
inst_walk_pair(obj_t cl, obj_t inst, void (*func)(obj_t))
{
  inst_walk_parent(cl, inst, func);  
}

void
inst_free_pair(obj_t cl, obj_t inst)
{
  inst_free_parent(cl, inst);  
}

void
m_pair_new(obj_t car, obj_t cdr)
{
  vm_push(0);

  m_inst_alloc(consts.cl.pair);
  inst_init(R0, car, cdr);

  vm_drop();
}

void
cm_pair_new(unsigned argc, obj_t args)
{
  obj_t recvr, arg, car = NIL, cdr = NIL;

  if (argc < 1 || argc > 2)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (inst_of(recvr) != consts.cl.metaclass)  error(ERR_INVALID_ARG, recvr);

  if (argc == 2) {
    arg = CAR(CDR(args));
    
    if (is_kind_of(arg, consts.cl.dptr)) {
      car = CAR(arg);
      cdr = CDR(arg);
    } else {
      error(ERR_INVALID_ARG, arg);
    }
  }

  m_dptr_new(recvr, car, cdr);
}

void
cm_pair_eval(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)                           error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.pair))  error(ERR_INVALID_ARG, recvr);

  vm_enter(1);
  
  m_method_call_1(consts.str.eval, CAR(recvr));
  vm_assign(1, R0);
  m_method_call_1(consts.str.eval, CDR(recvr));
  m_pair_new(R1, R0);
  
  vm_leave(1);
}

void
m_pair_tostring(obj_t pr)
{
  vm_enter(1);
  
  m_method_call_1(consts.str.tostring, CAR(pr));
  vm_assign(1, R0);
  m_method_call_1(consts.str.tostring, CDR(pr));
  m_string_newc(consts.cl.string, 5, 1, "(",
	                             string_len(R1), STRING(R1)->data,
	                             2, ", ",
	                             string_len(R0), STRING(R0)->data,
	                             1, ")"
		);
  
  vm_leave(1);
}

void
cm_pair_tostring(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)                           error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.pair))  error(ERR_INVALID_ARG, recvr);

  m_pair_tostring(recvr);
}

void
cm_pair_tostring_fmt(unsigned argc, obj_t args)
{
  obj_t    recvr, arg;
  unsigned n;
  struct printf_fmt_info fi[1];

  if (argc != 2)                           error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.pair))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  if (!is_kind_of(arg, consts.cl.string))  error(ERR_INVALID_ARG, arg);
  n = string_len(arg);
  if (printf_fmt_parse(n, STRING(arg)->data, fi) != (int) n
      || fi->mode != 'O'
      )  error(ERR_INVALID_ARG, arg);
  
  vm_push(0);

  m_pair_tostring(recvr);

  if (m_string_pad(R0, fi) < 0)  error(ERR_INVALID_ARG, arg);

  vm_drop();
}

void
cm_pair_at(unsigned argc, obj_t args)
{
  obj_t recvr, arg, result = NIL;

  if (argc != 2)                            error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.pair))   error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  if (!is_kind_of(arg, consts.cl.integer))  error(ERR_INVALID_ARG, arg);

  switch (INTEGER(arg)->val) {
  case 0:
    result = CAR(recvr);
    break;
  case 1:
    result = CDR(recvr);
    break;
  default:
    error(ERR_IDX_RANGE, arg);
  }
  
  vm_assign(0, result);
}

/***************************************************************************/

/* Class: List */

void
inst_init_list(obj_t cl, obj_t inst, va_list ap)
{
  inst_init_parent(cl, inst, ap);
}

void
inst_walk_list(obj_t cl, obj_t inst, void (*func)(obj_t))
{
  inst_walk_parent(cl, inst, func);  
}

void
inst_free_list(obj_t cl, obj_t inst)
{
  inst_free_parent(cl, inst);  
}

void
_list_concat(obj_t *li, obj_t el)
{
  obj_t p;

  for (; p = *li; li = &CDR(p));

  OBJ_ASSIGN(*li, el);
}

void
m_list_concat(obj_t *li, obj_t el)
{
  vm_push(0);

  m_dptr_new(consts.cl.list, el, NIL);
  _list_concat(li, R0);

  vm_pop(0);
}

void
m_dptr_new(obj_t cl, obj_t car, obj_t cdr)
{
  vm_push(0);

  m_inst_alloc(cl);
  inst_init(R0, car, cdr);

  vm_drop();
}

unsigned
list_len(obj_t list)
{
    unsigned result;

    for (result = 0; list; list = CDR(list), ++result) {
      ASSERT(inst_of(list) == consts.cl.list);
    }

    return (result);
}

void
cm_list_new(unsigned argc, obj_t args)
{
  obj_t recvr, arg;

  if (argc < 1 || argc > 2)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (inst_of(recvr) != consts.cl.metaclass)  error(ERR_INVALID_ARG, recvr);

  if (argc == 1) {
    vm_assign(0, NIL);
    return;
  }
  
  arg = CAR(CDR(args));

  if (arg == NIL) {
    vm_assign(0, arg);
    
    return;
  }
  if (is_kind_of(arg, consts.cl.dict)) {
    obj_t    *p, *q, r, s;
    unsigned n;
    
    vm_enter(1);
    
    vm_assign(1, NIL);
    for (p = &R1, q = DICT(arg)->base->base->data, n = DICT(arg)->base->base->size; n; --n, ++q) {
      for (r = *q; r; r = CDR(r)) {
	s = CAR(r);
	m_dptr_new(consts.cl.pair, CAR(s), CDR(s));
	m_dptr_new(recvr, R0, NIL);
	OBJ_ASSIGN(*p, R0);
	p = &CDR(R0);
      }
    }
    vm_assign(0, R1);
    
    vm_leave(1);

    return;
  }
  if (is_kind_of(arg, consts.cl.set)) {
    obj_t    *p, *q, r;
    unsigned n;
    
    vm_enter(1);
    
    vm_assign(1, NIL);
    for (p = &R1, q = SET(arg)->base->data, n = SET(arg)->base->size; n; --n, ++q) {
      for (r = *q; r; r = CDR(r)) {
	m_dptr_new(recvr, CAR(r), NIL);
	OBJ_ASSIGN(*p, R0);
	p = &CDR(R0);
      }
    }
    vm_assign(0, R1);
    
    vm_leave(1);

    return;
  }
  if (is_kind_of(arg, consts.cl.array)) {
    obj_t    *p, *q;
    unsigned n;

    vm_enter(1);
    
    vm_assign(1, NIL);
    for (p = &R1, q = ARRAY(arg)->data, n = ARRAY(arg)->size; n; --n, ++q) {
      m_dptr_new(recvr, *q, NIL);
      OBJ_ASSIGN(*p, R0);
      p = &CDR(R0);
    }
    vm_assign(0, R1);

    vm_leave(1);

    return;
  }
  if (is_kind_of(arg, consts.cl.list)) {
    vm_assign(0, arg);
    return;
  }
  if (is_kind_of(arg, consts.cl.pair)) {
    m_dptr_new(recvr, CDR(arg), NIL);
    m_dptr_new(recvr, CAR(arg), R0);

    return;
  }

  error(ERR_INVALID_ARG, arg);
}

void
cm_list_len(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_list(recvr))  error(ERR_INVALID_ARG, recvr);

  m_integer_new(consts.cl.integer, list_len(recvr));
}

void
m_list_tostr(obj_t list, char *delim)
{
  obj_t    *q;
  unsigned i, n;
  
  vm_enter(1);
  
  n = list_len(list);
  m_array_new(consts.cl.array, 2 + (n == 0 ? 0 : 2 * n - 1));
  vm_assign(1, R0);
  q = ARRAY(R1)->data;

  m_string_newc(consts.cl.string, 1, 1, &delim[0]);
  OBJ_ASSIGN(*q, R0);
  ++q;
  for (i = 0; list; list = CDR(list), ++i) {
    if (i > 0) {
      OBJ_ASSIGN(*q, consts.str.space);
      ++q;
    }

    m_method_call_1(consts.str.tostring, CAR(list));
    OBJ_ASSIGN(*q, R0);
    ++q;
  }
  m_string_newc(consts.cl.string, 1, 1, &delim[1]);
  OBJ_ASSIGN(*q, R0);
  
  m_string_newv(consts.cl.string, R1);
  
  vm_leave(1);
}

void
cm_list_tostring(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_list(recvr))  error(ERR_INVALID_ARG, recvr);

  m_list_tostr(recvr, "()");
}

void
cm_list_tostring_fmt(unsigned argc, obj_t args)
{
  obj_t    recvr, arg;
  unsigned n;
  struct printf_fmt_info fi[1];

  if (argc != 2)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_list(recvr))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  if (!is_kind_of(arg, consts.cl.string))  error(ERR_INVALID_ARG, recvr);
  n = string_len(arg);
  if (printf_fmt_parse(n, STRING(arg)->data, fi) != (int) n
      || fi->mode != 'O'
      )  error(ERR_INVALID_ARG, recvr);

  vm_push(0);

  m_list_tostr(recvr, "()");

  if (m_string_pad(R0, fi) < 0)  error(ERR_INVALID_ARG, arg);

  vm_drop();
}

void
m_list_eval(obj_t list)
{
  obj_t *q;
  
  vm_enter(1);
  
  vm_assign(1, NIL);
  for (q = &R1; list; list = CDR(list)) {
    m_method_call_1(consts.str.eval, CAR(list));
    m_dptr_new(consts.cl.list, R0, NIL);
    OBJ_ASSIGN(*q, R0);
    q = &CDR(R0);
  }
  vm_assign(0, R1);
  
  vm_leave(1);
}

void
cm_list_eval(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_list(recvr))  error(ERR_INVALID_ARG, recvr);

  m_list_eval(recvr);
}

void
cm_list_map(unsigned argc, obj_t args)
{
  obj_t recvr, arg, *p, q;

  if (argc != 2)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_list(recvr))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));

  vm_enter(1);

  vm_assign(1, NIL);
  for (p = &R1, q = recvr; q; q = CDR(q)) {
    m_method_call_2(consts.str.evalc, arg, CAR(q));
    m_dptr_new(consts.cl.list, R0, NIL);
    OBJ_ASSIGN(*p, R0);
    p = &CDR(R0);
  }
  vm_assign(0, R1);
  
  vm_leave(1);
}

void
cm_list_foreach(unsigned argc, obj_t args)
{
  obj_t recvr, arg, *p, q;

  if (argc != 2)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_list(recvr))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));

  vm_enter(1);

  vm_assign(1, NIL);
  for (p = &R1, q = recvr; q; q = CDR(q)) {
    m_dptr_new(consts.cl.list, CAR(q), NIL);
    m_method_call_2(consts.str.evalc, arg, R0);
    m_dptr_new(consts.cl.list, R0, NIL);
    OBJ_ASSIGN(*p, R0);
    p = &CDR(R0);
  }
  vm_assign(0, R1);
  
  vm_leave(1);
}

void
cm_list_splice(unsigned argc, obj_t args)
{
  obj_t recvr, arg;

  if (argc != 2)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_list(recvr))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  if (!is_kind_of(arg, consts.cl.string))  error(ERR_INVALID_ARG, arg);

  vm_enter(1);

  m_string_newc(consts.cl.string, 0);
  vm_assign(1, R0);
  for ( ; recvr; recvr = CDR(recvr)) {
    m_method_call_1(consts.str.tostring, CAR(recvr));
    if (string_len(R1) != 0) {
      m_string_newc(consts.cl.string, 3, string_len(R1), STRING(R1)->data,
		                        string_len(arg), STRING(arg)->data,
		                        string_len(R0), STRING(R0)->data
		   );
    }
    vm_assign(1, R0);
  }
  vm_assign(0, R1);
  
  vm_leave(1);
}

void
cm_list_append(unsigned argc, obj_t args)
{
  obj_t recvr, arg, *p, q;

  if (argc != 2)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_list(recvr))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  if (!is_list(arg))    error(ERR_INVALID_ARG, arg);
  
  vm_enter(1);
  
  vm_assign(1, NIL);
  for (p = &R1, q = recvr; q; q = CDR(q)) {
    m_dptr_new(consts.cl.list, CAR(q), NIL);
    OBJ_ASSIGN(*p, R0);
    p = &CDR(R0);
  }
  OBJ_ASSIGN(*p, arg);
  vm_assign(0, R1);
  
  vm_leave(1);
}
 
void
cm_list_hash(unsigned argc, obj_t args)
{
  obj_t         recvr;
  integer_val_t h;

  if (argc != 1)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_list(recvr))  error(ERR_INVALID_ARG, recvr);

  vm_push(0);
  
  for (h = 0; recvr; recvr = CDR(recvr)) {
    m_method_call_1(consts.str.hash, CAR(recvr));
    h += INTEGER(R0)->val;
  }
  m_integer_new(consts.cl.integer, h);
  
  vm_drop();
}

void
cm_list_equals(unsigned argc, obj_t args)
{
  obj_t    recvr, arg;
  unsigned f;

  if (argc != 2)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_list(recvr))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  if (!is_list(arg)) {
    f = 0;
  } else {
    vm_push(0);
    
    for (f = 1;
	 f && recvr && arg;
	 recvr = CDR(recvr), arg = CDR(arg)
	 ) {
      m_method_call_2(consts.str.equalsc, CAR(recvr), CAR(arg));
      f = BOOLEAN(R0)->val;
    }

    vm_drop();
  }

  m_boolean_new(consts.cl.boolean, f && recvr == 0 && arg == 0);
}

void
cm_list_at(unsigned argc, obj_t args)
{
  obj_t       recvr, arg, p;
  int         i, n;

  if (argc != 2)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_list(recvr))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  if (!is_kind_of(arg, consts.cl.integer))  error(ERR_INVALID_ARG, arg);
  i   = INTEGER(arg)->val;
  n   = 1;

  slice_idxs((int) list_len(recvr), &i, &n);

  if (n == 0)  error(ERR_IDX_RANGE, arg);

  for (p = recvr; i; p = CDR(p), --i);

  vm_assign(0, CAR(p));
}

void
cm_list_at_len(unsigned argc, obj_t args)
{
  obj_t recvr, idx, len, p, *q;
  int   i, n;

  if (argc != 3)  error(ERR_NUM_ARGS);
  recvr = CAR(args);  args  = CDR(args);
  if (!is_list(recvr))  error(ERR_INVALID_ARG, recvr);
  idx  = CAR(args);  args = CDR(args);
  if (!is_kind_of(idx, consts.cl.integer))  error(ERR_INVALID_ARG, idx);
  len = CAR(args);
  if (!is_kind_of(len, consts.cl.integer))  error(ERR_INVALID_ARG, len);
  i   = INTEGER(idx)->val;
  n   = INTEGER(len)->val;

  slice_idxs((int) list_len(recvr), &i, &n);

  if (n == 0)  error(ERR_IDX_RANGE_2, idx, len);

  vm_enter(1);

  vm_assign(1, NIL);
  for (q = &R1, p = recvr; n; p = CDR(p)) {
    if (i > 0) {
      --i;
      continue;
    }
    
    m_dptr_new(consts.cl.list, CAR(p), NIL);
    OBJ_ASSIGN(*q, R0);
    q = &CDR(R0);
    --n;
  }
  vm_assign(0, R1);

  vm_leave(1);
}

void
cm_list_filter(unsigned argc, obj_t args)
{
  obj_t recvr, arg, *p;

  if (argc != 2)                           error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_list(recvr))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  if (!is_list(arg))    error(ERR_INVALID_ARG, arg);

  vm_enter(1);

  vm_assign(1, NIL);
  for (p = &R1; recvr && arg; recvr = CDR(recvr), arg = CDR(arg)) {
    obj_t f = CAR(arg);

    if (!is_kind_of(f, consts.cl.boolean))  error(ERR_INVALID_ARG, arg);
    if (!BOOLEAN(f)->val)  continue;
    
    m_dptr_new(consts.cl.list, CAR(recvr), NIL);
    OBJ_ASSIGN(*p, R0);
    p = &CDR(R0);
  }
  vm_assign(0, R1);

  vm_leave(1);
}

void
cm_list_reduce(unsigned argc, obj_t args)
{
  obj_t recvr, func, p;

  if (argc != 3)  error(ERR_NUM_ARGS);
  recvr = CAR(args);  args = CDR(args);
  if (!is_list(recvr))  error(ERR_INVALID_ARG, recvr);
  func = CAR(args);  args = CDR(args);

  vm_enter(1);
  
  vm_assign(1, CAR(args));
  for (p = recvr; p; p = CDR(p)) {
    m_dptr_new(consts.cl.list, CAR(p), NIL);
    m_dptr_new(consts.cl.list, R1, R0);
    m_method_call_2(consts.str.evalc, func, R0);
    vm_assign(1, R0);
  }
  vm_assign(0, R1);
  
  vm_leave(1);
}

void
cm_list_format(unsigned argc, obj_t args)
{
  obj_t recvr, arg;

  if (argc != 2)                           error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_list(recvr))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  if (!is_list(arg))    error(ERR_INVALID_ARG, arg);

  vm_enter(1);

  m_string_newc(consts.cl.string, 0);
  vm_assign(1, R0);
  for ( ; recvr; recvr = CDR(recvr)) {
    obj_t    s = CAR(recvr);
    unsigned n;

    if (!is_kind_of(s, consts.cl.string))  error(ERR_INVALID_ARG, recvr);

    n = string_len(s);
    if (n >= 2 && STRING(s)->data[0] == '%') {
      switch (STRING(s)->data[1]) {
      case '*':
	{
	  obj_t p;

	  if (arg == NIL)  error(ERR_NUM_ARGS);
	  p = CAR(arg);
	  if (!is_kind_of(p, consts.cl.pair))  error(ERR_INVALID_ARG, arg);

	  m_method_call_2(consts.str.tostringc, CAR(p), CDR(p));
	  m_string_newc(consts.cl.string, 2, string_len(R1), STRING(R1)->data,
		                            string_len(R0), STRING(R0)->data
		       );
	  arg = CDR(arg);
	}
	break;
      case '%':
	m_string_newc(consts.cl.string, 2, string_len(R1), STRING(R1)->data,
		                          n - 1, STRING(s)->data + 1
		     );
	break;
      default:
	if (arg == NIL)  error(ERR_NUM_ARGS);

	m_method_call_2(consts.str.tostringc, CAR(arg), s);
	m_string_newc(consts.cl.string, 2, string_len(R1), STRING(R1)->data,
		                          string_len(R0), STRING(R0)->data
		     );
	arg = CDR(arg);
      }
    } else {
      m_string_newc(consts.cl.string, 2, string_len(R1), STRING(R1)->data,
		                        n, STRING(s)->data
		   );
    }
    
    vm_assign(1, R0);
  }

  vm_assign(0, R1);

  vm_leave(1);
}

void
cm_list_cond(unsigned argc, obj_t args)
{
  obj_t recvr, p, q;

  if (argc != 1)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_list(recvr))  error(ERR_INVALID_ARG, recvr);

  vm_push(0);

  vm_assign(0, NIL);
  for (p = recvr; p; p = CDR(p)) {
    q = CAR(p);
    if (!is_kind_of(q, consts.cl.pair))  error(ERR_INVALID_ARG, recvr);

    m_method_call_1(consts.str.eval, CAR(q));
    if (!is_kind_of(R0, consts.cl.boolean))  error(ERR_INVALID_ARG, recvr);
    if (BOOLEAN(R0)->val) {
      m_method_call_1(consts.str.eval, CDR(q));

      break;
    }
  }

  vm_drop();
}

void
cm_list_member(unsigned argc, obj_t args)
{
  obj_t    recvr, arg;
  unsigned f;

  if (argc != 2)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_list(recvr))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));

  vm_push(0);

  for (f = 0; !f && recvr; recvr = CDR(recvr)) {
    m_method_call_2(consts.str.equalsc, CAR(recvr), arg);
    if (!is_kind_of(R0, consts.cl.boolean)) {
      error(ERR_INVALID_VALUE, R0);
    }
    f = BOOLEAN(R0)->val;
  }

  m_boolean_new(consts.cl.boolean, f);

  vm_drop();
}

/***************************************************************************/

/* Class: Method call */

void
inst_init_method_call(obj_t cl, obj_t inst, va_list ap)
{
  OBJ_ASSIGN(METHOD_CALL(inst)->sel, va_arg(ap, obj_t));
  OBJ_ASSIGN(METHOD_CALL(inst)->args, va_arg(ap, obj_t));
  METHOD_CALL(inst)->argc = list_len(METHOD_CALL(inst)->args);

  inst_init_parent(cl, inst, ap);
}

void
inst_walk_method_call(obj_t cl, obj_t inst, void (*func)(obj_t))
{
  (*func)(METHOD_CALL(inst)->sel);
  (*func)(METHOD_CALL(inst)->args);

  inst_walk_parent(cl, inst, func);
}

void
inst_free_method_call(obj_t cl, obj_t inst)
{
  inst_free_parent(cl, inst);
}

void
m_method_call_new(obj_t cl, obj_t sel, obj_t args)
{
  vm_push(0);

  m_inst_alloc(cl);
  inst_init(R0, sel, args);

  vm_drop();
}

void
cm_method_call_new(unsigned argc, obj_t args)
{
  obj_t recvr, arg, sel, nargs;

  if (argc != 2)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (inst_of(recvr) != consts.cl.metaclass)  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  if (!is_kind_of(arg, consts.cl.pair))  error(ERR_INVALID_ARG, arg);
  sel = CAR(arg);
  if (!is_kind_of(sel, consts.cl.string))  error(ERR_INVALID_ARG, arg);
  nargs = CDR(arg);
  if (!is_kind_of(nargs, consts.cl.list))  error(ERR_INVALID_ARG, arg);
  
  m_method_call_new(recvr, sel, nargs);
}

void
cm_method_call_eval(unsigned argc, obj_t args)
{
  obj_t recvr, sel;
  
  if (argc != 1)                                  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.method_call))  error(ERR_INVALID_ARG, recvr);

  vm_push(0);

  sel = METHOD_CALL(recvr)->sel;
  if (STRING(sel)->data[0] == '&') {
    vm_assign(0, METHOD_CALL(recvr)->args);
  } else {
    m_list_eval(METHOD_CALL(recvr)->args);
  }

  m_method_call(sel, METHOD_CALL(recvr)->argc, R0);

  if (string_len(sel) > 2 && strncmp(STRING(sel)->data, "&&", 2) == 0) {
    m_method_call_1(consts.str.eval, R0);
  }

  vm_drop();
}

void
m_method_call_tostring(obj_t mc)
{
  obj_t    p, *q;
  char     *s, *t;
  unsigned n, k;

  if (METHOD_CALL(mc)->argc == 1 && strcmp(STRING(METHOD_CALL(mc)->sel)->data, "&quote") == 0) {
    p = CAR(METHOD_CALL(mc)->args);
    
    if (is_kind_of(p, consts.cl.string)) {
      m_string_newc(consts.cl.string, 3, 1, "\"",
		                        string_len(p), STRING(p)->data,
		                        1,  "\""
		   );
      return;
    }
    
    m_method_call_1(consts.str.tostring, p);

    m_string_newc(consts.cl.string, 2, 1, "'",
		                      string_len(R0), STRING(R0)->data
		 );

    return;
  }

  vm_enter(1);

  vm_assign(1, NIL);
  for (n = 0, q = &R1, s = STRING(METHOD_CALL(mc)->sel)->data, p = METHOD_CALL(mc)->args; ; ++n) {
    if (n & 1) {
      t = index(s, ':');
      k = t ? (unsigned)(t + 1 - s) : (unsigned) strlen(s);

      m_string_newc(consts.cl.string, 1, k, s);
      m_dptr_new(consts.cl.list, R0, NIL);

      s += k;
    } else {
      m_dptr_new(consts.cl.list, CAR(p), NIL);
      
      p = CDR(p);
    }

    OBJ_ASSIGN(*q, R0);
    q = &CDR(R0);

    if (n > 0 && p == NIL)  break;
  }
  vm_assign(0, R1);

  vm_leave(1);
  
  m_list_tostr(R0, "[]");
}

void
cm_method_call_tostring(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)                                  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.method_call))  error(ERR_INVALID_ARG, recvr);

  m_method_call_tostring(recvr);
}

void
cm_method_call_tostring_fmt(unsigned argc, obj_t args)
{
  obj_t    recvr, arg;
  unsigned n;
  struct printf_fmt_info fi[1];

  if (argc != 2)                                  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.method_call))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  if (!is_kind_of(arg, consts.cl.string))  error(ERR_INVALID_ARG, recvr);
  n = string_len(arg);
  if (printf_fmt_parse(n, STRING(arg)->data, fi) != (int) n
      || fi->mode != 'O'
      )  error(ERR_INVALID_ARG, recvr);

  vm_push(0);

  m_method_call_tostring(recvr);

  if (m_string_pad(R0, fi) < 0)  error(ERR_INVALID_ARG, arg);

  vm_drop();
}

void
cm_method_call_sel(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)                                  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.method_call))  error(ERR_INVALID_ARG, recvr);

  vm_assign(0, METHOD_CALL(recvr)->sel);
}

void
cm_method_call_args(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)                                  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.method_call))  error(ERR_INVALID_ARG, recvr);

  vm_assign(0, METHOD_CALL(recvr)->args);
}

/***************************************************************************/

/* Class: Block */

void
inst_init_block(obj_t cl, obj_t inst, va_list ap)
{
  OBJ_ASSIGN(BLOCK(inst)->args, va_arg(ap, obj_t));
  OBJ_ASSIGN(BLOCK(inst)->body, va_arg(ap, obj_t));
  BLOCK(inst)->argc = list_len(BLOCK(inst)->args);

  inst_init_parent(cl, inst, ap);
}

void
inst_walk_block(obj_t cl, obj_t inst, void (*func)(obj_t))
{
  (*func)(BLOCK(inst)->args);
  (*func)(BLOCK(inst)->body);

  inst_walk_parent(cl, inst, func);
}

void
inst_free_block(obj_t cl, obj_t inst)
{
  inst_free_parent(cl, inst);
}

void
m_block_new(obj_t cl, obj_t args, obj_t body)
{
  vm_push(0);

  m_inst_alloc(cl);
  inst_init(R0, args, body);

  vm_drop();
}

void
m_block_eval(obj_t env, obj_t body)
{
  obj_t p;

  vm_push(0);

  FRAME_BLOCK_BEGIN(env) {
    
    if (frame_jmp_code == 0) {
      vm_assign(0, NIL);
      for (p = body; p; p = CDR(p)) {
	m_method_call_1(consts.str.eval, CAR(p));
      }
    }
    
  } FRAME_BLOCK_END;

  vm_drop();
}

void
cm_block_eval(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)  error(ERR_NUM_ARGS);
  recvr = CAR(args);

  if (BLOCK(recvr)->argc != 0) {
    vm_assign(0, recvr);

    return;
  }

  m_block_eval(NIL, BLOCK(recvr)->body);
}

void
cm_block_evalc(unsigned argc, obj_t args)
{
  obj_t recvr, arg, p;

  if (argc != 2)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.block))  error(ERR_INVALID_ARG, recvr);

  arg = CAR(CDR(args));
  if (!is_list(arg))                        error(ERR_INVALID_ARG, arg);
  if (list_len(arg) != BLOCK(recvr)->argc)  error(ERR_NUM_ARGS);
  
  vm_push(0);

  m_string_dict_new(consts.cl.dict, 32);
  for (p = BLOCK(recvr)->args; p; p = CDR(p), arg = CDR(arg)) {
    dict_at_put(R0, CAR(p), CAR(arg));
  }

  m_block_eval(R0, BLOCK(recvr)->body);

  vm_drop();
}

void
m_block_tostring(obj_t blk)
{
  m_dptr_new(consts.cl.list, BLOCK(blk)->args, BLOCK(blk)->body);
  m_list_tostr(R0, "{}");
}

void
cm_block_tostring(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.block))  error(ERR_INVALID_ARG, recvr);

  m_block_tostring(recvr);
}

void
cm_block_tostring_fmt(unsigned argc, obj_t args)
{
  obj_t    recvr, arg;
  unsigned n;
  struct printf_fmt_info fi[1];

  if (argc != 2)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.block))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  if (!is_kind_of(arg, consts.cl.string))  error(ERR_INVALID_ARG, recvr);
  n = string_len(arg);
  if (printf_fmt_parse(n, STRING(arg)->data, fi) != (int) n
      || fi->mode != 'O'
      )  error (ERR_INVALID_ARG, arg);

  vm_push(0);

  m_block_tostring(recvr);

  if (m_string_pad(R0, fi) < 0)  error(ERR_INVALID_ARG, arg);

  vm_drop();
}

/***************************************************************************/

/* Class: Array */

void
inst_init_array(obj_t cl, obj_t inst, va_list ap)
{
  unsigned size = va_arg(ap, unsigned);

  ARRAY(inst)->data = _zcmalloc(size * sizeof(obj_t));
  ARRAY(inst)->size = size;

  inst_init_parent(cl, inst, ap);
}

void
inst_walk_array(obj_t cl, obj_t inst, void (*func)(obj_t))
{
  obj_t    *p;
  unsigned n;

  for (p = ARRAY(inst)->data, n = ARRAY(inst)->size; n; --n, ++p) {
    (*func)(*p);
  }

  inst_walk_parent(cl, inst, func);
}

void
inst_free_array(obj_t cl, obj_t inst)
{
  _cfree(ARRAY(inst)->size * sizeof(obj_t), ARRAY(inst)->data);

  inst_free_parent(cl, inst);
}

void
m_array_new(obj_t cl, unsigned size)
{
  m_inst_alloc(cl);
  inst_init(R0, size);
}

void
cm_array_new(unsigned argc, obj_t args)
{
  obj_t recvr, arg;

  if (argc != 2)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (inst_of(recvr) != consts.cl.metaclass)  error(ERR_INVALID_ARG, recvr);

  arg = CAR(CDR(args));

  if (is_kind_of(arg, consts.cl.integer)) {
    integer_val_t size = INTEGER(arg)->val;

    if (size < 0)  error(ERR_INVALID_ARG, arg);

    m_array_new(recvr, size);
    return;
  }
  if (is_kind_of(arg, consts.cl.dict)) {
    unsigned n;
    obj_t    *p, *q, r, s;

    vm_enter(1);

    m_array_new(recvr, DICT(arg)->base->cnt);
    vm_assign(1, R0);
    for (p = ARRAY(R1)->data, q = DICT(arg)->base->base->data, n = DICT(arg)->base->base->size;
	 n;
	 --n, ++q
	 ) {
      for (r = *q; r; r = CDR(r)) {
	s = CAR(r);
	m_dptr_new(consts.cl.pair, CAR(s), CDR(s));
	OBJ_ASSIGN(*p, R0);
	++p;
      }
    }

    vm_assign(0, R1);

    vm_leave(1);

    return;
  }
  if (is_kind_of(arg, consts.cl.set)) {
    unsigned n;
    obj_t    *p, *q, r;

    vm_push(0);

    m_array_new(recvr, SET(arg)->cnt);
    for (p = ARRAY(R0)->data, q = SET(arg)->base->data, n = SET(arg)->base->size;
	 n;
	 --n, ++q
	 ) {
      for (r = *q; r; r = CDR(r)) {
	OBJ_ASSIGN(*p, CAR(r));
	++p;
      }
    }

    vm_drop();

    return;
  }
  if (is_kind_of(arg, consts.cl.array)) {
    obj_t    *p, *q;
    unsigned n;

    vm_push(0);

    m_array_new(recvr, ARRAY(arg)->size);
    for (p = ARRAY(R0)->data, q = ARRAY(arg)->data, n = ARRAY(arg)->size;
	 n;
	 --n, ++p, ++q
	 ) {
      OBJ_ASSIGN(*p, *q);
    }

    vm_drop();

    return;
  }
  if (is_kind_of(arg, consts.cl.list)) {
    obj_t    *p;
    unsigned n;
    
    vm_push(0);

    m_array_new(recvr, list_len(arg));
    for (p = ARRAY(R0)->data, n = ARRAY(R0)->size; n; --n, ++p, arg = CDR(arg)) {
      OBJ_ASSIGN(*p, CAR(arg));
    }

    vm_drop();

    return;
  }
  
  error(ERR_INVALID_ARG, arg);
}

void
cm_array_at(unsigned argc, obj_t args)
{
  obj_t       recvr, arg;
  integer_val_t idx;

  if (argc != 2)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.array))   error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  if (!is_kind_of(arg, consts.cl.integer))   error(ERR_INVALID_ARG, arg);
  idx = INTEGER(arg)->val;
  if (idx < 0 || idx >= ARRAY(recvr)->size)  error(ERR_IDX_RANGE, arg);

  vm_assign(0, ARRAY(recvr)->data[idx]);
}

void
cm_array_at_put(unsigned argc, obj_t args)
{
  obj_t         recvr, arg;
  integer_val_t idx;

  if (argc != 3)  error(ERR_NUM_ARGS);
  recvr = CAR(args);  args = CDR(args);
  if (!is_kind_of(recvr, consts.cl.array))   error(ERR_INVALID_ARG, recvr);
  arg = CAR(args);  args = CDR(args);
  if (!is_kind_of(arg, consts.cl.integer))   error(ERR_INVALID_ARG, arg);
  idx = INTEGER(arg)->val;
  if (idx < 0 || idx >= ARRAY(recvr)->size)  error(ERR_IDX_RANGE, arg);
  arg = CAR(args);

  OBJ_ASSIGN(ARRAY(recvr)->data[idx], arg);
  
  vm_assign(0, arg);
}

void
m_array_tostr(obj_t a)
{
  obj_t    *p, *q;
  unsigned i, n;

  vm_enter(1);
  
  n = ARRAY(a)->size;
  m_array_new(consts.cl.array, 2 + (n == 0 ? 0 : 2 * n - 1));
  vm_assign(1, R0);
  q = ARRAY(R1)->data;

  m_string_newc(consts.cl.string, 1, 1, "[");
  OBJ_ASSIGN(*q, R0);
  ++q;
  for (i = 0, p = ARRAY(a)->data; n; --n, ++p, ++i) {
    if (i > 0) {
      OBJ_ASSIGN(*q, consts.str.space);
      ++q;
    }

    m_method_call_1(consts.str.tostring, *p);
    OBJ_ASSIGN(*q, R0);
    ++q;
  }
  m_string_newc(consts.cl.string, 1, 1, "]");
  OBJ_ASSIGN(*q, R0);
  
  m_string_newv(consts.cl.string, R1);
  
  vm_leave(1);
}

void
cm_array_tostring(unsigned argc, obj_t args)
{
  obj_t    recvr;

  if (argc != 1)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.array))  error(ERR_INVALID_ARG, recvr);
  
  m_array_tostr(recvr);
}

void
cm_array_tostring_fmt(unsigned argc, obj_t args)
{
  obj_t    recvr, arg;
  unsigned n;
  struct printf_fmt_info fi[1];

  if (argc != 2)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.array))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  if (!is_kind_of(arg, consts.cl.string))  error(ERR_INVALID_ARG, arg);
  n = string_len(arg);
  if (printf_fmt_parse(n, STRING(arg)->data, fi) != (int) n
      || fi->mode != 'O'
      )  error(ERR_INVALID_ARG, arg);

  vm_push(0);

  m_array_tostr(recvr);

  if (m_string_pad(R0, fi) < 0)  error(ERR_INVALID_ARG, arg);

  vm_drop();
}

void
cm_array_size(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.array))  error(ERR_INVALID_ARG, recvr);

  m_integer_new(consts.cl.integer, ARRAY(recvr)->size);
}

int
array_sort_cmp(obj_t obj1, obj_t obj2, obj_t cmp_func)
{
  if (cmp_func) {
    m_dptr_new(consts.cl.list, obj2, NIL);
    m_dptr_new(consts.cl.list, obj1, R0);
    m_method_call_2(consts.str.evalc, cmp_func, R0);
  } else {
    m_method_call_2(consts.str.comparec, obj1, obj2);
  }
  if (!is_kind_of(R0, consts.cl.integer))  error(ERR_INVALID_VALUE, R0);

  return (INTEGER(R0)->val);
}

void
array_sort_insert(obj_t *a, unsigned n, obj_t cmp_func)
{
  unsigned j, k;
  obj_t    *q, *r;

  if (n < 2)  return;

  for (q = &a[j = 1]; j < n; ++j, ++q) {
    OBJ_ASSIGN(R1, *q);

    for (r = q - 1, k = 0; k < j; ++k, --r) {
      if (array_sort_cmp(R1, r[0], cmp_func) >= 0) break;

      OBJ_ASSIGN(r[1], r[0]);
    }

    OBJ_ASSIGN(r[1], R1);
  }
}

void
array_sort_quick(obj_t *a, unsigned n, obj_t cmp_func)
{
  unsigned i, j, s, n1;
  obj_t    *p, *q, *r;

  if (n < 12) {
    array_sort_insert(a, n, cmp_func);
    return;
  }
  
  p = &a[i = 0];
  q = &a[n / 2];
  r = &a[j = n - 1];
  
  OBJ_ASSIGN(R0, *q);
  OBJ_ASSIGN(*q, *r);
  OBJ_ASSIGN(*r, R0);

  for (--j, q = r - 1;;) {
    for (s = 0;;) {
      if (i >= j)  goto done;

      if (array_sort_cmp(*p, *r, cmp_func) > 0) break;

      ++i;  ++p;
    }

    for (s = 1;;) {
      if (i >= j)  goto done;
      
      if (array_sort_cmp(*q, *r, cmp_func) < 0)  break;

      --j;  --q;
    }

    OBJ_ASSIGN(R0, *p);
    OBJ_ASSIGN(*p, *q);
    OBJ_ASSIGN(*q, R0);

    ++i;  ++p;
    --j;  --q;
  }

 done:

  n1 = i;
  if (i <= j && s == 0 && array_sort_cmp(*q, *r, cmp_func) <= 0)  ++n1;

  p = &a[n1];
  OBJ_ASSIGN(R0, *p);
  OBJ_ASSIGN(*p, *r);
  OBJ_ASSIGN(*r, R0);

  array_sort_quick(a, n1, cmp_func);
  array_sort_quick(a + (n1 + 1), n - n1 - 1, cmp_func);
}

void
cm_array_sort(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.array))  error(ERR_INVALID_ARG, recvr);

  vm_enter(1);

  array_sort_quick(ARRAY(recvr)->data, ARRAY(recvr)->size, 0);

  vm_assign(0, recvr);

  vm_leave(1);
}

void
cm_array_sortc(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 2)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.array))  error(ERR_INVALID_ARG, recvr);

  vm_enter(1);

  array_sort_quick(ARRAY(recvr)->data, ARRAY(recvr)->size, CAR(CDR(args)));

  vm_assign(0, recvr);

  vm_leave(1);
}

void
cm_array_equals(unsigned argc, obj_t args)
{
  obj_t    recvr, arg, *p, *q;
  unsigned n, f;

  if (argc != 2)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.array))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  if (!is_kind_of(arg, consts.cl.array))  error(ERR_INVALID_ARG, arg);
  
  n = ARRAY(recvr)->size;
  f = (n == ARRAY(arg)->size);
  for (p = ARRAY(recvr)->data, q = ARRAY(arg)->data; f && n; --n, ++p, ++q) {
    m_method_call_2(consts.str.equalsc, *p, *q);
    f = BOOLEAN(R0)->val;
  }

  m_boolean_new(consts.cl.boolean, f);
}

void
cm_array_foreach(unsigned argc, obj_t args)
{
  obj_t    recvr, arg, *p, *q;
  unsigned n;

  if (argc != 2)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.array))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));

  vm_enter(1);

  vm_assign(1, NIL);
  for (p = &R1, q = ARRAY(recvr)->data, n = ARRAY(recvr)->size; n; --n, ++q) {
    m_dptr_new(consts.cl.list, *q, NIL);
    m_method_call_2(consts.str.evalc, arg, R0);
    m_dptr_new(consts.cl.list, R0, NIL);
    OBJ_ASSIGN(*p, R0);
    p = &CDR(R0);
  }
  vm_assign(0, R1);
  
  vm_leave(1);
}

void
cm_array_member(unsigned argc, obj_t args)
{
  obj_t    recvr, arg, *p;
  unsigned n, f;

  if (argc != 2)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_list(recvr))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));

  vm_push(0);

  for (f = 0, p = ARRAY(recvr)->data, n = ARRAY(recvr)->size; !f && n > 0; --n, ++p) {
    m_method_call_2(consts.str.equalsc, *p, arg);
    if (!is_kind_of(R0, consts.cl.boolean)) {
      error(ERR_INVALID_VALUE, R0);
    }
    f = BOOLEAN(R0)->val;
  }

  m_boolean_new(consts.cl.boolean, f);

  vm_drop();
}

/***************************************************************************/

/* Class: Set */

unsigned
key_hash(obj_t obj)
{
  unsigned result;

  vm_push(0);

  m_method_call_1(consts.str.hash, obj);
  result = INTEGER(R0)->val;

  vm_pop(0);

  return (result);
}

unsigned
key_equal(obj_t obj1, obj_t obj2)
{
  unsigned result;

  vm_push(0);

  m_method_call_2(consts.str.equalsc, obj1, obj2);
  result = BOOLEAN(R0)->val;
  
  vm_pop(0);

  return (result);
}

void
inst_init_set(obj_t cl, obj_t inst, va_list ap)
{
  inst_init_parent(cl, inst, ap);
}

void
inst_walk_set(obj_t cl, obj_t inst, void (*func)(obj_t))
{
  inst_walk_parent(cl, inst, func);
}

void
inst_free_set(obj_t cl, obj_t inst)
{
  inst_free_parent(cl, inst);
}

unsigned
m_size_dflt(obj_t cl)
{
  obj_t         p;
  integer_val_t val;

  return ((p = dict_at(CLASS(cl)->cl_vars, consts.str.default_size))
	  && is_kind_of(p = CDR(p), consts.cl.integer)
	  && (val = INTEGER(p)->val) >= 0
	  ? (unsigned) val
	  : 32
	  );
}

unsigned
round_up_to_power_of_2(unsigned u)
{
  unsigned result, v;

  for (result = u; v = u & (u - 1); u = v)  result = v << 1;

  HARD_ASSERT(result != 0);

  return (result);
}

void
m_set_new(obj_t cl, unsigned size)
{
  m_inst_alloc(cl);
  inst_init(R0, round_up_to_power_of_2(size));
}

obj_t
set_find(obj_t set, obj_t key, obj_t **pprev)
{
  obj_t p, *pp, *b = &SET(set)->base->data[key_hash(key) & (SET(set)->base->size - 1)];

  for (pp = b; p = *pp; pp = &CDR(p)) {
    if (key_equal(CAR(p), key)) {
      if (pprev)  *pprev = pp; 
      return (p);
    }
  }
  
  if (pprev)  *pprev = b;
  return (NIL);
}

unsigned
set_member(obj_t set, obj_t key)
{
  return (set_find(set, key, 0) != NIL);
}

void
set_insert(obj_t set, obj_t key)
{
  obj_t *pp;
  
  if (set_find(set, key, &pp) != NIL)  return;

  vm_push(0);
  
  m_dptr_new(consts.cl.list, key, *pp);
  OBJ_ASSIGN(*pp, R0);

  ++SET(set)->cnt;
  
  vm_pop(0);
}

void
set_del(obj_t set, obj_t key)
{
  obj_t p, *pp;
  
  if ((p = set_find(set, key, &pp)) == NIL)  return;
  
  vm_push(1);
  
  vm_assign(1, p);
  OBJ_ASSIGN(*pp, CDR(p));

  --SET(set)->cnt;

  vm_pop(1);
}

void
cm_set_new(unsigned argc, obj_t args)
{
  obj_t recvr, arg;

  if (argc < 1 || argc > 2)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (inst_of(recvr) != consts.cl.metaclass)  error(ERR_INVALID_ARG, recvr);

  if (argc == 1) {
    m_set_new(recvr, m_size_dflt(recvr));
    return;
  }

  arg = CAR(CDR(args));

  if (is_kind_of(arg, consts.cl.integer)) {
    integer_val_t size = INTEGER(arg)->val;

    if (size <= 0)  error(ERR_INVALID_ARG, arg);

    m_set_new(recvr, (unsigned) size);
    return;
  }
  if (is_kind_of(arg, consts.cl.dict)) {
    unsigned n;
    obj_t    *q, r, s;

    vm_enter(1);

    m_set_new(recvr, m_size_dflt(recvr));
    vm_assign(1, R0);
    for (q = DICT(arg)->base->base->data, n = DICT(arg)->base->base->size;
	 n;
	 --n, ++q
	 ) {
      for (r = *q; r; r = CDR(r)) {
	s = CAR(r);
	m_dptr_new(consts.cl.pair, CAR(s), CDR(s));
	set_insert(R1, R0);
      }
    }

    vm_assign(0, R1);

    vm_leave(1);

    return;
  }
  if (is_kind_of(arg, consts.cl.set)) {
    unsigned n;
    obj_t    *q, r;

    vm_push(0);

    m_set_new(recvr, SET(arg)->base->size);
    for (q = SET(arg)->base->data, n = SET(arg)->base->size;
	 n;
	 --n, ++q
	 ) {
      for (r = *q; r; r = CDR(r)) {
	set_insert(R0, CAR(r));
      }
    }

    vm_drop();

    return;
  }
  if (is_kind_of(arg, consts.cl.array)) {
    unsigned n;
    obj_t    *q;

    vm_push(0);

    m_set_new(recvr, m_size_dflt(recvr));
    for (q = ARRAY(arg)->data, n = ARRAY(arg)->size;
	 n;
	 --n, ++q
	 ) {
      set_insert(R0, *q);
    }

    vm_drop();

    return;
  }
  if (is_kind_of(arg, consts.cl.list)) {
    obj_t p;

    vm_push(0);

    m_set_new(recvr, m_size_dflt(recvr));

    for (p = arg; p; p = CDR(p)) {
      set_insert(R0, CAR(p));
    }

    vm_drop();

    return;
  }

  error(ERR_INVALID_ARG, arg);
}

void
cm_set_member(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 2)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.set))  error(ERR_INVALID_ARG, recvr);

  m_boolean_new(consts.cl.boolean, set_find(recvr, CAR(CDR(args)), 0) != NIL);
}

void
cm_set_put(unsigned argc, obj_t args)
{
  obj_t recvr, k;

  if (argc != 2)  error(ERR_NUM_ARGS);
  recvr = CAR(args);  args = CDR(args);
  if (!is_kind_of(recvr, consts.cl.set))  error(ERR_INVALID_ARG, recvr);
  k = CAR(args);
  
  set_insert(recvr, k);
  
  vm_assign(0, k);
}

void
cm_set_del(unsigned argc, obj_t args)
{
  obj_t recvr, arg;

  if (argc != 2)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.set))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  
  set_del(recvr, arg);
  
  vm_assign(0, arg);
}

void
cm_set_count(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)                          error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.set))  error(ERR_INVALID_ARG, recvr);

  m_integer_new(consts.cl.integer, SET(recvr)->cnt);
}

void
cm_set_tostring(unsigned argc, obj_t args)
{
  obj_t    recvr, *p, *q, r;
  unsigned i, n;

  if (argc != 1)                          error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.set))  error(ERR_INVALID_ARG, recvr);

  vm_enter(1);
  
  n = SET(recvr)->cnt;
  m_array_new(consts.cl.array, 2 + (n == 0 ? 0 : 2 * n - 1));
  vm_assign(1, R0);
  q = ARRAY(R1)->data;

  m_string_newc(consts.cl.string, 1, 1, "(");
  OBJ_ASSIGN(*q, R0);
  ++q;
  for (i = 0, p = SET(recvr)->base->data, n = SET(recvr)->base->size; n; --n, ++p) {
    for (r = *p; r; r = CDR(r), ++i) {
      if (i > 0) {
	OBJ_ASSIGN(*q, consts.str.space);
	++q;
      }
      
      m_method_call_1(consts.str.tostring, CAR(r));
      OBJ_ASSIGN(*q, R0);
      ++q;
    }
  }
  m_string_newc(consts.cl.string, 1, 1, ")");
  OBJ_ASSIGN(*q, R0);
  
  m_string_newv(consts.cl.string, R1);
  
  vm_leave(1);
}

void
cm_set_foreach(unsigned argc, obj_t args)
{
  obj_t    recvr, arg, *p, *q, r;
  unsigned n;

  if (argc != 2)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.array))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));

  vm_enter(1);

  vm_assign(1, NIL);
  for (p = &R1, q = SET(recvr)->base->data, n = SET(recvr)->base->size; n; --n, ++q) {
    for (r = *q; r; r = CDR(r)) {
      m_dptr_new(consts.cl.list, CAR(r), NIL);
      m_method_call_2(consts.str.evalc, arg, R0);
      m_dptr_new(consts.cl.list, R0, NIL);
      OBJ_ASSIGN(*p, R0);
      p = &CDR(R0);
    }
  }
  vm_assign(0, R1);
  
  vm_leave(1);
}

void
cl_init_set(obj_t cl)
{
  vm_push(0);

  m_integer_new(consts.cl.integer, 32);
  dict_at_put(CLASS(cl)->cl_vars, consts.str.default_size, R0);

  vm_pop(0);
}

/***************************************************************************/

/* Class: Dictionary */

void
inst_init_dict(obj_t cl, obj_t inst, va_list ap)
{
  DICT(inst)->find = va_arg(ap, obj_t (*)(obj_t dict, obj_t key, obj_t **pprev));

  inst_init_parent(cl, inst, ap);
}

void
inst_walk_dict(obj_t cl, obj_t inst, void (*func)(obj_t))
{
  inst_walk_parent(cl, inst, func);
}

void
inst_free_dict(obj_t cl, obj_t inst)
{
  inst_free_parent(cl, inst);
}

obj_t
string_dict_find(obj_t dict, obj_t key, obj_t **pprev)
{
  obj_t p, *pp, *b;

  if (!is_kind_of(key, consts.cl.string))  error(ERR_INVALID_ARG, key);

  b = &DICT(dict)->base->base->data[string_hash(key) & (DICT(dict)->base->base->size - 1)];

  for (pp = b; p = *pp; pp = &CDR(p)) {
    obj_t q = CAR(p);
    
    if (string_equal(CAR(q), key)) {
      if (pprev)  *pprev = pp; 
      return (p);
    }
  }
  
  if (pprev)  *pprev = b;
  return (NIL);
}

void
m_string_dict_new(obj_t cl, unsigned size)
{
  m_inst_alloc(cl);
  inst_init(R0, string_dict_find, round_up_to_power_of_2(size));
}

unsigned
m_dict_size_dflt(void)
{
  return (32);			/* TODO: Make a class variable */
}

obj_t
dict_find(obj_t dict, obj_t key, obj_t **pprev)
{
  obj_t p, *pp, *b = &DICT(dict)->base->base->data[key_hash(key) & (DICT(dict)->base->base->size - 1)];

  for (pp = b; p = *pp; pp = &CDR(p)) {
    obj_t q = CAR(p);
    
    if (key_equal(CAR(q), key)) {
      if (pprev)  *pprev = pp; 
      return (p);
    }
  }
  
  if (pprev)  *pprev = b;
  return (NIL);
}

void
m_dict_new(obj_t cl, unsigned size)
{
  m_inst_alloc(cl);
  inst_init(R0, dict_find, round_up_to_power_of_2(size));
}

obj_t
dict_at(obj_t dict, obj_t key)
{
  obj_t p;
  
  return ((p = (*DICT(dict)->find)(dict, key, 0)) ? CAR(p) : NIL);
}

void
dict_at_put(obj_t dict, obj_t key, obj_t val)
{
  obj_t p, *pp;
  
  if (p = (*DICT(dict)->find)(dict, key, &pp)) {
    if (is_kind_of(key, consts.cl.string)
	&& string_len(key) > 0
	&& STRING(key)->data[0] == '#'
	) {
      error(ERR_CONST, key);
    }
    
    OBJ_ASSIGN(CDR(CAR(p)), val);
  } else {
    vm_push(0);
    
    m_pair_new(key, val);
    m_dptr_new(consts.cl.list, R0, *pp);
    OBJ_ASSIGN(*pp, R0);

    ++DICT(dict)->base->cnt;
    
    vm_pop(0);
  }
}

void
dict_del(obj_t dict, obj_t key)
{
  obj_t p, *pp;
  
  if ((p = (*DICT(dict)->find)(dict, key, &pp)) == 0)  return;
  
  vm_push(1);
  
  vm_assign(1, p);
  OBJ_ASSIGN(*pp, CDR(p));

  --DICT(dict)->base->cnt;
  
  vm_pop(1);
}

void
m_dict_keys(obj_t dict)
{
  obj_t    *p, q, *r;
  unsigned n;

  vm_enter(1);

  vm_assign(1, NIL);
  for (r = &R1, p = DICT(dict)->base->base->data, n = DICT(dict)->base->base->size;
       n;
       --n, ++p
       ) {
    for (q = *p; q; q = CDR(q)) {
      m_dptr_new(consts.cl.list, CAR(CAR(q)), NIL);
      OBJ_ASSIGN(*r, R0);
      r = &CDR(R0);
    }
  }
  vm_assign(0, R1);

  vm_leave(1);
}

void
cm_dict_new(unsigned argc, obj_t args)
{
  obj_t recvr, arg;

  if (argc < 1 || argc > 2)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (inst_of(recvr) != consts.cl.metaclass)  error(ERR_INVALID_ARG, recvr);

  if (argc == 1) {
    m_dict_new(recvr, m_size_dflt(recvr));
    return;
  }

  arg = CAR(CDR(args));

  if (is_kind_of(arg, consts.cl.integer)) {
    integer_val_t size = INTEGER(arg)->val;

    if (size <= 0)  error(ERR_INVALID_ARG, arg);

    m_dict_new(recvr, (unsigned) size);
    return;
  }
  if (is_kind_of(arg, consts.cl.dict)) {
    obj_t    *p, q, r;
    unsigned n;

    vm_push(0);

    m_dict_new(recvr, DICT(arg)->base->base->size);
    for (p = DICT(arg)->base->base->data, n = DICT(arg)->base->base->size; n; --n, ++p) {
      for (q = *p; q; q = CDR(q)) {
	r = CAR(q);

	dict_at_put(R0, CAR(r), CDR(r));
      }
    }

    vm_drop();

    return;
  }
  if (is_kind_of(arg, consts.cl.set)) {
    obj_t    *p, q;
    unsigned n;

    vm_push(0);

    m_dict_new(recvr, SET(arg)->base->size);
    for (p = SET(arg)->base->data, n = SET(arg)->base->size; n; --n, ++p) {
      for (q = *p; q; q = CDR(q)) {
	dict_at_put(R0, CAR(q), NIL);
      }
    }

    vm_drop();

    return;
  }
  if (is_kind_of(arg, consts.cl.array)) {
    obj_t    *p;
    unsigned n;

    vm_push(0);

    m_dict_new(recvr, m_dict_size_dflt());
    for (p = ARRAY(arg)->data, n = ARRAY(arg)->size; n; --n, ++p) {
      dict_at_put(R0, *p, NIL);
    }

    vm_drop();

    return;
  }
  if (is_kind_of(arg, consts.cl.list)) {
    obj_t p;

    for (p = arg; p; p = CDR(p)) {
      if (!is_kind_of(CAR(p), consts.cl.pair))  error(ERR_INVALID_ARG, arg);
    }

    vm_push(0);

    m_dict_new(recvr, m_dict_size_dflt());

    for (p = arg; p; p = CDR(p)) {
      obj_t q = CAR(p);
      
      dict_at_put(R0, CAR(q), CDR(q));
    }

    vm_drop();

    return;
  }

  error(ERR_INVALID_ARG, arg);
}

void
cm_dict_at(unsigned argc, obj_t args)
{
  obj_t recvr, p;

  if (argc != 2)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.dict))  error(ERR_INVALID_ARG, recvr);

  if (p = dict_at(recvr, CAR(CDR(args)))) {
    m_pair_new(CAR(p), CDR(p));
    return;
  }
  
  vm_assign(0, p);
}

void
cm_dict_at_put(unsigned argc, obj_t args)
{
  obj_t recvr, k, v;

  if (argc != 3)  error(ERR_NUM_ARGS);
  recvr = CAR(args);  args = CDR(args);
  if (!is_kind_of(recvr, consts.cl.dict))  error(ERR_INVALID_ARG, recvr);
  k = CAR(args);  args = CDR(args);
  v = CAR(args);
  
  dict_at_put(recvr, k, v);
  
  vm_assign(0, v);
}

void
cm_dict_member(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 2)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.dict))  error(ERR_INVALID_ARG, recvr);

  m_boolean_new(consts.cl.boolean, dict_at(recvr, CAR(CDR(args))) ? 1 : 0);
}

void
cm_dict_put(unsigned argc, obj_t args)
{
  obj_t recvr, arg;

  if (argc != 2)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.dict))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  if (!is_kind_of(arg, consts.cl.pair))    error(ERR_INVALID_ARG, arg);

  dict_at_put(recvr, CAR(arg), CDR(arg));
  
  vm_assign(0, arg);
}

void
cm_dict_del(unsigned argc, obj_t args)
{
  obj_t recvr, arg;

  if (argc != 2)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.dict))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  
  dict_del(recvr, arg);
  
  vm_assign(0, arg);
}

void
cm_dict_keys(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)                           error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.dict))  error(ERR_INVALID_ARG, recvr);

  m_dict_keys(recvr);
}

void
cl_init_dict(obj_t cl)
{
  vm_push(0);

  m_integer_new(consts.cl.integer, 32);
  dict_at_put(CLASS(cl)->cl_vars, consts.str.default_size, R0);

  vm_pop(0);
}

/***************************************************************************/

/* Class: File */

void
inst_init_file(obj_t cl, obj_t inst, va_list ap)
{
  OBJ_ASSIGN(_FILE(inst)->name, va_arg(ap, obj_t));
  OBJ_ASSIGN(_FILE(inst)->mode, va_arg(ap, obj_t));
  _FILE(inst)->fp = va_arg(ap, FILE *);

  inst_init_parent(cl, inst, ap);
}

void
inst_walk_file(obj_t cl, obj_t inst, void (*func)(obj_t))
{
  (*func)(_FILE(inst)->name);
  (*func)(_FILE(inst)->mode);

  inst_walk_parent(cl, inst, func);
}

void
inst_free_file(obj_t cl, obj_t inst)
{
  fclose(_FILE(inst)->fp);

  inst_free_parent(cl, inst);
}

void
m_file_new(obj_t cl, obj_t name, obj_t mode, FILE *fp)
{
  vm_push(0);

  m_inst_alloc(cl);
  inst_init(R0, name, mode, fp);

  vm_drop();
}

void
m_file_stdin(void)
{
  m_obj_at(consts.cl.file, consts.str._stdin);
  if (!is_kind_of(R0, consts.cl.file))  error(ERR_INVALID_VALUE_2, consts.str._stdin, R0);
} 

void
m_file_stdout(void)
{
  m_obj_at(consts.cl.file, consts.str._stdout);
  if (!is_kind_of(R0, consts.cl.file))  error(ERR_INVALID_VALUE_2, consts.str._stdout, R0);
} 
 
void
m_file_stderr(void)
{
  m_obj_at(consts.cl.file, consts.str._stderr);
  if (!is_kind_of(R0, consts.cl.file))  error(ERR_INVALID_VALUE_2, consts.str._stderr, R0);
} 

void
m_file_open(obj_t cl, obj_t filename, obj_t mode)
{
  FILE *fp;

  fp = fopen(STRING(filename)->data, STRING(mode)->data);
  if (fp == 0) {
    error(ERR_FILE_OPEN_FAIL, errno);
  }
  
  m_file_new(cl, filename, mode, fp);
}

void
cm_file_new(unsigned argc, obj_t args)
{
  obj_t recvr, filename, mode;

  if (argc != 3)  error(ERR_NUM_ARGS);
  recvr = CAR(args);     args = CDR(args); 
  if (inst_of(recvr) != consts.cl.metaclass)  error(ERR_INVALID_ARG, recvr);
  filename = CAR(args);  args = CDR(args);  
  mode = CAR(args);
  if (!is_kind_of(filename, consts.cl.string)
      || string_len(filename) == 0
      )  error(ERR_INVALID_ARG, filename);
  if (!is_kind_of(mode, consts.cl.string)
      || string_len(mode) == 0
      )  error(ERR_INVALID_ARG, mode);

  m_file_open(recvr, filename, mode);
}

void
cm_file_name(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.file))  error(ERR_INVALID_ARG, recvr);

  vm_assign(0, _FILE(recvr)->name);
}

void
cm_file_mode(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.file))  error(ERR_INVALID_ARG, recvr);

  vm_assign(0, _FILE(recvr)->mode);
}

void
m_file_tostring(obj_t f)
{
  m_string_newc(consts.cl.string, 6, string_len(CLASS(consts.cl.file)->name), STRING(CLASS(consts.cl.file)->name)->data,
	                             1, "(",
	                             string_len(_FILE(f)->name), STRING(_FILE(f)->name)->data,
	                             2, ", ",
	                             string_len(_FILE(f)->mode), STRING(_FILE(f)->mode)->data,
	                             1, ")"
		);
}

void
cm_file_tostring(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.file))  error(ERR_INVALID_ARG, recvr);

  m_file_tostring(recvr);
}

void
cm_file_tostring_fmt(unsigned argc, obj_t args)
{
  obj_t    recvr, arg;
  unsigned n;
  struct printf_fmt_info fi[1];

  if (argc != 2)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.file))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  if (!is_kind_of(arg, consts.cl.string))  error(ERR_INVALID_ARG, arg);
  n = string_len(arg);
  if (printf_fmt_parse(n, STRING(arg)->data, fi) != (int) n
      || fi->mode != 'O'
      )  error(ERR_INVALID_ARG, arg);

  vm_push(0);

  m_file_tostring(recvr);

  if (m_string_pad(R0, fi) < 0)  error(ERR_INVALID_ARG, arg);

  vm_drop();
}

void
cm_file_readc(unsigned argc, obj_t args)
{
  obj_t         recvr, arg;
  FILE          *fp;
  integer_val_t len;
  char          *p;
  unsigned      i;
  
  if (argc != 2)                            error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.file))   error(ERR_INVALID_ARG, recvr);
  fp = _FILE(recvr)->fp;
  arg = CAR(CDR(args));
  if (!is_kind_of(arg, consts.cl.integer))  error(ERR_INVALID_ARG, arg);
  len = INTEGER(arg)->val;
  if (len < 0)                              error(ERR_INVALID_ARG, arg);

  vm_push(0);

  m_inst_alloc(consts.cl.string);
  inst_init(R0, (unsigned) len);

  for (i = 0, p = STRING(R0)->data; len; --len, ++p, ++i) {
    int c = fgetc(fp);
    
    if (c == EOF)  break;
    
    *p = c;
  }

  if (ferror(fp))  error(ERR_FILE_IO, recvr);

  m_string_newc(consts.cl.string, 1, i, STRING(R0)->data);

  vm_drop();
}

void
cm_file_readln(unsigned argc, obj_t args)
{
  obj_t     recvr;
  FILE      *fp;
  unsigned  i;
  
  if (argc != 1)                           error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.file))  error(ERR_INVALID_ARG, recvr);
  fp = _FILE(recvr)->fp;

  vm_push(0);

  m_inst_alloc(consts.cl.string);
  inst_init(R0, 32);
  
  for (i = 0; ; ++i) {
    int c = fgetc(fp);
    
    if (c == EOF || c == '\n')  break;
    
    m_string_append_char(R0, i, c);
  }

  if (ferror(fp))  error(ERR_FILE_IO, recvr);
  
  m_string_newc(consts.cl.string, 1, i, STRING(R0)->data);

  vm_drop();
}

void
cm_file_read(unsigned argc, obj_t args)
{
  obj_t     recvr;
  FILE      *fp;
  unsigned  i;
  
  if (argc != 1)                           error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.file))  error(ERR_INVALID_ARG, recvr);
  fp = _FILE(recvr)->fp;

  vm_push(0);

  m_inst_alloc(consts.cl.string);
  inst_init(R0, 32);
  
  for (i = 0; ; ++i) {
    int c = fgetc(fp);
    
    if (c == EOF)  break;
    
    m_string_append_char(R0, i, c);
  }

  if (ferror(fp))  error(ERR_FILE_IO, recvr);
  
  m_string_newc(consts.cl.string, 1, i, STRING(R0)->data);

  vm_drop();
}

void
cm_file_write(unsigned argc, obj_t args)
{
  obj_t recvr, arg;
  FILE  *fp;

  if (argc != 2)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.file))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  if (!is_kind_of(arg, consts.cl.string))  error(ERR_INVALID_ARG, arg);

  fp = _FILE(recvr)->fp;
  if (string_len(arg) > 0)  fputs(STRING(arg)->data, fp);

  if (ferror(fp))  error(ERR_FILE_IO, recvr);

  vm_assign(0, recvr);
}

void
cm_file_flush(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.file))  error(ERR_INVALID_ARG, recvr);

  fflush(_FILE(recvr)->fp);

  vm_assign(0, recvr);
}

void
cm_file_eof(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.file))  error(ERR_INVALID_ARG, recvr);

  m_boolean_new(consts.cl.boolean, feof(_FILE(recvr)->fp));
}

void
m_file_load(obj_t file)
{
  vm_push(0);

  FRAME_INPUT_BEGIN(_FILE(file)->fp, STRING(_FILE(file)->name)->data, 0) {
    
    for (;;) {
      yyparse();
      
      if (yy_input_eof())  break;

      vm_dropn(frp->sp - sp); /* Discard all objs pushed during parsing */
      
      m_method_call_1(consts.str.eval, R0);
    }
    
  } FRAME_INPUT_END;

  vm_drop();
}

void
cm_file_load(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.file))  error(ERR_INVALID_ARG, recvr);

  m_file_load(recvr);
}

void
cl_init_file(obj_t cl)
{
  vm_push(0);

  m_string_newc(consts.cl.string, 1, 2, "r+");
  m_file_new(cl, consts.str._stdin, R0, stdin);
  dict_at_put(CLASS(cl)->cl_vars, consts.str._stdin, R0);
  m_string_newc(consts.cl.string, 1, 2, "w+");
  m_file_new(cl, consts.str._stdout, R0, stdout);
  dict_at_put(CLASS(cl)->cl_vars, consts.str._stdout, R0);
  m_string_newc(consts.cl.string, 1, 2, "w+");
  m_file_new(cl, consts.str._stderr, R0, stderr);
  dict_at_put(CLASS(cl)->cl_vars, consts.str._stderr, R0);

  vm_pop(0);
}

/***************************************************************************/

/* Class: Module */

void
inst_init_module(obj_t cl, obj_t inst, va_list ap)
{
  OBJ_ASSIGN(MODULE(inst)->name,   va_arg(ap, obj_t));
  OBJ_ASSIGN(MODULE(inst)->parent, va_arg(ap, obj_t));

  inst_init_parent(cl, inst, ap);
}

void
inst_walk_module(obj_t cl, obj_t inst, void (*func)(obj_t))
{
  obj_t    *p;
  unsigned n;

  (*func)(MODULE(inst)->name);
  (*func)(MODULE(inst)->parent);

  for (p = MODULE(inst)->consts, n = MODULE(inst)->nconsts;
       n;
       --n, ++p) {
    (*func)(*p);
  }

  inst_walk_parent(cl, inst, func);
}

void
inst_free_module(obj_t cl, obj_t inst)
{
  void *cookie;

  if (cookie = MODULE(inst)->dl_cookie) {
    FRAME_MODULE_BEGIN(inst) {

      (* MODULE(inst)->fini_func)();

    } FRAME_MODULE_END;

    dlclose(cookie);
  }
  
  inst_free_parent(cl, inst);
}

void
m_module_new(obj_t cl, obj_t name, obj_t parent)
{
  vm_push(0);

  m_inst_alloc(cl);
  inst_init(R0, name, parent, string_dict_find, 64);
  
  if (parent)  dict_at_put(parent, name, R0);

  vm_drop();
}

obj_t env_find(obj_t s);

void
_cm_module_new(obj_t recvr, obj_t name, obj_t modname)
{
  obj_t    path;
  unsigned oolf, sof;

  vm_enter(5);

  m_string_newc(consts.cl.string, 1, 2, "r+");
  vm_assign(1, R0);		/* R1 = file open mode */

  path = dict_at(CLASS(consts.cl.module)->cl_vars,
		 consts.str.path
		 );
  if (path) {
    path = CDR(path);
  } else {
    m_string_newc(consts.cl.string, 1, 1, ".");
    m_dptr_new(consts.cl.list, R0, NIL);
    vm_assign(2, R0);		/* R2 = path */
    path = R2;
  }

  oolf = string_len(name) >= 5 && strcmp(STRING(name)->data + (STRING(name)->size - 5), ".ool") == 0;
  sof  = string_len(name) >= 4 && strcmp(STRING(name)->data + (STRING(name)->size - 4), ".so") == 0;

  if (sof) {
    m_string_newc(consts.cl.string, 1, string_len(name) - 3, STRING(name)->data);
    vm_assign(3, R0);		/* R3 = module name */
  } else if (oolf) {
    m_string_newc(consts.cl.string, 1, string_len(name) - 4, STRING(name)->data);
    vm_assign(3, R0);		/* R3 = module name */
  } else {
    vm_assign(3, name);		/* R3 = module name */
  }
  vm_assign(4, name);		/* R4 = filename */

  {
    obj_t dn;

    dn = env_find(modname);
    if (dn != NIL) {
      dn = CDR(dn);
      if (is_kind_of(dn, consts.cl.module)) {
	vm_assign(0, dn);
	
	goto done;
      }
    }
  }

  for ( ; path; path = CDR(path)) {
    obj_t dir = CAR(path);
    FILE  *fp;
    void  *cookie;

    m_string_newc(consts.cl.string, 3, string_len(dir), STRING(dir)->data,
		                      1, "/",
		                      string_len(R4), STRING(R4)->data
		 );
    vm_assign(5, R0);		/* R5 = file base name */
    
    if (!oolf) {
      if (sof) {
	vm_assign(0, R5);
      } else {
	m_string_newc(consts.cl.string, 2, string_len(R5), STRING(R5)->data,
		     3, ".so"
		     );
      }
      
      cookie = dlopen(STRING(R0)->data, RTLD_NOW);
      if (cookie) {
	void *init_func, *fini_func;
	
	m_string_newc(consts.cl.string, 2, string_len(R3), STRING(R3)->data,
		     12, "_module_init"
		     );
	init_func = dlsym(cookie, STRING(R0)->data);
	m_string_newc(consts.cl.string, 2, string_len(R3), STRING(R3)->data,
		     12, "_module_fini"
		     );
	fini_func = dlsym(cookie, STRING(R0)->data);
	if (init_func && fini_func) {
	  m_module_new(recvr, modname, module_cur);
	  MODULE(R0)->dl_cookie = cookie;
	  MODULE(R0)->fini_func = (void (*)(void)) fini_func;
	  
	  FRAME_MODULE_BEGIN(R0) {
	    
	    (* (void (*)(void)) init_func)();
	    
	  } FRAME_MODULE_END;
	  
	  goto done;
	}
	
	dlclose(cookie);
      }
    }

    if (!sof) {
      if (oolf) {
	vm_assign(0, R5);
      } else {
	m_string_newc(consts.cl.string, 2, string_len(R5), STRING(R5)->data,
		     4, ".ool"
		     );
      }
      
      fp = fopen(STRING(R0)->data, STRING(R1)->data);
      if (fp) {
	m_file_new(consts.cl.file, R0, R1, fp);
	vm_assign(1, R0);		/* R1 = file */
	
	m_module_new(recvr, modname, module_cur);
	vm_assign(2, R0);

	FRAME_MODULE_BEGIN(R0) {
	  
	  m_file_load(R1);
	  
	} FRAME_MODULE_END;
	
	vm_assign(0, R2);

	goto done;
      }
    }
  }

  error(ERR_MODULE_OPEN_FAIL, name);

 done:
  vm_leave(5);
}

void
cm_module_new(unsigned argc, obj_t args)
{
  obj_t recvr, name;

  if (argc != 2)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (inst_of(recvr) != consts.cl.metaclass)  error(ERR_INVALID_ARG, recvr);
  name = CAR(CDR(args));
  if (!is_kind_of(name, consts.cl.string))  error(ERR_INVALID_ARG, name);

  _cm_module_new(recvr, name, name);
}

void
cm_module_new_as(unsigned argc, obj_t args)
{
  obj_t recvr, name, modname;

  if (argc != 3)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (inst_of(recvr) != consts.cl.metaclass)  error(ERR_INVALID_ARG, recvr);
  args = CDR(args);  name = CAR(args);
  if (!is_kind_of(name, consts.cl.string))  error(ERR_INVALID_ARG, name);
  args = CDR(args);  modname = CAR(args);
  if (!is_kind_of(name, consts.cl.string))  error(ERR_INVALID_ARG, name);

  _cm_module_new(recvr, name, modname);
}

void
m_fqmodname(obj_t mod)
{
    obj_t s;

    vm_push(0);

    m_string_newc(consts.cl.string, 0);
    for ( ; mod; mod = MODULE(mod)->parent) {
	s = MODULE(mod)->name;

	if (string_len(R0) == 0) {
	    vm_assign(0, s);
	    continue;
	}

	m_string_newc(consts.cl.string, 3, string_len(s), STRING(s)->data,
		                          1, ".",
		                          string_len(R0), STRING(R0)->data
		     );
    }

    vm_drop();
}

void
cm_module_name(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.module))  error(ERR_INVALID_ARG, recvr);
  
  m_fqmodname(recvr);
}

void
cm_module_parent(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.module))  error(ERR_INVALID_ARG, recvr);
  
  vm_assign(0, MODULE(recvr)->parent);
}

void
cm_module_at(unsigned argc, obj_t args)
{
  obj_t recvr, arg, p;

  if (argc != 2)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.module))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));

  if (p = dict_at(OBJ(MODULE(recvr)->base), arg)) {
    vm_assign(0, CDR(p));
    return;
  }

  error(ERR_NOT_BOUND, arg);
}

void
cm_module_tostring(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.module))  error(ERR_INVALID_ARG, recvr);
  
  m_fqmodname(recvr);
}

void
cm_module_tostring_fmt(unsigned argc, obj_t args)
{
  obj_t    recvr, arg;
  unsigned n;
  struct printf_fmt_info fi[1];

  if (argc != 2)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.module))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  if (!is_kind_of(arg, consts.cl.string))  error(ERR_INVALID_ARG, recvr);
  n = string_len(arg);
  if (printf_fmt_parse(n, STRING(arg)->data, fi) != (int) n
      || fi->mode != 'O'
      )  error(ERR_INVALID_ARG, recvr);

  vm_push(0);

  m_fqmodname(recvr);

  if (m_string_pad(R0, fi) < 0)  error(ERR_INVALID_ARG, arg);

  vm_drop();
}

void
cm_module_cur(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (inst_of(recvr) != consts.cl.metaclass)  error(ERR_INVALID_ARG, recvr);
  
  vm_assign(0, module_cur);
}

void
cl_init_module(obj_t cl)
{
  vm_push(0);

  m_string_newc(consts.cl.string, 1, 1, ".");
  m_dptr_new(consts.cl.list, R0, NIL);

  dict_at_put(CLASS(cl)->cl_vars,
	      consts.str.path,
	      R0
	      );

  vm_pop(0);
}

/***************************************************************************/

/* Class: Environment */

obj_t
env_top(void)
{
  return (envp->env_dict);
}

obj_t
env_find(obj_t s)
{
  obj_t        result = NIL;
  struct frame *fr;

  for (fr = envp; fr; fr = fr->envp_prev) {
    result = dict_at(fr->env_dict, s);
    if (result != NIL)  break;
  }

  return (result);
}

void
cm_env_at(unsigned argc, obj_t args)
{
  obj_t arg, dn;

  if (argc != 2)  error(ERR_NUM_ARGS);
  arg = CAR(CDR(args));

  dn = env_find(arg);
  if (dn == NIL)  error(ERR_NOT_BOUND, arg);

  vm_assign(0, CDR(dn));
}

void
cm_env_def_put(unsigned argc, obj_t args)
{
  obj_t val;

  if (argc != 3)  error(ERR_NUM_ARGS);
  val = CAR(CDR(CDR(args)));

  dict_at_put(env_top(), CAR(CDR(args)), val);

  vm_assign(0, val);
}

void
cm_env_at_put(unsigned argc, obj_t args)
{
  obj_t arg, val, dn;

  if (argc != 3)  error(ERR_NUM_ARGS);

  arg = CAR(CDR(args));
  val = CAR(CDR(CDR(args)));

  dn = env_find(arg);

  if (dn == NIL)                    error(ERR_NOT_BOUND, arg);
  if (STRING(arg)->data[0] == '#')  error(ERR_CONST, arg);

  OBJ_ASSIGN(CDR(dn), val);

  vm_assign(0, val);
}

void
cm_env_del(unsigned argc, obj_t args)
{
  if (argc != 2)  error(ERR_NUM_ARGS);

  dict_del(env_top(), CAR(CDR(args)));

  vm_assign(0, NIL);
}

void
cm_env_current(unsigned argc, obj_t args)
{
  struct frame *fr;
  obj_t        *q;

  vm_enter(1);

  vm_assign(1, NIL);
  q = &R1;
  for (fr = envp; fr; fr = fr->envp_prev) {
    m_dptr_new(consts.cl.list, fr->env_dict, NIL);
    OBJ_ASSIGN(*q, R0);
    q = &CDR(R0);
  }
  
  vm_assign(0, R1);

  vm_leave(1);
}

/***************************************************************************/

/* Class: Error */

void
cm_error_raise(unsigned argc, obj_t args)
{
  if (argc != 2)  error(ERR_NUM_ARGS);
  
  error(ERR_USER, CAR(CDR(args)));
}

/***************************************************************************/

/* Class: System */

void
zexit(int code)
{
#ifndef NDEBUG
  collect();			/* Check consistency */

  stats_print();
#endif

  exit(code);
}

void
cm_system_exit(unsigned argc, obj_t args)
{
  if (argc != 1)  error(ERR_NUM_ARGS);

  zexit(0);
}

void
cm_system_exitc(unsigned argc, obj_t args)
{
  obj_t arg;

  if (argc != 2)                            error(ERR_NUM_ARGS);
  arg = CAR(CDR(args));
  if (!is_kind_of(arg, consts.cl.integer))  error(ERR_INVALID_ARG, arg);

  zexit(INTEGER(arg)->val);
}

#ifndef NDEBUG

void
cm_system_collect(unsigned argc, obj_t args)
{
  collect();

  vm_assign(0, NIL);
}


void
cm_system_debug(unsigned argc, obj_t args)
{
  mem_chk();

  stats_print();

  vm_assign(0, NIL);
}

#endif
/***************************************************************************/

void
bt_print(obj_t outf)
{
  FILE         *fp = _FILE(outf)->fp;
  struct frame *p;
  unsigned     n;

  for (n = 0, p = frp; p; p = p->prev) {
    switch (p->type) {
    case FRAME_TYPE_METHOD_CALL:
      {
	struct frame_method_call *q = (struct frame_method_call *) p;
	
	fprintf(fp, "%u: ", n);
	if (q->cl) {
	  m_method_call_2(consts.str.printc, q->cl, outf);
	  fprintf(fp, ".");
	}
	m_method_call_2(consts.str.printc, q->sel, outf);
	m_method_call_2(consts.str.printc, q->args, outf);
	putchar('\n');

	++n;
      }
      break;
    case FRAME_TYPE_INPUT:
      {
	struct frame_input *q = (struct frame_input *) p;

	if (q->inp_desc.fp) {
	  fprintf(fp, "In file %s, line %u:\n", q->inp_desc.filename, q->inp_desc.line);
	}
      }
      break;
    default:
      ;
    }
  }
}

void
error(unsigned errcode, ...)
{
  FILE    *fp;
  va_list ap;

  if (++err_lvl > 1)  fatal(FATAL_DOUBLE_ERR);

  stack_fence = stack;

  vm_enter(1);

  m_file_stderr();
  vm_assign(1, R0);
  fp = _FILE(R1)->fp;

  va_start(ap, errcode);

  fprintf(fp, "\n*** ERROR - ");
  switch (errcode) {
  case ERR_PARSE:
    {
      char *s = va_arg(ap, char *);

      fprintf(fp, "Parser error: %s\n", s);
    }
    break;
  case ERR_STACK_OVF:
    fprintf(fp, "Stack overflow\n");
    break;
  case ERR_NUM_ARGS:
    fprintf(fp, "Incorrent number of arugments\n");
    break;
  case ERR_INVALID_ARG:
    {
      obj_t arg = va_arg(ap, obj_t);

      fprintf(fp, "Invalid argument: ");
      m_obj_printc(arg, R1);
      fprintf(fp, "\n");
    }
    break;
  case ERR_INVALID_VALUE:
    {
      obj_t arg = va_arg(ap, obj_t);

      fprintf(fp, "Invalid value: ");
      m_obj_printc(arg, R1);
      fprintf(fp, "\n");
    }
    break;
  case ERR_INVALID_VALUE_2:
    {
      obj_t arg1 = va_arg(ap, obj_t);
      obj_t arg2 = va_arg(ap, obj_t);

      fprintf(fp, "Invalid value for ");
      m_obj_printc(arg1, R1);
      fprintf(fp, ": ");
      m_obj_printc(arg2, R1);
      fprintf(fp, "\n");
    }
    break;
  case ERR_NO_METHOD:
    {
      obj_t arg = va_arg(ap, obj_t);

      fprintf(fp, "No such method: ");
      m_obj_printc(arg, R1);
      fprintf(fp, "\n");
    }
    break;
  case ERR_NO_ATTR:
    {
      obj_t arg = va_arg(ap, obj_t);

      fprintf(fp, "No such attribute: ");
      m_obj_printc(arg, R1);
      fprintf(fp, "\n");
    }
    break;
  case ERR_NOT_BOUND:
    {
      obj_t arg = va_arg(ap, obj_t);

      fprintf(fp, "Symbol not bound: ");
      m_obj_printc(arg, R1);
      fprintf(fp, "\n");
    }
    break;
  case ERR_OVF:
    fprintf(fp, "Arithmetic overflow\n");
    break;
  case ERR_IDX_RANGE:
    {
      obj_t arg = va_arg(ap, obj_t);

      fprintf(fp, "Index out of range: ");
      m_obj_printc(arg, R1);
      fprintf(fp, "\n");
    }
    break;
  case ERR_IDX_RANGE_2:
    {
      obj_t arg1 = va_arg(ap, obj_t);
      obj_t arg2 = va_arg(ap, obj_t);

      fprintf(fp, "Range out of range: ");
      m_obj_printc(arg1, R1);
      fprintf(fp, ", ");
      m_obj_printc(arg2, R1);
      fprintf(fp, "\n");
    }
    break;
  case ERR_CONST:
    {
      obj_t arg = va_arg(ap, obj_t);

      fprintf(fp, "Write to constant: ");
      m_obj_printc(arg, R1);
      fprintf(fp, "\n");
    }
    break;
  case ERR_FILE_OPEN_FAIL:
    {
      int errnum = va_arg(ap, int);

      fprintf(fp, "File open failed: %s\n", sys_errlist[errnum]);
    }
    break;
  case ERR_FILE_IO:
    {
      obj_t arg = va_arg(ap, obj_t);

      fprintf(fp, "File I/O error: ");
      m_obj_printc(arg, R1);
      fprintf(fp, "\n");
    }
    break;
  case ERR_MODULE_OPEN_FAIL:
    fprintf(fp, "Module open failed\n");
    break;
  case ERR_WHILE:
    fprintf(fp, "Not within while\n");
    break;
  case ERR_BLOCK:
    fprintf(fp, "Not within block\n");
    break;
  case ERR_ASSERT_FAIL:
    fprintf(fp, "Assertion failed\n");
    break;
  case ERR_CANNOT_INST:
    {
      obj_t arg = va_arg(ap, obj_t);

      fprintf(fp, "Cannot instantiate: ");
      m_obj_printc(arg, R1);
      fprintf(fp, "\n");
    }
    break;
  case ERR_USER:
    {
      obj_t arg = va_arg(ap, obj_t);

      m_obj_printc(arg, R1);
      fprintf(fp, "\n");
    }
    break;
  default:
    HARD_ASSERT(0);
  }

  bt_print(R1);

  va_end(ap);
  
  --err_lvl;

  frame_jmp(FRAME_TYPE_RESTART, 1);

  HARD_ASSERT(0);
}

/***************************************************************************/

const struct init_cl init_cl_tbl[] = {
  { &consts.cl.metaclass,
    &consts.str.metaclass,
    &consts.cl.object,
    sizeof(struct inst_metaclass),
    inst_init_class,
    inst_walk_class,
    inst_free_class
  },
  { &consts.cl.object,
    &consts.str.object,
    0,
    sizeof(struct obj),
    inst_init_object,
    inst_walk_object,
    inst_free_object
  },
  { &consts.cl.code_method,
    &consts.str.code_method,
    &consts.cl.object,
    sizeof(struct inst_code_method),
    inst_init_code_method,
    inst_walk_code_method,
    inst_free_code_method
  },
  { &consts.cl.boolean,
    &consts.str.boolean,
    &consts.cl.object,
    sizeof(struct inst_boolean),
    inst_init_boolean,
    inst_walk_boolean,
    inst_free_boolean
  },
  { &consts.cl.integer,
    &consts.str.integer,
    &consts.cl.object,
    sizeof(struct inst_integer),
    inst_init_integer,
    inst_walk_integer,
    inst_free_integer
  },
  { &consts.cl._float,
    &consts.str._float,
    &consts.cl.object,
    sizeof(struct inst_float),
    inst_init_float,
    inst_walk_float,
    inst_free_float
  },
  { &consts.cl.string,
    &consts.str.string,
    &consts.cl.object,
    sizeof(struct inst_string),
    inst_init_string,
    inst_walk_string,
    inst_free_string
  },
  { &consts.cl.dptr,
    &consts.str.dptr,
    &consts.cl.object,
    sizeof(struct inst_dptr),
    inst_init_dptr,
    inst_walk_dptr,
    inst_free_dptr
  },
  { &consts.cl.pair,
    &consts.str.pair,
    &consts.cl.dptr,
    sizeof(struct inst_dptr),
    inst_init_pair,
    inst_walk_pair,
    inst_free_pair
  },
  { &consts.cl.list,
    &consts.str.list,
    &consts.cl.dptr,
    sizeof(struct inst_dptr),
    inst_init_list,
    inst_walk_list,
    inst_free_list
  },
  { &consts.cl.method_call,
    &consts.str.method_call,
    &consts.cl.object,
    sizeof(struct inst_method_call),
    inst_init_method_call,
    inst_walk_method_call,
    inst_free_method_call
  },
  { &consts.cl.block,
    &consts.str.block,
    &consts.cl.object,
    sizeof(struct inst_block),
    inst_init_block,
    inst_walk_block,
    inst_free_block
  },
  { &consts.cl.array,
    &consts.str.array,
    &consts.cl.object,
    sizeof(struct inst_array),
    inst_init_array,
    inst_walk_array,
    inst_free_array
  },
  { &consts.cl.set,
    &consts.str.set,
    &consts.cl.array,
    sizeof(struct inst_set),
    inst_init_set,
    inst_walk_set,
    inst_free_set,
    cl_init_set
  },
  { &consts.cl.dict,
    &consts.str.dictionary,
    &consts.cl.set,
    sizeof(struct inst_dict),
    inst_init_dict,
    inst_walk_dict,
    inst_free_dict,
    cl_init_dict
  },
  { &consts.cl.file,
    &consts.str.file,
    &consts.cl.object,
    sizeof(struct inst_file),
    inst_init_file,
    inst_walk_file,
    inst_free_file,
    cl_init_file
  },
  { &consts.cl.module,
    &consts.str.module,
    &consts.cl.dict,
    sizeof(struct inst_module),
    inst_init_module,
    inst_walk_module,
    inst_free_module,
    cl_init_module
  },
  
  /* Non-instantiable */

  { &consts.cl.env,
    &consts.str.environment,
    &consts.cl.object
  },
  { &consts.cl.error,
    &consts.str.error,
    &consts.cl.object
  },
  { &consts.cl.system,
    &consts.str.system,
    &consts.cl.object
  }
};

const struct init_str init_str_tbl[] = {
    { &consts.str.array,       "#Array" },
    { &consts.str.block,       "#Block" },
    { &consts.str.boolean,     "#Boolean" },
    { &consts.str.code_method, "#Code-Method" },    
    { &consts.str.dictionary,  "#Dictionary" },
    { &consts.str.dptr,        "#Dptr" },
    { &consts.str.environment, "#Environment" },
    { &consts.str.error,       "#Error" },
    { &consts.str.file,        "#File" },
    { &consts.str._float,      "#Float" },
    { &consts.str.integer,     "#Integer" },
    { &consts.str.list,        "#List" },    
    { &consts.str.metaclass,   "#Metaclass" },    
    { &consts.str.method_call, "#Method-Call" },
    { &consts.str.module,      "#Module" },
    { &consts.str.object,      "#Object" },    
    { &consts.str.pair,        "#Pair" },
    { &consts.str.set,         "#Set" },
    { &consts.str.string,      "#String" },
    { &consts.str.system,      "#System" },
    { &consts.str.aandc,       "&and:" },
    { &consts.str.abs,         "abs" },
    { &consts.str.acond,       "&cond" },
    { &consts.str.addc,        "add:" },
    { &consts.str.aforc_loopc, "&for:loop:" },
    { &consts.str.aifc,        "&if:" },
    { &consts.str.aifc_elsec,  "&if:else:" },
    { &consts.str.andc,        "and:" },
    { &consts.str.aorc,        "&or:" },
    { &consts.str.appendc,     "append:" },
    { &consts.str.aquote,      "&quote" },
    { &consts.str.args,        "args" },
    { &consts.str.asc,         "asc" },
    { &consts.str.assert,      "assert" },
    { &consts.str.atc,         "at:" },
    { &consts.str.atc_lengthc, "at:length:" },
    { &consts.str.atc_putc,    "at:put:" },
    { &consts.str.awhilec,     "&while:" },
    { &consts.str._break,      "break" },
    { &consts.str.car,         "car" },
    { &consts.str.cdr,         "cdr" },
    { &consts.str.chr,         "chr" },
    { &consts.str.class_methodc, "class-method:" },
    { &consts.str.class_methods, "class-methods" },
    { &consts.str.class_variables, "class-variables" },
    { &consts.str.comparec,    "compare:" },
    { &consts.str.consc,       "cons:" },
    { &consts.str._continue,   "continue" },
    { &consts.str.copy,        "copy" },
    { &consts.str.count,       "count" },
    { &consts.str.current,     "current" },
    { &consts.str.default_size,   "default-size" },
    { &consts.str.defc_putc,   "def:put:" },
    { &consts.str.delc,        "del:" },
    { &consts.str.divc,        "div:" },
    { &consts.str.equalsc,     "equals:" },
    { &consts.str.eof,         "eof" },
    { &consts.str.eval,        "eval" },
    { &consts.str.evalc,       "eval:" },
    { &consts.str.exit,        "exit" },
    { &consts.str.exitc,       "exit:" },
    { &consts.str._false,      "#false" },
    { &consts.str.filterc,     "filter:" },
    { &consts.str.flush,       "flush" },
    { &consts.str.foreachc,    "foreach:" },
    { &consts.str.formatc,     "format:" },
    { &consts.str.gec,         "ge:" },
    { &consts.str.gtc,         "gt:" },
    { &consts.str.hash,        "hash" },
    { &consts.str.indexc,      "index:" },
    { &consts.str.instance_methodc, "instance-method:" },
    { &consts.str.instance_methods, "instance-methods" },
    { &consts.str.instance_variables, "instance-variables" },
    { &consts.str.instance_of, "instance-of" },
    { &consts.str.keys,        "keys" },
    { &consts.str.lec,         "le:" },
    { &consts.str.length,      "length" },
    { &consts.str.load,        "load" },
    { &consts.str.ltc,         "lt:" },
    { &consts.str.main,        "main" },
    { &consts.str.mapc,        "map:" },
    { &consts.str.memberc,     "member:" },
    { &consts.str.minus,       "minus" },
    { &consts.str.mode,        "mode" },
    { &consts.str.modc,        "mod:" },
    { &consts.str._module,     "module" },
    { &consts.str.multc,       "mult:" },
    { &consts.str.name,        "name" },
    { &consts.str.new,         "new" },
    { &consts.str.newc,        "new:" },
    { &consts.str.newc_asc,    "new:as:" },
    { &consts.str.newc_modec,  "new:mode:" },
    { &consts.str.newc_parentc_instance_variablesc, "new:parent:instance-variables:" },
    { &consts.str.nil,         "#nil" },
    { &consts.str.not,         "not" },
    { &consts.str.orc,         "or:" },
    { &consts.str.parent,      "parent" },
    { &consts.str.path,        "path" },
    { &consts.str.print,       "print" },
    { &consts.str.printc,      "print:" },
    { &consts.str.putc,        "put:" },
    { &consts.str.raisec,      "raise:" },
    { &consts.str.range,       "range" },
    { &consts.str.rangec,      "range:" },
    { &consts.str.rangec_stepc, "range:step:" },
    { &consts.str.read,        "read" },
    { &consts.str.readc,       "read:" },
    { &consts.str.readln,      "readln" },
    { &consts.str.reducec_initc, "reduce:init:" },
    { &consts.str._return,     "return" },
    { &consts.str.rindexc,     "rindex:" },
    { &consts.str._stderr,     "stderr" },
    { &consts.str._stdin,      "stdin" },
    { &consts.str._stdout,     "stdout" },
    { &consts.str.sel,         "sel" },
    { &consts.str.size,        "size" },
    { &consts.str.space,       " " },
    { &consts.str.splicec,     "splice:" },
    { &consts.str.sort,        "sort" },
    { &consts.str.sortc,       "sort:" },
    { &consts.str.splitc,      "split:" },
    { &consts.str.subc,        "sub:" },
    { &consts.str.tostring,    "tostring" },
    { &consts.str.tostringc,   "tostring:" },
    { &consts.str._true,       "#true" },
    { &consts.str.writec,      "write:" },
    { &consts.str.xorc,        "xor:" }
#ifndef NDEBUG
    ,
    { &consts.str.collect,     "collect" },
    { &consts.str.debugc,      "debug:" }
#endif
};

const struct init_method init_cl_method_tbl[] = {
  { &consts.cl.metaclass, &consts.str.name,               cm_metaclass_name },
  { &consts.cl.metaclass, &consts.str.parent,             cm_metaclass_parent },
  { &consts.cl.metaclass, &consts.str.class_methods,      cm_metaclass_cl_methods },
  { &consts.cl.metaclass, &consts.str.class_variables,    cm_metaclass_cl_vars },
  { &consts.cl.metaclass, &consts.str.instance_methods,   cm_metaclass_inst_methods },
  { &consts.cl.metaclass, &consts.str.instance_variables, cm_metaclass_inst_vars },
  { &consts.cl.metaclass, &consts.str.tostring,           cm_metaclass_name },
  { &consts.cl.metaclass, &consts.str.hash,               cm_metaclass_hash },
  /* { &consts.cl.metaclass, &consts.str.tostringc,          cm_metaclass_tostring_fmt }, */
  { &consts.cl.metaclass, &consts.str.newc_parentc_instance_variablesc, cm_class_new },

  { &consts.cl.object, &consts.str.new,  cm_object_new },

  { &consts.cl.boolean, &consts.str.new,  cm_boolean_new },
  { &consts.cl.boolean, &consts.str.newc, cm_boolean_new },

  { &consts.cl.integer, &consts.str.new,  cm_integer_new },
  { &consts.cl.integer, &consts.str.newc, cm_integer_new },

  { &consts.cl._float, &consts.str.new,  cm_float_new },
  { &consts.cl._float, &consts.str.newc, cm_float_new },

  { &consts.cl.string, &consts.str.newc, cm_string_new },

  { &consts.cl.pair, &consts.str.newc, cm_pair_new },

  { &consts.cl.list, &consts.str.new,  cm_list_new },
  { &consts.cl.list, &consts.str.newc, cm_list_new },

  { &consts.cl.method_call, &consts.str.newc,  cm_method_call_new },

  { &consts.cl.array, &consts.str.newc, cm_array_new },

  { &consts.cl.set, &consts.str.new,  cm_set_new },
  { &consts.cl.set, &consts.str.newc, cm_set_new },

  { &consts.cl.dict, &consts.str.new,  cm_dict_new },
  { &consts.cl.dict, &consts.str.newc, cm_dict_new },

  { &consts.cl.module, &consts.str.newc,     cm_module_new },
  { &consts.cl.module, &consts.str.newc_asc, cm_module_new_as },
  { &consts.cl.module, &consts.str.current,  cm_module_cur },

  { &consts.cl.file, &consts.str.newc_modec, cm_file_new },

  { &consts.cl.env, &consts.str.current,   cm_env_current },
  { &consts.cl.env, &consts.str.atc,       cm_env_at },
  { &consts.cl.env, &consts.str.atc_putc,  cm_env_at_put },
  { &consts.cl.env, &consts.str.defc_putc, cm_env_def_put },
  { &consts.cl.env, &consts.str.delc,      cm_env_del },

  { &consts.cl.error, &consts.str.raisec,  cm_error_raise },

  { &consts.cl.system, &consts.str.exit,   cm_system_exit },
  { &consts.cl.system, &consts.str.exitc,  cm_system_exitc }
#ifndef NDEBUG
  ,
  { &consts.cl.system, &consts.str.collect, cm_system_collect },
  { &consts.cl.system, &consts.str.debugc,  cm_system_debug }
#endif

}, init_inst_method_tbl[] = {
  { &consts.cl.metaclass, &consts.str.name,               cm_class_name },
  { &consts.cl.metaclass, &consts.str.parent,             cm_class_parent },
  { &consts.cl.metaclass, &consts.str._module,            cm_class_module },
  { &consts.cl.metaclass, &consts.str.class_methods,      cm_class_cl_methods },
  { &consts.cl.metaclass, &consts.str.class_variables,    cm_class_cl_vars },
  { &consts.cl.metaclass, &consts.str.instance_methods,   cm_class_inst_methods },
  { &consts.cl.metaclass, &consts.str.instance_variables, cm_class_inst_vars },
  { &consts.cl.metaclass, &consts.str.tostring,           cm_class_name },
  { &consts.cl.metaclass, &consts.str.hash,               cm_class_hash },
  { &consts.cl.metaclass, &consts.str.class_methodc,      cm_class_cl_method },
  { &consts.cl.metaclass, &consts.str.instance_methodc,   cm_class_inst_method },

  { &consts.cl.object, &consts.str.aquote,      cm_object_quote },
  { &consts.cl.object, &consts.str.eval,        cm_object_eval },
  { &consts.cl.object, &consts.str.instance_of, cm_object_instof },
  { &consts.cl.object, &consts.str.equalsc,     cm_object_eq },
  { &consts.cl.object, &consts.str.tostring,    cm_object_tostring },
  { &consts.cl.object, &consts.str.tostringc,   cm_object_tostring_fmt },
  { &consts.cl.object, &consts.str.print,       cm_object_print },
  { &consts.cl.object, &consts.str.printc,      cm_object_printc },
  { &consts.cl.object, &consts.str.atc,         cm_object_at },
  { &consts.cl.object, &consts.str.atc_putc,    cm_object_at_put },
  { &consts.cl.object, &consts.str.aifc,        cm_object_if },
  { &consts.cl.object, &consts.str.aifc_elsec,  cm_object_if_else },
  { &consts.cl.object, &consts.str.awhilec,     cm_object_while },
  { &consts.cl.object, &consts.str.aforc_loopc, cm_object_for_loop },
  { &consts.cl.object, &consts.str._break,      cm_object_break },
  { &consts.cl.object, &consts.str._continue,   cm_object_cont },
  { &consts.cl.object, &consts.str._return,     cm_object_return },
  { &consts.cl.object, &consts.str.aandc,       cm_object_and },
  { &consts.cl.object, &consts.str.aorc,        cm_object_or },
  { &consts.cl.object, &consts.str.consc,       cm_object_cons },
  { &consts.cl.object, &consts.str.copy,        cm_object_copy },
  { &consts.cl.object, &consts.str.gec,         cm_object_ge },
  { &consts.cl.object, &consts.str.gtc,         cm_object_gt },
  { &consts.cl.object, &consts.str.lec,         cm_object_le },
  { &consts.cl.object, &consts.str.ltc,         cm_object_lt },

  { &consts.cl.code_method, &consts.str.evalc, cm_code_method_eval },

  { &consts.cl.boolean, &consts.str.andc,      cm_boolean_and },
  { &consts.cl.boolean, &consts.str.orc,       cm_boolean_or },
  { &consts.cl.boolean, &consts.str.xorc,      cm_boolean_xor },
  { &consts.cl.boolean, &consts.str.not,       cm_boolean_not },
  { &consts.cl.boolean, &consts.str.tostring,  cm_boolean_tostring },
  { &consts.cl.boolean, &consts.str.tostringc, cm_boolean_tostring_fmt },
  { &consts.cl.boolean, &consts.str.equalsc,   cm_boolean_equals },
  { &consts.cl.boolean, &consts.str.assert,    cm_boolean_assert },

  { &consts.cl.integer, &consts.str.abs,          cm_integer_abs },
  { &consts.cl.integer, &consts.str.addc,         cm_integer_add },
  { &consts.cl.integer, &consts.str.subc,         cm_integer_sub },
  { &consts.cl.integer, &consts.str.multc,        cm_integer_mult },
  { &consts.cl.integer, &consts.str.divc,         cm_integer_div },
  { &consts.cl.integer, &consts.str.modc,         cm_integer_mod },
  { &consts.cl.integer, &consts.str.range,        cm_integer_range },
  { &consts.cl.integer, &consts.str.rangec,       cm_integer_range_init },
  { &consts.cl.integer, &consts.str.rangec_stepc, cm_integer_range_init_step },
  { &consts.cl.integer, &consts.str.chr,          cm_integer_chr },
  { &consts.cl.integer, &consts.str.equalsc,      cm_integer_equals },
  { &consts.cl.integer, &consts.str.hash,         cm_integer_hash },
  { &consts.cl.integer, &consts.str.minus,        cm_integer_minus },
  { &consts.cl.integer, &consts.str.andc,         cm_integer_and },
  { &consts.cl.integer, &consts.str.orc,          cm_integer_or },
  { &consts.cl.integer, &consts.str.xorc,         cm_integer_xor },
  { &consts.cl.integer, &consts.str.not,          cm_integer_not },
  { &consts.cl.integer, &consts.str.tostring,     cm_integer_tostring },
  { &consts.cl.integer, &consts.str.tostringc,    cm_integer_tostring_fmt },
  { &consts.cl.integer, &consts.str.comparec,     cm_integer_compare },

  { &consts.cl._float, &consts.str.abs,       cm_float_abs },
  { &consts.cl._float, &consts.str.addc,      cm_float_add },
  { &consts.cl._float, &consts.str.subc,      cm_float_sub },
  { &consts.cl._float, &consts.str.multc,     cm_float_mult },
  { &consts.cl._float, &consts.str.divc,      cm_float_div },
  { &consts.cl._float, &consts.str.minus,     cm_float_minus },
  { &consts.cl._float, &consts.str.hash,      cm_float_hash },
  { &consts.cl._float, &consts.str.equalsc,   cm_float_equals },
  { &consts.cl._float, &consts.str.tostring,  cm_float_tostring },
  { &consts.cl._float, &consts.str.tostringc, cm_float_tostring_fmt },
  { &consts.cl._float, &consts.str.comparec,  cm_float_compare },

  { &consts.cl.string, &consts.str.hash,        cm_string_hash },
  { &consts.cl.string, &consts.str.equalsc,     cm_string_equal },
  { &consts.cl.string, &consts.str.appendc,     cm_string_append },
  { &consts.cl.string, &consts.str.tostring,    cm_string_tostring },
  { &consts.cl.string, &consts.str.tostringc,   cm_string_tostring_fmt },
  { &consts.cl.string, &consts.str.eval,        cm_string_eval },
  { &consts.cl.string, &consts.str.print,       cm_string_print },
  { &consts.cl.string, &consts.str.printc,      cm_string_printc },
  { &consts.cl.string, &consts.str.length,      cm_string_len },
  { &consts.cl.string, &consts.str.atc,         cm_string_at },
  { &consts.cl.string, &consts.str.atc_lengthc, cm_string_at_len },
  { &consts.cl.string, &consts.str.asc,         cm_string_asc },
  { &consts.cl.string, &consts.str.foreachc,    cm_string_foreach },
  { &consts.cl.string, &consts.str.indexc,      cm_string_index },
  { &consts.cl.string, &consts.str.rindexc,     cm_string_rindex },
  { &consts.cl.string, &consts.str.splitc,      cm_string_split },
  { &consts.cl.string, &consts.str.comparec,    cm_string_compare },
  { &consts.cl.string, &consts.str.load,        cm_string_load },
  { &consts.cl.string, &consts.str.memberc,     cm_string_member },

  { &consts.cl.dptr, &consts.str.car,      cm_dptr_car },
  { &consts.cl.dptr, &consts.str.cdr,      cm_dptr_cdr },
  { &consts.cl.dptr, &consts.str.hash,     cm_dptr_hash },
  { &consts.cl.dptr, &consts.str.equalsc,  cm_dptr_equals },

  { &consts.cl.pair, &consts.str.eval,      cm_pair_eval },
  { &consts.cl.pair, &consts.str.tostring,  cm_pair_tostring },
  { &consts.cl.pair, &consts.str.tostringc, cm_pair_tostring_fmt },
  { &consts.cl.pair, &consts.str.atc,       cm_pair_at },
  
  { &consts.cl.list, &consts.str.length,        cm_list_len },
  { &consts.cl.list, &consts.str.tostring,      cm_list_tostring },
  { &consts.cl.list, &consts.str.tostringc,     cm_list_tostring_fmt },
  { &consts.cl.list, &consts.str.eval,          cm_list_eval },
  { &consts.cl.list, &consts.str.mapc,          cm_list_map },
  { &consts.cl.list, &consts.str.foreachc,      cm_list_foreach },
  { &consts.cl.list, &consts.str.splicec,       cm_list_splice },
  { &consts.cl.list, &consts.str.appendc,       cm_list_append },
  { &consts.cl.list, &consts.str.hash,          cm_list_hash },
  { &consts.cl.list, &consts.str.equalsc,       cm_list_equals },
  { &consts.cl.list, &consts.str.atc,           cm_list_at },
  { &consts.cl.list, &consts.str.atc_lengthc,   cm_list_at_len },
  { &consts.cl.list, &consts.str.filterc,       cm_list_filter },
  { &consts.cl.list, &consts.str.reducec_initc, cm_list_reduce },
  { &consts.cl.list, &consts.str.formatc,       cm_list_format },
  { &consts.cl.list, &consts.str.acond,         cm_list_cond },
  { &consts.cl.list, &consts.str.memberc,       cm_list_member },

  { &consts.cl.method_call, &consts.str.tostring,  cm_method_call_tostring },
  { &consts.cl.method_call, &consts.str.tostringc, cm_method_call_tostring_fmt },
  { &consts.cl.method_call, &consts.str.eval,      cm_method_call_eval },
  { &consts.cl.method_call, &consts.str.sel,       cm_method_call_sel },
  { &consts.cl.method_call, &consts.str.args,      cm_method_call_args },

  { &consts.cl.block, &consts.str.eval,      cm_block_eval },
  { &consts.cl.block, &consts.str.evalc,     cm_block_evalc },
  { &consts.cl.block, &consts.str.tostring,  cm_block_tostring },
  { &consts.cl.block, &consts.str.tostringc, cm_block_tostring_fmt },

  { &consts.cl.array, &consts.str.atc,       cm_array_at },
  { &consts.cl.array, &consts.str.atc_putc,  cm_array_at_put },
  { &consts.cl.array, &consts.str.tostring,  cm_array_tostring },
  { &consts.cl.array, &consts.str.tostringc, cm_array_tostring_fmt },
  { &consts.cl.array, &consts.str.size,      cm_array_size },
  { &consts.cl.array, &consts.str.sort,      cm_array_sort },
  { &consts.cl.array, &consts.str.sortc,     cm_array_sortc },
  { &consts.cl.array, &consts.str.equalsc,   cm_array_equals },
  { &consts.cl.array, &consts.str.foreachc,  cm_array_foreach },
  { &consts.cl.array, &consts.str.memberc,   cm_array_member },
  
  { &consts.cl.set, &consts.str.memberc,  cm_set_member },
  { &consts.cl.set, &consts.str.putc,     cm_set_put },
  { &consts.cl.set, &consts.str.delc,     cm_set_del },
  { &consts.cl.set, &consts.str.count,    cm_set_count },
  { &consts.cl.set, &consts.str.tostring, cm_set_tostring },
  { &consts.cl.set, &consts.str.foreachc, cm_set_foreach },

  { &consts.cl.dict, &consts.str.atc,       cm_dict_at },
  { &consts.cl.dict, &consts.str.atc_putc,  cm_dict_at_put },
  { &consts.cl.dict, &consts.str.delc,      cm_dict_del },
  { &consts.cl.dict, &consts.str.keys,      cm_dict_keys },
  { &consts.cl.dict, &consts.str.memberc,   cm_dict_member },
  { &consts.cl.dict, &consts.str.putc,      cm_dict_put },

  { &consts.cl.module, &consts.str.name,      cm_module_name },
  { &consts.cl.module, &consts.str.parent,    cm_module_parent },
  { &consts.cl.module, &consts.str.atc,       cm_module_at },
  { &consts.cl.module, &consts.str.tostring,  cm_module_tostring },
  { &consts.cl.module, &consts.str.tostringc, cm_module_tostring_fmt },

  { &consts.cl.file, &consts.str.name,      cm_file_name },
  { &consts.cl.file, &consts.str.mode,      cm_file_mode },
  { &consts.cl.file, &consts.str.tostring,  cm_file_tostring },
  { &consts.cl.file, &consts.str.tostringc, cm_file_tostring_fmt },
  { &consts.cl.file, &consts.str.read,      cm_file_read },
  { &consts.cl.file, &consts.str.readc,     cm_file_readc },
  { &consts.cl.file, &consts.str.readln,    cm_file_readln },
  { &consts.cl.file, &consts.str.writec,    cm_file_write },
  { &consts.cl.file, &consts.str.flush,     cm_file_flush },
  { &consts.cl.file, &consts.str.eof,       cm_file_eof },
  { &consts.cl.file, &consts.str.load,      cm_file_load }
};

void
init_cls(const struct init_cl *tbl, unsigned n)
{
  for ( ; n; --n, ++tbl) {
    void (*f)(obj_t);
    
    m_class_new(*tbl->name,
		*tbl->parent,
		module_cur, 
		tbl->inst_size,
		tbl->inst_init,
		tbl->inst_walk,
		tbl->inst_free
		);
    OBJ_ASSIGN(*tbl->cl, R0);

    if (f = tbl->cl_init)  (*f)(*tbl->cl);
  }
}

void
init_strs(const struct init_str *tbl, unsigned n)
{
  for ( ; n; --n, ++tbl) {
    m_string_newc(consts.cl.string, 1, strlen(tbl->str), tbl->str);
    OBJ_ASSIGN(*tbl->obj, R0);
  }
}

void
init_cl_methods(const struct init_method *tbl, unsigned n)
{
  for ( ; n; --n, ++tbl) {
    m_code_method_new(consts.cl.code_method, tbl->func);
    dict_at_put(CLASS(*tbl->cl)->cl_methods,
		*tbl->sel,
		R0
		);
  }
}

void
init_inst_methods(const struct init_method *tbl, unsigned n)
{
  for ( ; n; --n, ++tbl) {
    m_code_method_new(consts.cl.code_method, tbl->func);
    dict_at_put(CLASS(*tbl->cl)->inst_methods,
		*tbl->sel,
		R0
		);
  }
}

void
init(void)
{
  unsigned i;

  initf = 1;

  /* Step 0. Init memory management */

  stack       = _cmalloc(STACK_SIZE * sizeof(obj_t));
  stack_end   = stack + STACK_SIZE;
  stack_fence = stack + STACK_HEADROOM;
  sp          = stack_end;

  for (i = 0; i < ARRAY_SIZE(obj_list); ++i)  list_init(&obj_list[i]);

  /* Step 1. Create classes, first pass */

  for (i = 0; i < ARRAY_SIZE(init_cl_tbl); ++i) {
    if (i == 0) {
      vm_assign(0, obj_alloc(sizeof(struct inst_metaclass)));
    } else {
      m_inst_alloc(consts.cl.metaclass);
    }
    CLASS(R0)->inst_size = init_cl_tbl[i].inst_size;
    CLASS(R0)->inst_init = init_cl_tbl[i].inst_init;
    CLASS(R0)->inst_walk = init_cl_tbl[i].inst_walk;
    CLASS(R0)->inst_free = init_cl_tbl[i].inst_free;
    list_init(CLASS(R0)->inst_cache);

    OBJ_ASSIGN(*init_cl_tbl[i].cl, R0);
  }

  /* Step 2. Fix up class hierarchy */

  for (i = 0; i < ARRAY_SIZE(init_cl_tbl); ++i) {
    OBJ_ASSIGN(CLASS(*init_cl_tbl[i].cl)->parent,
	       init_cl_tbl[i].parent ? *init_cl_tbl[i].parent : NIL
	       );
  }

  /* Step 3. Create constants */

  m_inst_alloc(consts.cl.boolean);
  inst_init(R0, 0);
  OBJ_ASSIGN(consts._bool._false, R0);
  m_inst_alloc(consts.cl.boolean);
  inst_init(R0, 1);
  OBJ_ASSIGN(consts._bool._true, R0);
  
  init_strs(init_str_tbl, ARRAY_SIZE(init_str_tbl));

  /* Step 4. Create classes, second pass */

  for (i = 0; i < ARRAY_SIZE(init_cl_tbl); ++i) {
    OBJ_ASSIGN(CLASS(*init_cl_tbl[i].cl)->name, *init_cl_tbl[i].name);
    m_string_dict_new(consts.cl.dict, 16);
    OBJ_ASSIGN(CLASS(*init_cl_tbl[i].cl)->cl_vars, R0);
    m_string_dict_new(consts.cl.dict, 16);
    OBJ_ASSIGN(CLASS(*init_cl_tbl[i].cl)->cl_methods, R0);
    m_string_dict_new(consts.cl.dict, 16);
    OBJ_ASSIGN(CLASS(*init_cl_tbl[i].cl)->inst_vars, R0);
    m_string_dict_new(consts.cl.dict, 16);
    OBJ_ASSIGN(CLASS(*init_cl_tbl[i].cl)->inst_methods, R0);
  }  

  init_cl_methods(init_cl_method_tbl, ARRAY_SIZE(init_cl_method_tbl));

  init_inst_methods(init_inst_method_tbl, ARRAY_SIZE(init_inst_method_tbl));

  for (i = 0; i < ARRAY_SIZE(init_cl_tbl); ++i) {
    void (*f)(obj_t);
    
    if (f = init_cl_tbl[i].cl_init)  (*f)(*init_cl_tbl[i].cl);
  }

  /* Step 5. Create main module */

  m_module_new(consts.cl.module, consts.str.main, NIL);
  MODULE(R0)->consts  = (obj_t *) &consts;
  MODULE(R0)->nconsts = sizeof(consts) / sizeof(obj_t);

  OBJ_ASSIGN(module_main, R0);

  dict_at_put(OBJ(MODULE(module_main)->base),
	      consts.str.nil,
	      NIL
	      );
  dict_at_put(OBJ(MODULE(module_main)->base),
	      consts.str._true,
	      consts._bool._true
	      );
  dict_at_put(OBJ(MODULE(module_main)->base),
	      consts.str._false,
	      consts._bool._false
	      );

  for (i = 0; i < ARRAY_SIZE(init_cl_tbl); ++i) {
    dict_at_put(OBJ(MODULE(module_main)->base),
		*init_cl_tbl[i].name,
		*init_cl_tbl[i].cl
		);
  }

  dict_at_put(OBJ(MODULE(module_main)->base),
	      consts.str.main,
	      module_main
	      );

  module_cur = module_main;

  /* Step 6. Fix up class module membership */

  for (i = 0; i < ARRAY_SIZE(init_cl_tbl); ++i) {
    OBJ_ASSIGN(CLASS(*init_cl_tbl[i].cl)->module, module_main);
  }

  yy_input_init();

  initf = 0;
}

int
init_load(void)
{
  static const char init_file_name[] = "/usr/share/ool2/init.ool";
  static const char init_file_mode[] = "r+";

  int  result = 0;
  FILE *fp;

  if ((fp = fopen(init_file_name, init_file_mode)) == 0)  return (result);

  FRAME_RESTART_BEGIN {
    if (frame_jmp_code != 0) {
      result = -1;
    } else {
      m_string_newc(consts.cl.string, 1, sizeof(init_file_name) - 1, init_file_name);
      vm_assign(1, R0);
      m_string_newc(consts.cl.string, 1, sizeof(init_file_mode) - 1, init_file_mode);
      m_file_new(consts.cl.file, R1, R0, fp);
      m_file_load(R0);
    }
  } FRAME_RESTART_END;

  return (result);
}

void
interactive(void)
{
  FRAME_RESTART_BEGIN {
    frame_jmp_code = frame_jmp_code; /* Suppress unused variable warning */
    
    for (;;) {
      printf("\nok ");
      fflush(stdout);
      
      yyparse();
 
      if (yy_input_eof())  break;

      vm_dropn(frp->sp - sp); /* Discard all objs pushed during parsing */

      m_method_call_1(consts.str.eval, R0);
      m_method_call_1(consts.str.print, R0);
    }
  } FRAME_RESTART_END;
}


void
batch(char *filename)
{
  static const char mode[] = "r+";

  FILE *fp;

  fp = fopen(filename, mode);
  if (fp == 0) {
    fprintf(stderr, "Failed to open %s: ", filename);
    perror(0);
    zexit(1);
  }

  FRAME_RESTART_BEGIN {
    if (frame_jmp_code == 0) {
      m_string_newc(consts.cl.string, 1, strlen(filename), filename);
      vm_assign(1, R0);
      m_string_newc(consts.cl.string, 1, sizeof(mode) - 1, mode);
      
      m_file_new(consts.cl.file, R1, R0, fp);
      
      m_file_load(R0);
    }
  } FRAME_RESTART_END;
}


int
yyerror(char *s)
{
  yy_input_flush();

  error(ERR_PARSE, s);

  return (0);
}


int
main(int argc, char **argv)
{
  init();

#ifndef NDEBUG
  collect();			/* Check consistency */
  ASSERT(stats->mem->collected_cnt == 0);
  ASSERT(stats->mem->bytes_collected == 0);
#endif

  FRAME_MODULE_BEGIN(module_main) {
    if (argc > 1) {
      if (init_load() == 0)  batch(argv[1]);
    } else {
      init_load();

      interactive();
    }
  } FRAME_MODULE_END;

  zexit(0);

  return (0);			/* Suppress warning */
}
