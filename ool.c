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
  } vm;
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
  } mem;
} stats;

void
stats_print(void)
{
#define PRINT_STAT(x)  printf("%s = %u\n", #x, x)

  PRINT_STAT(stats.vm.stack_depth);
  PRINT_STAT(stats.vm.stack_depth_max);
  PRINT_STAT(stats.mem.alloc_cnt);
  PRINT_STAT(stats.mem.bytes_alloced);
  PRINT_STAT(stats.mem.free_cnt);
  PRINT_STAT(stats.mem.bytes_freed);
  PRINT_STAT(stats.mem.bytes_in_use);
  PRINT_STAT(stats.mem.bytes_in_use_max);
  PRINT_STAT(stats.mem.bytes_in_use_collect);
  PRINT_STAT(stats.mem.collect_cnt);
  PRINT_STAT(stats.mem.collected_cnt);
  PRINT_STAT(stats.mem.bytes_collected);
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

#define CRC32_INIT(r)  ((r) = 0xffffffff)

unsigned
_crc32(unsigned *r, void *buf, unsigned n)
{
  unsigned char *p = (unsigned char *) buf;

  for ( ; n; --n, ++p) {
    *r = crc32_tbl[(*r ^ *p) & 0xFF] ^ (*r >> 8);
  }
  
  return (~*r);
}
#define CRC32(r, p, n)  (_crc32(&(r), (p), (n)))

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
  ++stats.mem.alloc_cnt;
  stats.mem.bytes_alloced += size;
  stats.mem.bytes_in_use  += size;
  if (stats.mem.bytes_in_use > stats.mem.bytes_in_use_max) {
    stats.mem.bytes_in_use_max = stats.mem.bytes_in_use;
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
  ++stats.mem.free_cnt;
  stats.mem.bytes_freed += size;
  stats.mem.bytes_in_use -= size;
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

void  obj_release(obj_t obj);

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
_obj_alloc(unsigned size)
{
  obj_t result = OBJ(_zcmalloc(size));

  list_insert(result->list_node, LIST_END(OBJ_LIST_ACTIVE));

  return (result);
}

/* Allocate storage for an instance of the given class */

obj_t
obj_alloc(obj_t cl)
{
  obj_t result = _obj_alloc(CLASS(cl)->inst_size);

  OBJ_ASSIGN(result->inst_of, cl);

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

  /* The actual free */
  _cfree(CLASS(inst_of(obj))->inst_size, obj);
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

#ifndef NDEBUG
  ++stats.mem.collect_cnt;
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
    ++stats.mem.collected_cnt;
    stats.mem.bytes_collected += CLASS(inst_of(r))->inst_size;
#endif

    obj_free(r);
  }
  
  obj_list_swap();
}

#ifndef NDEBUG

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

void
vm_inst_alloc(obj_t cl)
{
  OBJ_ASSIGN(R0, obj_alloc(cl));
}

#define ASSERT_REG_VALID(r)  (HARD_ASSERT((r) < ARRAY_SIZE(regs)))

void
vm_assign(unsigned dst, obj_t val)
{
  ASSERT_REG_VALID(dst);

  OBJ_ASSIGN(REG(dst), val);
}

#define VM_STACK_CHECK_PUSH(n) \
  do { if ((sp - ((n) + 8)) < stack)  error(ERR_STACK_OVF); } while (0)
#define VM_STACK_CHECK_POP(n) \
  HARD_ASSERT((sp + (n)) <= stack_end)

#ifndef NDEBUG
#define VM_STATS_UPDATE_PUSH(n)						\
  do {									\
    if ((stats.vm.stack_depth += (n)) > stats.vm.stack_depth_max) {	\
      stats.vm.stack_depth_max = stats.vm.stack_depth;			\
    }									\
  } while (0)
#define VM_STATS_UPDATE_POP(n) \
  (stats.vm.stack_depth -= (n))
#else
#define VM_STATS_UPDATE_PUSH(n)
#define VM_STATS_UPDATE_POP(n)
#endif

void
vm_pushl(obj_t obj)
{
  VM_STACK_CHECK_PUSH(1);
  VM_STATS_UPDATE_PUSH(1);

  *--sp = obj_retain(obj);
}

void
vm_push(unsigned src)
{
  ASSERT_REG_VALID(src);

  vm_pushl(REG(src));
}

void
vm_pushm(unsigned src, unsigned n)
{
  obj_t *p;

  ASSERT_REG_VALID(src + n - 1);
  VM_STACK_CHECK_PUSH(n);
  VM_STATS_UPDATE_PUSH(n);

  for (p = &REG(src); n; --n, ++p)  *--sp = obj_retain(*p);
}

void
vm_pop(unsigned dst)
{
  ASSERT_REG_VALID(dst);
  VM_STACK_CHECK_POP(1);
  VM_STATS_UPDATE_POP(1);

  _obj_assign(&REG(dst), *sp);
  ++sp;
}

void
vm_popm(unsigned dst, unsigned n)
{
  obj_t *p;

  ASSERT_REG_VALID(dst + n - 1);
  VM_STACK_CHECK_POP(n);
  VM_STATS_UPDATE_POP(n);

  for (p = &REG(dst + n - 1); n; --n, --p, ++sp)  _obj_assign(p, *sp);
}

void
vm_drop(void)
{
  VM_STACK_CHECK_POP(1);
  VM_STATS_UPDATE_POP(1);

  obj_release(*sp);
  ++sp;
}

void
vm_dropn(unsigned n)
{
  VM_STACK_CHECK_POP(n);
  VM_STATS_UPDATE_POP(n);

  for ( ; n; --n, ++sp)  obj_release(*sp);
}

/***************************************************************************/

struct frame {
  struct frame *prev;
  unsigned     type;
  obj_t        *sp;
};

struct frame_jmp {
  struct frame base;
  jmp_buf      jmp_buf;
};

struct frame *frp;

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
  frame_jmp_code = setjmp(__frame->jmp_buf);

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
  __frame->module_prev = module_cur;		\
  __frame->module = module_cur = (m);		\
  frp = &__frame->base;

#define FRAME_MODULE_POP \
  do { module_cur = ((struct frame_module *) frp)->module_prev;  FRAME_POP; } while (0)

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
  obj_t            dict;
};

#define FRAME_BLOCK_BEGIN(d)				\
  {							\
  struct frame_block __frame[1];			\
  int                frame_jmp_code;			\
  __frame->base.base.prev = frp;			\
  __frame->base.base.type = FRAME_TYPE_BLOCK;		\
  __frame->base.base.sp   = sp;				\
  __frame->dict = (d);					\
  frp = &__frame->base.base;				\
  frame_jmp_code = setjmp(__frame->base.jmp_buf);

#define FRAME_BLOCK_POP  FRAME_POP

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

void m_method_call(obj_t sel, unsigned argc, obj_t args);
void m_method_call_1(obj_t sel, obj_t recvr);
void m_method_call_2(obj_t sel, obj_t recvr, obj_t arg);
void m_method_call_3(obj_t sel, obj_t recvr, obj_t arg1, obj_t arg2);

void
method_run(struct frame_method_call *mcfrp, obj_t cl, obj_t sel, obj_t func, unsigned argc, obj_t args)
{
  if (mcfrp) {
    mcfrp->cl   = cl;
    mcfrp->args = args;
  }

  cl = inst_of(func);

  if (cl == consts.cl.code_method) {
    (*CODE_METHOD(func)->func)(argc, args);
    return;
  }
  if (cl == consts.cl.block) {
    if (mcfrp && mcfrp->cl) {
      FRAME_MODULE_BEGIN(CLASS(mcfrp->cl)->module) {

	m_method_call_2(consts.str.evalc, func, args);
	
      } FRAME_MODULE_END;
    } else {
      m_method_call_2(consts.str.evalc, func, args);
    }

    return;
  }

  error(ERR_INVALID_METHOD);
}

void
m_method_call(obj_t sel, unsigned argc, obj_t args)
{
  obj_t recvr = CAR(args);
  obj_t cl, p;

  vm_push(0);

  FRAME_METHOD_CALL_BEGIN(sel, args) {
    struct frame_method_call *mcfrp = (struct frame_method_call *) frp;

    cl = inst_of(recvr);
    if (cl == NIL || cl == consts.cl.metaclass) {
      for (cl = recvr; cl; cl = CLASS(cl)->parent) {
	if (p = dict_at(CLASS(cl)->cl_methods, sel)) {
	  method_run(mcfrp, cl, sel, CDR(p), argc, args);
	  goto done;
	}
      }
    }
    
    for (cl = inst_of(recvr); cl; cl = CLASS(cl)->parent) {
      if (p = dict_at(CLASS(cl)->inst_methods, sel)) {
	method_run(mcfrp, cl, sel, CDR(p), argc, args);
	goto done;
      }
    }
    
    if (mcfrp) {
      cl = inst_of(recvr);
      if (!(cl == NIL || cl == consts.cl.metaclass)) {
	mcfrp->cl = cl;
      }
    }
    
    error(ERR_NO_METHOD);

  done:
    ;
  } FRAME_METHOD_CALL_END;
  
  vm_drop();
}

void
m_method_call_1(obj_t sel, obj_t recvr)
{
  vm_push(0);

  m_cons(recvr, NIL);

  m_method_call(sel, 1, R0);

  vm_drop();
}

void
m_method_call_2(obj_t sel, obj_t recvr, obj_t arg)
{
  vm_push(0);

  m_cons(arg, NIL);
  m_cons(recvr, R0);

  m_method_call(sel, 2, R0);

  vm_drop();
}

void
m_method_call_3(obj_t sel, obj_t recvr, obj_t arg1, obj_t arg2)
{
  vm_push(0);

  m_cons(arg2, NIL);
  m_cons(arg1, R0);
  m_cons(recvr, R0);

  m_method_call(sel, 3, R0);

  vm_drop();
}

/***************************************************************************/

/* Metaclass */

void
metaclass_init(void)
{
  vm_push(0);

  m_integer_new(FIELD_OFS(struct inst_metaclass, cl_methods));
  dict_at_put(CLASS(consts.cl.metaclass)->inst_vars, consts.str.class_methods, R0);
  m_integer_new(FIELD_OFS(struct inst_metaclass, cl_vars));
  dict_at_put(CLASS(consts.cl.metaclass)->inst_vars, consts.str.class_variables, R0);
  m_integer_new(FIELD_OFS(struct inst_metaclass, inst_methods));
  dict_at_put(CLASS(consts.cl.metaclass)->inst_vars, consts.str.instance_methods, R0);

  vm_pop(0);
}

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
  
  vm_assign(0, NIL);
}

void
cm_metaclass_inst_vars(unsigned argc, obj_t args)
{
  if (argc != 1)  error(ERR_NUM_ARGS);

  /* Must be hard-coded, because
     inst_vars(#Metaclass) only has mutable ones
  */
  
  m_cons(consts.str.instance_variables, NIL);
  m_cons(consts.str.instance_methods, R0);
  m_cons(consts.str.class_variables, R0);
  m_cons(consts.str.class_methods, R0);
  m_cons(consts.str.parent, R0);
  m_cons(consts.str.name, R0);
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

  m_string_dict_new(16);
  OBJ_ASSIGN(CLASS(inst)->cl_vars, R0);
  m_string_dict_new(16);
  OBJ_ASSIGN(CLASS(inst)->cl_methods, R0);
  m_string_dict_new(16);
  OBJ_ASSIGN(CLASS(inst)->inst_vars, R0);
  m_string_dict_new(16);
  OBJ_ASSIGN(CLASS(inst)->inst_methods, R0);
  
  dict_at_put(OBJ(MODULE(CLASS(inst)->module)->base), CLASS(inst)->name, inst);

  vm_pop(0);

  inst_init_parent(cl, inst, ap);
}

void
inst_walk_class(obj_t cl, obj_t inst, void (*func)(obj_t))
{
  _inst_walk_metaclass(inst, func);

  inst_walk_parent(cl, inst, func);
}

void
inst_free_class(obj_t cl, obj_t inst)
{
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

  vm_inst_alloc(consts.cl.metaclass);
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

  vm_push(1);

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
    m_integer_new(ofs);
    dict_at_put(CLASS(R1)->inst_vars, CAR(inst_vars), R0);
  }

  vm_assign(0, R1);

  vm_pop(1);
}

void
m_fqclname(obj_t cl)
{
    obj_t s;

    vm_push(0);

    s = CLASS(cl)->name;
    m_fqmodname(CLASS(cl)->module);
    if (string_len(R0) == 0) {
      vm_assign(0, s);
    } else {
      m_string_new(3, string_len(R0), STRING(R0)->data,
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
cm_class_inst_vars(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.metaclass))  error(ERR_INVALID_ARG, recvr);
  
  m_dict_keys(CLASS(recvr)->inst_vars);
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
  
  vm_inst_alloc(recvr);
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
  
  m_boolean_new(CAR(args) == CAR(CDR(args)));
}

void
cm_object_tostring(unsigned argc, obj_t args)
{
  obj_t recvr;
  char  buf[64];
  
  if (argc != 1)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (recvr == NIL) {
    m_string_new(1, 2, "()");
    return;
  }
  
  m_fqclname(inst_of(recvr));

  m_string_new(1,
	       snprintf(buf, sizeof(buf), "<instance of %s @ %p>",
			STRING(R0)->data,
			recvr
			),
	       buf
	       );
}

void
cm_object_print(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)  error(ERR_NUM_ARGS);
  recvr = CAR(args);

  m_method_call_1(consts.str.tostring, recvr);
  m_method_call_1(consts.str.print, R0);
  
  vm_assign(0, recvr);
}

void
m_obj_printc(obj_t obj, obj_t outf)
{
  m_method_call_1(consts.str.tostring, obj);
  m_method_call_2(consts.str.printc, R0, outf);
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

void
cm_object_append(unsigned argc, obj_t args)
{
  obj_t recvr, arg;
  
  if (argc != 2)     error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (recvr != NIL)  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  if (!(arg == NIL || is_kind_of(arg, consts.cl.list)))  error(ERR_INVALID_ARG, arg);
  
  vm_assign(0, arg);
}

obj_t *
obj_attr_find(obj_t inst, obj_t s)
{
  obj_t cl, p;

  cl = inst_of(inst);
  if (cl == NIL || cl == consts.cl.metaclass) {
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

  error(ERR_NO_ATTR);

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
  m_method_call_1(consts.str.eval, recvr);
  if (!is_kind_of(R0, consts.cl.boolean))     error(ERR_INVALID_ARG, recvr);
  
  if (BOOLEAN(R0)->val) {
    m_method_call_1(consts.str.eval, CAR(CDR(args)));
  } else {
    vm_assign(0, R0);
  }
}

void
cm_object_if_else(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 3)                              error(ERR_NUM_ARGS);
  recvr = CAR(args);
  m_method_call_1(consts.str.eval, recvr);
  if (!is_kind_of(R0, consts.cl.boolean))     error(ERR_INVALID_ARG, recvr);
  args = CDR(args);
  
  m_method_call_1(consts.str.eval,
		  BOOLEAN(R0)->val ? CAR(args) : CAR(CDR(args))
		  );
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
m_code_method_new(void (*func)(unsigned, obj_t))
{
  vm_inst_alloc(consts.cl.code_method);
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
  if (!(nargs == NIL || is_kind_of(nargs, consts.cl.list)))  error(ERR_INVALID_ARG, nargs);

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
m_boolean_new(unsigned val)
{
  vm_assign(0, val ? consts._bool._true : consts._bool._false);
}

void
cm_boolean_new(unsigned argc, obj_t args)
{
  obj_t    arg;
  unsigned bval;
  
  if (argc < 1 || argc > 2)  error(ERR_NUM_ARGS);
  if (argc < 2) {
    bval = 0;
  } else {
    arg = CAR(CDR(args));

    if (is_kind_of(arg, consts.cl.boolean)) {
      vm_assign(0, arg);
      return;
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

  m_boolean_new(bval);
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
  
  m_boolean_new(BOOLEAN(recvr)->val && BOOLEAN(arg)->val);
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
  
  m_boolean_new(BOOLEAN(recvr)->val || BOOLEAN(arg)->val);
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
  
  m_boolean_new((BOOLEAN(recvr)->val != 0) ^ (BOOLEAN(arg)->val != 0));
}

void
cm_boolean_not(unsigned argc, obj_t args)
{
  obj_t recvr;
  
  if (argc != 1)                              error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.boolean))  error(ERR_INVALID_ARG, recvr);
  
  m_boolean_new(!BOOLEAN(recvr)->val);
}

void
cm_boolean_tostring(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)                              error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.boolean))  error(ERR_INVALID_ARG, recvr);
  
  vm_assign(0, BOOLEAN(recvr)->val ? consts.str._true : consts.str._false);
}

void
cm_boolean_equals(unsigned argc, obj_t args)
{
  obj_t recvr, arg;

  if (argc != 2)                              error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.boolean))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  
  m_boolean_new(inst_of(arg) == inst_of(recvr)
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
m_integer_new(integer_val_t val)
{
  vm_inst_alloc(consts.cl.integer);
  inst_init(R0, val);
}

void
cm_integer_new(unsigned argc, obj_t args)
{
  obj_t         arg;
  integer_val_t ival;

  if (argc < 1 || argc > 2)  error(ERR_NUM_ARGS);
  if (argc < 2) {
    ival = 0;
  } else {
    arg = CAR(CDR(args));

    if (is_kind_of(arg, consts.cl.boolean)) {
      ival = BOOLEAN(arg)->val != 0;
    } else if (is_kind_of(arg, consts.cl.integer)) {
      vm_assign(0, arg);
      return;
    } else if (is_kind_of(arg, consts.cl._float)) {
      ival = (integer_val_t) FLOAT(arg)->val;
    } else if (is_kind_of(arg, consts.cl.string)) {
      unsigned n = string_len(arg);
      char     *fmt;
      
      if (n >= 3
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

  m_integer_new(ival);
}

void
cm_integer_tostring(unsigned argc, obj_t args)
{
  obj_t recvr;
  char  buf[32];
  
  if (argc != 1)                              error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.integer))  error(ERR_INVALID_ARG, recvr);
  
  m_string_new(1, snprintf(buf, sizeof(buf), "%lld", INTEGER(recvr)->val), buf);
}

void
cm_integer_tostring_base(unsigned argc, obj_t args)
{
  obj_t          recvr, arg;
  integer_val_t  val, base;
  uinteger_val_t uval, ubase;
  char           buf[32], *p;
  unsigned char  negf;
  unsigned       n;
  
  if (argc != 2)                              error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.integer))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  if (!is_kind_of(arg, consts.cl.integer))    error(ERR_INVALID_ARG, arg);
  base = INTEGER(arg)->val;
  if (base <= 0 || base > 36)                 error(ERR_INVALID_ARG, arg);
  ubase = (uinteger_val_t) base;

  p = &buf[ARRAY_SIZE(buf)];
  n = 0;

  val = INTEGER(recvr)->val;
  negf = 0;
  if (base == 10 && val < 0) {
    negf = 1;
    val = -val;
  }
  uval = (uinteger_val_t) val;

  for ( ; n == 0 || uval != 0; uval /= ubase) {
    uinteger_val_t d = uval % ubase;
    char           c = d + (d <= 9 ? '0' : 'A' - 10);
    
    ASSERT(n < sizeof(buf));
    
    *--p = c;
    ++n;
    
    if (uval == 0)  break;
  }
  if (negf) {
    ASSERT(n < sizeof(buf));

    *--p = '-';
    ++n;
  }

  m_string_new(1, n, p);
}

void
cm_integer_hash(unsigned argc, obj_t args)
{
  obj_t    recvr;
  unsigned r;
  
  if (argc != 1)                              error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.integer))  error(ERR_INVALID_ARG, recvr);
  
  CRC32_INIT(r);
  m_integer_new(CRC32(r, &INTEGER(recvr)->val, sizeof(INTEGER(recvr)->val)));
}

void
cm_integer_equals(unsigned argc, obj_t args)
{
  obj_t recvr, arg;

  if (argc != 2)                              error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.integer))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  
  m_boolean_new(is_kind_of(arg, consts.cl.integer)
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
  
  m_integer_new(-INTEGER(recvr)->val);
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
  
  m_integer_new(iresult);
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
  
  m_integer_new(iresult);
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
  
  m_integer_new(iresult);
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

  m_integer_new(ival1 / ival2);
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

  m_integer_new(ival1 % ival2);
}

void
m_integer_range(integer_val_t init, integer_val_t lim, integer_val_t step)
{
    integer_val_t val;
    obj_t         *p;

    vm_push(0);
    vm_push(1);

    vm_assign(1, NIL);
    for (p = &R1, val = init; val < lim; val += step) {
      m_integer_new(val);
      m_cons(R0, NIL);
      OBJ_ASSIGN(*p, R0);
      p = &CDR(R0);
    }
    vm_assign(0, R1);

    vm_pop(1);
    vm_drop();
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
  m_string_new(1, 1, &c);
}

void
cm_integer_lt(unsigned argc, obj_t args)
{
  obj_t recvr, arg;

  if (argc != 2)                              error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.integer))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  if (!is_kind_of(arg, consts.cl.integer))    error(ERR_INVALID_ARG, arg);

  m_boolean_new(INTEGER(recvr)->val < INTEGER(arg)->val);
}

void
cm_integer_gt(unsigned argc, obj_t args)
{
  obj_t recvr, arg;

  if (argc != 2)                              error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.integer))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  if (!is_kind_of(arg, consts.cl.integer))    error(ERR_INVALID_ARG, arg);

  m_boolean_new(INTEGER(recvr)->val > INTEGER(arg)->val);
}

void
cm_integer_le(unsigned argc, obj_t args)
{
  obj_t recvr, arg;

  if (argc != 2)                              error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.integer))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  if (!is_kind_of(arg, consts.cl.integer))    error(ERR_INVALID_ARG, arg);

  m_boolean_new(INTEGER(recvr)->val <= INTEGER(arg)->val);
}

void
cm_integer_ge(unsigned argc, obj_t args)
{
  obj_t recvr, arg;

  if (argc != 2)                              error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.integer))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  if (!is_kind_of(arg, consts.cl.integer))    error(ERR_INVALID_ARG, arg);

  m_boolean_new(INTEGER(recvr)->val >= INTEGER(arg)->val);
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

  m_integer_new(INTEGER(recvr)->val & INTEGER(arg)->val);
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

  m_integer_new(INTEGER(recvr)->val | INTEGER(arg)->val);
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

  m_integer_new(INTEGER(recvr)->val ^ INTEGER(arg)->val);
}

void
cm_integer_not(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)                              error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.integer))  error(ERR_INVALID_ARG, recvr);

  m_integer_new(~INTEGER(recvr)->val);
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
m_float_new(float_val_t val)
{
  vm_inst_alloc(consts.cl._float);
  inst_init(R0, val);
}

void
cm_float_new(unsigned argc, obj_t args)
{
  obj_t       arg;
  float_val_t fval;

  if (argc < 1 || argc > 2)  error(ERR_NUM_ARGS);
  if (argc < 2) {
    fval = 0.0;
  } else {
    arg = CAR(CDR(args));
  
    if (is_kind_of(arg, consts.cl.boolean)) {
      fval = BOOLEAN(arg)->val ? 1.0 : 0.0;
    } else if (is_kind_of(arg, consts.cl.integer)) {
      fval = (float_val_t) INTEGER(arg)->val;
    } else if (is_kind_of(arg, consts.cl._float)) {
      vm_assign(0, arg);
      return;
    } else if (is_kind_of(arg, consts.cl.string)) {
      if (sscanf(STRING(arg)->data, FLOAT_SCANF_FMT, &fval) != 1) {
	error(ERR_INVALID_ARG, arg);
      }
    } else {
      error(ERR_INVALID_ARG, arg);
    }
  }

  m_float_new(fval);
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
cm_float_add(unsigned argc, obj_t args)
{
  obj_t recvr, arg;

  if (argc != 2)                             error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl._float))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  if (!is_kind_of(arg, consts.cl._float))    error(ERR_INVALID_ARG, arg);

  m_float_new(FLOAT(recvr)->val + FLOAT(arg)->val);
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

  m_float_new(FLOAT(recvr)->val - FLOAT(arg)->val);
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

  m_float_new(FLOAT(recvr)->val * FLOAT(arg)->val);
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

  m_float_new(FLOAT(recvr)->val / FLOAT(arg)->val);
}

void
cm_float_minus(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)                             error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl._float))  error(ERR_INVALID_ARG, recvr);

  m_float_new(-FLOAT(recvr)->val);
}

void
cm_float_hash(unsigned argc, obj_t args)
{
  obj_t    recvr;
  unsigned r;

  if (argc != 1)                             error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl._float))  error(ERR_INVALID_ARG, recvr);

  CRC32_INIT(r);
  m_integer_new(CRC32(r, &FLOAT(recvr)->val, sizeof(FLOAT(recvr)->val)));
}

void
cm_float_equals(unsigned argc, obj_t args)
{
  obj_t recvr, arg;

  if (argc != 2)                             error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl._float))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));

  m_boolean_new(inst_of(arg) == inst_of(recvr)
		&& FLOAT(arg)->val == FLOAT(recvr)->val
		);
}

void
cm_float_ge(unsigned argc, obj_t args)
{
  obj_t recvr, arg;

  if (argc != 2)                             error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl._float))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));

  m_boolean_new(inst_of(arg) == inst_of(recvr)
		&& FLOAT(arg)->val <= FLOAT(recvr)->val
		);
}

void
cm_float_tostring(unsigned argc, obj_t args)
{
  obj_t recvr;
  char  buf[64];

  if (argc != 1)                             error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl._float))  error(ERR_INVALID_ARG, recvr);

  m_string_new(1, snprintf(buf, sizeof(buf), FLOAT_PRINTF_FMT, FLOAT(recvr)->val), buf);
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
m_string_new(unsigned n, ...)
{
  va_list  ap;
  unsigned j, k;
  unsigned size;
  char     *p, *q;

  vm_push(0);

  va_start(ap, n);
  for (size = 0, k = n; k; --k) {
    j = va_arg(ap, unsigned);
    p = va_arg(ap, char *);
    size += j;
  }
  if (size > 0)  ++size;
  va_end(ap);
  
  vm_inst_alloc(consts.cl.string);
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
cm_string_new(unsigned argc, obj_t args)
{
  if (argc < 1 || argc > 2)  error(ERR_NUM_ARGS);
  if (argc == 1) {
    m_string_new(0);
    return;
  }

  m_method_call_1(CAR(CDR(args)), consts.str.tostring);
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
  unsigned r;

  CRC32_INIT(r);

  return (CRC32(r, STRING(s)->data, string_len(s)));
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
  
  m_integer_new(string_hash(recvr));
}

void
cm_string_equal(unsigned argc, obj_t args)
{
  obj_t recvr, arg;

  if (argc != 2)                             error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.string))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  
  m_boolean_new(inst_of(arg) == inst_of(recvr)
		&& string_equal(arg, recvr)
		);
}

void
cm_string_tostring(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)                             error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.string))  error(ERR_INVALID_ARG, recvr);

  vm_assign(0, recvr);
}

void
cm_string_pquote(unsigned argc, obj_t args)
{
  obj_t    recvr;
  char     *p;
  unsigned n;

  if (argc != 1)                             error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.string))  error(ERR_INVALID_ARG, recvr);

  for (p = STRING(recvr)->data, n = string_len(recvr); n; --n, ++p) {
    if (isspace(*p)) {
      m_string_new(3, 1, "\"",
		      string_len(recvr), STRING(recvr)->data, 
		      1, "\""
		 );
      return;
    }
  }

  vm_assign(0, recvr);
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

  m_string_new(2, string_len(recvr), STRING(recvr)->data,
	          string_len(arg), STRING(arg)->data
	       );
}

void
cm_string_eval(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)                             error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.string))  error(ERR_INVALID_ARG, recvr);

  vm_assign(0, env_at(recvr));
}

void
string_print(obj_t s, obj_t outf)
{
  FILE     *fp = _FILE(outf)->fp;
  char     *p, c;
  unsigned n;
  
  for (p = STRING(s)->data, n = string_len(s); n; --n, ++p) {
    c = *p;
    
    if (isprint(c) || isspace(c)) {
      putc(c, fp);
    } else {
      fprintf(fp, "\\x%02x", * (unsigned char *) p);
    }
  }
}

void
cm_string_print(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)                             error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.string))  error(ERR_INVALID_ARG, recvr);
  
  m_file_stdout();
  string_print(recvr, R0);
  
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
  
  string_print(recvr, arg);
  
  vm_assign(0, recvr);
}

void
cm_string_len(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)                             error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.string))  error(ERR_INVALID_ARG, recvr);
  
  m_integer_new(string_len(recvr));
}

int
m_string_substr(obj_t s, int ofs, int len)
{
  int result = 0;

  vm_push(0);

  if (ofs < 0)  ofs = (int) string_len(s) + ofs;
  
  if (ofs >= 0 && (ofs + len) <= (int) string_len(s)) {
    m_string_new(1, len, STRING(s)->data + ofs);
  } else {
    result = -1;
  }

  vm_drop();
  
  return (result);
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

  m_integer_new(STRING(recvr)->data[0]);
}

void
cm_string_foreach(unsigned argc, obj_t args)
{
  obj_t    recvr, arg, *p;
  char     *s;
  unsigned n;

  if (argc != 2)                             error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.string))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));

  vm_push(1);
  
  vm_assign(1, NIL);
  for (p = &R1, s = STRING(recvr)->data, n = string_len(recvr); n; --n, ++s) {
    m_string_new(1, 1, s);
    m_cons(R0, NIL);
    m_method_call_2(consts.str.evalc, arg, R0);
    m_cons(R0, NIL);
    OBJ_ASSIGN(*p, R0);
    p = &CDR(R0);
  }
  vm_assign(0, R1);
  
  vm_pop(1);
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

  m_integer_new(string_index(recvr, arg, 0, 1));
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

  m_integer_new(string_index(recvr, arg, 0, -1));
}

void
cm_string_split(unsigned argc, obj_t args)
{
  obj_t    recvr, arg, *p;
  unsigned ofs, recvr_len;
    
  if (argc != 2)                             error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.string))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  if (!is_kind_of(arg, consts.cl.string))    error(ERR_INVALID_ARG, arg);

  vm_push(1);
  
  vm_assign(1, NIL);
  recvr_len = string_len(recvr);
  for (p = &R1, ofs = 0; ofs < recvr_len; ) {
    int      i = string_index(recvr, arg, ofs, 1);
    unsigned n = (i < 0) ? recvr_len - ofs : (unsigned) i - ofs;
    
    m_string_new(1, n, STRING(recvr)->data + ofs);
    m_cons(R0, NIL);
    OBJ_ASSIGN(*p, R0);
    p = &CDR(R0);
    ofs += n + 1;
  }
  vm_assign(0, R1);
  
  vm_pop(1);
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
  obj_t recvr;

  if (argc != 1)                           error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.dptr))  error(ERR_INVALID_ARG, recvr);
  
  vm_assign(0, CDR(recvr));
}

void
cm_dptr_hash(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)                           error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.dptr))  error(ERR_INVALID_ARG, recvr);
  
  vm_pushm(1, 2);
  
  m_method_call_1(consts.str.hash, CAR(recvr));
  vm_assign(1, R0);
  m_method_call_1(consts.str.hash, CDR(recvr));
  vm_assign(2, R0);
  m_method_call_2(consts.str.addc, R1, R2); /* TBD: Or call directly? */
  
  vm_popm(1, 2);
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
    m_boolean_new(0);
    return;
  }

  vm_pushm(1, 2);
  
  m_method_call_2(consts.str.equalsc, CAR(recvr), CAR(arg));
  vm_assign(1, R0);
  m_method_call_2(consts.str.equalsc, CDR(recvr), CDR(arg));
  vm_assign(2, R0);
  m_method_call_2(consts.str.andc, R1, R2); /* TBD: Or call directly? */
  
  vm_popm(1, 2);
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

  vm_inst_alloc(consts.cl.pair);
  inst_init(R0, car, cdr);

  vm_drop();
}

void
cm_pair_new(unsigned argc, obj_t args)
{
  obj_t arg, car = NIL, cdr = NIL;

  if (argc < 1 || argc > 2)  error(ERR_NUM_ARGS);
  if (argc ==2) {
    arg = CAR(CDR(args));
    
    if (is_kind_of(arg, consts.cl.dptr)) {
      car = CAR(arg);
      cdr = CDR(arg);
    } else {
      error(ERR_INVALID_ARG, arg);
    }
  }

  m_pair_new(car, cdr);
}

void
cm_pair_eval(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)                           error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.pair))  error(ERR_INVALID_ARG, recvr);

  vm_push(1);
  
  m_method_call_1(consts.str.eval, CAR(recvr));
  vm_assign(1, R0);
  m_method_call_1(consts.str.eval, CDR(recvr));
  m_pair_new(R1, R0);
  
  vm_pop(1);
}

void
cm_pair_tostring(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)                           error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.pair))  error(ERR_INVALID_ARG, recvr);

  vm_pushm(1, 2);
  
  m_method_call_1(consts.str.tostring, CAR(recvr));
  vm_assign(1, R0);
  m_method_call_1(consts.str.tostring, CDR(recvr));
  vm_assign(2, R0);
  m_string_new(5, 1, "(",
	          string_len(R1), STRING(R1)->data,
	          2, ", ",
	          string_len(R2), STRING(R2)->data,
	          1, ")"
	       );
  
  vm_popm(1, 2);
}

void
cm_pair_at(unsigned argc, obj_t args)
{
  obj_t recvr, arg, result;

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

  m_cons(el, NIL);
  _list_concat(li, R0);

  vm_pop(0);
}

void
m_cons(obj_t car, obj_t cdr)
{
  vm_push(0);

  vm_inst_alloc(consts.cl.list);
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
  obj_t arg;

  if (argc < 1 || argc > 2)  error(ERR_NUM_ARGS);

  if (argc == 1) {
    vm_assign(0, NIL);
    return;
  }
  
  arg = CAR(CDR(args));

  if (is_kind_of(arg, consts.cl.pair)) {
    m_cons(CDR(arg), NIL);
    m_cons(CAR(arg), R0);

    return;
  }
  if (is_kind_of(arg, consts.cl.list)) {
    vm_assign(0, arg);
    
    return;
  }
  if (is_kind_of(arg, consts.cl.dict)) {
    obj_t    *p, *q, r;
    unsigned n;

    vm_push(1);
    
    vm_assign(1, NIL);
    for (p = &R1, q = DICT(arg)->base->data, n = DICT(arg)->base->size; n; --n, ++q) {
      for (r = *q; r; r = CDR(r)) {
	m_cons(CAR(r), NIL);
	OBJ_ASSIGN(*p, R0);
	p = &CDR(R0);
      }
    }
    vm_assign(0, R1);

    vm_pop(1);

    return;
  }
  if (is_kind_of(arg, consts.cl.array)) {
    obj_t    *p, *q;
    unsigned n;

    vm_push(1);
    
    vm_assign(1, NIL);
    for (p = &R1, q = ARRAY(arg)->data, n = ARRAY(arg)->size; n; --n, ++q) {
      m_cons(*q, NIL);
      OBJ_ASSIGN(*p, R0);
      p = &CDR(R0);
    }
    vm_assign(0, R1);

    vm_pop(1);

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
  if (!(recvr == NIL || is_kind_of(recvr, consts.cl.list)))  error(ERR_INVALID_ARG, recvr);

  m_integer_new(list_len(recvr));
}

void
m_list_tostr(obj_t list, char *delim)
{
    char  c;
    obj_t p;

    vm_push(0);
    vm_push(1);

    m_string_new(0);
    vm_assign(1, R0);
    c = delim[0];
    for (p = list; p; p = CDR(p), c = ' ') {
      m_method_call_1(consts.str.tostring, CAR(p));
      m_string_new(3, string_len(R1), STRING(R1)->data,
		      1,                &c,
		      string_len(R0), STRING(R0)->data
		   );
      vm_assign(1, R0);
    }
    m_string_new(2, string_len(R1), STRING(R1)->data, 1, &delim[1]);

    vm_pop(1);
    vm_drop();
}

void
cm_list_tostring(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!(recvr == NIL || is_kind_of(recvr, consts.cl.list)))  error(ERR_INVALID_ARG, recvr);

  m_list_tostr(recvr, "()");
}

void
m_list_eval(obj_t list)
{
  obj_t *q;
  
  vm_push(0);
  vm_push(1);
  
  vm_assign(1, NIL);
  for (q = &R1; list; list = CDR(list)) {
    m_method_call_1(consts.str.eval, CAR(list));
    m_cons(R0, NIL);
    OBJ_ASSIGN(*q, R0);
    q = &CDR(R0);
  }
  vm_assign(0, R1);
  
  vm_pop(1);
  vm_drop();
}

void
cm_list_eval(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!(recvr == NIL || is_kind_of(recvr, consts.cl.list)))  error(ERR_INVALID_ARG, recvr);

  m_list_eval(recvr);
}

void
cm_list_map(unsigned argc, obj_t args)
{
  obj_t recvr, arg, *p, q;

  if (argc != 2)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!(recvr == NIL || is_kind_of(recvr, consts.cl.list)))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));

  vm_push(1);

  vm_assign(1, NIL);
  for (p = &R1, q = recvr; q; q = CDR(q)) {
    m_method_call_2(consts.str.evalc, arg, CAR(q));
    m_cons(R0, NIL);
    OBJ_ASSIGN(*p, R0);
    p = &CDR(R0);
  }
  vm_assign(0, R1);
  
  vm_pop(1);
}

void
cm_list_foreach(unsigned argc, obj_t args)
{
  obj_t recvr, arg, *p, q;

  if (argc != 2)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!(recvr == NIL || is_kind_of(recvr, consts.cl.list)))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));

  vm_push(1);

  vm_assign(1, NIL);
  for (p = &R1, q = recvr; q; q = CDR(q)) {
    m_cons(CAR(q), NIL);
    m_method_call_2(consts.str.evalc, arg, R0);
    m_cons(R0, NIL);
    OBJ_ASSIGN(*p, R0);
    p = &CDR(R0);
  }
  vm_assign(0, R1);
  
  vm_pop(1);
}

void
cm_list_splice(unsigned argc, obj_t args)
{
  obj_t recvr, arg;

  if (argc != 2)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!(recvr == NIL || is_kind_of(recvr, consts.cl.list)))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  if (!is_kind_of(arg, consts.cl.string))  error(ERR_INVALID_ARG, arg);

  vm_push(1);

  m_string_new(0);
  vm_assign(1, R0);
  for ( ; recvr; recvr = CDR(recvr)) {
    m_method_call_1(consts.str.tostring, CAR(recvr));
    if (string_len(R1) != 0) {
      m_string_new(3, string_len(R1), STRING(R1)->data,
		      string_len(arg), STRING(arg)->data,
		      string_len(R0), STRING(R0)->data
		   );
    }
    vm_assign(1, R0);
  }
  vm_assign(0, R1);
  
  vm_pop(1);
}

void
cm_list_append(unsigned argc, obj_t args)
{
  obj_t recvr, arg, *p, q;

  if (argc != 2)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!(recvr == NIL || is_kind_of(recvr, consts.cl.list)))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  if (!(arg == NIL || is_kind_of(arg, consts.cl.list)))      error(ERR_INVALID_ARG, arg);
  
  vm_push(1);
  
  vm_assign(1, NIL);
  for (p = &R1, q = recvr; q; q = CDR(q)) {
    m_cons(CAR(q), NIL);
    OBJ_ASSIGN(*p, R0);
    p = &CDR(R0);
  }
  OBJ_ASSIGN(*p, arg);
  vm_assign(0, R1);
  
  vm_pop(1);
}
 
void
cm_list_hash(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!(recvr == NIL || is_kind_of(recvr, consts.cl.list)))  error(ERR_INVALID_ARG, recvr);

  vm_push(1);
  
  m_integer_new(0);
  vm_assign(1, R0);
  for ( ; recvr; recvr = CDR(recvr)) {
    m_method_call_1(consts.str.hash, CAR(recvr));
    m_method_call_2(consts.str.addc, R1, R0); /* TBD: Or call directly? */
    vm_assign(1, R0);
  }
  vm_assign(0, R1);
  
  vm_pop(1);
}

void
cm_list_equals(unsigned argc, obj_t args)
{
  obj_t recvr, arg;

  if (argc != 2)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!(recvr == NIL || is_kind_of(recvr, consts.cl.list)))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  if (!(arg == NIL || is_kind_of(arg, consts.cl.list)))      error(ERR_INVALID_ARG, arg);

  vm_push(1);
  
  m_boolean_new(1);
  vm_assign(1, R0);
  for ( ;
	BOOLEAN(R1)->val && recvr && arg;
	recvr = CDR(recvr), arg = CDR(arg)
	) {
    m_method_call_2(consts.str.equalsc, CAR(recvr), CAR(arg));
    m_method_call_2(consts.str.andc, R1, R0); /* TBD: Or call directly? */
    vm_assign(1, R0);
  }
  if (recvr || arg)  m_boolean_new(0);  else  vm_assign(0, R1);
  
  vm_pop(1);
}

void
cm_list_at(unsigned argc, obj_t args)
{
  obj_t       recvr, arg, p;
  integer_val_t i;
  unsigned    len;

  if (argc != 2)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!(recvr == NIL || is_kind_of(recvr, consts.cl.list)))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  if (!is_kind_of(arg, consts.cl.integer))  error(ERR_INVALID_ARG, arg);
  i   = INTEGER(arg)->val;
  len = list_len(recvr);
  if (i < 0)  i = (integer_val_t) len - i;
  if (i < 0 || i >= (integer_val_t) len)  error(ERR_IDX_RANGE, arg);

  for (p = recvr; p; p = CDR(p), --i) {
    if (i == 0) {
      vm_assign(0, CAR(p));
      return;
    }
  }

  HARD_ASSERT(0);
}

void
cm_list_at_len(unsigned argc, obj_t args)
{
  obj_t       recvr, idx, len, p, *q;
  integer_val_t i, n;
  unsigned    rlen;

  if (argc != 2)                            error(ERR_NUM_ARGS);
  recvr = CAR(args);  args  = CDR(args);
  if (!is_kind_of(recvr, consts.cl.list))   error(ERR_INVALID_ARG, recvr);
  idx  = CAR(args);  args = CDR(args);
  if (!is_kind_of(idx, consts.cl.integer))  error(ERR_INVALID_ARG, idx);
  len = CAR(args);
  if (!is_kind_of(len, consts.cl.integer))  error(ERR_INVALID_ARG, len);
  i   = INTEGER(idx)->val;
  n   = INTEGER(len)->val;
  rlen = list_len(recvr);
  if (i < 0)  i = (integer_val_t) rlen - i;
  if (i < 0 || (i + n) > (integer_val_t) rlen)  error(ERR_IDX_RANGE_2, idx, len);

  vm_push(1);

  vm_assign(1, NIL);
  for (q = &R1, p = recvr; p && n; p = CDR(p)) {
    if (i == 0) {
      m_cons(CAR(p), NIL);
      OBJ_ASSIGN(*q, R0);
      q = &CDR(R0);
      --n;
    } else {
      --i;
    }
  }
  vm_assign(0, R1);

  vm_pop(1);
}

void
cm_list_filter(unsigned argc, obj_t args)
{
  obj_t recvr, arg, *p;

  if (argc != 2)                           error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.list))  error(ERR_INVALID_ARG, recvr);
  arg = CDR(CDR(args));
  if (!is_kind_of(arg, consts.cl.list))    error(ERR_INVALID_ARG, arg);

  vm_push(1);

  vm_assign(1, NIL);
  for (p = &R1; recvr && arg; recvr = CDR(recvr), arg = CDR(arg)) {
    obj_t f = CAR(arg);

    if (!is_kind_of(f, consts.cl.boolean))  error(ERR_INVALID_ARG, arg);
    if (!BOOLEAN(f)->val)  continue;
    
    m_cons(CAR(recvr), NIL);
    OBJ_ASSIGN(*p, R0);
    p = &CDR(R0);
  }
  vm_assign(0, R1);

  vm_pop(1);
}

void
cm_list_reduce(unsigned argc, obj_t args)
{
  obj_t recvr, func, p;

  if (argc != 3)  error(ERR_NUM_ARGS);
  recvr = CAR(args);  args = CDR(args);
  if (!(recvr == NIL || is_kind_of(recvr, consts.cl.list)))  error(ERR_INVALID_ARG, recvr);
  func = CAR(args);  args = CDR(args);

  vm_push(1);
  
  vm_assign(1, CAR(args));
  for (p = recvr; p; p = CDR(p)) {
    m_cons(CAR(p), NIL);
    m_cons(R1, R0);
    m_method_call_2(consts.str.evalc, func, R0);
    vm_assign(1, R0);
  }
  vm_assign(0, R1);
  
  vm_pop(1);
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
m_method_call_new(obj_t sel, obj_t args)
{
  vm_push(0);

  vm_inst_alloc(consts.cl.method_call);
  inst_init(R0, sel, args);

  vm_drop();
}

void
cm_method_call_eval(unsigned argc, obj_t args)
{
  obj_t recvr;
  
  if (argc != 1)                                  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.method_call))  error(ERR_INVALID_ARG, recvr);

  if (STRING(METHOD_CALL(recvr)->sel)->data[0] == '&') {
    vm_assign(0, METHOD_CALL(recvr)->args);
  } else {
    m_list_eval(METHOD_CALL(recvr)->args);
  }

  m_method_call(METHOD_CALL(recvr)->sel,
		METHOD_CALL(recvr)->argc,
		R0);
}

void
cm_method_call_tostring(unsigned argc, obj_t args)
{
  obj_t    recvr, p, *q;
  char     *s, *t;
  unsigned n, k;

  if (argc != 1)                                  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.method_call))  error(ERR_INVALID_ARG, recvr);

  vm_push(1);

  vm_assign(1, NIL);
  for (n = 0, q = &R1, s = STRING(METHOD_CALL(recvr)->sel)->data, p = METHOD_CALL(recvr)->args; ; ++n) {
    if (n & 1) {
      t = index(s, ':');
      k = t ? (unsigned)(t + 1 - s) : (unsigned) strlen(s);

      m_string_new(1, k, s);
      m_cons(R0, NIL);

      s += k;
    } else {
      m_cons(CAR(p), NIL);
      
      p = CDR(p);
    }

    OBJ_ASSIGN(*q, R0);
    q = &CDR(R0);

    if (n > 0 && p == NIL)  break;
  }
  vm_assign(0, R1);

  vm_pop(1);
  
  m_list_tostr(R0, "[]");
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
m_block_new(obj_t args, obj_t body)
{
  vm_push(0);

  vm_inst_alloc(consts.cl.block);
  inst_init(R0, args, body);

  vm_drop();
}

void
cm_block_eval(unsigned argc, obj_t args)
{
  obj_t recvr, arg, p;

  if (argc < 1 || argc > 2)                 error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.block))  error(ERR_INVALID_ARG);
  if (argc == 2) {
    arg = CAR(CDR(args));
    if (list_len(arg) != BLOCK(recvr)->argc)  error(ERR_NUM_ARGS);

    m_string_dict_new(16);
    for (p = BLOCK(recvr)->args; p; p = CDR(p), arg = CDR(arg)) {
      dict_at_put(R0, CAR(p), CAR(arg));
    }

  } else {
    if (BLOCK(recvr)->argc > 0) {
      vm_assign(0, recvr);

      return;
    }

    m_string_dict_new(16);
  }

  vm_push(0);
  
  FRAME_BLOCK_BEGIN(R0) {

    if (frame_jmp_code == 0) {
      vm_assign(0, NIL);
      for (p = BLOCK(recvr)->body; p; p = CDR(p)) {
	m_method_call_1(consts.str.eval, CAR(p));
      }
    }
    
  } FRAME_BLOCK_END;
  
  vm_drop();
}

void
cm_block_tostring(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.block))  error(ERR_INVALID_ARG, recvr);

  m_cons(BLOCK(recvr)->args, BLOCK(recvr)->body);
  m_list_tostr(R0, "{}");
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
m_array_new(unsigned size)
{
  vm_inst_alloc(consts.cl.array);
  inst_init(R0, size);
}

void
cm_array_new(unsigned argc, obj_t args)
{
  obj_t arg;

  if (argc != 2)  error(ERR_NUM_ARGS);
  arg = CAR(CDR(args));

  if (is_kind_of(arg, consts.cl.integer)) {
    integer_val_t size = INTEGER(arg)->val;

    if (size < 0)  error(ERR_INVALID_ARG, arg);

    m_array_new(size);
    return;
  }
  if (is_kind_of(arg, consts.cl.list)) {
    obj_t    *p;
    unsigned n;
    
    m_array_new(list_len(arg));
    for (p = ARRAY(R0)->data, n = ARRAY(R0)->size; n; --n, ++p, arg = CDR(arg)) {
      OBJ_ASSIGN(*p, CAR(arg));
    }
    return;
  }
  if (is_kind_of(arg, consts.cl.array)) {
    obj_t    *p, *q;
    unsigned n;

    m_array_new(ARRAY(arg)->size);
    for (p = ARRAY(R0)->data, q = ARRAY(arg)->data, n = ARRAY(arg)->size;
	 n;
	 --n, ++p, ++q
	 ) {
      OBJ_ASSIGN(*p, *q);
    }
    return;
  }
  if (is_kind_of(arg, consts.cl.dict)) {
    unsigned size = dict_count(arg), n;
    obj_t    *p, *q, r;

    m_array_new(size);
    for (p = ARRAY(R0)->data, q = DICT(arg)->base->data, n = DICT(arg)->base->size;
	 n;
	 --n, ++q
	 ) {
      for (r = *q; r; r = CDR(r)) {
	OBJ_ASSIGN(*p, CAR(r));
	++p;
      }
    }
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
m_collection_tostring(obj_t obj)
{
  vm_push(0);

  m_method_call_2(consts.str.newc, consts.cl.list, obj);
  m_method_call_1(consts.str.tostring, R0); /* TBD: Or call directly? */

  vm_drop();
}

void
cm_array_tostring(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.array))  error(ERR_INVALID_ARG, recvr);

  m_collection_tostring(recvr);
}

/***************************************************************************/

/* Class: Dictionary */

void
inst_init_dict(obj_t cl, obj_t inst, va_list ap)
{
  DICT(inst)->key_hash  = va_arg(ap, unsigned (*)(obj_t));
  DICT(inst)->key_equal = va_arg(ap, unsigned (*)(obj_t, obj_t));

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

void
m_string_dict_new(unsigned size)
{
  vm_inst_alloc(consts.cl.dict);
  inst_init(R0, string_hash, string_equal, size);
}

unsigned
m_dict_size_dflt(void)
{
  return (32);
}

unsigned
dict_key_hash(obj_t obj)
{
  unsigned result;

  vm_push(0);

  m_method_call_1(consts.str.hash, obj);
  result = INTEGER(R0)->val;

  vm_pop(0);

  return (result);
}

unsigned
dict_key_equal(obj_t obj1, obj_t obj2)
{
  unsigned result;

  vm_push(0);

  m_method_call_2(consts.str.equalsc, obj1, obj2);
  result = BOOLEAN(R0)->val;
  
  vm_pop(0);

  return (result);
}

void
m_dict_new(unsigned size)
{
  vm_inst_alloc(consts.cl.dict);
  inst_init(R0, dict_key_hash, dict_key_equal, size);
}

obj_t
dict_find(obj_t dict, obj_t key, obj_t **pprev)
{
  obj_t p, *pp, *b = &DICT(dict)->base->data[(*DICT(dict)->key_hash)(key) % DICT(dict)->base->size];
  unsigned (*f)(obj_t, obj_t) = DICT(dict)->key_equal;

  for (pp = b; p = *pp; pp = &CDR(p)) {
    obj_t q = CAR(p);
    
    if ((*f)(CAR(q), key)) {
      if (pprev)  *pprev = pp; 
      return (p);
    }
  }
  
  if (pprev)  *pprev = b;
  return (NIL);
}

obj_t
dict_at(obj_t dict, obj_t key)
{
  obj_t p;
  
  return ((p = dict_find(dict, key, 0)) ? CAR(p) : NIL);
}

void
dict_at_put(obj_t dict, obj_t key, obj_t val)
{
  obj_t p, *pp;
  
  if (p = dict_find(dict, key, &pp)) {
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
    m_cons(R0, *pp);
    OBJ_ASSIGN(*pp, R0);
    
    vm_pop(0);
  }
}

void
dict_del(obj_t dict, obj_t key)
{
  obj_t p, *pp;
  
  if ((p = dict_find(dict, key, &pp)) == 0)  return;
  
  vm_push(1);
  
  vm_assign(1, p);
  OBJ_ASSIGN(*pp, CDR(p));
  
  vm_pop(1);
}

unsigned
dict_count(obj_t dict)
{
  unsigned result, n;
  obj_t    *p;
  
  for (result = 0, p = DICT(dict)->base->data, n = DICT(dict)->base->size;
       n;
       --n, ++p
       ) {
    result += list_len(*p);
  }
  
  return (result);
}

void
m_dict_keys(obj_t dict)
{
  obj_t    *p, q, *r;
  unsigned n;

  vm_push(0);
  vm_push(1);

  vm_assign(1, NIL);
  for (r = &R1, p = DICT(dict)->base->data, n = DICT(dict)->base->size;
       n;
       --n, ++p
       ) {
    for (q = *p; q; q = CDR(q)) {
      m_cons(CAR(CAR(q)), NIL);
      OBJ_ASSIGN(*r, R0);
      r = &CDR(R0);
    }
  }
  vm_assign(0, R1);

  vm_pop(1);
  vm_drop();
}

void
cm_dict_new(unsigned argc, obj_t args)
{
  obj_t arg;

  if (argc < 1 || argc > 2)  error(ERR_NUM_ARGS);

  if (argc == 1) {
    m_dict_new(m_dict_size_dflt());
    return;
  }

  arg = CAR(CDR(args));

  if (is_kind_of(arg, consts.cl.integer)) {
    integer_val_t size = INTEGER(arg)->val;

    if (size <= 0)  error(ERR_INVALID_ARG, arg);

    m_dict_new((unsigned) size);
    return;
  }
  if (is_kind_of(arg, consts.cl.list)) {
    obj_t p;

    for (p = arg; p; p = CDR(p)) {
      if (!is_kind_of(CAR(p), consts.cl.pair))  error(ERR_INVALID_ARG, arg);
    }

    m_dict_new(m_dict_size_dflt());

    for (p = arg; p; p = CDR(p)) {
      obj_t q = CAR(p);
      
      dict_at_put(R0, CAR(q), CDR(q));
    }

    return;
  }

  error(ERR_INVALID_ARG, arg);
}

void
cm_dict_at(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 2)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.dict))  error(ERR_INVALID_ARG, recvr);

  vm_assign(0, dict_at(recvr, CAR(CDR(args))));
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
cm_dict_tostring(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)                           error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.dict))  error(ERR_INVALID_ARG, recvr);

  m_collection_tostring(recvr);
}

void
cm_dict_count(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)                           error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.dict))  error(ERR_INVALID_ARG, recvr);

  m_integer_new(dict_count(recvr));
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
m_file_new(obj_t name, obj_t mode, FILE *fp)
{
  vm_push(0);

  vm_inst_alloc(consts.cl.file);
  inst_init(R0, name, mode, fp);

  vm_drop();
}

void
m_file_stdin(void)
{
  m_obj_at(consts.cl.file, consts.str._stdin);
  if (!is_kind_of(R0, consts.cl.file))  error(ERR_INVALID_VALUE_2, "stdout", R0);
} 

void
m_file_stdout(void)
{
  m_obj_at(consts.cl.file, consts.str._stdout);
  if (!is_kind_of(R0, consts.cl.file))  error(ERR_INVALID_VALUE_2, "stdout", R0);
} 
 
void
m_file_stderr(void)
{
  m_obj_at(consts.cl.file, consts.str._stderr);
  if (!is_kind_of(R0, consts.cl.file))  error(ERR_INVALID_VALUE_2, "stderr", R0);
} 

void
m_file_open(obj_t filename, obj_t mode)
{
  FILE *fp;

  fp = fopen(STRING(filename)->data, STRING(mode)->data);
  if (fp == 0) {
    error(ERR_FILE_OPEN_FAIL, errno);
  }
  
  m_file_new(filename, mode, fp);
}

void
cm_file_new(unsigned argc, obj_t args)
{
  obj_t filename, mode;

  if (argc != 3)  error(ERR_NUM_ARGS);
  args = CDR(args);  filename = CAR(args);
  if (!is_kind_of(filename, consts.cl.string)
      || string_len(filename) == 0
      )  error(ERR_INVALID_ARG, filename);
  args = CDR(args);  mode = CAR(args);
  if (!is_kind_of(mode, consts.cl.string)
      || string_len(mode) == 0
      )  error(ERR_INVALID_ARG, mode);

  m_file_open(filename, mode);
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
cm_file_tostring(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.file))  error(ERR_INVALID_ARG, recvr);

  m_string_new(6, string_len(CLASS(consts.cl.file)->name), STRING(CLASS(consts.cl.file)->name)->data,
	          1, "(",
		  string_len(_FILE(recvr)->name), STRING(_FILE(recvr)->name)->data,
		  2, ", ",
		  string_len(_FILE(recvr)->mode), STRING(_FILE(recvr)->mode)->data,
		  1, ")"
		);
}

void
cm_file_read(unsigned argc, obj_t args)
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

  vm_inst_alloc(consts.cl.string);
  inst_init(R0, (unsigned) len);

  for (i = 0, p = STRING(R0)->data; len; --len, ++p, ++i) {
    int c = fgetc(fp);
    
    if (c == EOF)  break;
    
    *p = c;
  }

  m_string_new(1, i, STRING(R0)->data);
}

void
cm_file_readln(unsigned argc, obj_t args)
{
  obj_t     recvr;
  FILE      *fp;
  char      *p;
  unsigned  i;
  
  if (argc != 1)                           error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.file))  error(ERR_INVALID_ARG, recvr);
  fp = _FILE(recvr)->fp;

  vm_push(1);
  
  vm_inst_alloc(consts.cl.string);
  inst_init(R0, 32);
  
  for (i = 0, p = STRING(R0)->data; ; ++p, ++i) {
    int c = fgetc(fp);
    
    if (c == EOF || c == '\n')  break;
    
    if (i >= string_len(R0)) {
      vm_assign(1, R0);
      vm_inst_alloc(consts.cl.string);
      inst_init(R0, string_len(R1) << 1);
      memcpy(STRING(R0)->data, STRING(R1)->data, i);
      p = STRING(R0)->data + i;
    }
    
    *p = c;
  }
  
  m_string_new(1, i, STRING(R0)->data);
  
  vm_pop(1);
}

void
cm_file_write(unsigned argc, obj_t args)
{
  obj_t recvr, arg;

  if (argc != 2)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.file))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  if (!is_kind_of(arg, consts.cl.string))  error(ERR_INVALID_ARG, arg);

  if (string_len(arg) > 0)  fputs(STRING(arg)->data, _FILE(recvr)->fp);

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

  m_boolean_new(feof(_FILE(recvr)->fp));
}

void
m_file_load(obj_t file)
{
  FRAME_INPUT_BEGIN(_FILE(file)->fp, STRING(_FILE(file)->name)->data, 0) {
    
    for (;;) {
      if (yyparse() != 0)  break;
      
      vm_dropn(frp->sp - sp); /* Discard all objs pushed during parsing */
      
      m_method_call_1(consts.str.eval, R0);
    }
    
  } FRAME_INPUT_END;
}

void
cm_file_load(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.file))  error(ERR_INVALID_ARG, recvr);

  FRAME_INPUT_BEGIN(_FILE(recvr)->fp, STRING(_FILE(recvr)->name)->data, 0) {
    
    for (;;) {
      if (yyparse() != 0)  break;
      
      vm_dropn(frp->sp - sp); /* Discard all objs pushed during parsing */
      
      m_method_call_1(consts.str.eval, R0);
    }
    
  } FRAME_INPUT_END;
}

void
cl_init_file(void)
{
  vm_push(0);

  m_string_new(1, 2, "r+");
  m_file_new(consts.str._stdin, R0, stdin);
  dict_at_put(CLASS(consts.cl.file)->cl_vars, consts.str._stdin, R0);
  m_string_new(1, 2, "w+");
  m_file_new(consts.str._stdout, R0, stdout);
  dict_at_put(CLASS(consts.cl.file)->cl_vars, consts.str._stdout, R0);
  m_string_new(1, 2, "w+");
  m_file_new(consts.str._stderr, R0, stderr);
  dict_at_put(CLASS(consts.cl.file)->cl_vars, consts.str._stderr, R0);

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
m_module_new(obj_t name, obj_t parent)
{
  vm_push(0);

  vm_inst_alloc(consts.cl.module);
  inst_init(R0, name, parent, string_hash, string_equal, 32);
  
  if (parent) {
    dict_at_put(OBJ(MODULE(parent)->base), name, R0);
  }

  vm_drop();
}

void
cm_module_new(unsigned argc, obj_t args)
{
  obj_t name, path;

  if (argc != 2)  error(ERR_NUM_ARGS);
  name = CAR(CDR(args));
  if (!is_kind_of(name, consts.cl.string))  error(ERR_INVALID_ARG, name);

  vm_pushm(1, 3);

  m_string_new(1, 2, "r+");
  vm_assign(1, R0);		/* R1 = file open mode */

  path = dict_at(CLASS(consts.cl.module)->cl_vars,
		 consts.str.path
		 );
  if (path) {
    path = CDR(path);
  } else {
    m_string_new(1, 1, ".");
    m_cons(R0, NIL);
    vm_assign(2, R0);		/* R2 = path */
    path = R2;
  }

  for ( ; path; path = CDR(path)) {
    obj_t dir = CAR(path);
    FILE  *fp;
    void  *cookie;

    m_string_new(3, string_len(dir), STRING(dir)->data,
		    1, "/",
		    string_len(name), STRING(name)->data
		 );
    vm_assign(3, R0);		/* R3 = file base name */
    
    m_string_new(2, string_len(R3), STRING(R3)->data,
		    3, ".so"
		 );
    cookie = dlopen(STRING(R0)->data, RTLD_NOW);
    if (cookie) {
      void *init_func, *fini_func;

      m_string_new(2, string_len(name), STRING(name)->data,
		      12, "_module_init"
		   );
      init_func = dlsym(cookie, STRING(R0)->data);
      m_string_new(2, string_len(name), STRING(name)->data,
		      12, "_module_fini"
		   );
      fini_func = dlsym(cookie, STRING(R0)->data);
      if (init_func && fini_func) {
	m_module_new(name, module_cur);
	MODULE(R0)->dl_cookie = cookie;
	MODULE(R0)->fini_func = (void (*)(void)) fini_func;

	FRAME_MODULE_BEGIN(R0) {

	  (* (void (*)(void)) init_func)();

	} FRAME_MODULE_END;
	
	goto done;
      }

      dlclose(cookie);
    }

    m_string_new(2, string_len(R3), STRING(R3)->data,
		    4, ".ool"
		 );
    fp = fopen(STRING(R0)->data, STRING(R1)->data);
    if (fp) {
      m_file_new(R0, R1, fp);
      vm_assign(1, R0);		/* R1 = file */
      
      m_module_new(name, module_cur);
      
      FRAME_MODULE_BEGIN(R0) {
	
	m_file_load(R1);
	
      } FRAME_MODULE_END;
      
      goto done;
    }
  }

  error(ERR_MODULE_OPEN_FAIL, name);

 done:
  vm_popm(1, 3);
}

void
m_fqmodname(obj_t mod)
{
    obj_t p, s;

    vm_push(0);

    m_string_new(0);
    for ( ; p = MODULE(mod)->parent; mod = p) {
	s = MODULE(mod)->name;

	if (string_len(R0) == 0) {
	    vm_assign(0, s);
	    continue;
	}

	m_string_new(3, string_len(s), STRING(s)->data,
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
  cm_module_name(argc, args);
}

void
cl_init_module(void)
{
  m_string_new(1, 1, ".");
  m_cons(R0, NIL);

  dict_at_put(CLASS(consts.cl.module)->cl_vars,
	      consts.str.path,
	      R0
	      );
}

/***************************************************************************/

/* Class: Environment */

void
env_find(obj_t s, obj_t *pdn_found, obj_t *pdict_top)
{
  struct frame *p;
  obj_t        m, d, dn_found = NIL, dict_top = NIL;

  for (p = frp; p; p = p->prev) {
    switch (p->type) {
    case FRAME_TYPE_BLOCK:
      d = ((struct frame_block *) p)->dict;
      
      if (dict_top == NIL)  dict_top = d;
      if (pdn_found == 0 || (dn_found = dict_at(d, s)))  goto done;
      break;
      
    case FRAME_TYPE_MODULE:
      for (m = ((struct frame_module *) p)->module; m; m = MODULE(m)->parent) {
	d = OBJ(MODULE(m)->base);
	
	if (dict_top == NIL)  dict_top = d;
	if (pdn_found == 0 || (dn_found = dict_at(d, s)))  goto done;
      }
      goto done;
    default:
      ;
    }
  }
  
 done:  
  if (pdn_found)  *pdn_found = dn_found;
  if (pdict_top)  *pdict_top = dict_top;
}

obj_t
env_at(obj_t s)
{
  obj_t dn_found;

  env_find(s, &dn_found, 0);
  if (dn_found == NIL)  error(ERR_NOT_BOUND, s);

  return (CDR(dn_found));
}

void
env_new_put(obj_t s, obj_t val)
{
  obj_t dict_top;

  env_find(s, 0, &dict_top);

  dict_at_put(dict_top, s, val);
}

void
env_at_put(obj_t s, obj_t val)
{
  obj_t dn_found, dict_top;

  env_find(s, &dn_found, &dict_top);

  if (dn_found) {
    if (STRING(s)->data[0] == '#')  error(ERR_CONST, s);

    OBJ_ASSIGN(CDR(dn_found), val);
  } else {
    dict_at_put(dict_top, s, val);
  }
}

void
env_del(obj_t s)
{
  obj_t dict_top;

  env_find(s, 0, &dict_top);

  dict_del(dict_top, s);
}

void
cm_env_new_put(unsigned argc, obj_t args)
{
  obj_t val;

  if (argc != 3)  error(ERR_NUM_ARGS);
  args = CDR(args);

  env_new_put(CAR(args), val = CAR(CDR(args)));

  vm_assign(0, val);
}

void
cm_env_new(unsigned argc, obj_t args)
{
  obj_t val = NIL;

  if (argc != 2)  error(ERR_NUM_ARGS);

  env_new_put(CAR(CDR(args)), val);

  vm_assign(0, val);
}

void
cm_env_at_put(unsigned argc, obj_t args)
{
  obj_t val;

  if (argc != 3)  error(ERR_NUM_ARGS);
  args = CDR(args);

  env_at_put(CAR(args), val = CAR(CDR(args)));

  vm_assign(0, val);
}

void
cm_env_at(unsigned argc, obj_t args)
{
  if (argc != 2)  error(ERR_NUM_ARGS);

  vm_assign(0, env_at(CAR(CDR(args))));
}

void
cm_env_del(unsigned argc, obj_t args)
{
  if (argc != 2)  error(ERR_NUM_ARGS);
  
  env_del(CAR(CDR(args)));
  
  vm_assign(0, NIL);
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
  zexit(0);
}

/***************************************************************************/

void
bt_print(obj_t outf)
{
  FILE         *fp = _FILE(outf)->fp;
  struct frame *p;

  for (p = frp; p; p = p->prev) {
    switch (p->type) {
    case FRAME_TYPE_METHOD_CALL:
      {
	struct frame_method_call *q = (struct frame_method_call *) p;
	
	if (q->cl) {
	  m_method_call_2(consts.str.printc, q->cl, outf);
	  fprintf(fp, ".");
	}
	m_method_call_2(consts.str.printc, q->sel, outf);
	m_method_call_2(consts.str.printc, q->args, outf);
	putchar('\n');
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
  obj_t   outf;
  FILE    *fp;
  va_list ap;

  if (++err_lvl > 1)  fatal(FATAL_DOUBLE_ERR);

  m_file_stderr();
  outf = R0;
  fp   = _FILE(outf)->fp;
  va_start(ap, errcode);

  fprintf(fp, "\n*** ERROR - ");
  switch (errcode) {
  case ERR_STACK_OVF:
    fprintf(fp, "Stack overflow\n");
    break;
  case ERR_NUM_ARGS:
    fprintf(fp, "Incorrent number of arugments\n");
    break;
  case ERR_INVALID_ARG:
    {
      obj_t arg = va_arg(ap, obj_t);

      fprintf(fp, "Invaliad argument: ");
      m_obj_printc(arg, outf);
      fprintf(fp, "\n");
    }
    break;
  case ERR_INVALID_VALUE:
    {
      obj_t arg = va_arg(ap, obj_t);

      fprintf(fp, "Invalid value: ");
      m_obj_printc(arg, outf);
      fprintf(fp, "\n");
    }
    break;
  case ERR_INVALID_VALUE_2:
    {
      char  *nm = va_arg(ap, char *);
      obj_t arg = va_arg(ap, obj_t);

      fprintf(fp, "Invalid value for %s: ", nm);
      m_obj_printc(arg, outf);
      fprintf(fp, "\n");
    }
    break;
  case ERR_INVALID_METHOD:
    {
      obj_t arg = va_arg(ap, obj_t);

      fprintf(fp, "Invalid method: ");
      m_obj_printc(arg, outf);
      fprintf(fp, "\n");
    }
    break;
  case ERR_NO_METHOD:
    {
      obj_t arg = va_arg(ap, obj_t);

      fprintf(fp, "No such method: ");
      m_obj_printc(arg, outf);
      fprintf(fp, "\n");
    }
    break;
  case ERR_NO_ATTR:
    {
      obj_t arg = va_arg(ap, obj_t);

      fprintf(fp, "No such attribute: ");
      m_obj_printc(arg, outf);
      fprintf(fp, "\n");
    }
    break;
  case ERR_BAD_FORM:
    {
      obj_t arg = va_arg(ap, obj_t);

      fprintf(fp, "Bad form: ");
      m_obj_printc(arg, outf);
      fprintf(fp, "\n");
    }
    break;
  case ERR_NOT_BOUND:
    {
      obj_t arg = va_arg(ap, obj_t);

      fprintf(fp, "Symbol not bound: ");
      m_obj_printc(arg, outf);
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
      m_obj_printc(arg, outf);
      fprintf(fp, "\n");
    }
    break;
  case ERR_IDX_RANGE_2:
    {
      obj_t arg1 = va_arg(ap, obj_t);
      obj_t arg2 = va_arg(ap, obj_t);

      fprintf(fp, "Range out of range: ");
      m_obj_printc(arg1, outf);
      fprintf(fp, ", ");
      m_obj_printc(arg2, outf);
      fprintf(fp, "\n");
    }
    break;
  case ERR_CONST:
    {
      obj_t arg = va_arg(ap, obj_t);

      fprintf(fp, "Write to constant: ");
      m_obj_printc(arg, outf);
      fprintf(fp, "\n");
    }
    break;
  case ERR_FILE_OPEN_FAIL:
    {
      int errnum = va_arg(ap, int);

      fprintf(fp, "File open failed: %s\n", sys_errlist[errnum]);
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
  default:
    HARD_ASSERT(0);
  }

  bt_print(outf);

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
    inst_free_class,
    metaclass_init
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
  { &consts.cl.dict,
    &consts.str.dictionary,
    &consts.cl.array,
    sizeof(struct inst_dict),
    inst_init_dict,
    inst_walk_dict,
    inst_free_dict
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
  { &consts.cl.env,
    &consts.str.environment,
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
    { &consts.str.file,        "#File" },
    { &consts.str._float,      "#Float" },
    { &consts.str.integer,     "#Integer" },
    { &consts.str.list,        "#List" },    
    { &consts.str.metaclass,   "#Metaclass" },    
    { &consts.str.method_call, "#Method-Call" },
    { &consts.str.module,      "#Module" },
    { &consts.str.object,      "#Object" },    
    { &consts.str.pair,        "#Pair" },
    { &consts.str.string,      "#String" },
    { &consts.str.system,      "#System" },
    { &consts.str.addc,        "add:" },
    { &consts.str.aifc,        "&if:" },
    { &consts.str.aifc_elsec,  "&if:else:" },
    { &consts.str.andc,        "and:" },
    { &consts.str.appendc,     "append:" },
    { &consts.str.aquote,      "&quote" },
    { &consts.str.asc,         "asc" },
    { &consts.str.atc,         "at:" },
    { &consts.str.atc_lengthc, "at:length:" },
    { &consts.str.atc_putc,    "at:put:" },
    { &consts.str.awhilec,     "&while:" },
    { &consts.str._break,      "break" },
    { &consts.str.car,         "car" },
    { &consts.str.cdr,         "cdr" },
    { &consts.str.chr,         "chr" },
    { &consts.str.class_methods, "class-methods" },
    { &consts.str.class_variables, "class-variables" },
    { &consts.str._continue,   "continue" },
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
    { &consts.str.gec,         "ge:" },
    { &consts.str.gtc,         "gt:" },
    { &consts.str.hash,        "hash" },
    { &consts.str.indexc,      "index:" },
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
    { &consts.str.minus,       "minus" },
    { &consts.str.mode,        "mode" },
    { &consts.str.modc,        "mod:" },
    { &consts.str.multc,       "mult:" },
    { &consts.str.name,        "name" },
    { &consts.str.new,         "new" },
    { &consts.str.newc,        "new:" },
    { &consts.str.newc_modec,  "new:mode:" },
    { &consts.str.newc_parentc_instance_variablesc, "new:parent:instance-variables:" },
    { &consts.str.newc_putc,   "new:put:" },
    { &consts.str.nil,         "#nil" },
    { &consts.str.not,         "not" },
    { &consts.str.orc,         "or:" },
    { &consts.str.parent,      "parent" },
    { &consts.str.path,        "path" },
    { &consts.str.pquote,      "pquote" },
    { &consts.str.print,       "print" },
    { &consts.str.printc,      "print:" },
    { &consts.str.range,       "range" },
    { &consts.str.rangec,      "range:" },
    { &consts.str.rangec_stepc, "range:step:" },
    { &consts.str.readc,       "read:" },
    { &consts.str.readln,      "readln" },
    { &consts.str.reducec_initc, "reduce:init:" },
    { &consts.str._return,     "return" },
    { &consts.str.rindexc,     "rindex:" },
    { &consts.str._stderr,     "stderr" },
    { &consts.str._stdin,      "stdin" },
    { &consts.str._stdout,     "stdout" },
    { &consts.str.splicec,     "splice:" },
    { &consts.str.splitc,      "split:" },
    { &consts.str.subc,        "sub:" },
    { &consts.str.tostring,    "tostring" },
    { &consts.str.tostringc,   "tostring:" },
    { &consts.str._true,       "#true" },
    { &consts.str.writec,      "write:" },
    { &consts.str.xorc,        "xor:" }
#ifndef NDEBUG
    ,
    { &consts.str.assert,      "assert" },
    { &consts.str.collect,     "collect" },
    { &consts.str.debugc,      "debug:" }
#endif
};

const struct init_method init_cl_method_tbl[] = {
  { &consts.cl.metaclass, &consts.str.name,                   cm_metaclass_name },
  { &consts.cl.metaclass, &consts.str.tostring,               cm_metaclass_name },
  { &consts.cl.metaclass, &consts.str.parent,                 cm_metaclass_parent },
  { &consts.cl.metaclass, &consts.str.class_methods,          cm_metaclass_cl_methods },
  { &consts.cl.metaclass, &consts.str.class_variables,        cm_metaclass_cl_vars },
  { &consts.cl.metaclass, &consts.str.instance_methods,       cm_metaclass_inst_methods },
  { &consts.cl.metaclass, &consts.str.instance_variables,     cm_metaclass_inst_vars },
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

  { &consts.cl.array, &consts.str.newc, cm_array_new },

  { &consts.cl.dict, &consts.str.new, cm_dict_new },

  { &consts.cl.module, &consts.str.newc, cm_module_new },

  { &consts.cl.file, &consts.str.newc_modec, cm_file_new },

  { &consts.cl.env, &consts.str.newc,      cm_env_new },
  { &consts.cl.env, &consts.str.newc_putc, cm_env_new_put },
  { &consts.cl.env, &consts.str.atc,       cm_env_at },
  { &consts.cl.env, &consts.str.atc_putc,  cm_env_at_put },
  { &consts.cl.env, &consts.str.delc,      cm_env_del },

  { &consts.cl.system, &consts.str.exit,  cm_system_exit }
}, init_inst_method_tbl[] = {
  { &consts.cl.metaclass, &consts.str.name,               cm_class_name },
  { &consts.cl.metaclass, &consts.str.tostring,           cm_class_name },
  { &consts.cl.metaclass, &consts.str.parent,             cm_class_parent },
  { &consts.cl.metaclass, &consts.str.instance_variables, cm_class_inst_vars },

  { &consts.cl.object, &consts.str.aquote,      cm_object_quote },
  { &consts.cl.object, &consts.str.eval,        cm_object_eval },
  { &consts.cl.object, &consts.str.instance_of, cm_object_instof },
  { &consts.cl.object, &consts.str.equalsc,     cm_object_eq },
  { &consts.cl.object, &consts.str.tostring,    cm_object_tostring },
  { &consts.cl.object, &consts.str.print,       cm_object_print },
  { &consts.cl.object, &consts.str.printc,      cm_object_printc },
  { &consts.cl.object, &consts.str.atc,         cm_object_at },
  { &consts.cl.object, &consts.str.atc_putc,    cm_object_at_put },
  { &consts.cl.object, &consts.str.aifc,        cm_object_if },
  { &consts.cl.object, &consts.str.aifc_elsec,  cm_object_if_else },
  { &consts.cl.object, &consts.str.awhilec,     cm_object_while },
  { &consts.cl.object, &consts.str._break,      cm_object_break },
  { &consts.cl.object, &consts.str._continue,   cm_object_cont },
  { &consts.cl.object, &consts.str._return,     cm_object_return },

  { &consts.cl.code_method, &consts.str.evalc, cm_code_method_eval },

  { &consts.cl.boolean, &consts.str.andc,      cm_boolean_and },
  { &consts.cl.boolean, &consts.str.orc,       cm_boolean_or },
  { &consts.cl.boolean, &consts.str.xorc,      cm_boolean_xor },
  { &consts.cl.boolean, &consts.str.not,       cm_boolean_not },
  { &consts.cl.boolean, &consts.str.tostring,  cm_boolean_tostring },
  { &consts.cl.boolean, &consts.str.equalsc,   cm_boolean_equals },
  { &consts.cl.boolean, &consts.str.assert,    cm_boolean_assert },

  { &consts.cl.integer, &consts.str.addc,         cm_integer_add },
  { &consts.cl.integer, &consts.str.subc,         cm_integer_sub },
  { &consts.cl.integer, &consts.str.multc,        cm_integer_mult },
  { &consts.cl.integer, &consts.str.divc,         cm_integer_div },
  { &consts.cl.integer, &consts.str.modc,         cm_integer_mod },
  { &consts.cl.integer, &consts.str.range,        cm_integer_range },
  { &consts.cl.integer, &consts.str.rangec,       cm_integer_range_init },
  { &consts.cl.integer, &consts.str.rangec_stepc, cm_integer_range_init_step },
  { &consts.cl.integer, &consts.str.chr,          cm_integer_chr },
  { &consts.cl.integer, &consts.str.ltc,          cm_integer_lt },
  { &consts.cl.integer, &consts.str.lec,          cm_integer_le },
  { &consts.cl.integer, &consts.str.gtc,          cm_integer_gt },
  { &consts.cl.integer, &consts.str.gec,          cm_integer_ge },
  { &consts.cl.integer, &consts.str.equalsc,      cm_integer_equals },
  { &consts.cl.integer, &consts.str.hash,         cm_integer_hash },
  { &consts.cl.integer, &consts.str.minus,        cm_integer_minus },
  { &consts.cl.integer, &consts.str.andc,         cm_integer_and },
  { &consts.cl.integer, &consts.str.orc,          cm_integer_or },
  { &consts.cl.integer, &consts.str.xorc,         cm_integer_xor },
  { &consts.cl.integer, &consts.str.not,          cm_integer_not },
  { &consts.cl.integer, &consts.str.tostring,     cm_integer_tostring },
  { &consts.cl.integer, &consts.str.tostringc,    cm_integer_tostring_base },

  { &consts.cl._float, &consts.str.addc,     cm_float_add },
  { &consts.cl._float, &consts.str.subc,     cm_float_sub },
  { &consts.cl._float, &consts.str.multc,    cm_float_mult },
  { &consts.cl._float, &consts.str.divc,     cm_float_div },
  { &consts.cl._float, &consts.str.minus,    cm_float_minus },
  { &consts.cl._float, &consts.str.hash,     cm_float_hash },
  { &consts.cl._float, &consts.str.equalsc,  cm_float_equals },
  { &consts.cl._float, &consts.str.gec,      cm_float_ge },
  { &consts.cl._float, &consts.str.tostring, cm_float_tostring },

  { &consts.cl.string, &consts.str.hash,        cm_string_hash },
  { &consts.cl.string, &consts.str.equalsc,     cm_string_equal },
  { &consts.cl.string, &consts.str.appendc,     cm_string_append },
  { &consts.cl.string, &consts.str.tostring,    cm_string_tostring },
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
  { &consts.cl.string, &consts.str.pquote,      cm_string_pquote },

  { &consts.cl.dptr, &consts.str.car,      cm_dptr_car },
  { &consts.cl.dptr, &consts.str.cdr,      cm_dptr_cdr },
  { &consts.cl.dptr, &consts.str.hash,     cm_dptr_hash },
  { &consts.cl.dptr, &consts.str.equalsc,  cm_dptr_equals },

  { &consts.cl.pair, &consts.str.eval,     cm_pair_eval },
  { &consts.cl.pair, &consts.str.tostring, cm_pair_tostring },
  { &consts.cl.pair, &consts.str.atc,      cm_pair_at },
  
  { &consts.cl.list, &consts.str.length,        cm_list_len },
  { &consts.cl.list, &consts.str.tostring,      cm_list_tostring },
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

  { &consts.cl.method_call, &consts.str.tostring, cm_method_call_tostring },
  { &consts.cl.method_call, &consts.str.eval,     cm_method_call_eval },

  { &consts.cl.block, &consts.str.eval,     cm_block_eval },
  { &consts.cl.block, &consts.str.evalc,    cm_block_eval },
  { &consts.cl.block, &consts.str.tostring, cm_block_tostring },

  { &consts.cl.array, &consts.str.atc,      cm_array_at },
  { &consts.cl.array, &consts.str.atc_putc, cm_array_at_put },
  { &consts.cl.array, &consts.str.tostring, cm_array_tostring },
  
  { &consts.cl.dict, &consts.str.atc,      cm_dict_at },
  { &consts.cl.dict, &consts.str.atc_putc, cm_dict_at_put },
  { &consts.cl.dict, &consts.str.delc,     cm_dict_del },
  { &consts.cl.dict, &consts.str.keys,     cm_dict_keys },
  { &consts.cl.dict, &consts.str.tostring, cm_dict_tostring },

  { &consts.cl.module, &consts.str.name,     cm_module_name },
  { &consts.cl.module, &consts.str.parent,   cm_module_parent },
  { &consts.cl.module, &consts.str.atc,      cm_module_at },
  { &consts.cl.module, &consts.str.tostring, cm_module_tostring },

  { &consts.cl.file, &consts.str.name,     cm_file_name },
  { &consts.cl.file, &consts.str.mode,     cm_file_mode },
  { &consts.cl.file, &consts.str.tostring, cm_file_tostring },
  { &consts.cl.file, &consts.str.readc,    cm_file_read },
  { &consts.cl.file, &consts.str.readln,   cm_file_readln },
  { &consts.cl.file, &consts.str.writec,   cm_file_write },
  { &consts.cl.file, &consts.str.flush,    cm_file_flush },
  { &consts.cl.file, &consts.str.eof,      cm_file_eof },
  { &consts.cl.file, &consts.str.load,     cm_file_load }
};

void
init_cls(const struct init_cl *tbl, unsigned n)
{
  for ( ; n; --n, ++tbl) {
    void (*f)(void);
    
    m_class_new(*tbl->name,
		*tbl->parent,
		module_cur, 
		tbl->inst_size,
		tbl->inst_init,
		tbl->inst_walk,
		tbl->inst_free
		);
    OBJ_ASSIGN(*tbl->cl, R0);

    if (f = tbl->cl_init)  (*f)();
  }
}

void
init_strs(const struct init_str *tbl, unsigned n)
{
  for ( ; n; --n, ++tbl) {
    m_string_new(1, strlen(tbl->str), tbl->str);
    OBJ_ASSIGN(*tbl->obj, R0);
  }
}

void
init_cl_methods(const struct init_method *tbl, unsigned n)
{
  for ( ; n; --n, ++tbl) {
    m_code_method_new(tbl->func);
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
    m_code_method_new(tbl->func);
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

  stack = _cmalloc(STACK_SIZE * sizeof(obj_t));
  sp = stack_end = stack + STACK_SIZE;

  for (i = 0; i < ARRAY_SIZE(obj_list); ++i)  list_init(&obj_list[i]);

  /* Step 1. Create classes, first pass */

  for (i = 0; i < ARRAY_SIZE(init_cl_tbl); ++i) {
    if (i == 0) {
      vm_assign(0, _obj_alloc(sizeof(struct inst_metaclass)));
    } else {
      vm_inst_alloc(consts.cl.metaclass);
    }
    CLASS(R0)->inst_size = init_cl_tbl[i].inst_size;
    CLASS(R0)->inst_init = init_cl_tbl[i].inst_init;
    CLASS(R0)->inst_walk = init_cl_tbl[i].inst_walk;
    CLASS(R0)->inst_free = init_cl_tbl[i].inst_free;
    OBJ_ASSIGN(*init_cl_tbl[i].cl, R0);
  }

  /* Step 2. Fix up class hierarchy */

  for (i = 0; i < ARRAY_SIZE(init_cl_tbl); ++i) {
    OBJ_ASSIGN(CLASS(*init_cl_tbl[i].cl)->parent,
	       init_cl_tbl[i].parent ? *init_cl_tbl[i].parent : NIL
	       );
  }

  /* Step 3. Create constants */

  vm_inst_alloc(consts.cl.boolean);
  inst_init(R0, 0);
  OBJ_ASSIGN(consts._bool._false, R0);
  vm_inst_alloc(consts.cl.boolean);
  inst_init(R0, 1);
  OBJ_ASSIGN(consts._bool._true, R0);
  
  init_strs(init_str_tbl, ARRAY_SIZE(init_str_tbl));

  /* Step 4. Create classes, second pass */

  for (i = 0; i < ARRAY_SIZE(init_cl_tbl); ++i) {
    OBJ_ASSIGN(CLASS(*init_cl_tbl[i].cl)->name, *init_cl_tbl[i].name);
    m_string_dict_new(16);
    OBJ_ASSIGN(CLASS(*init_cl_tbl[i].cl)->cl_vars, R0);
    m_string_dict_new(16);
    OBJ_ASSIGN(CLASS(*init_cl_tbl[i].cl)->cl_methods, R0);
    m_string_dict_new(16);
    OBJ_ASSIGN(CLASS(*init_cl_tbl[i].cl)->inst_vars, R0);
    m_string_dict_new(16);
    OBJ_ASSIGN(CLASS(*init_cl_tbl[i].cl)->inst_methods, R0);
  }  

  init_cl_methods(init_cl_method_tbl, ARRAY_SIZE(init_cl_method_tbl));

  init_inst_methods(init_inst_method_tbl, ARRAY_SIZE(init_inst_method_tbl));

  for (i = 0; i < ARRAY_SIZE(init_cl_tbl); ++i) {
    void (*f)(void);
    
    if (f = init_cl_tbl[i].cl_init)  (*f)();
  }

  /* Step 5. Create main module */

  m_module_new(consts.str.main, NIL);
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


void
interactive(void)
{
  FRAME_RESTART_BEGIN {
    frame_jmp_code = frame_jmp_code; /* Suppress unused variable warning */
    
    for (;;) {
      printf("\nok ");
      fflush(stdout);
      
      if (yyparse() != 0)  break;
 
      vm_dropn(frp->sp - sp); /* Discard all objs pushed during parsing */

      m_method_call_1(consts.str.eval, R0);
      m_method_call_1(consts.str.print, R0);
    }
  } FRAME_RESTART_END;
}


void
batch(char *filename)
{
  FILE *fp;

  fp = fopen(filename, "r+");
  if (fp == 0) {
    fprintf(stderr, "Failed to open %s: ", filename);
    perror(0);
    zexit(1);
  }

  FRAME_RESTART_BEGIN {

    if (frame_jmp_code == 0) {
      FRAME_INPUT_BEGIN(fp, filename, 0) {

	for (;;) {
	  if (yyparse() != 0)  break;
	  
	  vm_dropn(frp->sp - sp); /* Discard all objs pushed during parsing */
	  
	  m_method_call_1(consts.str.eval, R0);
	}

      } FRAME_INPUT_END;
    }

  } FRAME_RESTART_END;

  fclose(fp);
}


int
yyerror(void)
{
    return (0);
}


int
main(int argc, char **argv)
{
  init();

#ifndef NDEBUG
  collect();			/* Check consistency */
  ASSERT(stats.mem.collected_cnt == 0);
  ASSERT(stats.mem.bytes_collected == 0);
#endif

  FRAME_MODULE_BEGIN(module_main) {

    if (argc > 1)  batch(argv[1]);  else  interactive();

  } FRAME_MODULE_END;

  zexit(0);

  return (0);			/* Suppress warning */
}
