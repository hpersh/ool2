#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>
#include <setjmp.h>
#include <assert.h>

#include "ool.h"

#undef  PTR_64_BITS


#ifndef NDEBUG
#define ASSERT(x)  assert(x)
#else
#define ASSERT(x)
#endif
#define HARD_ASSERT  assert

#define ARRAY_SIZE(a)  (sizeof(a) / sizeof((a)[0]))
#ifdef PTR_64_BITS
#define PTR_TO_INT(x)  ((long long)(x))
#else
#define PTR_TO_INT(x)  ((long)(x))
#endif
#define FIELD_OFS(s, f)                   PTR_TO_INT(&((s *) 0)->f)
#define FIELD_PTR_TO_STRUCT_PTR(p, s, f)  ((s *)((char *)(p) - FIELD_OFS(s, f)))

obj_t inst_of(obj_t obj);
unsigned is_kind_of(obj_t obj, obj_t cl);

struct inst_metaclass {
  struct obj base[1];
  obj_t      name, parent, module;
  obj_t      cl_vars, cl_methods, inst_vars, inst_methods;
  unsigned   inst_size;
  void       (*inst_init)(obj_t cl, obj_t inst, va_list ap);
  void       (*inst_walk)(obj_t cl, obj_t inst, void (*func)(obj_t));
  void       (*inst_free)(obj_t cl, obj_t inst);
};
#define CLASS(x)    ((struct inst_metaclass *)(x))
void inst_walk_metaclass(obj_t inst, void (*func)(obj_t));
unsigned is_subclass_of(obj_t cl1, obj_t cl2);

struct inst_code_method {
  struct obj base[1];
  void       (*func)(unsigned argc, obj_t args);
};
#define CODE_METHOD(x)  ((struct inst_code_method *)(x))

struct inst_boolean {
  struct obj base[1];
  unsigned   val;
};
#define BOOLEAN(x)  ((struct inst_boolean *)(x))

struct inst_integer {
  struct obj base[1];
  integer_val_t  val;
};
#define INTEGER(x)  ((struct inst_integer *)(x))

struct inst_float {
  struct obj  base[1];
  float_val_t val;
};
#define FLOAT(x)  ((struct inst_float *)(x))

struct inst_string {
  struct obj base[1];
  unsigned   size;		/* N.B. Includes the '\0' terminator */
  char       *data;
};
#define STRING(x)  ((struct inst_string *)(x))
unsigned string_len(obj_t s);

struct inst_dptr {
  struct obj base[1];
  obj_t      car, cdr;
};
#define DPTR(x)  ((struct inst_dptr *)(x))
#define CAR(x)   (DPTR(x)->car)
#define CDR(x)   (DPTR(x)->cdr)
unsigned list_len(obj_t li);

struct inst_method_call {
  struct obj base[1];
  obj_t      list;
};
#define METHOD_CALL(x)  ((struct inst_method_call *)(x))

struct inst_block {
  struct obj base[1];
  obj_t      list;
};
#define BLOCK(x)  ((struct inst_block *)(x))

struct inst_array {
  struct obj base[1];
  unsigned   size;
  obj_t      *data;
};
#define ARRAY(x)  ((struct inst_array *)(x))
obj_t array_at(obj_t arr, integer_val_t idx);
void  array_at_put(obj_t arr, integer_val_t idx, obj_t val);

struct inst_dict {
  struct inst_array base[1];
  unsigned (*key_hash)(obj_t key);
  unsigned (*key_equal)(obj_t key1, obj_t key2);
};
#define DICT(x)  ((struct inst_dict *)(x))
obj_t    dict_at(obj_t dict, obj_t key);
void     dict_at_put(obj_t dict, obj_t key, obj_t val);
void     dict_del(obj_t dict, obj_t key);
unsigned dict_count(obj_t dict);
void  m_dict_keys(obj_t dict);

struct inst_module {
  struct inst_dict base[1];
  obj_t            name, parent;
  void             *ptr;	/* Cookie from dl_open() */
};
#define MODULE(x)  ((struct inst_module *)(x))

struct inst_file {
  struct obj base[1];
  obj_t      name, mode;
  FILE       *fp;
};
#define _FILE(x)  ((struct inst_file *)(x))

obj_t env_at(obj_t s);
void  env_new_put(obj_t s, obj_t val);
void  env_at_put(obj_t s, obj_t val);
void  env_del(obj_t s);

/*
  Class instance relationship

    Metaclass [i.e. inst_of(Metaclass) == NIL]
      Object
      Boolean
      Integer
      ...

  Class parent relationship (hierarchy)

    Object  [i.e. parent(Object) == NIL]
      Metaclass
      Boolean
      Integer
      ...

  Note that this leads to the following reference cycles:
  - parent(inst_of(Object)) == Object
  - inst_of(name(String)) == String
*/

obj_t *stack, *stack_end, *sp;
#define STACK_SIZE  8192	/* In objects */
obj_t module_main, module_cur;

/* The root set is made up of:
   - VM registers       (regs)
   - VM stack           (stack..stack_end)
   - main module        (module_main)
   - internal constants (consts)
*/

#ifndef NDEBUG

struct {
  struct {
    unsigned stack_depth;
    unsigned stack_depth_max;
  } vm;
  struct {
    unsigned alloc_cnt;
    unsigned alloc_lim;
    unsigned bytes_alloced;
    unsigned free_cnt;
    unsigned bytes_freed;
    unsigned bytes_in_use;
    unsigned bytes_in_use_max;
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
  PRINT_STAT(stats.mem.alloc_lim);
  PRINT_STAT(stats.mem.bytes_alloced);
  PRINT_STAT(stats.mem.free_cnt);
  PRINT_STAT(stats.mem.bytes_freed);
  PRINT_STAT(stats.mem.bytes_in_use);
  PRINT_STAT(stats.mem.bytes_in_use_max);
  PRINT_STAT(stats.mem.collect_cnt);
  PRINT_STAT(stats.mem.collected_cnt);
  PRINT_STAT(stats.mem.bytes_collected);
}

#endif

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

unsigned hash_val;

void
hash_init(void)
{
  hash_val = 3123425579;
}

unsigned
hash(void *buf, unsigned n)
{
  unsigned char *p;

  for (p = (unsigned char *) buf; n; --n, ++p)  hash_val = 37 * hash_val + *p;

  return (hash_val);
}

unsigned
hashb(void *buf, unsigned n)
{
  unsigned char *p;

  for (p = (unsigned char *) buf + n - 1; n; --n, --p)  hash_val = 37 * hash_val + *p;

  return (hash_val);
}

/***************************************************************************/

enum {
  FATAL_DOUBLE_ERR,
  FATAL_NO_MEM,
  FATAL_STACK_UNF,
};

const char * const fatal_msgs[] = {
  "Double error",
  "Out of memory",
  "Stack underflow"
};

void
fatal(unsigned errcode)
{
  HARD_ASSERT(errcode < ARRAY_SIZE(fatal_msgs));

  fprintf(stderr, "*** FATAL ERROR: %s\n", fatal_msgs[errcode]);

  abort();
}

enum {
  ERR_STACK_OVF,
  ERR_NUM_ARGS,
  ERR_INVALID_ARG,
  ERR_INVALID_VALUE,
  ERR_INVALID_VALUE_2,
  ERR_INVALID_METHOD,
  ERR_NO_METHOD,
  ERR_NO_ATTR,
  ERR_BAD_FORM,
  ERR_NOT_BOUND,
  ERR_OVF,
  ERR_IDX_RANGE,
  ERR_IDX_RANGE_2,
  ERR_CONST,
  ERR_ASSERT_FAIL
};

const char * const error_msgs[] = {
  "Stack overflow",
  "Incorrect number of arguments",
  "Invalid argument",
  "Invalid value",
  "Invalid value",
  "Invalid method",
  "No such method",
  "No such attribute",
  "Bad form",
  "Symbol not bound",
  "Arithmetic overflow",
  "Index out of range",
  "Index out of range",
  "Write constant",
  "Assertion failed"
};

void error(unsigned errcode, ...);

/***************************************************************************/

unsigned initf;

void collect(void);

void *
_cmalloc(unsigned size)
{
  static unsigned alloc_lim = 100000, alloc_cnt;

  void *result = 0;

  if (!initf) {
    if (alloc_cnt < alloc_lim)  result = malloc(size);
  
    if (result == 0) {
      collect();
      
      if (alloc_cnt < alloc_lim)  alloc_lim = alloc_cnt >> 1;
      alloc_cnt = 0;
    }
  }

  if (result == 0) {
    if ((result = malloc(size)) == 0)  fatal(FATAL_NO_MEM);
  }

  ++alloc_cnt;

#ifndef NDEBUG
  ++stats.mem.alloc_cnt;
  stats.mem.alloc_lim     = alloc_lim;
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

void
inst_init(obj_t inst, ...)
{
  obj_t   cl = inst_of(inst);
  va_list ap;

  va_start(ap, inst);
  (*CLASS(cl)->inst_init)(cl, inst, ap);
  va_end(ap);
}

void
inst_init_parent(obj_t cl, obj_t inst, va_list ap)
{
  obj_t parent = CLASS(cl)->parent;

  (*CLASS(parent)->inst_init)(parent, inst, ap);
}

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

void
inst_walk_parent(obj_t cl, obj_t inst, void (*func)(obj_t))
{
  obj_t parent = CLASS(cl)->parent;

  (*CLASS(parent)->inst_walk)(parent, inst, func);
}

void
inst_free(obj_t inst)
{
  obj_t cl = inst_of(inst);

  (*CLASS(cl)->inst_free)(cl, inst);
}

void
inst_free_parent(obj_t cl, obj_t inst)
{
  obj_t parent = CLASS(cl)->parent;

  (*CLASS(parent)->inst_free)(parent, inst);
}

obj_t obj_retain(obj_t obj);
void  obj_release(obj_t obj);

void
_obj_assign(obj_t *dst, obj_t src)
{
  obj_t temp = *dst;

  *dst = src;
  obj_release(temp);
}
#define OBJ_ASSIGN(dst, src)  (_obj_assign(&(dst), obj_retain(src)))

obj_t
_obj_alloc(unsigned size)
{
  obj_t result = OBJ(_zcmalloc(size));

  list_insert(result->list_node, LIST_END(OBJ_LIST_ACTIVE));

  return (result);
}

obj_t
obj_alloc(obj_t cl)
{
  obj_t result = _obj_alloc(CLASS(cl)->inst_size);

  OBJ_ASSIGN(result->inst_of, cl);

  return (result);
}

void
obj_free(obj_t obj)
{
  list_erase(obj->list_node);

  _cfree(CLASS(inst_of(obj))->inst_size, obj);
}

obj_t
obj_retain(obj_t obj)
{
  if (obj != NIL) {
    ++obj->ref_cnt;
    
    HARD_ASSERT(obj->ref_cnt != 0);
  }
  
  return (obj);
}

void
obj_release(obj_t obj)
{
  if (obj == NIL
      || obj->ref_cnt == 0  /* Because of reference cycles */
      || --obj->ref_cnt != 0
      )  return;

  inst_walk(obj, obj_release);
  inst_free(obj);

  obj_free(obj);
}

void
obj_mark(obj_t obj)
{
  if (obj == NIL)  return;

  obj_retain(obj);

  if (obj->ref_cnt > 1)  return;

  list_erase(obj->list_node);
  list_insert(obj->list_node, LIST_END(OBJ_LIST_MARKED));

  inst_walk(obj, obj_mark);
}

void
root_walk(void (*func)(obj_t))
{
  obj_t    *q;
  unsigned n;

  for (q = regs, n = ARRAY_SIZE(regs); n; --n, ++q)  (*func)(*q);
  for (q = sp; q < stack_end; ++q)  (*func)(*q);
  (*func)(module_main);
  for (q = (obj_t *) &consts, n = sizeof(consts) / sizeof(obj_t);
       n;
       --n, ++q
       ) {
    (*func)(*q);
  }
}

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
    r = FIELD_PTR_TO_STRUCT_PTR(p, struct obj, list_node);

#ifndef NDEBUG
    ASSERT(r->ref_cnt != 0);
    r->old_ref_cnt = r->ref_cnt;
#endif

    r->ref_cnt = 0;
  }

  root_walk(obj_mark);

  for (e = LIST_END(OBJ_LIST_ACTIVE); (p = LIST_FIRST(OBJ_LIST_ACTIVE)) != e; ) {
    r = FIELD_PTR_TO_STRUCT_PTR(p, struct obj, list_node);
    
#ifndef NDEBUG
    ++stats.mem.collected_cnt;
    stats.mem.bytes_collected = CLASS(inst_of(r))->inst_size;
#endif

    obj_free(r);
  }
  
  obj_list_swap();

#ifndef NDEBUG
  for (e = LIST_END(OBJ_LIST_ACTIVE), p = LIST_FIRST(OBJ_LIST_ACTIVE); p != e; p = LIST_NEXT(p)) {
    r = FIELD_PTR_TO_STRUCT_PTR(p, struct obj, list_node);

    ASSERT(r->ref_cnt == r->old_ref_cnt);
  }
#endif
}

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

#define VM_STACK_CHECK_UP(n)    do { if ((sp - ((n) + 8)) < stack)  error(ERR_STACK_OVF); } while (0)
#define VM_STACK_CHECK_DOWN(n)  HARD_ASSERT((sp + (n)) <= stack_end)

void
vm_pushl(obj_t obj)
{
  VM_STACK_CHECK_UP(1);

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
  VM_STACK_CHECK_UP(n);

  for (p = &REG(src); n; --n, ++p)  *--sp = obj_retain(*p);
}

void
vm_pop(unsigned dst)
{
  ASSERT_REG_VALID(dst);
  VM_STACK_CHECK_DOWN(1);

  _obj_assign(&REG(dst), *sp);
  ++sp;
}

void
vm_popm(unsigned dst, unsigned n)
{
  obj_t *p;

  ASSERT_REG_VALID(dst + n - 1);
  VM_STACK_CHECK_DOWN(n);

  for (p = &REG(dst + n - 1); n; --n, --p, ++sp)  _obj_assign(p, *sp);
}

void
vm_drop(void)
{
  VM_STACK_CHECK_DOWN(1);

  obj_release(*sp);
  ++sp;
}

void
vm_dropn(unsigned n)
{
  VM_STACK_CHECK_DOWN(n);

  for ( ; n; --n, ++sp)  obj_release(*sp);
}

/***************************************************************************/

enum {
  FRAME_TYPE_RESTART,
  FRAME_TYPE_MODULE,
  FRAME_TYPE_BLOCK,
  FRAME_TYPE_METHOD_CALL
};

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

#define FRAME_RESTART_END			\
  FRAME_POP;					\
  }

struct frame_module {
  struct frame base;
  obj_t        module;
};

#define FRAME_MODULE_BEGIN(m)			\
  {						\
  struct frame_module __frame[1];		\
  __frame->base.prev = frp;			\
  __frame->base.type = FRAME_TYPE_MODULE;	\
  __frame->base.sp   = sp;			\
  __frame->module    = (m);			\
  frp = &__frame->base;

#define FRAME_MODULE_END			\
  FRAME_POP;					\
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

#define FRAME_BLOCK_END				\
  FRAME_POP;					\
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

#define FRAME_METHOD_CALL_END \
  FRAME_POP;		      \
  }

void
frame_jmp(unsigned type, int frame_jmp_code)
{
  switch (type) {
  case FRAME_TYPE_RESTART:
  case FRAME_TYPE_BLOCK:
    break;
  default:
    HARD_ASSERT(0);
  }

  while (frp) {
    if (frp->type == type)  longjmp(((struct frame_jmp *) frp)->jmp_buf, frame_jmp_code);
    FRAME_POP;
  }

  HARD_ASSERT(0);
}

/***************************************************************************/

void m_method_call(struct frame_method_call *mcfrp, obj_t sel, unsigned argc, obj_t args);
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
    m_method_call_2(consts.str.evalc, func, args);
    return;
  }

  error(ERR_INVALID_METHOD);
}

void
m_method_call(struct frame_method_call *mcfrp, obj_t sel, unsigned argc, obj_t args)
{
  obj_t recvr = CAR(args);
  obj_t cl, p;

  vm_push(0);

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
  vm_drop();
}

void
m_method_call_1(obj_t sel, obj_t recvr)
{
  vm_push(0);

  m_cons(recvr, NIL);

  m_method_call(0, sel, 1, R0);

  vm_drop();
}

void
m_method_call_2(obj_t sel, obj_t recvr, obj_t arg)
{
  vm_push(0);

  m_cons(arg, NIL);
  m_cons(recvr, R0);

  m_method_call(0, sel, 2, R0);

  vm_drop();
}

void
m_method_call_3(obj_t sel, obj_t recvr, obj_t arg1, obj_t arg2)
{
  vm_push(0);

  m_cons(arg2, NIL);
  m_cons(arg1, R0);
  m_cons(recvr, R0);

  m_method_call(0, sel, 3, R0);

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
  OBJ_ASSIGN(CLASS(inst)->name,   va_arg(ap, obj_t));
  OBJ_ASSIGN(CLASS(inst)->parent, va_arg(ap, obj_t));
  OBJ_ASSIGN(CLASS(inst)->module, va_arg(ap, obj_t));

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
m_class_new(obj_t name, obj_t parent, obj_t module)
{
  vm_push(0);

  vm_inst_alloc(consts.cl.metaclass);
  inst_init(R0, name, parent, module);

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

  m_class_new(name, parent, module_cur);
  vm_assign(1, R0);

  m_string_dict_new(16);
  OBJ_ASSIGN(CLASS(R1)->cl_vars, R0);
  m_string_dict_new(16);
  OBJ_ASSIGN(CLASS(R1)->cl_methods, R0);
  m_string_dict_new(16);
  OBJ_ASSIGN(CLASS(R1)->inst_vars, R0);
  m_string_dict_new(16);
  OBJ_ASSIGN(CLASS(R1)->inst_methods, R0);
  CLASS(R1)->inst_size = CLASS(parent)->inst_size + list_len(inst_vars) * sizeof(obj_t);
  CLASS(R1)->inst_init = inst_init_user;
  CLASS(R1)->inst_walk = inst_walk_user;
  CLASS(R1)->inst_free = inst_free_user;

  for (ofs = CLASS(parent)->inst_size; inst_vars; inst_vars = CDR(inst_vars), ofs += sizeof(obj_t)) {
    m_integer_new(ofs);
    dict_at_put(CLASS(R1)->inst_vars, CAR(inst_vars), R0);
  }

  m_code_method_new(cm_object_new);
  dict_at_put(CLASS(R1)->cl_methods, consts.str.new, R0);

  dict_at_put(OBJ(MODULE(module_cur)->base), name, R1);

  vm_assign(0, R1);

  vm_pop(1);
}

void
cm_class_name(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.metaclass))  error(ERR_INVALID_ARG, recvr);
  
  vm_assign(0, CLASS(recvr)->name);
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
  
  m_string_new(1,
	       snprintf(buf, sizeof(buf), "<instance of %s @ %p>",
			STRING(CLASS(inst_of(recvr))->name)->data,
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

  for (cl = inst_of(inst); cl; cl = CLASS(cl)->parent) {
    if (p = dict_at(CLASS(cl)->inst_vars, s)) {
      return ((obj_t *)((char *) inst + INTEGER(CDR(p))->val));
    }
  }

  return (0);
}

void
cm_object_at(unsigned argc, obj_t args)
{
  obj_t recvr, arg, *p;

  if (argc != 2)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  arg   = CAR(CDR(args));
  
  if (p = obj_attr_find(recvr, arg)) {
    vm_assign(0, *p);
    return;
  }

  error(ERR_NO_ATTR, recvr, arg);
}

void
cm_object_at_put(unsigned argc, obj_t args)
{
  obj_t recvr, k, val, *p;

  if (argc != 3)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  args  = CDR(args);
  k     = CAR(args);
  args  = CDR(args);
  val   = CAR(args);
  
  if (p = obj_attr_find(recvr, k)) {
    OBJ_ASSIGN(*p, val);

    vm_assign(0, val);
    return;
  }

  error(ERR_NO_ATTR, recvr, k);
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
cm_boolean_if(unsigned argc, obj_t args)
{
  obj_t recvr;
  
  if (argc != 2)                              error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.boolean))  error(ERR_INVALID_ARG, recvr);
  
  if (BOOLEAN(recvr)->val) {
    m_method_call_1(consts.str.eval, CAR(CDR(args)));
  } else {
    vm_assign(0, recvr);
  }
}

void
cm_boolean_if_else(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 3)                              error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.boolean))  error(ERR_INVALID_ARG, recvr);
  args = CDR(args);
  
  m_method_call_1(consts.str.eval,
		  BOOLEAN(recvr)->val ? CAR(args) : CAR(CDR(args))
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
  obj_t recvr;
  
  if (argc != 1)                              error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.integer))  error(ERR_INVALID_ARG, recvr);
  
  hash_init();
  m_integer_new(
#ifdef LITTLE_ENDIAN
		hash
#else
		hashb
#endif
		(&INTEGER(recvr)->val, sizeof(INTEGER(recvr)->val))
		);
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
  
  if (argc != 2)                              error(ERR_NUM_ARGS);
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
  obj_t recvr;

  if (argc != 1)                             error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl._float))  error(ERR_INVALID_ARG, recvr);

  hash_init();
  m_integer_new(
#ifdef LITTLE_ENDIAN
		hash
#else
		hashb
#endif
		(&FLOAT(recvr)->val, sizeof(FLOAT(recvr)->val))
		);
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
  hash_init();

  return (hashb(STRING(s)->data, string_len(s)));
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
string_print(obj_t s, FILE *fp)
{
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
  
  string_print(recvr, stdout);
  
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
  
  string_print(recvr, _FILE(arg)->fp);
  
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
  OBJ_ASSIGN(METHOD_CALL(inst)->list, va_arg(ap, obj_t));

  inst_init_parent(cl, inst, ap);
}

void
inst_walk_method_call(obj_t cl, obj_t inst, void (*func)(obj_t))
{
  (*func)(METHOD_CALL(inst)->list);

  inst_walk_parent(cl, inst, func);
}

void
inst_free_method_call(obj_t cl, obj_t inst)
{
  inst_free_parent(cl, inst);
}

void
m_method_call_new(obj_t li)
{
  vm_push(0);

  vm_inst_alloc(consts.cl.method_call);
  inst_init(R0, li);

  vm_drop();
}

void
_method_call_concat(obj_t mc, obj_t el)
{
  _list_concat(&METHOD_CALL(mc)->list, el);
}

void
cm_method_call_new(unsigned argc, obj_t args)
{
  obj_t arg;

  if (argc != 2)                         error(ERR_NUM_ARGS);
  arg = CAR(CDR(args));
  if (!is_kind_of(arg, consts.cl.list))  error(ERR_INVALID_ARG, arg);

  m_method_call_new(arg);
}

void
cm_method_call_eval(unsigned argc, obj_t args)
{
  obj_t    recvr;
  unsigned n, nargc, s, quotef = 0;
  obj_t    li, p, *q;
  char     *r;
  
  if (argc != 1)                                  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.method_call))  error(ERR_INVALID_ARG, recvr);

  /* Scan to calculate length of selector and number of args */
  
  li = METHOD_CALL(recvr)->list;
  for (s = 0, n = 0, p = li; p; p = CDR(p), ++n) {
    if (n & 1) {
      ASSERT(is_kind_of(CAR(p), consts.cl.string));

      s += string_len(CAR(p));
    }
  }
  ++s;
  
  /* Verify well-formedness, and check for quote */

  if (n == 2) {
    nargc = 1;
    
    quotef = string_equal(CAR(CDR(li)), consts.str.quote);
  } else if (n >= 3) {
    ASSERT((n & 1) == 1);
    
    nargc = 1 + (n >> 1);
  } else  ASSERT(0);

  /* Concatenate selector, and build arg list */

  vm_pushm(1, 2);

  vm_assign(1, NIL);
  vm_inst_alloc(consts.cl.string);
  inst_init(R0, s);
  vm_assign(2, R0);
  for (q = &R1, r = STRING(R2)->data, n = 0, p = li; p; p = CDR(p), ++n) {
    if (n & 1) {
      s = string_len(CAR(p));
      memcpy(r, STRING(CAR(p))->data, s);
      r += s;
      continue;
    }
    
    vm_assign(0, CAR(p));
    if (!quotef)  m_method_call_1(consts.str.eval, R0);
    m_cons(R0, NIL);
    OBJ_ASSIGN(*q, R0);
    q = &CDR(R0);
  }
  *r = 0;
    
  /* Call method */

  FRAME_METHOD_CALL_BEGIN(R2, R1) {

    m_method_call((struct frame_method_call *) frp, R2, nargc, R1);

  } FRAME_METHOD_CALL_END;
  
  vm_popm(1, 2);
}

void
cm_method_call_tostring(unsigned argc, obj_t args)
{
  obj_t recvr;

  if (argc != 1)                                  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.method_call))  error(ERR_INVALID_ARG, recvr);

  m_list_tostr(METHOD_CALL(recvr)->list, "[]");
}

/***************************************************************************/

/* Class: Block */

void
inst_init_block(obj_t cl, obj_t inst, va_list ap)
{
  OBJ_ASSIGN(BLOCK(inst)->list, va_arg(ap, obj_t));

  inst_init_parent(cl, inst, ap);
}

void
inst_walk_block(obj_t cl, obj_t inst, void (*func)(obj_t))
{
  (*func)(BLOCK(inst)->list);

  inst_walk_parent(cl, inst, func);
}

void
inst_free_block(obj_t cl, obj_t inst)
{
  inst_free_parent(cl, inst);
}

void
m_block_new(obj_t li)
{
  vm_push(0);

  vm_inst_alloc(consts.cl.block);
  inst_init(R0, li);

  vm_drop();
}

void
cm_block_new(unsigned argc, obj_t args)
{
  obj_t arg;

  if (argc != 2)  error(ERR_NUM_ARGS);
  arg = CAR(CDR(args));
  if (!is_kind_of(arg, consts.cl.list))  error(ERR_INVALID_ARG);

  m_block_new(arg);
}

void
cm_block_eval(unsigned argc, obj_t args)
{
  obj_t recvr, arg, li, p;

  if (argc != 2)                            error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, consts.cl.block))  error(ERR_INVALID_ARG);
  li = BLOCK(recvr)->list;
  if (li == NIL)                            error(ERR_BAD_FORM, recvr);
  p = CAR(li);
  if (!(p == NIL || is_kind_of(p, consts.cl.list)))      error(ERR_BAD_FORM, recvr);
  arg = CAR(CDR(args));
  if (!(arg == NIL || is_kind_of(arg, consts.cl.list)))  error(ERR_INVALID_ARG);
  if (list_len(p) != list_len(arg))         error(ERR_NUM_ARGS);

  m_string_dict_new(16);
  for ( ; p; p = CDR(p), arg = CDR(arg)) {
    dict_at_put(R0, CAR(p), CAR(arg));
  }
  vm_push(0);
  
  FRAME_BLOCK_BEGIN(R0) {

    if (frame_jmp_code == 0) {
      vm_assign(0, NIL);
      for (p = CDR(li); p; p = CDR(p)) {
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

  m_list_tostr(BLOCK(recvr)->list, "{}");
}

/***************************************************************************/

/* Class: Array */

void
inst_init_array(obj_t cl, obj_t inst, va_list ap)
{
  ARRAY(inst)->size = va_arg(ap, unsigned);
  ARRAY(inst)->data = _zcmalloc(ARRAY(inst)->size * sizeof(obj_t));

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
  _cfree(ARRAY(inst)->size, ARRAY(inst)->data);

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
dict_const_chk(obj_t key)
{
  if (inst_of(key) == consts.cl.string
      && STRING(key)->size > 0
      && STRING(key)->data[0] == '#'
      ) {
    error(ERR_CONST);
  }
}

void
dict_at_put(obj_t dict, obj_t key, obj_t val)
{
  obj_t p, *pp;
  
  if (p = dict_find(dict, key, &pp)) {
    dict_const_chk(key);
    
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

  if (argc != 1)  error(ERR_NUM_ARGS);
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
  (*func)(MODULE(inst)->name);
  (*func)(MODULE(inst)->parent);

  inst_walk_parent(cl, inst, func);
}

void
inst_free_module(obj_t cl, obj_t inst)
{
  inst_free_parent(cl, inst);
}

void
m_module_new(obj_t name, obj_t parent)
{
  vm_push(0);

  vm_inst_alloc(consts.cl.module);
  inst_init(R0, name, parent, string_hash, string_equal, 32);

  vm_drop();
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

FILE *
m_file_stdin(void)
{
  FILE *result;

  vm_push(0);

  m_method_call_1(consts.str._stdin, consts.cl.file);
  if (!is_kind_of(R0, consts.cl.file))  error(ERR_INVALID_VALUE_2, "stdout", R0);
  result = _FILE(R0)->fp;

  vm_pop(0);

  return (result);
} 

FILE *
m_file_stdout(void)
{
  FILE *result;

  vm_push(0);

  m_method_call_1(consts.str._stdout, consts.cl.file);
  if (!is_kind_of(R0, consts.cl.file))  error(ERR_INVALID_VALUE_2, "stdout", R0);
  result = _FILE(R0)->fp;

  vm_pop(0);

  return (result);
} 
 
FILE *
m_file_stderr(void)
{
  FILE *result;

  vm_push(0);

  m_method_call_1(consts.str._stderr, consts.cl.file);
  if (!is_kind_of(R0, consts.cl.file))  error(ERR_INVALID_VALUE_2, "stderr", R0);
  result = _FILE(R0)->fp;

  vm_pop(0);

  return (result);
} 

/***************************************************************************/

/* Class: Environment */

obj_t
env_at(obj_t s)
{
  struct frame *p;
  obj_t        r;

  for (p = frp; p; p = p->prev) {
    switch (p->type) {
    case FRAME_TYPE_BLOCK:
      if (r = dict_at(((struct frame_block *) p)->dict, s)) {
	return (CDR(r));
      }
      break;
    case FRAME_TYPE_MODULE:
      if (r = dict_at(OBJ(MODULE(((struct frame_module *) p)->module)->base), s)) {
	return (CDR(r));
      }
      break;
    default:
      ;
    }
  }

  error(ERR_NOT_BOUND, s);

  return (NIL);
}

obj_t
env_top_dict(void)
{
  struct frame *p;

  for (p = frp; p; p = p->prev) {
    switch (p->type) {
    case FRAME_TYPE_BLOCK:
      return (((struct frame_block *) p)->dict);
    case FRAME_TYPE_MODULE:
      return (OBJ(MODULE(((struct frame_module *) p)->module)->base));
    default:
      ;
    }
  }

  return (NIL);
}

void
env_new_put(obj_t s, obj_t val)
{
  dict_at_put(env_top_dict(), s, val);
}

void
env_at_put(obj_t s, obj_t val)
{
  struct frame *p;
  obj_t        dd = NIL, d, q;

  for (p = frp; p; p = p->prev) {
    switch (p->type) {
    case FRAME_TYPE_BLOCK:
      d = ((struct frame_block *) p)->dict;
      break;
    case FRAME_TYPE_MODULE:
      d = OBJ(MODULE(((struct frame_module *) p)->module)->base);
      break;
    default:
      continue;
    }

    if (dd == NIL)  dd = d;
    if (q = dict_find(d, s, 0)) {
      OBJ_ASSIGN(CDR(CAR(q)), val);
      return;
    }
  }

  HARD_ASSERT(dd != NIL);

  dict_at_put(dd, s, val);
}

void
env_del(obj_t s)
{
  dict_del(env_top_dict(), s);
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
cm_system_exit(unsigned argc, obj_t args)
{
  exit(0);
}

/***************************************************************************/

void
bt_print(void)
{
  struct frame *p;

  for (p = frp; p; p = p->prev) {
    struct frame_method_call *q;

    if (p->type != FRAME_TYPE_METHOD_CALL)  continue;

    q = (struct frame_method_call *) p;

    if (q->cl) {
      m_method_call_1(consts.str.print, q->cl);
      putchar('.');
    }
    m_method_call_1(consts.str.print, q->sel);
    m_method_call_1(consts.str.print, q->args);
    putchar('\n');
  }
}

void
error(unsigned errcode, ...)
{
  static unsigned lvl;

  va_list ap;

  if (++lvl > 1)  fatal(FATAL_DOUBLE_ERR);

  HARD_ASSERT(errcode < ARRAY_SIZE(error_msgs));

  va_start(ap, errcode);

  fprintf(stderr, "%s\n", error_msgs[errcode]);

  bt_print();

  va_end(ap);
  
  --lvl;

  frame_jmp(FRAME_TYPE_RESTART, 1);
}

/***************************************************************************/

const struct {
  obj_t    *cl, *name, *parent;
  unsigned inst_size;
  void     (*inst_init)(obj_t cl, obj_t inst, va_list ap);
  void     (*inst_walk)(obj_t cl, obj_t inst, void (*func)(obj_t));
  void     (*inst_free)(obj_t cl, obj_t inst);
} init_cl_tbl[] = {
  { &consts.cl.metaclass,
    &consts.str.metaclass,
    &consts.cl.object,
    sizeof(struct inst_metaclass),
    inst_init_object,
    inst_walk_object,
    inst_free_object
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
  { &consts.cl.module,
    &consts.str.module,
    &consts.cl.dict,
    sizeof(struct inst_module),
    inst_init_module,
    inst_walk_module,
    inst_free_module
  },
  { &consts.cl.file,
    &consts.str.file,
    &consts.cl.object,
    sizeof(struct inst_file),
    inst_init_file,
    inst_walk_file,
    inst_free_file
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

const struct {
  obj_t *obj;
  char  *str;
} init_str_tbl[] = {
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
    { &consts.str.andc,        "and:" },
    { &consts.str.appendc,     "append:" },
    { &consts.str.asc,         "asc" },
    { &consts.str.atc,         "at:" },
    { &consts.str.atc_lengthc, "at:length:" },
    { &consts.str.atc_putc,    "at:put:" },
    { &consts.str._break,      "break" },
    { &consts.str.car,         "car" },
    { &consts.str.cdr,         "cdr" },
    { &consts.str.chr,         "chr" },
    { &consts.str.class_methods, "class-methods" },
    { &consts.str.class_variables, "class-variables" },
    { &consts.str._continue,   "continue" },
    { &consts.str.deletec,     "delete:" },
    { &consts.str.divc,        "div:" },
    { &consts.str.equalsc,     "equals:" },
    { &consts.str.eval,        "eval" },
    { &consts.str.evalc,       "eval:" },
    { &consts.str.exit,        "exit" },
    { &consts.str.exitc,       "exit:" },
    { &consts.str._false,       "#false" },
    { &consts.str.filterc,     "filter:" },
    { &consts.str.foreachc,    "foreach:" },
    { &consts.str.gec,         "ge:" },
    { &consts.str.gtc,         "gt:" },
    { &consts.str.hash,        "hash" },
    { &consts.str.ifc,         "if:" },
    { &consts.str.ifc_elsec,   "if:else:" },
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
    { &consts.str.quote,       "quote" },
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
    { &consts.str.whilec,      "while:" },
    { &consts.str.xorc,        "xor:" }
#ifndef NDEBUG
    ,
    { &consts.str.assert,      "assert" },
    { &consts.str.collect,     "collect" },
    { &consts.str.debugc,      "debug:" }
#endif
};

const struct {
  obj_t *cl, *sel;
  void  (*func)(unsigned, obj_t);
} init_cl_method_tbl[] = {
  { &consts.cl.metaclass, &consts.str.name,                   cm_metaclass_name },
  { &consts.cl.metaclass, &consts.str.tostring,               cm_metaclass_name },
  { &consts.cl.metaclass, &consts.str.parent,                 cm_metaclass_parent },
  { &consts.cl.metaclass, &consts.str.class_methods,          cm_metaclass_cl_methods },
  { &consts.cl.metaclass, &consts.str.class_variables,        cm_metaclass_cl_vars },
  { &consts.cl.metaclass, &consts.str.instance_methods,       cm_metaclass_inst_methods },
  { &consts.cl.metaclass, &consts.str.instance_variables,     cm_metaclass_inst_vars },
  { &consts.cl.metaclass, &consts.str.newc_parentc_instance_variablesc, cm_class_new },

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

  { &consts.cl.method_call, &consts.str.newc, cm_method_call_new },

  { &consts.cl.array, &consts.str.newc, cm_array_new },

  { &consts.cl.dict, &consts.str.new, cm_dict_new },

  { &consts.cl.env, &consts.str.newc,      cm_env_new },
  { &consts.cl.env, &consts.str.newc_putc, cm_env_new_put },
  { &consts.cl.env, &consts.str.atc,       cm_env_at },
  { &consts.cl.env, &consts.str.atc_putc,  cm_env_at_put },
  { &consts.cl.env, &consts.str.deletec,   cm_env_del }
}, init_inst_method_tbl[] = {
  { &consts.cl.metaclass, &consts.str.name,               cm_class_name },
  { &consts.cl.metaclass, &consts.str.tostring,           cm_class_name },
  { &consts.cl.metaclass, &consts.str.parent,             cm_class_parent },
  { &consts.cl.metaclass, &consts.str.class_methods,      cm_class_cl_methods },
  { &consts.cl.metaclass, &consts.str.class_variables,    cm_class_cl_vars },
  { &consts.cl.metaclass, &consts.str.instance_methods,   cm_class_inst_methods },
  { &consts.cl.metaclass, &consts.str.instance_variables, cm_class_inst_vars },

  { &consts.cl.object, &consts.str.quote,       cm_object_quote },
  { &consts.cl.object, &consts.str.eval,        cm_object_eval },
  { &consts.cl.object, &consts.str.instance_of, cm_object_instof },
  { &consts.cl.object, &consts.str.equalsc,     cm_object_eq },
  { &consts.cl.object, &consts.str.tostring,    cm_object_tostring },
  { &consts.cl.object, &consts.str.print,       cm_object_print },
  { &consts.cl.object, &consts.str.atc,         cm_object_at },
  { &consts.cl.object, &consts.str.atc_putc,    cm_object_at_put },

  { &consts.cl.code_method, &consts.str.evalc, cm_code_method_eval },

  { &consts.cl.boolean, &consts.str.andc,      cm_boolean_and },
  { &consts.cl.boolean, &consts.str.orc,       cm_boolean_or },
  { &consts.cl.boolean, &consts.str.xorc,      cm_boolean_xor },
  { &consts.cl.boolean, &consts.str.not,       cm_boolean_not },
  { &consts.cl.boolean, &consts.str.tostring,  cm_boolean_tostring },
  { &consts.cl.boolean, &consts.str.equalsc,   cm_boolean_equals },
  { &consts.cl.boolean, &consts.str.ifc,       cm_boolean_if },
  { &consts.cl.boolean, &consts.str.ifc_elsec, cm_boolean_if_else },
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
  { &consts.cl.integer, &consts.str.tostring,     cm_integer_tostring },
  { &consts.cl.integer, &consts.str.tostringc,    cm_integer_tostring_base },

  { &consts.cl._float, &consts.str.addc,     cm_float_add },
  { &consts.cl._float, &consts.str.subc,     cm_float_sub },
  { &consts.cl._float, &consts.str.multc,    cm_float_mult },
  { &consts.cl._float, &consts.str.divc,     cm_float_div },
  { &consts.cl._float, &consts.str.minus,    cm_float_minus },
  { &consts.cl._float, &consts.str.hash,     cm_float_hash },
  { &consts.cl._float, &consts.str.equalsc,  cm_float_equals },
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

  { &consts.cl.block, &consts.str.evalc,    cm_block_eval },
  { &consts.cl.block, &consts.str.tostring, cm_block_tostring },

  { &consts.cl.array, &consts.str.atc,      cm_array_at },
  { &consts.cl.array, &consts.str.atc_putc, cm_array_at_put },
  { &consts.cl.array, &consts.str.tostring, cm_array_tostring },
  
  { &consts.cl.dict, &consts.str.atc,      cm_dict_at },
  { &consts.cl.dict, &consts.str.atc_putc, cm_dict_at_put },
  { &consts.cl.dict, &consts.str.deletec,  cm_dict_del },
  { &consts.cl.dict, &consts.str.keys,     cm_dict_keys },
  { &consts.cl.dict, &consts.str.tostring, cm_dict_tostring }
};


void
init(void)
{
  unsigned i;

  initf = 1;

  /* Step 0. Init memory management */

  stack = _cmalloc(STACK_SIZE * sizeof(obj_t));
  sp = stack_end = stack + STACK_SIZE;

  for (i = 0; i < ARRAY_SIZE(obj_list); ++i)  list_init(&obj_list[i]);

  /* Step 1. Create metaclass */

  OBJ_ASSIGN(consts.cl.metaclass, _obj_alloc(sizeof(struct inst_metaclass)));
  CLASS(consts.cl.metaclass)->inst_size = sizeof(struct inst_metaclass);
  CLASS(consts.cl.metaclass)->inst_init = inst_init_class;
  CLASS(consts.cl.metaclass)->inst_walk = inst_walk_class;
  CLASS(consts.cl.metaclass)->inst_free = inst_free_class;
  
  /* Step 2. Create classes, first pass */

  for (i = 1; i < ARRAY_SIZE(init_cl_tbl); ++i) {
    vm_inst_alloc(consts.cl.metaclass);
    CLASS(R0)->inst_size = init_cl_tbl[i].inst_size;
    CLASS(R0)->inst_init = init_cl_tbl[i].inst_init;
    CLASS(R0)->inst_walk = init_cl_tbl[i].inst_walk;
    CLASS(R0)->inst_free = init_cl_tbl[i].inst_free;
    OBJ_ASSIGN(*init_cl_tbl[i].cl, R0);
  }

  /* Step 3. Fix up class hierarchy */

  for (i = 0; i < ARRAY_SIZE(init_cl_tbl); ++i) {
    OBJ_ASSIGN(CLASS(*init_cl_tbl[i].cl)->parent,
	       init_cl_tbl[i].parent ? *init_cl_tbl[i].parent : NIL
	       );
  }

  /* Step 4. Create constants */

  vm_inst_alloc(consts.cl.boolean);
  inst_init(R0, 0);
  OBJ_ASSIGN(consts._bool._false, R0);
  vm_inst_alloc(consts.cl.boolean);
  inst_init(R0, 1);
  OBJ_ASSIGN(consts._bool._true, R0);
  
  for (i = 0; i < ARRAY_SIZE(init_str_tbl); ++i) {
    m_string_new(1, strlen(init_str_tbl[i].str), init_str_tbl[i].str);
    OBJ_ASSIGN(*init_str_tbl[i].obj, R0);
  }

  /* Step 5. Create classes, second pass */

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

  for (i = 0; i < ARRAY_SIZE(init_cl_method_tbl); ++i) {
    m_code_method_new(init_cl_method_tbl[i].func);
    dict_at_put(CLASS(*init_cl_method_tbl[i].cl)->cl_methods,
		*init_cl_method_tbl[i].sel,
		R0
		);
  }

  for (i = 0; i < ARRAY_SIZE(init_inst_method_tbl); ++i) {
    m_code_method_new(init_inst_method_tbl[i].func);
    dict_at_put(CLASS(*init_inst_method_tbl[i].cl)->inst_methods,
		*init_inst_method_tbl[i].sel,
		R0
		);
  }

  /* Step 6. Create main module */

  m_module_new(consts.str.main, NIL);
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

  module_cur = module_main;

  /* Step 7. Fix up class module membership */

  for (i = 0; i < ARRAY_SIZE(init_cl_tbl); ++i) {
    OBJ_ASSIGN(CLASS(*init_cl_tbl[i].cl)->module, module_main);
  }

  initf = 0;
}

extern int yyparse();

void
interactive(void)
{
  FRAME_RESTART_BEGIN {
    frame_jmp_code = frame_jmp_code; /* Suppress unused variable warning */
    
    for (;;) {
      printf("\nok ");
      fflush(stdout);
      
      if (yyparse() != 0)  break;
 
      vm_dropn(stack_end - sp); /* Discard all objs pushed during parsing */

      m_method_call_1(consts.str.eval, R0);
      m_method_call_1(consts.str.print, R0);
    }
  } FRAME_RESTART_END;
}


void
batch(void)
{
  FRAME_RESTART_BEGIN {
    if (frame_jmp_code != 0)  return;
  } FRAME_RESTART_END;
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

    if (argc > 1)  batch();  else  interactive();

  } FRAME_MODULE_END;

#ifndef NDEBUG
  collect();			/* Check consistency */

  stats_print();
#endif

  return (0);
}
