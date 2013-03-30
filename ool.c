#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <setjmp.h>
#include <assert.h>

#ifndef NDEBUG
#define ASSERT(x)  assert(x)
#else
#define ASSERT(x)
#endif
#define HARD_ASSERT  assert

#define ARRAY_SIZE(a)  (sizeof(a) / sizeof((a)[0]))
#if 1
#define PTR_TO_INT(x)  ((long)(x))
#else
#define PTR_TO_INT(x)  ((long long)(x))
#endif
#define FIELD_OFS(s, f)                   PTR_TO_INT(&((s *) 0)->f)
#define FIELD_PTR_TO_STRUCT_PTR(p, s, f)  ((s *)((char *)(p) - FIELD_OFS(s, f)))

struct list {
  struct list *prev, *next;
};

struct obj;
typedef struct obj *obj_t;
#define OBJ(x)  ((obj_t)(x))
#define NIL     OBJ(0)

struct obj {
  struct list list_node[1];
  unsigned    ref_cnt;
#ifndef NDEBUG
  unsigned    old_ref_cnt;
#endif
  obj_t       inst_of;
};
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
void m_class_new(obj_t name, obj_t parent, obj_t module);
unsigned is_subclass_of(obj_t cl1, obj_t cl2);

struct inst_code_method {
  struct obj base[1];
  void       (*func)(unsigned argc, obj_t args);
};
#define CODE_METHOD(x)  ((struct inst_code_method *)(x))
void m_code_method_new(void (*func)(unsigned, obj_t));

struct inst_boolean {
  struct obj base[1];
  unsigned   val;
};
#define BOOLEAN(x)  ((struct inst_boolean *)(x))

typedef long long int_val_t;
struct inst_integer {
  struct obj base[1];
  int_val_t  val;
};
#define INTEGER(x)  ((struct inst_integer *)(x))
void m_integer_new(int_val_t val);

typedef long double float_val_t;
struct inst_float {
  struct obj  base[1];
  float_val_t val;
};
#define FLOAT(x)  ((struct inst_float *)(x))
void m_float_new(float_val_t val);

struct inst_string {
  struct obj base[1];
  unsigned   size;		/* N.B. Includes the '\0' terminator */
  char       *data;
};
#define STRING(x)  ((struct inst_string *)(x))
void m_string_new(unsigned n, ...);

struct inst_dptr {
  struct obj base[1];
  obj_t      car, cdr;
};
#define DPTR(x)  ((struct inst_dptr *)(x))
#define CAR(x)   (DPTR(x)->car)
#define CDR(x)   (DPTR(x)->cdr)
void m_pair_new(obj_t car, obj_t cdr);
void m_cons(obj_t car, obj_t cdr);

struct inst_method_call {
  struct obj base[1];
  obj_t      list;
};
#define METHOD_CALL(x)  ((struct inst_method_call *)(x))
void m_method_call_new(obj_t list);

struct inst_block {
  struct obj base[1];
  obj_t      list;
};
#define BLOCK(x)  ((struct inst_block *)(x))
void m_block_new(obj_t list);

struct inst_array {
  struct obj base[1];
  unsigned   size;
  obj_t      *data;
};
#define ARRAY(x)  ((struct inst_array *)(x))
void m_array_new(unsigned size);
obj_t array_at(obj_t arr, int_val_t idx);
void  array_at_put(obj_t arr, int_val_t idx, obj_t val);

struct inst_dict {
  struct inst_array base[1];
  unsigned (*key_hash)(obj_t key);
  unsigned (*key_equal)(obj_t key1, obj_t key2);
};
#define DICT(x)  ((struct inst_dict *)(x))
void m_string_dict_new(unsigned size);
void m_dict_new(unsigned size);
obj_t dict_at(obj_t dict, obj_t key);
void  dict_at_put(obj_t dict, obj_t key, obj_t val);
void  dict_del(obj_t dict, obj_t key);

struct inst_module {
  struct inst_dict base[1];
  obj_t            name, parent;
};
#define MODULE(x)  ((struct inst_module *)(x))
void m_module_new(obj_t name, obj_t parent);

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
  - inst_of(Object) = Metaclass, parent(Metaclass) = Object
*/

/* The root set */
obj_t *stack, *stack_end;
#define STACK_SIZE  16384	/* In objects */
obj_t regs[8];
#define REG(i)  (regs[i])
#define R0      REG(0)
#define R1      REG(1)
#define R2      REG(2)
#define R3      REG(3)
#define R4      REG(4)
#define R5      REG(5)
#define R6      REG(6)
#define R7      REG(7)
obj_t module_main;
struct {
  struct {
    obj_t _false, _true;
  } boolean;
  struct {
    obj_t addc, array, atc, atc_putc;
    obj_t block, boolean;
    obj_t code_method;
    obj_t dictionary, dptr;
    obj_t equalc, eval, evalc;
    obj_t _false, _float;
    obj_t hash;
    obj_t integer;
    obj_t list;
    obj_t main, metaclass, method_call, module;
    obj_t new, newc, newc_parentc_instvarsc;
    obj_t object;
    obj_t pair, print;
    obj_t quote;
    obj_t string;
    obj_t tostring, _true;
  } str;
  struct {
    obj_t metaclass, object, code_method, boolean, integer, _float, string;
    obj_t dptr, pair, list, method_call, block, array, dict, module;
  } cl;
} consts;
obj_t *sp, module_cur;

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

void vm_inst_alloc(obj_t cl);
void vm_assign(unsigned dst, obj_t val);
void vm_pushl(obj_t obj), vm_push(unsigned src), vm_pushn(unsigned src, unsigned n);
void vm_pop(unsigned dst), vm_popn(unsigned dst, unsigned n);
void vm_drop(void), vm_dropn(unsigned n);

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
hash(unsigned char *buf, unsigned n)
{
  for ( ; n; --n, ++buf)  hash_val = 37 * hash_val + *buf;

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
  ERR_INVALID_METHOD,
  ERR_NO_METHOD
};

const char * const error_msgs[] = {
  "Stack overflow",
  "Incorrect number of arguments",
  "Invalid argument",
};

void error(unsigned errcode);

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

void inst_walk_metametaclass(obj_t inst, void (*func)(obj_t));

void
inst_walk(obj_t inst, void (*func)(obj_t))
{
  obj_t cl = inst_of(inst);

  if (cl == NIL) {
    inst_walk_metametaclass(inst, func);

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
  if (obj == NIL)  return;

  HARD_ASSERT(obj->ref_cnt != 0);

  if (--obj->ref_cnt != 0)  return;

  inst_free(obj);

  obj_release(inst_of(obj));
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
collect(void)
{
  struct list *p, *e;
  obj_t       *q, r;
  unsigned    n;

#ifndef NDEBUG
  ++stats.mem.collect_cnt;
#endif

  for (e = LIST_END(OBJ_LIST_ACTIVE), p = LIST_FIRST(OBJ_LIST_ACTIVE);
       p != e;
       p = LIST_NEXT(p)
       ) {
    r = FIELD_PTR_TO_STRUCT_PTR(p, struct obj, list_node);

#ifndef NDEBUG
    r->old_ref_cnt = r->ref_cnt;
#endif

    r->ref_cnt = 0;
  }

  for (q = regs, n = ARRAY_SIZE(regs); n; --n, ++q)  obj_mark(*q);
  for (q = sp; q < stack_end; ++q)  obj_mark(*q);
  obj_mark(module_main);
  for (q = (obj_t *) &consts, n = sizeof(consts) / sizeof(obj_t);
       n;
       --n, ++q
       ) {
    obj_mark(*q);
  }

  for (e = LIST_END(OBJ_LIST_ACTIVE); (p = LIST_FIRST(OBJ_LIST_ACTIVE)) != e; ) {
    r = FIELD_PTR_TO_STRUCT_PTR(p, struct obj, list_node);
    
#ifndef NDEBUG
    ++stats.mem.collected_cnt;
    stats.mem.bytes_collected = CLASS(inst_of(r))->inst_size;
#endif

    obj_free(r);
  }
  
  n = obj_list_idx_active;
  obj_list_idx_active = obj_list_idx_marked;
  obj_list_idx_marked = n;

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

#define HARD_ASSERT_REG_VALID(r)  (HARD_ASSERT((r) < ARRAY_SIZE(regs)))

void
vm_assign(unsigned dst, obj_t val)
{
  HARD_ASSERT_REG_VALID(dst);

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
  HARD_ASSERT_REG_VALID(src);

  vm_pushl(REG(src));
}

void
vm_pushn(unsigned src, unsigned n)
{
  obj_t *p;

  HARD_ASSERT_REG_VALID(src + n - 1);
  VM_STACK_CHECK_UP(n);

  for (p = &REG(src); n; --n, ++p)  *--sp = obj_retain(*p);
}

void
vm_pop(unsigned dst)
{
  HARD_ASSERT_REG_VALID(dst);
  VM_STACK_CHECK_DOWN(1);

  _obj_assign(&REG(dst), *sp);
  ++sp;
}

void
vm_popn(unsigned dst, unsigned n)
{
  obj_t *p;

  HARD_ASSERT_REG_VALID(dst + n - 1);
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
  FRAME_METHOD_CALL
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
  do { frp = frp->prev;  vm_dropn(frp->sp - sp); } while (0)

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
  frp = __frame->base.prev;			\
  }

void
frame_jmp(struct frame *p, int frame_jmp_code)
{
  unsigned char f;

  while (frp) {
    if (frp == p)  longjmp(((struct frame_jmp *) frp)->jmp_buf, frame_jmp_code);
    FRAME_POP;
  }

  HARD_ASSERT(0);
}

/***************************************************************************/

void m_method_call_1(obj_t recvr, obj_t sel);
void m_method_call_2(obj_t recvr, obj_t sel, obj_t arg);
void m_method_call_3(obj_t recvr, obj_t sel, obj_t arg1, obj_t arg2);

void
method_run(obj_t func, unsigned argc, obj_t args)
{
  obj_t cl = inst_of(func);

  if (cl == consts.cl.code_method) {
    (*CODE_METHOD(func)->func)(argc, args);
    return;
  }
  if (cl == consts.cl.block) {
    m_method_call_2(func, consts.str.evalc, args);
    return;
  }

  error(ERR_INVALID_METHOD);
}

void
_method_call(obj_t sel, unsigned argc, obj_t args)
{
  obj_t recvr = CAR(args);
  obj_t cl, p;

  vm_pushl(sel);
  vm_pushl(args);

  cl = inst_of(recvr);
  if (cl == consts.cl.metaclass || cl == NIL) {
    for (cl = recvr; cl; cl = CLASS(cl)->parent) {
      if (p = dict_at(CLASS(cl)->cl_methods, sel)) {
	method_run(CDR(p), argc, args);
	goto done;
      }
    }
  }

  for (cl = inst_of(recvr); cl; cl = CLASS(cl)->parent) {
    if (p = dict_at(CLASS(cl)->inst_methods, sel)) {
      method_run(CDR(p), argc, args);
      goto done;
    }
  }

  error(ERR_NO_METHOD);

 done:
  vm_dropn(2);
}

void
m_method_call(obj_t sel, unsigned argc, obj_t args)
{
  vm_push(0);

  _method_call(sel, argc, args);

  vm_dropn(3);
}

void
m_method_call_1(obj_t recvr, obj_t sel)
{
  vm_push(0);

  m_cons(recvr, NIL);
  _method_call(sel, 1, R0);

  vm_drop();
}

void
m_method_call_2(obj_t recvr, obj_t sel, obj_t arg)
{
  vm_push(0);

  m_cons(arg, NIL);
  m_cons(recvr, R0);
  _method_call(sel, 2, R0);

  vm_drop();
}

void
m_method_call_3(obj_t recvr, obj_t sel, obj_t arg1, obj_t arg2)
{
  vm_push(0);

  m_cons(arg2, NIL);
  m_cons(arg1, R0);
  m_cons(recvr, R0);
  _method_call(sel, 3, R0);

  vm_drop();
}

/***************************************************************************/

unsigned list_len(obj_t li);

/***************************************************************************/

/* Metaclass itself */

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
inst_walk_metametaclass(obj_t inst, void (*func)(obj_t))
{
  _inst_walk_metaclass(inst, func);

  (*func)(inst_of(inst));
}

/***************************************************************************/

/* Class: Metaclass (i.e. classes) */

unsigned
is_subclass_of(obj_t cl1, obj_t cl2)
{
  for (; cl1; cl1 = CLASS(cl1)->parent) {
    if (cl1 == cl2)  return (1);
  }

  return (0);
}

void
inst_init_metaclass(obj_t cl, obj_t inst, va_list ap)
{
  OBJ_ASSIGN(CLASS(inst)->name,   va_arg(ap, obj_t));
  OBJ_ASSIGN(CLASS(inst)->parent, va_arg(ap, obj_t));
  OBJ_ASSIGN(CLASS(inst)->module, va_arg(ap, obj_t));

  inst_init_parent(cl, inst, ap);
}

void
inst_walk_metaclass(obj_t cl, obj_t inst, void (*func)(obj_t))
{
  _inst_walk_metaclass(inst, func);

  inst_walk_parent(cl, inst, func);
}

void
inst_free_metaclass(obj_t cl, obj_t inst)
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
  for (p = (obj_t *)((unsigned char *) inst + ofs); ofs < CLASS(inst)->inst_size; ofs += sizeof(obj_t)) {
    (*func)(*p);
  }

  inst_walk_parent(cl, inst, func);
}

void
inst_free_user(obj_t cl, obj_t inst)
{
  inst_free_parent(cl, inst);
}

void
cm_class_new(unsigned argc, obj_t args)
{
  obj_t    name, parent, inst_vars;
  unsigned ofs;

  vm_push(1);

  name = CAR(args);    args = CDR(args);
  parent = CAR(args);  args = CDR(args);
  inst_vars = CAR(args);

  m_class_new(name, parent, module_cur);
  CLASS(R0)->inst_size = CLASS(parent)->inst_size + list_len(inst_vars) * sizeof(obj_t);
  CLASS(R0)->inst_init = inst_init_user;
  CLASS(R0)->inst_walk = inst_walk_user;
  CLASS(R0)->inst_free = inst_free_user;

  vm_assign(1, R0);
  for (ofs = CLASS(parent)->inst_size; inst_vars; inst_vars = CDR(inst_vars), ofs += sizeof(obj_t)) {
    m_integer_new(ofs);
    dict_at_put(CLASS(R1)->inst_vars, CAR(inst_vars), R0);
  }
  vm_assign(0, R1);

  dict_at_put(OBJ(MODULE(module_cur)->base), name, R0);

  vm_pop(1);
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
cm_object_eval(unsigned argc, obj_t args)
{
  vm_assign(0, CAR(args));
}

void
cm_object_quote(unsigned argc, obj_t args)
{
  vm_assign(0, CAR(args));
}

void
cm_object_inst_of(unsigned argc, obj_t args)
{
  vm_assign(0, inst_of(CAR(args)));
}

void
cm_object_tostring(unsigned argc, obj_t args)
{
  obj_t recvr = CAR(args);
  obj_t cl    = inst_of(recvr);
  char  buf[64];

  if (cl == 0) {
  }

  snprintf(buf, sizeof(buf), "<instance of %s @ %p>",
	   STRING(CLASS(cl)->name)->data,
	   recvr
	   );
  m_string_new(1, strlen(buf) + 1, buf);
}

void
cm_object_print(unsigned argc, obj_t args)
{
  obj_t recvr = CAR(args);

  m_method_call_1(CAR(args), consts.str.tostring);
  m_method_call_1(R0, consts.str.print);

  vm_assign(0, recvr);
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
  vm_inst_alloc(consts.cl.boolean);
  inst_init(R0, val);
}

void
cm_boolean_new(unsigned argc, obj_t args)
{
  
}

/***************************************************************************/

/* Class: Integer */

void
inst_init_integer(obj_t cl, obj_t inst, va_list ap)
{
  INTEGER(inst)->val = va_arg(ap, int_val_t);

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
m_integer_new(int_val_t val)
{
  vm_inst_alloc(consts.cl.integer);
  inst_init(R0, val);
}

void
cm_integer_new(unsigned argc, obj_t args)
{
  
}

void
cm_integer_add(unsigned argc, obj_t args)
{
  m_integer_new(INTEGER(CAR(args))->val + INTEGER(CAR(CDR(args)))->val);
}

void
cm_integer_tostring(unsigned argc, obj_t args)
{
  obj_t recvr = CAR(args);
  char  buf[32];

  m_string_new(1, snprintf(buf, sizeof(buf), "%lld", INTEGER(recvr)->val) + 1, buf);
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
    if (j > 1)  size += j - 1;
  }
  if (size > 0)  ++size;
  va_end(ap);
  
  vm_inst_alloc(consts.cl.string);
  inst_init(R0, size);

  va_start(ap, n);
  for (q = STRING(R0)->data, k = n; k; --k) {
    j = va_arg(ap, unsigned);
    p = va_arg(ap, char *);

    if (j > 1)  --j;  else  j = 0;
    memcpy(q, p, j);
    q += j;
  }
  *q = 0;
  va_end(ap);

  vm_drop();
}

unsigned
string_len(obj_t s)
{
  unsigned result = STRING(s)->size;

  if (result > 1)  --result;

  return (result);
}

void
cm_string_len(unsigned argc, obj_t args)
{
  m_integer_new(string_len(CAR(args)));
}

unsigned
string_hash(obj_t s)
{
  hash_init();

  return (hash(STRING(s)->data, string_len(s)));
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
m_string_eval(obj_t s)
{
  struct frame *p;
  obj_t        q, r;

  for (p = frp; p; p = p->prev) {
    if (p->type != FRAME_TYPE_BLOCK)  continue;

    if (r = dict_at(((struct frame_block *) p)->dict, s)) {
      return (CDR(r));
    }
  }

  for (q = module_cur; q; q = MODULE(q)->parent) {
    if (r = dict_at(DICT(MODULE(q)->base), s)) {
      return (r);
    }
  }

  error(ERR_NOT_BOUND);
}

void
cm_string_print(unsigned argc, obj_t args)
{
  obj_t recvr = CAR(args);

  if (string_len(recvr) != 0)  fputs(STRING(recvr)->data, stdout);

  vm_assign(0, recvr);
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
  obj_release(CAR(inst));
  obj_release(CDR(inst));

  inst_free_parent(cl, inst);  
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
m_cons(obj_t car, obj_t cdr)
{
  vm_push(0);

  vm_inst_alloc(consts.cl.list);
  inst_init(R0, car, cdr);

  vm_drop();
}

unsigned
list_len(obj_t li)
{
  unsigned result;

  for (result = 0; li; li = CDR(li))  ++result;

  return (result);
}

void
m_eval(obj_t li)
{
  obj_t *p;

  vm_push(0);
  vm_push(1);

  vm_assign(1, NIL);
  for (p = &R1; li; li = CDR(li)) {
    m_method_call_1(CAR(li), consts.str.eval);
    m_cons(R0, NIL);
    OBJ_ASSIGN(*p, R0);
    p = &CDR(R0);
  }
  vm_assign(0, R1);

  vm_pop(1);
  vm_drop();
}

void
cm_list_eval(unsigned argc, obj_t args)
{
  m_eval(CAR(args));
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
  vm_inst_alloc(consts.cl.method_call);
  inst_init(R0, li);
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
  vm_inst_alloc(consts.cl.block);
  inst_init(R0, li);
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
dict_key_hash(obj_t obj)
{
  unsigned result;

  vm_push(0);

  m_method_call_1(obj, consts.str.hash);
  result = INTEGER(R0)->val;

  vm_pop(0);

  return (result);
}

unsigned
dict_key_equal(obj_t obj1, obj_t obj2)
{
  unsigned result;

  vm_push(0);

  m_method_call_2(obj1, consts.str.equalc, obj2);
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
dict_find(obj_t dict, obj_t key, obj_t **bucket)
{
  obj_t *b = &DICT(dict)->base->data[(*DICT(dict)->key_hash)(key) % DICT(dict)->base->size];
  obj_t p, q;
  unsigned (*f)(obj_t, obj_t) = DICT(dict)->key_equal;
  
  if (bucket)  *bucket = b;

  for (p = *b; p; p = CDR(p)) {
    q = CAR(p);
    if ((*f)(key, CAR(q)))  return (q);
  }

  return (NIL);
}

obj_t
dict_at(obj_t dict, obj_t key)
{
  return (dict_find(dict, key, 0));
}

void
dict_at_put(obj_t dict, obj_t key, obj_t val)
{
  obj_t p, *b;

  if (p = dict_find(dict, key, &b)) {
    OBJ_ASSIGN(CDR(p), val);

    return;
  }

  vm_push(0);
  
  m_pair_new(key, val);
  m_cons(R0, *b);
  OBJ_ASSIGN(*b, R0);
  
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
  vm_inst_alloc(consts.cl.module);
  inst_init(R0, name, parent, string_hash, string_equal, 32);
}

/***************************************************************************/

void
error(unsigned errcode)
{
}

/***************************************************************************/

struct {
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
    0,
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
  }
};

struct {
  obj_t *obj;
  char  *str;
} init_str_tbl[] = {
  { &consts.str.addc, "add:" },
  { &consts.str.array, "#Array" },
  { &consts.str.atc, "at:" },
  { &consts.str.atc_putc, "at:put:" },
  { &consts.str.block, "#Block" },
  { &consts.str.boolean, "#Boolean" },
  { &consts.str.code_method, "#Code_Method" },
  { &consts.str.dictionary, "#Dictionary" },
  { &consts.str.dptr, "#Dptr" },
  { &consts.str.equalc, "equal:" },
  { &consts.str.eval, "eval" },
  { &consts.str.evalc, "eval:" },
  { &consts.str._false, "#false" },
  { &consts.str._float, "#Float" },
  { &consts.str.hash, "hash" },
  { &consts.str.integer, "#Integer" },
  { &consts.str.list, "#List" },
  { &consts.str.main, "main" },
  { &consts.str.metaclass, "#Metaclass" },
  { &consts.str.method_call, "#Method_Call" },
  { &consts.str.module, "#Module" },
  { &consts.str.new, "new" },
  { &consts.str.newc, "new:" },
  { &consts.str.newc_parentc_instvarsc, "new:parent:instance-variables:" },
  { &consts.str.object, "#Object" },
  { &consts.str.pair, "#Pair" },
  { &consts.str.print, "print" },
  { &consts.str.quote, "quote" },
  { &consts.str.string, "#String" },
  { &consts.str.tostring, "tostring" },
  { &consts.str._true, "#true" }
};

struct {
  obj_t *cl, *sel;
  void  (*func)(unsigned, obj_t);
} init_cl_method_tbl[] = {
  { &consts.cl.metaclass, &consts.str.newc_parentc_instvarsc, cm_class_new },

  { &consts.cl.boolean, &consts.str.new,  cm_boolean_new },
  { &consts.cl.boolean, &consts.str.newc, cm_boolean_new },

  { &consts.cl.integer, &consts.str.new,  cm_integer_new },
  { &consts.cl.integer, &consts.str.newc, cm_integer_new }
}, init_inst_method_tbl[] = {
  { &consts.cl.object, &consts.str.tostring, cm_object_tostring },
  { &consts.cl.object, &consts.str.print,    cm_object_print },

  { &consts.cl.integer, &consts.str.addc,     cm_integer_add },
  { &consts.cl.integer, &consts.str.tostring, cm_integer_tostring },

  { &consts.cl.string, &consts.str.print, cm_string_print }
};


void
init(void)
{
  unsigned i;

  initf = 1;

  stack = _cmalloc(STACK_SIZE * sizeof(obj_t));
  sp = stack_end = stack + STACK_SIZE;

  for (i = 0; i < ARRAY_SIZE(obj_list); ++i)  list_init(&obj_list[i]);

  /* Step 1. Create metaclass */

  OBJ_ASSIGN(consts.cl.metaclass, _obj_alloc(sizeof(struct inst_metaclass)));
  CLASS(consts.cl.metaclass)->inst_size = sizeof(struct inst_metaclass);
  CLASS(consts.cl.metaclass)->inst_init = inst_init_metaclass;
  CLASS(consts.cl.metaclass)->inst_walk = inst_walk_metaclass;
  CLASS(consts.cl.metaclass)->inst_free = inst_free_metaclass;
  
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

  m_boolean_new(0);
  OBJ_ASSIGN(consts.boolean._false, R0);
  m_boolean_new(1);
  OBJ_ASSIGN(consts.boolean._true, R0);
  
  for (i = 0; i < ARRAY_SIZE(init_str_tbl); ++i) {
    m_string_new(1, strlen(init_str_tbl[i].str) + 1, init_str_tbl[i].str);
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
	      consts.str._true,
	      consts.boolean._true
	      );
  dict_at_put(OBJ(MODULE(module_main)->base),
	      consts.str._false,
	      consts.boolean._false
	      );

  for (i = 0; i < ARRAY_SIZE(init_cl_tbl); ++i) {
    dict_at_put(OBJ(MODULE(module_main)->base),
		*init_cl_tbl[i].name,
		*init_cl_tbl[i].cl
		);
  }

  /* Step 7. Fix up class module membership */

  for (i = 0; i < ARRAY_SIZE(init_cl_tbl); ++i) {
    OBJ_ASSIGN(CLASS(*init_cl_tbl[i].cl)->module, module_main);
  }

  initf = 0;
}


void
interactive(void)
{
  FRAME_RESTART_BEGIN {
    
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
main(int argc, char **argv)
{
  init();

#ifndef NDEBUG
  collect();			/* Check consistency */
  ASSERT(stats.mem.collected_cnt == 0);
  ASSERT(stats.mem.bytes_collected == 0);
#endif

  if (argc == 0)  interactive();  else  batch();

#ifndef NDEBUG
  collect();			/* Check consistency */

  stats_print();
#endif

  return (0);
}
