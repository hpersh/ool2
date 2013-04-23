#include <stdio.h>
#include <assert.h>

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

void *_cmalloc(unsigned size);
void _cfree(unsigned size, void *ptr);

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
  obj_t       inst_of;
};

obj_t inst_of(obj_t obj);
unsigned is_kind_of(obj_t obj, obj_t cl);
void _obj_assign(obj_t *dst, obj_t src);
obj_t obj_retain(obj_t obj);
#define OBJ_ASSIGN(dst, src)  (_obj_assign(&(dst), obj_retain(src)))

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
void m_class_new(obj_t    name,
		 obj_t    parent,
		 obj_t    module, 
		 unsigned inst_size,
		 void     (*inst_init)(obj_t cl, obj_t inst, va_list ap),
		 void     (*inst_walk)(obj_t cl, obj_t inst, void (*func)(obj_t)),
		 void     (*inst_free)(obj_t cl, obj_t inst)
		 );
void inst_walk_metaclass(obj_t inst, void (*func)(obj_t));
unsigned is_subclass_of(obj_t cl1, obj_t cl2);
void m_fqclname(obj_t cl);

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
void m_boolean_new(unsigned val);

typedef long long          integer_val_t;
typedef unsigned long long uinteger_val_t;
struct inst_integer {
  struct obj base[1];
  integer_val_t  val;
};
#define INTEGER(x)  ((struct inst_integer *)(x))
#define INTEGER_SCANF_FMT_DEC  "%lld"
#define INTEGER_SCANF_FMT_OCT  "%llo"
#define INTEGER_SCANF_FMT_HEX  "%llx"
#define INTEGER_PRINTF_FMT  "%lld"
void m_integer_new(integer_val_t val);

typedef long double float_val_t;
struct inst_float {
  struct obj  base[1];
  float_val_t val;
};
#define FLOAT(x)  ((struct inst_float *)(x))
#define FLOAT_SCANF_FMT  "%Lf"
#define FLOAT_PRINTF_FMT  "%Lg"
void m_float_new(float_val_t val);

struct inst_string {
  struct obj base[1];
  unsigned   size;		/* N.B. Includes the '\0' terminator */
  char       *data;
};
#define STRING(x)  ((struct inst_string *)(x))
void m_string_new(unsigned n, ...);
unsigned string_len(obj_t s);

struct inst_dptr {
  struct obj base[1];
  obj_t      car, cdr;
};
#define DPTR(x)  ((struct inst_dptr *)(x))
#define CAR(x)   (DPTR(x)->car)
#define CDR(x)   (DPTR(x)->cdr)
void m_pair_new(obj_t car, obj_t cdr);
void m_cons(obj_t car, obj_t cdr);
void _list_concat(obj_t *li, obj_t el);
void m_list_concat(obj_t *li, obj_t el);
unsigned list_len(obj_t li);

struct inst_method_call {
  struct obj base[1];
  obj_t      sel;
  unsigned   argc;
  obj_t      args;
};
#define METHOD_CALL(x)  ((struct inst_method_call *)(x))
void m_method_call_new(obj_t sel, obj_t args);

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
obj_t array_at(obj_t arr, integer_val_t idx);
void  array_at_put(obj_t arr, integer_val_t idx, obj_t val);

struct inst_dict {
  struct inst_array base[1];
  unsigned (*key_hash)(obj_t key);
  unsigned (*key_equal)(obj_t key1, obj_t key2);
};
#define DICT(x)  ((struct inst_dict *)(x))
void m_string_dict_new(unsigned size);
void m_dict_new(unsigned size);
obj_t    dict_at(obj_t dict, obj_t key);
void     dict_at_put(obj_t dict, obj_t key, obj_t val);
void     dict_del(obj_t dict, obj_t key);
unsigned dict_count(obj_t dict);
void  m_dict_keys(obj_t dict);

struct inst_file {
  struct obj base[1];
  obj_t      name, mode;
  FILE       *fp;
};
#define _FILE(x)  ((struct inst_file *)(x))
void m_file_new(obj_t name, obj_t mode, FILE *fp);
void m_file_stdin(void), m_file_stdout(void), m_file_stderr(void);

struct inst_module {
  struct inst_dict base[1];
  obj_t            name, parent;
  void             *dl_cookie;	/* Cookie from dl_open() */
  void             (*fini_func)(void);
  obj_t            *consts;
  unsigned         nconsts;
};
#define MODULE(x)  ((struct inst_module *)(x))
void m_module_new(obj_t name, obj_t parent);
void m_fqmodname(obj_t mod);

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
  - for class C,  C is a member of dict(module(C))
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

void vm_inst_alloc(obj_t cl);
void vm_assign(unsigned dst, obj_t val);
void vm_pushl(obj_t obj), vm_push(unsigned src), vm_pushm(unsigned src, unsigned n);
void vm_pop(unsigned dst), vm_popm(unsigned dst, unsigned n);
void vm_drop(void), vm_dropn(unsigned n);

struct {
  struct {
    obj_t _false, _true;
  } _bool;
  struct {
    obj_t array;
    obj_t block;
    obj_t boolean;
    obj_t code_method;
    obj_t dictionary;
    obj_t dptr;
    obj_t environment;
    obj_t file;
    obj_t _float;
    obj_t integer;
    obj_t list;
    obj_t metaclass;
    obj_t method_call;
    obj_t module;
    obj_t object;
    obj_t pair;
    obj_t string;
    obj_t system;
    obj_t addc, aifc, aifc_elsec, andc, appendc, aquote, asc, atc, atc_lengthc, atc_putc, awhilec;
    obj_t _break;
    obj_t car, cdr, chr, class_methods, class_variables, _continue;
    obj_t delc, divc;
    obj_t eof, equalsc, eval, evalc, exit, exitc;
    obj_t _false, filterc, flush, foreachc;
    obj_t gec, gtc;
    obj_t hash;
    obj_t indexc, instance_methods, instance_variables, instance_of;
    obj_t keys;
    obj_t lec, length, load, ltc;
    obj_t main, mapc, minus, mode, modc, multc;
    obj_t name, new, newc, newc_modec, newc_parentc_instance_variablesc, newc_putc, nil, not;
    obj_t orc;
    obj_t parent, path, pquote, print, printc;
    obj_t range, rangec, rangec_stepc, readc, readln, reducec_initc, _return, rindexc;
    obj_t _stderr, _stdin, _stdout, splicec, splitc, subc;
    obj_t tostring, tostringc, _true;
    obj_t writec;
    obj_t xorc;
#ifndef NDEBUG
    obj_t assert;
    obj_t collect;
    obj_t debugc;
#endif
  } str;
  struct {
    obj_t metaclass, object, code_method, boolean, integer, _float, string;
    obj_t dptr, pair, list, method_call, block, array, dict, module, file, env, system;
  } cl;
} consts;

enum {
  FATAL_DOUBLE_ERR = 1,
  FATAL_NO_MEM,
  FATAL_STACK_UNF,
};

void fatal(unsigned errcode);

enum {
  ERR_NONE,
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
  ERR_FILE_OPEN_FAIL,
  ERR_MODULE_OPEN_FAIL,
  ERR_WHILE,
  ERR_BLOCK,
  ERR_ASSERT_FAIL,
  ERR_LAST = ERR_ASSERT_FAIL
};

unsigned err_lvl;

void error(unsigned errcode, ...);

enum {
  FRAME_TYPE_RESTART,
  FRAME_TYPE_INPUT,
  FRAME_TYPE_MODULE,
  FRAME_TYPE_WHILE,
  FRAME_TYPE_BLOCK,
  FRAME_TYPE_METHOD_CALL
};

void frame_jmp(unsigned type, int frame_jmp_code);

void inst_init_parent(obj_t cl, obj_t inst, va_list ap);
void inst_walk_parent(obj_t cl, obj_t inst, void (*func)(obj_t));
void inst_free_parent(obj_t cl, obj_t inst);

struct init_cl {
  obj_t    *cl, *name, *parent;
  unsigned inst_size;
  void     (*inst_init)(obj_t cl, obj_t inst, va_list ap);
  void     (*inst_walk)(obj_t cl, obj_t inst, void (*func)(obj_t));
  void     (*inst_free)(obj_t cl, obj_t inst);
  void     (*cl_init)(void);
};

void init_cls(const struct init_cl *tbl, unsigned n);

struct init_str {
  obj_t *obj;
  char  *str;
};

void init_strs(const struct init_str *tbl, unsigned n);

struct init_method {
  obj_t *cl, *sel;
  void  (*func)(unsigned, obj_t);
};

void init_cl_methods(const struct init_method *tbl, unsigned n);
void init_inst_methods(const struct init_method *tbl, unsigned n);

