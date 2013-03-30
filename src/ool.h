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

/* Based memory allocation, with garbage collection */
void *_cmalloc(unsigned size);
void _cfree(unsigned size, void *ptr);

/* Node for implementing circular doubly-linked lists */
struct list {
  struct list *prev, *next;
};

/* Base for all objects */
struct obj;
typedef struct obj *obj_t;
#define OBJ(x)  ((obj_t)(x))
#define NIL     OBJ(0)

struct obj {
  struct list list_node[1];	/* Node for object membership in active/marked lists; see allocator */
  unsigned    ref_cnt;		/* Object reference count */
  obj_t       inst_of;		/* Class of which object is an instance */
  struct {
    unsigned valid;
    unsigned h;
  } hash_cache[1];
};

/* Notes about objects

Reference loops can and do occur.  Therefore, reference counts are not a reliable way
to free objects.  However, that doesn't change the fact that a reference count of 0, i.e.
an object not referred to by any other object or the VM itself, can be freed.  So, a hybrid
approach is taken.  A reference count going to 0 causes an object to be freed, and garbage
collection is also used, to free any object not referenced by some path from the root set.

All classes are an instance of the metaclass.  The metaclass not an instance of anything,
and hence inst_of for metaclass is NIL.
TBD:  Creates many special cases.  Make metaclass an instance of Object?

*/

obj_t inst_of(obj_t obj);
unsigned is_kind_of(obj_t obj, obj_t cl);
void _obj_assign(obj_t *dst, obj_t src);
obj_t obj_retain(obj_t obj);
#define OBJ_ASSIGN(dst, src)  (_obj_assign(&(dst), obj_retain(src)))

/* A class, i.e. an instance of metaclass */

struct inst_metaclass {
  struct obj base[1];
  obj_t      name;		/* Name of class */
  obj_t      parent;		/* Parent class */
  obj_t      module;		/* Parent module */
  obj_t      cl_vars;		/* Dictionary of class variables */
  obj_t      cl_methods;	/* Dictionary of class methods */
  obj_t      inst_vars;		/* Dictionary of instance variables */
  obj_t      inst_methods;	/* Dictionary of instance methods */
  unsigned   inst_size;		/* Size of an instance, in bytes  */
  /* Instance functions */
  /* - initialize given instance */
  void        (*inst_init)(obj_t cl, obj_t inst, va_list ap);
  /* - apply given function to all objects referred to by given instance */
  void        (*inst_walk)(obj_t cl, obj_t inst, void (*func)(obj_t));
  /* - free given instance */
  void        (*inst_free)(obj_t cl, obj_t inst);
  struct list inst_cache[1];	/* List of cached free instances */
};
#define CLASS(x)    ((struct inst_metaclass *)(x))
/* Constructor */
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
unsigned is_class(obj_t obj);

/* A code method */

struct inst_code_method {
  struct obj base[1];
  void       (*func)(unsigned argc, obj_t args); /* The C function to call */
};
#define CODE_METHOD(x)  ((struct inst_code_method *)(x))
/* Constructor */
void m_code_method_new(obj_t cl, void (*func)(unsigned, obj_t));

/* A boolean */

struct inst_boolean {
  struct obj base[1];
  unsigned   val;		/* 0 <=> false, non-zero <=> true */
};
#define BOOLEAN(x)  ((struct inst_boolean *)(x))
void m_boolean_new(obj_t cl, unsigned val);  /* Constructor */

/* An integer (signed fixed-point value) */

typedef long long          integer_val_t;
typedef unsigned long long uinteger_val_t;
struct inst_integer {
  struct obj    base[1];
  integer_val_t val;
};
#define INTEGER(x)  ((struct inst_integer *)(x))
#define INTEGER_SCANF_FMT_DEC  "%lld"
#define INTEGER_SCANF_FMT_OCT  "%llo"
#define INTEGER_SCANF_FMT_HEX  "%llx"
#define INTEGER_PRINTF_FMT  "%lld"
void m_integer_new(obj_t cl, integer_val_t val);  /* Constructor */

/* A float (floating-point value) */

typedef long double float_val_t;
struct inst_float {
  struct obj  base[1];
  float_val_t val;
};
#define FLOAT(x)  ((struct inst_float *)(x))
#define FLOAT_SCANF_FMT  "%Lf"
#define FLOAT_PRINTF_FMT  "%Lg"
void m_float_new(obj_t cl, float_val_t val);  /* Constructor */

/* A string */

struct inst_string {
  struct obj base[1];
  unsigned   size;		/* N.B. Includes the '\0' terminator */
  char       *data;
};
#define STRING(x)  ((struct inst_string *)(x))
void m_string_newc(obj_t cl, unsigned n, ...);  /* Constructor */
void m_string_newv(obj_t cl, obj_t a);  /* Constructor */
struct printf_fmt_info;
void _m_string_pad(obj_t s, struct printf_fmt_info *fi);
int  m_string_pad(obj_t s, struct printf_fmt_info *fi);
unsigned string_len(obj_t s);
unsigned string_hash(obj_t s);
void m_string_fprint(obj_t s, obj_t fi);
void m_string_print(obj_t s);

/* A dptr (dual-pointer, based on the LISP cons cell).
   Used to implement instances of both Pair and List classes.
   Note that for Lists, cdr must always point to another dptr, or NIL.
*/
struct inst_dptr {
  struct obj base[1];
  obj_t      car, cdr;
};
#define DPTR(x)  ((struct inst_dptr *)(x))
#define CAR(x)   (DPTR(x)->car)
#define CDR(x)   (DPTR(x)->cdr)
void m_dptr_new(obj_t cl, obj_t car, obj_t cdr);  /* Constructor */
void _list_concat(obj_t *li, obj_t el);
void m_list_concat(obj_t *li, obj_t el);
unsigned is_list(obj_t obj);
unsigned list_len(obj_t li);

/* A method call */

struct inst_method_call {
  struct obj base[1];
  obj_t      sel;   /* Method selector */
  unsigned   argc;  /* Number of arguments */
  obj_t      args;  /* List of arguments */
};
#define METHOD_CALL(x)  ((struct inst_method_call *)(x))
void m_method_call_new(obj_t cl, obj_t sel, obj_t args);  /* Constructor */

/* A block */

struct inst_block {
  struct obj base[1];
  unsigned argc;		/* Number of arguments */
  obj_t    args;		/* Argument list */
  obj_t    body;		/* List of expressions in body */
};
#define BLOCK(x)  ((struct inst_block *)(x))
void m_block_new(obj_t cl, obj_t args, obj_t body);  /* Comstructor */

/* An array */

struct inst_array {
  struct obj base[1];
  unsigned   size;		/* Number of entries */
  obj_t      *data;		/* Entries */
};
#define ARRAY(x)  ((struct inst_array *)(x))
void m_array_new(obj_t cl, unsigned size);  /* Constructor */
obj_t array_at(obj_t arr, integer_val_t idx);
void  array_at_put(obj_t arr, integer_val_t idx, obj_t val);

/* A set */

struct inst_set {
  struct inst_array base[1];
  unsigned          cnt;	/* Number of members */
};
#define SET(x)  ((struct inst_set *)(x))
void m_set_new(obj_t cl, unsigned size);  /* Constructor */

/* A dictionary */

struct inst_dict {
  struct inst_set base[1];
  /* Lookup function */
  obj_t (*find)(obj_t dict, obj_t key, obj_t **pprev);
};
#define DICT(x)  ((struct inst_dict *)(x))
void  m_string_dict_new(obj_t cl, unsigned size); /* Constructor for string dicts */
void  m_dict_new(obj_t cl, unsigned size);        /* General constructor */
obj_t dict_at(obj_t dict, obj_t key);
void  dict_at_put(obj_t dict, obj_t key, obj_t val);
void  dict_del(obj_t dict, obj_t key);
void  m_dict_keys(obj_t dict);

/* A file */

struct inst_file {
  struct obj base[1];
  obj_t      name;		/* Filename */
  obj_t      mode;		/* Opened mode */
  FILE       *fp;
};
#define _FILE(x)  ((struct inst_file *)(x))
void m_file_new(obj_t cl, obj_t name, obj_t mode, FILE *fp);  /* Constructor */
void m_file_stdin(void), m_file_stdout(void), m_file_stderr(void);

/* A module */

struct inst_module {
  struct inst_dict base[1];
  obj_t            name;	/* Module name */
  obj_t            parent;	/* Parent module */
  void             *dl_cookie;	/* Cookie from dlopen() */
  void             (*fini_func)(void);  /* Function to call before dlclose() */
  unsigned         nconsts;	/* Size of following constant table */
  obj_t            *consts;	/* Constants for module */
};
#define MODULE(x)  ((struct inst_module *)(x))
void m_module_new(obj_t cl, obj_t name, obj_t parent);  /* Constructor */
void m_fqmodname(obj_t mod);

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

obj_t *stack, *stack_end, *stack_fence;
#define STACK_SIZE      8192	/* In objects */
#define STACK_HEADROOM  64	/* In objects */

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
obj_t *sp;

void vm_assign(unsigned dst, obj_t val);
void vm_push(unsigned src);
void vm_pop(unsigned dst);
void vm_drop(void), vm_dropn(unsigned n);
void vm_enter(unsigned n), vm_leave(unsigned n);
void m_inst_alloc(obj_t cl);

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
    obj_t error;
    obj_t file;
    obj_t _float;
    obj_t integer;
    obj_t list;
    obj_t metaclass;
    obj_t method_call;
    obj_t _module, module;
    obj_t object;
    obj_t pair;
    obj_t set;
    obj_t string;
    obj_t system;
    obj_t aandc, abs, acond, addc, aforc_loopc, aifc, aifc_elsec, andc, aorc, appendc, aquote, args, asc, assert, atc, atc_lengthc, atc_putc, awhilec;
    obj_t _break;
    obj_t car, cdr, chr, class_methodc, class_methods, class_variables, comparec, consc, _continue, copy, count, current;
    obj_t default_size, defc_putc, delc, divc;
    obj_t eof, equalsc, eval, evalc, exit, exitc;
    obj_t _false, filterc, flush, foreachc, formatc;
    obj_t gec, gtc;
    obj_t hash;
    obj_t indexc, instance_methodc, instance_methods, instance_variables, instance_of;
    obj_t keys;
    obj_t lec, length, load, ltc;
    obj_t main, mapc, memberc, minus, mode, modc, multc;
    obj_t name, new, newc, newc_asc, newc_modec, newc_parentc_instance_variablesc, nil, not;
    obj_t orc;
    obj_t parent, path, print, printc, putc;
    obj_t raisec, range, rangec, rangec_stepc, read, readc, readln, reducec_initc, _return, rindexc;
    obj_t _stderr, _stdin, _stdout, sel, size, sort, sortc, space, splicec, splitc, subc;
    obj_t tostring, tostringc, _true;
    obj_t writec;
    obj_t xorc;
#ifndef NDEBUG
    obj_t collect;
    obj_t debugc;
#endif
  } str;
  struct {
    obj_t metaclass, object, code_method, boolean, integer, _float, string;
    obj_t dptr, pair, list, method_call, block, array, set, dict, module, file, env, error, system;
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
  ERR_PARSE,
  ERR_STACK_OVF,
  ERR_NUM_ARGS,
  ERR_INVALID_ARG,
  ERR_INVALID_VALUE,
  ERR_INVALID_VALUE_2,
  ERR_NO_METHOD,
  ERR_NO_ATTR,
  ERR_NOT_BOUND,
  ERR_OVF,
  ERR_IDX_RANGE,
  ERR_IDX_RANGE_2,
  ERR_CONST,
  ERR_FILE_OPEN_FAIL,
  ERR_FILE_IO,
  ERR_MODULE_OPEN_FAIL,
  ERR_WHILE,
  ERR_BLOCK,
  ERR_ASSERT_FAIL,
  ERR_CANNOT_INST,
  ERR_USER,
  ERR_LAST = ERR_USER
};

unsigned err_lvl;

void error(unsigned errcode, ...);

enum frame_type {
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
  void     (*cl_init)(obj_t cl);
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

