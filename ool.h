#include <stdio.h>

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
    obj_t addc, andc, appendc, asc, atc, atc_lengthc, atc_putc;
    obj_t _break;
    obj_t car, cdr, chr, class_methods, class_variables, _continue;
    obj_t deletec, divc;
    obj_t equalsc, eval, evalc, exit, exitc;
    obj_t _false, filterc, foreachc;
    obj_t gec, gtc;
    obj_t hash;
    obj_t ifc, ifc_elsec, indexc, instance_methods, instance_variables, instance_of;
    obj_t keys;
    obj_t lec, length, load, ltc;
    obj_t main, mapc, minus, mode, modc, multc;
    obj_t name, new, newc, newc_modec, newc_parentc_instance_variablesc, newc_putc, nil, not;
    obj_t orc;
    obj_t parent, path, pquote, print, printc;
    obj_t quote;
    obj_t range, rangec, rangec_stepc, readc, readln, reducec_initc, _return, rindexc;
    obj_t _stderr, _stdin, _stdout, splicec, splitc, subc;
    obj_t tostring, tostringc, _true;
    obj_t whilec;
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

void m_class_new(obj_t name, obj_t parent, obj_t module);
void m_code_method_new(void (*func)(unsigned, obj_t));
void m_boolean_new(unsigned val);
typedef long long integer_val_t;
typedef unsigned long long uinteger_val_t;
#define INTEGER_SCANF_FMT_DEC  "%lld"
#define INTEGER_SCANF_FMT_OCT  "%llo"
#define INTEGER_SCANF_FMT_HEX  "%llx"
#define INTEGER_PRINTF_FMT  "%lld"
void m_integer_new(integer_val_t val);
typedef long double float_val_t;
#define FLOAT_SCANF_FMT  "%Lf"
void m_float_new(float_val_t val);
void m_string_new(unsigned n, ...);
void m_pair_new(obj_t car, obj_t cdr);
void m_cons(obj_t car, obj_t cdr);
void _list_concat(obj_t *li, obj_t el);
void m_list_concat(obj_t *li, obj_t el);
void m_method_call_new(obj_t list);
void m_block_new(obj_t list);
void m_array_new(unsigned size);
void m_string_dict_new(unsigned size);
void m_dict_new(unsigned size);
void m_module_new(obj_t name, obj_t parent);
void m_file_new(obj_t name, obj_t mode, FILE *fp);
