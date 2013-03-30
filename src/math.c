#include <assert.h>
#include <stdarg.h>
#include <string.h>
#include <math.h>

#include "ool.h"

struct {
  struct {
    obj_t atan2;
    obj_t cos;
    obj_t exp;
    obj_t log;
    obj_t sin;
    obj_t sqrt;
  } str;
} math_consts;


void
cm_float_sin(unsigned argc, obj_t args)
{
  obj_t recvr = CAR(args);

  m_float_new(consts.cl._float, sinl(FLOAT(recvr)->val));
}

void
cm_float_cos(unsigned argc, obj_t args)
{
  obj_t recvr = CAR(args);
  
  m_float_new(consts.cl._float, cosl(FLOAT(recvr)->val));
}

void
cm_float_atan2(unsigned argc, obj_t args)
{
  obj_t recvr = CAR(args);
  
  m_float_new(consts.cl._float, atan2l(FLOAT(recvr)->val, FLOAT(CAR(CDR(args)))->val));
}

void
cm_float_exp(unsigned argc, obj_t args)
{
  obj_t recvr = CAR(args);

  m_float_new(consts.cl._float, expl(FLOAT(recvr)->val));
}

void
cm_float_log(unsigned argc, obj_t args)
{
  obj_t recvr = CAR(args);

  m_float_new(consts.cl._float, logl(FLOAT(recvr)->val));
}

void
cm_float_sqrt(unsigned argc, obj_t args)
{
  obj_t recvr = CAR(args);

  m_float_new(consts.cl._float, sqrtl(FLOAT(recvr)->val));
}

const struct init_str math_init_str_tbl[] = {
    { &math_consts.str.atan2, "atan2:" },
    { &math_consts.str.cos,   "cos" },
    { &math_consts.str.exp,   "exp" },
    { &math_consts.str.log,   "log" },
    { &math_consts.str.sin,   "sin" },
    { &math_consts.str.sqrt,  "sqrt" }
};

const struct init_method math_init_inst_method_tbl[] = {
    { &consts.cl._float, &math_consts.str.atan2, cm_float_atan2 },
    { &consts.cl._float, &math_consts.str.sin,   cm_float_sin },
    { &consts.cl._float, &math_consts.str.cos,   cm_float_cos },
    { &consts.cl._float, &math_consts.str.exp,   cm_float_exp },
    { &consts.cl._float, &math_consts.str.log,   cm_float_log },
    { &consts.cl._float, &math_consts.str.sqrt,  cm_float_sqrt }
};

void
math_module_init(void)
{
  vm_push(0);
  
  MODULE(module_cur)->consts  = (obj_t *) &math_consts;
  MODULE(module_cur)->nconsts = sizeof(math_consts) / sizeof(obj_t);
  
  init_strs(math_init_str_tbl, ARRAY_SIZE(math_init_str_tbl));
  
  init_inst_methods(math_init_inst_method_tbl, ARRAY_SIZE(math_init_inst_method_tbl));
  
  vm_pop(0);
}


void
math_module_fini(void)
{
  unsigned i;

  for (i = 0; i < ARRAY_SIZE(math_init_inst_method_tbl); ++i) {
    obj_t dict = CLASS(*math_init_inst_method_tbl[i].cl)->inst_methods;
    obj_t sel  = *math_init_inst_method_tbl[i].sel;
    obj_t p, q;

    if (p = dict_at(dict, sel)) {
      q = CDR(p);
      if (is_kind_of(q, consts.cl.code_method)
	  && CODE_METHOD(q)->func == math_init_inst_method_tbl[i].func
	  ) {
	dict_del(dict, sel);
      }
    }
  }
}

