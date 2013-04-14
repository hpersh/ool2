#include <assert.h>
#include <stdarg.h>
#include <string.h>
#include <math.h>

#include "ool.h"

struct {
    struct {
	obj_t sin;
	obj_t cos;
	obj_t exp;
    } str;
} math_consts;


void
cm_float_sin(unsigned argc, obj_t args)
{
  obj_t recvr = CAR(args);

  m_float_new(sinl(FLOAT(recvr)->val));
}

void
cm_float_cos(unsigned argc, obj_t args)
{
  obj_t recvr = CAR(args);
  
  m_float_new(cosl(FLOAT(recvr)->val));
}

void
cm_float_exp(unsigned argc, obj_t args)
{
  obj_t recvr = CAR(args);

  m_float_new(expl(FLOAT(recvr)->val));
}

const struct init_str math_init_str_tbl[] = {
    { &math_consts.str.sin, "sin" },
    { &math_consts.str.cos, "cos" },
    { &math_consts.str.exp, "exp" }
};

const struct init_cl math_init_cl_tbl[] = {
};

const struct init_method math_init_cl_method_tbl[] = {
};

const struct init_method math_init_inst_method_tbl[] = {
    { &consts.cl._float, &math_consts.str.sin, cm_float_sin },
    { &consts.cl._float, &math_consts.str.cos, cm_float_cos },
    { &consts.cl._float, &math_consts.str.exp, cm_float_exp }
};

void
math_module_init(void)
{
  vm_push(0);
  
  MODULE(module_cur)->consts  = (obj_t *) &math_consts;
  MODULE(module_cur)->nconsts = sizeof(math_consts) / sizeof(obj_t);
  
  init_strs(math_init_str_tbl, ARRAY_SIZE(math_init_str_tbl));
  
  init_cls(math_init_cl_tbl, ARRAY_SIZE(math_init_cl_tbl));
  
  init_cl_methods(math_init_cl_method_tbl, ARRAY_SIZE(math_init_cl_method_tbl));
  init_inst_methods(math_init_inst_method_tbl, ARRAY_SIZE(math_init_inst_method_tbl));
  
  vm_pop(0);
}


void
math_module_fini(void)
{
}

