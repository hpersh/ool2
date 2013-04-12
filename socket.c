#include <assert.h>
#include <stdarg.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>

#include "ool.h"

struct inst_socket {
    struct obj base;
    int        fd;
};
#define SOCKET(obj)  ((struct inst_socket *)(obj))

struct {
    struct {
	obj_t socket;
    } cl;
    struct {
	obj_t Socket;
	obj_t af_inet;
	obj_t bindc;
	obj_t connectc;
	obj_t newc_protoc;
	obj_t sendc;
	obj_t sock_dgram;
	obj_t sock_stream;
	obj_t recvc;
    } str;
} socket_consts;


void
inst_init_socket(obj_t cl, obj_t inst, va_list ap)
{
    int fd = va_arg(ap, int);

    SOCKET(inst)->fd = fd;
    inst_init_parent(cl, inst, ap);
}

void
inst_walk_socket(obj_t cl, obj_t inst, void (*func)(obj_t))
{
  inst_walk_parent(cl, inst, func);
}

void
inst_free_socket(obj_t cl, obj_t inst)
{
    close(SOCKET(inst)->fd);
    inst_free_parent(cl, inst);
}

void
cm_socket_new(unsigned argc, obj_t args)
{
    obj_t recvr, af, proto;
    int   fd;
    
    if (argc != 3)  error(ERR_NUM_ARGS);
    recvr = CAR(args);
    if (!is_kind_of(recvr, socket_consts.cl.socket))  error(ERR_INVALID_ARG, recvr);
    args = CDR(args);  af = CAR(args);
    if (!is_kind_of(af, consts.cl.integer))  error(ERR_INVALID_ARG, af);
    args = CDR(args);  proto = CAR(args);
    if (!is_kind_of(proto, consts.cl.integer))  error(ERR_INVALID_ARG, proto);
    
    fd = socket(INTEGER(af)->val, INTEGER(proto)->val, 0);
    /* Extend error processing in modules */
    ASSERT(fd >= 0);

    vm_inst_alloc(socket_consts.cl.socket);
    inst_init(R0, fd);
}

void
cm_socket_bind(unsigned argc, obj_t args)
{
  obj_t recvr, arg, ip_addr, port;
  struct sockaddr_in sockaddr;
  int rc;

  if (argc != 2)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  if (!is_kind_of(recvr, socket_consts.cl.socket))  error(ERR_INVALID_ARG, recvr);
  arg = CAR(CDR(args));
  if (!is_kind_of(arg, consts.cl.pair))  error(ERR_INVALID_ARG, arg);
  ip_addr = CAR(arg);
  port    = CDR(arg);
  if (!is_kind_of(ip_addr, consts.cl.string))  error(ERR_INVALID_ARG, arg);
  if (!is_kind_of(port, consts.cl.integer))    error(ERR_INVALID_ARG, arg);

  memset(&sockaddr, 0, sizeof(sockaddr));
  sockaddr.sin_family = AF_INET;
  inet_aton(STRING(ip_addr)->data, &sockaddr.sin_addr);
  sockaddr.sin_port   = htons(INTEGER(port)->val);
  rc = bind(SOCKET(recvr)->fd, (const struct sockaddr *) &sockaddr, sizeof(sockaddr));
  
  ASSERT(rc == 0);
  
  vm_assign(0, recvr);
}

void
cm_socket_connect(unsigned argc, obj_t args)
{
  obj_t recvr = CAR(args), arg = CAR(CDR(args)), ip_addr = CAR(arg), port = CDR(arg);
  struct sockaddr_in sockaddr;
  int rc;
  
  memset(&sockaddr, 0, sizeof(sockaddr));
  sockaddr.sin_family = AF_INET;
  inet_aton(STRING(ip_addr)->data, &sockaddr.sin_addr);
  sockaddr.sin_port   = htons(INTEGER(port)->val);
  rc = connect(SOCKET(recvr)->fd, (const struct sockaddr *) &sockaddr, sizeof(sockaddr));
  
  ASSERT(rc == 0);
  
  vm_assign(0, recvr);
}

void
cm_socket_send(unsigned argc, obj_t args)
{
  obj_t recvr = CAR(args), arg = CAR(CDR(args));
  int rc;
  
  rc = send(SOCKET(recvr)->fd, STRING(arg)->data, STRING(arg)->size, 0);
  
  ASSERT(rc >= 0);
  
  vm_assign(0, recvr);
}

void
cm_socket_recv(unsigned argc, obj_t args)
{
  obj_t recvr = CAR(args), arg = CAR(CDR(args));
  int   n, rc;
  
  vm_inst_alloc(consts.cl.string);
  n = INTEGER(arg)->val;
  inst_init(R0, n + 1);
  memset(STRING(R0)->data, 0, n + 1);
  
  rc = recv(SOCKET(recvr)->fd, STRING(R0)->data, n, 0);
  
  ASSERT(rc >= 0);
  
  m_string_new(1, rc, STRING(R0)->data);
}

const struct init_str socket_init_str_tbl[] = {
    { &socket_consts.str.Socket,      "Socket" },
    { &socket_consts.str.bindc,       "bind:" },
    { &socket_consts.str.connectc,    "connect:" },
    { &socket_consts.str.sendc,       "send:" },
    { &socket_consts.str.recvc,       "recv:" },
    { &socket_consts.str.newc_protoc, "new:proto:" },
    { &socket_consts.str.af_inet,     "#AF_INET" },
    { &socket_consts.str.sock_dgram,  "#SOCK_DGRAM" },
    { &socket_consts.str.sock_stream, "#SOCK_STREAM" }
};

const struct init_cl socket_init_cl_tbl[] = {
    { &socket_consts.cl.socket,
      &consts.cl.object,
      &socket_consts.str.Socket,
      sizeof(struct inst_socket),
      inst_init_socket,
      inst_walk_socket,
      inst_free_socket
    }
};

const struct init_method socket_init_cl_method_tbl[] = {
    { &socket_consts.cl.socket, &socket_consts.str.newc_protoc, cm_socket_new }
};

const struct init_method socket_init_inst_method_tbl[] = {
    { &socket_consts.cl.socket, &socket_consts.str.bindc,    cm_socket_bind },
    { &socket_consts.cl.socket, &socket_consts.str.connectc, cm_socket_connect },
    { &socket_consts.cl.socket, &socket_consts.str.sendc,    cm_socket_send },
    { &socket_consts.cl.socket, &socket_consts.str.recvc,    cm_socket_recv }
};


void
socket_module_init(void)
{
  vm_push(0);

  init_strs((const struct init_str *) &socket_init_str_tbl,
	    ARRAY_SIZE(socket_init_str_tbl)
	    );
  
  init_cls((const struct init_cl *) &socket_init_cl_tbl,
	   ARRAY_SIZE(socket_init_cl_tbl)
	   );
  
  init_cl_methods((const struct init_method *) &socket_init_cl_method_tbl,
		  ARRAY_SIZE(socket_init_cl_method_tbl)
		  );
  init_inst_methods((const struct init_method *) &socket_init_inst_method_tbl,
		    ARRAY_SIZE(socket_init_inst_method_tbl)
		    );
  
  m_integer_new(AF_INET);
  env_new_put(socket_consts.str.af_inet, R0);
  m_integer_new(SOCK_DGRAM);
  env_new_put(socket_consts.str.sock_dgram, R0);
  m_integer_new(SOCK_STREAM);
  env_new_put(socket_consts.str.sock_stream, R0);
  
  /* TODO: Free everything instead, and rely on entry in environment?
     Maybe simpler than deleting from root set at unload...
  */
  root_add(&socket_consts.hdr,
	   (sizeof(socket_consts) - sizeof(socket_consts.hdr)) / sizeof(obj_t)
	   );
  
  vm_pop(0);
}


void
socket_module_fini(void)
{
    /* TODO: Need to delete from root set */
}

