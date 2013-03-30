#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <signal.h>
#include <errno.h>

#include "ool.h"

enum {
  ERR_PIPE_FAILED = ERR_LAST,
  ERR_FORK_FAILED
};

void
process_error(unsigned errcode, ...)
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

  switch (errcode) {
  case ERR_PIPE_FAILED:
    {
      int errnum = va_arg(ap, int);
      
      fprintf(fp, "Pipe failed: %s\n", sys_errlist[errnum]);
    }
    break;
  case ERR_FORK_FAILED:
    {
      int errnum = va_arg(ap, int);
      
      fprintf(fp, "Fork failed: %s\n", sys_errlist[errnum]);
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

struct {
  struct {
    obj_t Process;
    obj_t kill, killc;
    obj_t pid;
    obj_t systemc;
  } str;
  struct {
    obj_t process;
  } cl;
} process_consts;

struct inst_process {
  struct obj base;
  int pid;
  struct {
    obj_t _stdin, _stdout, _stderr;
  } file;
};
#define PROCESS(x)  ((struct inst_process *)(x))

void
inst_init_process(obj_t cl, obj_t inst, va_list ap)
{
  PROCESS(inst)->pid = va_arg(ap, int);
  OBJ_ASSIGN(PROCESS(inst)->file._stdin,  va_arg(ap, obj_t));
  OBJ_ASSIGN(PROCESS(inst)->file._stdout, va_arg(ap, obj_t));
  OBJ_ASSIGN(PROCESS(inst)->file._stderr, va_arg(ap, obj_t));

  inst_init_parent(cl, inst, ap);
}

void
inst_walk_process(obj_t cl, obj_t inst, void (*func)(obj_t))
{
  (*func)(PROCESS(inst)->file._stdin);
  (*func)(PROCESS(inst)->file._stdout);
  (*func)(PROCESS(inst)->file._stderr);

  inst_walk_parent(cl, inst, func);
}

void
inst_free_process(obj_t cl, obj_t inst)
{
  int status;

  kill(PROCESS(inst)->pid, SIGKILL);

  waitpid(PROCESS(inst)->pid, &status, 0);

  inst_free_parent(cl, inst);
}

void
m_process_new(int pid, FILE **fp)
{
  vm_enter(5);

  m_string_newc(consts.cl.string, 1, 1, "w");
  vm_assign(1, R0);
  m_string_newc(consts.cl.string, 1, 1, "r");
  vm_assign(2, R0);

  m_file_new(consts.cl.file, consts.str._stdin, R1, fp[0]);
  vm_assign(3, R0);
  m_file_new(consts.cl.file, consts.str._stdout, R2, fp[1]);
  vm_assign(4, R0);
  m_file_new(consts.cl.file, consts.str._stderr, R2, fp[2]);
  vm_assign(5, R0);

  m_inst_alloc(process_consts.cl.process);
  inst_init(R0, pid, R3, R4, R5);

  vm_leave(5);
}

void
m_process_fork(obj_t args)
{
  obj_t    arg, p, q;
  char     **argv, **r;
  int      fd[3 * 2], pid, errnum;
  FILE     *fp[3];
  unsigned i, argv_size, err = ERR_NONE;

  for (i = 0; i < ARRAY_SIZE(fd); ++i)  fd[i] = -1;
  memset(fp, 0, sizeof(fp));
  argv = 0;

  if (pipe(&fd[0]) < 0
      || pipe(&fd[2]) < 0
      || pipe(&fd[4]) < 0
      ) {
    err = ERR_PIPE_FAILED;
    errnum = errno;
    goto done;
  }

  if ((fp[0] = fdopen(fd[1], "w")) == 0) {
    err = ERR_PIPE_FAILED;
    errnum = errno;
    goto done;
  }
  fd[1] = -1;
  if ((fp[1] = fdopen(fd[2], "r")) == 0) {
    err = ERR_PIPE_FAILED;
    errnum= errno;
    goto done;
  }
  fd[2] = -1;
  if ((fp[2] = fdopen(fd[4], "r")) == 0) {
    err = ERR_PIPE_FAILED;
    errnum = errno;
    goto done;
  }
  fd[4] = -1;

  argv_size = (list_len(args) + 1) * sizeof(char *);
  argv = (char **) _cmalloc(argv_size);
  for (r = argv, p = args; p; p = CDR(p), ++r) {
    *r = STRING(CAR(p))->data;
  }
  *r = 0;

  pid = fork();
  if (pid < 0) {
    err = ERR_FORK_FAILED;
    errnum = errno;
    goto done;
  }
  if (pid == 0) {
    close(0);
    dup(fd[0]);
    close(fd[0]);

    close(1);
    dup(fd[3]);
    close(fd[3]);

    close(2);
    dup(fd[5]);
    close(fd[5]);

    execvp(argv[0], &argv[1]);

    exit(65535);
  }

  m_process_new(pid, fp);

  memset(fp, 0, sizeof(fp));

 done:
  if (argv)  _cfree(argv_size, argv);

  for (i = 0; i < ARRAY_SIZE(fp); ++i) {
    if (fp[i])  fclose(fp[i]);
  }

  for (i = 0; i < ARRAY_SIZE(fd); ++i) {
    if (fd[i] >= 0)  close(fd[i]);
  }

  if (err != ERR_NONE)  process_error(err, errnum);
}

void
cm_process_new(unsigned argc, obj_t args)
{
  obj_t    arg, p, q;
  char     **argv, **r;
  int      fd[3 * 2], pid, errnum;
  FILE     *fp[3];
  unsigned i, argv_size, err = ERR_NONE;

  if (argc != 2)  error(ERR_NUM_ARGS);
  arg = CAR(CDR(args));
  if (!is_kind_of(arg, consts.cl.list))  error(ERR_INVALID_ARG, arg);
  for (p = arg; p; p = CDR(p)) {
    q = CAR(p);
    if (!is_kind_of(q, consts.cl.string))  error(ERR_INVALID_ARG, arg);
  }

  m_process_fork(arg);
}

void
cm_process_system(unsigned argc, obj_t args)
{
  obj_t    arg, p, q;
  char     **argv, **r;
  int      fd[3 * 2], pid, errnum;
  FILE     *fp[3];
  unsigned i, argv_size, err = ERR_NONE;

  if (argc != 2)  error(ERR_NUM_ARGS);
  arg = CAR(CDR(args));
  if (!is_kind_of(arg, consts.cl.string))  error(ERR_INVALID_ARG, arg);

  vm_enter(1);

  m_dptr_new(consts.cl.list, arg, NIL);
  vm_assign(1, R0);
  m_string_newc(consts.cl.string, 1, 2, "-c");
  m_dptr_new(consts.cl.list, R0, R1);
  vm_assign(1, R0);
  m_string_newc(consts.cl.string, 1, 4, "bash");
  m_dptr_new(consts.cl.list, R0, R1);
  vm_assign(1, R0);
  m_string_newc(consts.cl.string, 1, 4, "bash");
  m_dptr_new(consts.cl.list, R0, R1);
  vm_assign(1, R0);
  
  m_process_fork(R1);

  vm_leave(1);
}

void
cm_process_pid(unsigned argc, obj_t args)
{
  obj_t recvr;

  recvr = CAR(args);

  m_integer_new(consts.cl.integer, PROCESS(recvr)->pid);
}

void
cm_process_stdin(unsigned argc, obj_t args)
{
  obj_t recvr;

  recvr = CAR(args);

  vm_assign(0, PROCESS(recvr)->file._stdin);
}

void
cm_process_stdout(unsigned argc, obj_t args)
{
  obj_t recvr;

  recvr = CAR(args);

  vm_assign(0, PROCESS(recvr)->file._stdout);
}

void
cm_process_stderr(unsigned argc, obj_t args)
{
  obj_t recvr;

  recvr = CAR(args);

  vm_assign(0, PROCESS(recvr)->file._stderr);
}

void
cm_process_kill(unsigned argc, obj_t args)
{
  obj_t recvr;

  recvr = CAR(args);

  kill(PROCESS(recvr)->pid, 15);

  vm_assign(0, recvr);
}

void
cm_process_killc(unsigned argc, obj_t args)
{
  obj_t recvr, arg;

  if (argc != 2)  error(ERR_NUM_ARGS);
  recvr = CAR(args);
  arg = CAR(CDR(args));
  if (!is_kind_of(arg, consts.cl.integer))  error(ERR_INVALID_ARG, arg);

  kill(PROCESS(recvr)->pid, INTEGER(arg)->val);

  vm_assign(0, recvr);
}


const struct init_str process_init_str_tbl[] = {
  { &process_consts.str.kill,    "kill" },
  { &process_consts.str.killc,   "kill:" },
  { &process_consts.str.Process, "Process" },
  { &process_consts.str.pid,     "pid" },
  { &process_consts.str.systemc, "system:" }
};

const struct init_cl process_init_cl_tbl[] = {
  { &process_consts.cl.process,
    &process_consts.str.Process,
    &consts.cl.object,
    sizeof(struct inst_process),
    inst_init_process,
    inst_walk_process,
    inst_free_process
  }
};

const struct init_method process_init_cl_method_tbl[] = {
  { &process_consts.cl.process, &consts.str.newc,            cm_process_new },
  { &process_consts.cl.process, &process_consts.str.systemc, cm_process_system }
};

const struct init_method process_init_inst_method_tbl[] = {
  { &process_consts.cl.process, &process_consts.str.pid,   cm_process_pid },
  { &process_consts.cl.process, &consts.str._stdin,        cm_process_stdin },
  { &process_consts.cl.process, &consts.str._stdout,       cm_process_stdout },
  { &process_consts.cl.process, &consts.str._stderr,       cm_process_stderr },
  { &process_consts.cl.process, &process_consts.str.kill,  cm_process_kill },
  { &process_consts.cl.process, &process_consts.str.killc, cm_process_killc },
};

void
process_module_init(void)
{
  vm_push(0);

  MODULE(module_cur)->consts  = (obj_t *) &process_consts;
  MODULE(module_cur)->nconsts = sizeof(process_consts) / sizeof(obj_t);

  init_strs(process_init_str_tbl, ARRAY_SIZE(process_init_str_tbl));

  init_cls(process_init_cl_tbl, ARRAY_SIZE(process_init_cl_tbl));

  init_cl_methods(process_init_cl_method_tbl, ARRAY_SIZE(process_init_cl_method_tbl));
  init_inst_methods(process_init_inst_method_tbl, ARRAY_SIZE(process_init_inst_method_tbl));

  vm_pop(0);
}

void
process_module_fini(void)
{
}
