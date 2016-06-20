EXTERN oidtype co_yieldfn(bindtype env, oidtype co);
EXTERN void co_enterbg0();
EXTERN void co_leavebg(oidtype cr);
EXTERN int co_this_thread_busy();
EXTERN void co_leavebg0();
EXTERN void a_sleep0(double s);
#define ENTERBG co_enterbg0()
#define LEAVEBG co_leavebg0()

struct globals
{
  bindtype varstack;
  int varstacksize;
  bindtype varstacktop;
  bindtype varstackslack;
  bindtype topenv;
  jmp_buf *resetlabelp;
  int errorflag;
  int a_errno;
  oidtype errform;
  char *errstr;
};

void SaveGlobals(struct globals *gl);
void RestoreGlobals(struct globals *gl);

