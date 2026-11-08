/* glibc >= 2.34 keeps pthread_yield only as a hidden compat symbol
   (pthread_yield@GLIBC_2.2 on i386), so the unversioned reference in
   AoW3's libiggy_linux.so no longer resolves. Provide it via sched_yield. */
extern int sched_yield(void);
int pthread_yield(void) { return sched_yield(); }
