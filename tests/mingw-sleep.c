/* A native Windows console program for tests/ghostel-interrupt-test.el.
   CI builds it with MinGW-w64 (UCRT64), so it doesn't load msys-2.0.dll
   and only sees C-c as a console control event.  Sleeps SECONDS (60). */
#include <stdlib.h>
#include <windows.h>

int
main (int argc, char **argv)
{
  int seconds = argc > 1 ? atoi (argv[1]) : 60;
  Sleep (seconds * 1000);
  return 0;
}
