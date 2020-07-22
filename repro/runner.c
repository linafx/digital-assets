#ifdef _WIN32
#include <stdio.h>
#include <windows.h>
#else
#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#endif

#ifdef _WIN32
VOID exec(LPCTSTR lpApplicationName) {
  STARTUPINFO si;
  PROCESS_INFORMATION pi;

  ZeroMemory(&si, sizeof(si));
  si.cb = sizeof(si);
  ZeroMemory(&pi, sizeof(pi));

  BOOL r = CreateProcess(lpApplicationName, NULL, NULL, NULL, FALSE, 0, NULL,
                         NULL, &si, &pi);
  if (!r) {
    LPVOID lpMsgBuf;
    DWORD dw = GetLastError();

    FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM |
                      FORMAT_MESSAGE_IGNORE_INSERTS,
                  NULL, dw, MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
                  (LPTSTR)&lpMsgBuf, 0, NULL);
    printf("ERROR %s: %s\n", lpApplicationName, lpMsgBuf);
    exit(EXIT_FAILURE);
  }
  WaitForSingleObject(pi.hProcess, INFINITE);
  CloseHandle(pi.hProcess);
  CloseHandle(pi.hThread);
}
#else
void exec(char *const prog) {
  char *const argv[] = {prog, NULL};
  execve(prog, argv, environ);
  perror("ERROR");
  exit(EXIT_FAILURE);
}
#endif

int main(int argc, char **argv) { exec(argv[1]); }
