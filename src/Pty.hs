{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Pty where

import qualified Language.C.Inline as C
import Foreign.C.String

C.verbatim "#define _XOPEN_SOURCE 600"
C.verbatim "#define __USE_BSD"

C.include "<stdlib.h>"
C.include "<stddef.h>"
C.include "<stdio.h>"
C.include "<fcntl.h>"
C.include "<unistd.h>"
C.include "<termios.h>"
C.include "<utmp.h>"
C.include "<poll.h>"
C.include "<sys/time.h>"
C.include "<sys/types.h>"
C.include "<sys/wait.h>"
C.include "<termios.h>"
C.include "<string.h>"
C.include "bite.h"
C.include "<sys/ioctl.h>"

readSon :: IO CString
readSon = [C.block| char * {
  register char *outputRet = malloc(4096);
  register int rd = 0;

  rd = read(masterFd, outputRet, 4096);
  if (rd == -1) {
    perror("read");
    return strdup("Fuk\n");
  }
  return outputRet;
} |]
        
setupSon :: IO ()
setupSon = [C.block| void {
  struct termios slave_orig_term_settings;
  struct termios new_term_settings;

  char *slavename = ptsname(masterFd);
  if (slavename == NULL) {
    perror("ptsname");
    return;
  }
  close(masterFd);
  int slaveFd = open(slavename, O_RDWR);
  if (slaveFd == -1) return;
  int rc = tcgetattr(slaveFd, &slave_orig_term_settings);
  new_term_settings = slave_orig_term_settings;
  cfmakeraw(&new_term_settings);
  tcsetattr(slaveFd, TCSANOW, &new_term_settings);
  struct winsize w;
  ioctl(0, TIOCGWINSZ, &w);
  ioctl(slaveFd, TIOCSWINSZ, &w);
  ioctl(slaveFd, TIOCGWINSZ, &w);
  close(0);
  close(1);
  close(2);
  dup(slaveFd);
  dup(slaveFd);
  dup(slaveFd);
  }|]
