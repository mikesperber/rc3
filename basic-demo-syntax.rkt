#lang reader "basic-reader.rkt"
10 A = 1
20 PRINT A
30 A = A + 1
40 IF A < 100 THEN GOTO 20 ELSE PRINT "DONE"
