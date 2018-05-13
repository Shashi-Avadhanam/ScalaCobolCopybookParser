000001* Example COBOL Copybook file                                     AAAAAAAA
000002  01  EXAMPLE-GROUP.                                              AAAAAAAA
000003       05  ANOTHER-GROUP OCCURS 0003 TIMES.                       AAAAAAAA
000004           10  FIELD-1 PIC X(3).                                  AAAAAAAA
000004              20  FIELD-1_nested PIC X(3).                        AAAAAAAA
000004           88  FLDVAL1  VALUES 'A.                                AAAAAAAA
000005           10  FIELD-2 REDEFINES FIELD-1 PIC 9(3).                AAAAAAAA
000006           10  FIELD-3 OCCURS 0002 TIMES                          AAAAAAAA
000007                           INDEXED BY IND                         AAAAAAAA
000007                           PIC S9(3)V99 COMP-3.                   AAAAAAAA
000008       05  THIS-IS-ANOTHER-GROUP.                                 AAAAAAAA
000009           10  YES PIC X(5).                                      AAAAAAAA