//USERID11 JOB (ACCT),'JOHN C',CLASS=A,MSGCLASS=X,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID,
//             TYPRUN=SCAN
//*-------------------------------------------------------------------*
//* HCL INTERNAL ASSESSMENT 1 - JCL QUESTION 11                       *
//* PURPOSE: SYNTAX CHECKING JOB STREAM WITHOUT RUNNING STEPS         *
//*-------------------------------------------------------------------*
//STEP01   EXEC PGM=IEFBR14
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//*
//STEP02   EXEC PGM=SORT
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DSN=USERID.BANK.TRANIN,DISP=SHR
//SORTOUT  DD DSN=USERID.BANK.TRANOUT,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(CYL,(5,1)),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=800)
//SYSIN    DD *
  SORT FIELDS=(1,10,CH,A)
/*
//
