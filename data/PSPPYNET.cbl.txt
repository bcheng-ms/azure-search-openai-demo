       IDENTIFICATION DIVISION.

       PROGRAM-ID. PSPPYNET.

       ENVIRONMENT DIVISION.

      ******************************************************************
      *                                                                *
      *                                                                *
      *                                                                *
      *                                                                *
      * This software and related documentation are provided under a   *
      * license agreement containing restrictions on use and           *
      * disclosure and are protected by intellectual property          *
      * laws. Except as expressly permitted in your license agreement  *
      * or allowed by law, you may not use, copy, reproduce,           *
      * translate, broadcast, modify, license, transmit, distribute,   *
      * exhibit, perform, publish or display any part, in any form or  *
      * by any means. Reverse engineering, disassembly, or             *
      * decompilation of this software, unless required by law for     *
      * interoperability, is prohibited.                               *
      * The information contained herein is subject to change without  *
      * notice and is not warranted to be error-free. If you find any  *
      * errors, please report them to us in writing.                   *
      *                                                                *
      *                                                                *
      * Copyright (C) 1988, 2023, Oracle and/or its affiliates.        *
      * All Rights Reserved.                                           *
      ******************************************************************
      *                                                               **
      *       $Release:  HR92                                         *
      *           $Bug:  35697465                                     *
      *                                                                *
      ******************************************************************

      ******************************************************************
      *                                                                *
      *                   PROGRAM DESCRIPTION:                         *
      *                                                                *
      * GROSS TO NET CALCULATION.                                      *
      *                                                                *
      * MODIFIED FOR FEDERAL                                CC 73-80   *
      * ENH0001  SUPPORT FOR PAY LIMITS/CAPS BY REMOVING FED0001 CHGS  *
      *          WHICH RESIDE IN OTHER PROGRAMS WITH SAME ENH0001 ID   *
      * FED0001  COORDINATION WITH FICA TAX(41/112)                    *
      * FED0999       FICA CREDIT ENHANCEMENT                          *
      * FEDMERG       MERGE OF FEDERAL/COMMERCIAL                      *
      ******************************************************************

      ******************************************************************
      * MODIFIED FOR EDUCATION & GOVERNMENT                            *
      * HP99999       RELEASE 8 TECHNICAL MERGE                        *
      * HP00001       ADD PUBLIC SECTOR INDICATOR                      *
      * HP00009       E&G MERGE3                                       *
      * HP99998       E&G 7.51 AU MERGE                                *
      * HP99997       E&G APRD MERGE                                   *
      * HP00010       CONTRACT PREPAY OPTIONS                          *
      * HP90003       FICA CREDIT ENHANCEMENT                          *
      * HP99995       BEFORE TAX AND TAXABLE DEDUCTION FOR NRA         *
      * HP99994  1042 TAXABLE GROSS EFFECT INDICATOR                   *
      ******************************************************************
      ******************************************************************
      * MAINTENANCE RECORD                                             *     
      * Port Authority customization                                   *      
      *  X000. 06/05/2015   Sierra-Cedar                               *
      *        Tax Update 15b correction                               *
      *  X001. 04/28/2016   Sierra-Cedar                               *
      *        Tax Update 16b correction                               *      
      *                                                                *
      ******************************************************************      


       DATA DIVISION.


       WORKING-STORAGE SECTION.


       01  PROGRAM-IDENTITY            PIC X(8)    VALUE 'PSPPYNET'.


       01  W-SW.
           02  CREATE-SW               PIC X       VALUE 'Y'.
               88  CREATE-YES                      VALUE 'Y'.
               88  CREATE-NO                       VALUE 'N'.
           02  BEFORE-TAX-SW           PIC X       VALUE 'N'.
               88  BEFORE-TAX-YES                  VALUE 'Y'.
               88  BEFORE-TAX-NO                   VALUE 'N'.
           02  AFTER-TAX-SW            PIC X       VALUE 'N'.
               88  AFTER-TAX-YES                   VALUE 'Y'.
               88  AFTER-TAX-NO                    VALUE 'N'.
           02  REDWRK-ADJ-TARRY-SW     PIC X.
               88  REDWRK-ADJ-TARRY                VALUE 'Y'.
               88  REDWRK-NO-ADJ-TARRY             VALUE 'N'.
           02  NOWRK-ADJ-TARRY-SW      PIC X.
               88  NOWRK-ADJ-TARRY                 VALUE 'Y'.
               88  NOWRK-NO-ADJ-TARRY              VALUE 'N'.
           02  HIGHER-ADJ-TARRY-SW     PIC X.
               88  HIGHER-ADJ-TARRY                VALUE 'Y'.
               88  HIGHER-NO-ADJ-TARRY             VALUE 'N'.
           02  ADJUST-PA-SW            PIC X.
               88  ADJUST-PA-YES                   VALUE 'Y'.
               88  ADJUST-PA-NO                    VALUE 'N'.
           02  NEGATIVE-TAX-REQ-SW     PIC X       VALUE 'N'.
               88  NEGATIVE-TAX-REQ-YES            VALUE 'Y'.
               88  NEGATIVE-TAX-REQ-NO             VALUE 'N'.
           02  WK-WK-ADJ-TARRY-SW      PIC X.
               88  WK-WK-ADJ-TARRY                 VALUE 'Y'.
               88  WK-WK-NO-ADJ-TARRY              VALUE 'N'.
           02  REDRES-ADJ-TARRY-SW     PIC X.
               88  REDRES-ADJ-TARRY                VALUE 'Y'.
               88  REDRES-NO-ADJ-TARRY             VALUE 'N'.
           02  DED-FOUND-SW           PIC X.                            HP00003
               88  DED-FOUND-YES                   VALUE 'Y'.           HP00003
               88  DED-FOUND-NO                    VALUE 'N'.           HP00003
           02  GARN-FOUND-SW           PIC X.
               88  GARN-FOUND-YES                  VALUE 'Y'.
               88  GARN-FOUND-NO                   VALUE 'N'.
           02  PREPAY-SW               PIC X.                           HP00010
               88  PREPAY-YES                      VALUE 'Y'.           HP00010
               88  PREPAY-NO                       VALUE 'N'.           HP00010
           02  CALC-DED-SW             PIC X.
               88  CALC-DED-YES                    VALUE 'Y'.
               88  CALC-DED-NO                     VALUE 'N'.
           02  PCT-NET-SW              PIC X.
               88  PCT-NET-YES                     VALUE 'Y'.
               88  PCT-NET-NO                      VALUE 'N'.
           02  STATE-RCP-TAKEN-SW      PIC X.
               88  STATE-RCP-TAKEN-YES             VALUE 'Y'.
               88  STATE-RCP-TAKEN-NO              VALUE 'N'.

           02  LOCAL-RCP-TO-BE-TAKEN-SW PIC X.
               88  LOCAL-RCP-TO-BE-TAKEN-YES       VALUE 'Y'.
               88  LOCAL-RCP-TO-BE-TAKEN-NO        VALUE 'N'.

           02  SINGLE-JOB-FOUND-SW     PIC X.
               88  SINGLE-JOB-FOUND-NO             VALUE 'N'.
               88  SINGLE-JOB-FOUND-YES            VALUE 'Y'.

           02  REG-DED-FOUND-SW        PIC X.
               88  REG-DED-FOUND-NO                VALUE 'N'.
               88  REG-DED-FOUND-YES               VALUE 'Y'.

           02  EARNS-SUPPLE-SW         PIC X.
               88  EARNS-SUPPLE-YES                VALUE 'Y'.
               88  EARNS-SUPPLE-NO                 VALUE 'N'.

           02  EARNS-SPCL-SW           PIC X.
               88  EARNS-SPCL-YES                  VALUE 'Y'.
               88  EARNS-SPCL-NO                   VALUE 'N'.

           02  RECIP-RULE-REDRTE-SW    PIC X.
               88  RECIP-RULE-REDRTE-YES           VALUE 'Y'.
               88  RECIP-RULE-REDRTE-NO            VALUE 'N'.

           02  APPLY-ONCE-SW           PIC X.
               88  APPLY-ONCE-YES                  VALUE 'Y'.
               88  APPLY-ONCE-NO                   VALUE 'N'.

           02  ADD-TO-GROSS-SW         PIC X.
               88  ADD-TO-GROSS-YES                VALUE 'Y'.
               88  ADD-TO-GROSS-NO                 VALUE 'N'.

           02  DED-NOT-TAKEN-SW        PIC X.
               88  DED-NOT-TAKEN-YES               VALUE 'Y'.
               88  DED-NOT-TAKEN-NO                VALUE 'N'.

           02  DED-CUR-PAYBK-SW        PIC X.
               88  DED-CUR-PAYBK-YES               VALUE 'Y'.
               88  DED-CUR-PAYBK-NO                VALUE 'N'.

           02  EARNS-NEG-SW            PIC X.
               88  EARNS-NEG-YES                   VALUE 'Y'.
               88  EARNS-NEG-NO                    VALUE 'N'.

           02  CHANGE-FICA-SW-SAV      PIC X       VALUE 'N'.
               88  CHANGE-FICA-YES-SAV             VALUE 'Y'.
               88  CHANGE-FICA-NO-SAV              VALUE 'N'.

           02  GENERATE-378-SW         PIC X       VALUE 'N'.
               88  GENERATE-378-YES                VALUE 'Y'.
               88  GENERATE-378-NO                 VALUE 'N'.

           02  MTA-WORK-LOCAL-SW       PIC X       VALUE 'N'.
               88  MTA-WORK-YES                    VALUE 'Y'.
               88  MTA-WORK-NO                     VALUE 'N'.

           02  SUPPL-LOCAL-RCP-SW      PIC X       VALUE 'N'.
               88  SUPPL-LOCAL-RCP-YES             VALUE 'Y'.
               88  SUPPL-LOCAL-RCP-NO              VALUE 'N'.

           02  NEG-LOCAL-RCP-SW        PIC X       VALUE 'N'.
               88  NEG-LOCAL-RCP-YES               VALUE 'Y'.
               88  NEG-LOCAL-RCP-NO                VALUE 'N'.

           02  PROC-SUPPL-FOR-CT-SW    PIC X       VALUE 'N'.
               88  PROC-SUPPL-FOR-CT-YES           VALUE 'Y'.
               88  PROC-SUPPL-FOR-CT-NO            VALUE 'N'.

           02  TXARY-FOR-RES-SW        PIC X       VALUE 'N'.
               88  TXARY-FOR-RES-YES               VALUE 'Y'.
               88  TXARY-FOR-RES-NO                VALUE 'N'.

           02  TARRY-FOR-RES-SW        PIC X       VALUE 'N'.
               88  TARRY-FOR-RES-YES               VALUE 'Y'.
               88  TARRY-FOR-RES-NO                VALUE 'N'.

           02  TARRY-FOR-WORK-SW       PIC X       VALUE 'N'.
               88  TARRY-FOR-WORK-YES              VALUE 'Y'.
               88  TARRY-FOR-WORK-NO               VALUE 'N'.

           02  PROCESS-PA-RES-SW       PIC X       VALUE 'N'.
               88  PROCESS-PA-RES-YES              VALUE 'Y'.
               88  PROCESS-PA-RES-NO               VALUE 'N'.

           02  TAX-CUR-ADDL-AMT-SW     PIC X       VALUE 'N'.
               88  TAX-CUR-ADDL-AMT-ADDED          VALUE 'Y'.
               88  TAX-CUR-ADDL-AMT-NOTADDED       VALUE 'N'.

           02  MO-TAX-ADDL-AMT-SW      PIC X       VALUE 'N'.
               88  MO-TAX-ADDL-AMT-ADDED           VALUE 'Y'.
               88  MO-TAX-ADDL-AMT-NOTADDED        VALUE 'N'.

           02  PHIL-LOCAL-FOUND                 PIC X.
               88  PHIL-LOCAL-FOUND-YES            VALUE 'Y'.
               88  PHIL-LOCAL-FOUND-NO             VALUE 'N'.

           02  NON-PHIL-LOCAL-FOUND             PIC X.
               88  NON-PHIL-LOCAL-FOUND-YES        VALUE 'Y'.
               88  NON-PHIL-LOCAL-FOUND-NO         VALUE 'N'.

           02  PA-BLANK-FOUND                   PIC X.
               88  PA-BLANK-FOUND-YES              VALUE 'Y'.
               88  PA-BLANK-FOUND-NO               VALUE 'N'.

           02  PA-RES-ERN-FOUND                 PIC X.
               88  PA-RES-ERN-FOUND-YES            VALUE 'Y'.
               88  PA-RES-ERN-FOUND-NO             VALUE 'N'.

           02  NON-PA-ERN-FOUND                 PIC X.
               88  NON-PA-ERN-FOUND-YES            VALUE 'Y'.
               88  NON-PA-ERN-FOUND-NO             VALUE 'N'.

           02  MULTI-PA-LOC-AND-OTHER           PIC X.
               88  MULTI-PA-LOC-AND-OTHER-YES      VALUE 'Y'.
               88  MULTI-PA-LOC-AND-OTHER-NO       VALUE 'N'.

           02  PA-RES-880000-ROW                PIC X.
               88  PA-RES-880000-ROW-YES           VALUE 'Y'.
               88  PA-RES-880000-ROW-NO            VALUE 'N'.

           02  PA-880000-ROW-FOUND              PIC X.
               88  PA-880000-ROW-FOUND-YES         VALUE 'Y'.
               88  PA-880000-ROW-FOUND-NO          VALUE 'N'.

           02  PA-PHILLY-ROW-FOUND              PIC X.
               88  PA-PHILLY-ROW-FOUND-YES         VALUE 'Y'.
               88  PA-PHILLY-ROW-FOUND-NO          VALUE 'N'.

           02  PA-RES-WRK-PSD-FOUND             PIC X.
               88  PA-RES-WRK-PSD-FOUND-YES         VALUE 'Y'.
               88  PA-RES-WRK-PSD-FOUND-NO          VALUE 'N'.

           02  PA-RES-LOCAL-FOUND               PIC X.
               88 PA-RES-LOCAL-FOUND-YES            VALUE 'Y'.
               88  PA-RES-LOCAL-FOUND-NO            VALUE 'N'.

           02  PA-RES-LOCAL-UPDATE              PIC X.
               88 PA-RES-LOCAL-UPDATE-YES           VALUE 'Y'.
               88 PA-RES-LOCAL-UPDATE-NO            VALUE 'N'.

           02  PA-WRK-TXGRS-FOUND               PIC X.
               88  PA-WRK-TXGRS-FOUND-YES           VALUE 'Y'.
               88  PA-WRK-TXGRS-FOUND-NO            VALUE 'N'.

           02  TGR-SUT-MATCH-FOUND               PIC X.
               88 TGR-SUT-MATCH-FOUND-YES           VALUE 'Y'.
               88 TGR-SUT-MATCH-FOUND-NO            VALUE 'N'.

           02  MULTIPLE-SUPPL-FOUND             PIC X.
               88  MULTIPLE-SUPPL-FOUND-YES         VALUE 'Y'.
               88  MULTIPLE-SUPPL-FOUND-NO          VALUE 'N'.

           02  IND-LWT-CALCULATE                 PIC X.
               88 IND-LWT-CALCULATE-YES             VALUE 'Y'.
               88 IND-LWT-CALCULATE-NO              VALUE 'N'.
           02  EARNS-NOT-SUPPL-FOUND            PIC X.
               88  EARNS-NOT-SUPPL-FOUND-YES        VALUE 'Y'.
               88  EARNS-NOT-SUPPL-FOUND-NO         VALUE 'N'.

           02  FIRST-TAX-METHOD-FOUND           PIC X.
               88  FIRST-TAX-METHOD-FOUND-YES       VALUE 'Y'.
               88  FIRST-TAX-METHOD-FOUND-NO        VALUE 'N'.

           02  MO-SUPPLE-SW                     PIC X.
               88  MO-SUPPLE-YES                    VALUE 'Y'.
               88  MO-SUPPLE-NO                     VALUE 'N'.

           02  MO-WRK-OUTSIDE                   PIC X.
               88  MO-WRK-OUTSIDE-YES               VALUE 'Y'.
               88  MO-WRK-OUTSIDE-NO                VALUE 'N'.

           02  MO-WRK-CALCED                    PIC X.
               88  MO-WRK-CALCED-YES                VALUE 'Y'.
               88  MO-WRK-CALCED-NO                 VALUE 'N'.

           02  SAV-ATAX-CALC-SW        PIC X(3).
               88  SAV-ATAX-CALC-NO                VALUE 'N'.
               88  SAV-ATAX-CALC-YES               VALUE 'Y'.

           02  STATE-NEG-RCP-SW                 PIC X.
               88  STATE-NEG-RCP-YES               VALUE 'Y'.
               88  STATE-NEG-RCP-NO                VALUE 'N'.

           02  AGT-FWT-NEG-SW                   PIC X.
               88  AGT-FWT-NEG-YES                 VALUE 'Y'.
               88  AGT-FWT-NEG-NO                  VALUE 'N'.

           02  SPEC-SWT-EXEMPT-SW               PIC X.
               88  SPEC-SWT-EXEMPT-YES             VALUE 'Y'.
               88  SPEC-SWT-EXEMPT-NO              VALUE 'N'.

           02  PFML-MA-SW              PIC X.
               88  PFML-MA-YES             VALUE 'I','2','3','4'.
               88  PFML-MA-NO              VALUE SPACE.

           02  PFML-WA-SW              PIC X.
               88  PFML-WA-YES             VALUE 'I','2','3','4',
                                                 'O','Y','1','0'.
               88  PFML-WA-SUBJECT         VALUE 'I','2','3','4'.
               88  PFML-WA-NO              VALUE SPACE.

           02  PFML-CT-SW              PIC X(2).
               88  PFML-CT-YES             VALUE 'AA','AB','AC','AD'.
               88  PFML-CT-NO              VALUE SPACE.

           02  SEATTLE-TAX-CALC       PIC X.
               88  SEATTLE-TAX-CALC-YES           VALUE 'Y'.
               88  SEATTLE-TAX-CALC-NO            VALUE 'N'.

           02  SEP-JOB-PAY-SW         PIC X.
               88  SEP-JOB-PAY-YES                VALUE 'Y'.
               88  SEP-JOB-PAY-NO                 VALUE 'N'.

           02  PFF-TGC-ERNCD-SW       PIC X.
               88  PFF-TGC-ERNCD-YES              VALUE 'Y'.
               88  PFF-TGC-ERNCD-NO               VALUE 'N'.

           02  PFF-TGC-DEDCD-SW       PIC X.
               88  PFF-TGC-DEDCD-YES              VALUE 'Y'.
               88  PFF-TGC-DEDCD-NO               VALUE 'N'.

           02  PFF-MAX-LIM-SW         PIC X.
               88  PFF-MAX-LIM-YES                VALUE 'Y'.
               88  PFF-MAX-LIM-NO                 VALUE 'N'.

           02  PFML-CT-MAX-SW         PIC X.
               88  PFML-CT-MAX-YES                VALUE 'Y'.
               88  PFML-CT-MAX-NO                 VALUE 'N'.

           02  PFF-STATE-ERN-SW       PIC X.
               88  PFF-STATE-ERN-YES              VALUE 'Y'.
               88  PFF-STATE-ERN-NO               VALUE 'N'.

           02  PFF-PROCESS-STATE-SW   PIC X.
               88  PFF-PROCESS-STATE-YES          VALUE 'Y'.
               88  PFF-PROCESS-STATE-NO           VALUE 'N'.

           02  TIPS-ERNS-SW           PIC X.
               88  TIPS-ERNS-YES                  VALUE 'Y'.
               88  TIPS-ERNS-NO                   VALUE 'N'.

           02  TIPS-WA-SW             PIC X.
               88  TIPS-WA-YES                    VALUE 'Y'.
               88  TIPS-WA-NO                     VALUE 'N'.


       01  W-WK.
           02  START-VALUE             PIC 99                  COMP.
           02  HRS                     PIC 9999V99             COMP-3.
           02  TXGRS-ADJ               PIC S9(11)V99           COMP-3.
           02  TAX-ADJ                 PIC S9(11)V99           COMP-3.

           02  AG-FWT                  PIC S9(11)V99           COMP-3.
           02  AG-SWT                  PIC S9(11)V99           COMP-3.
           02  AG-LWT                  PIC S9(11)V99           COMP-3.
           02  T-FWT                   PIC S9(11)V99           COMP-3.
           02  T-SWT                   PIC S9(11)V99           COMP-3.
           02  T-LWT                   PIC S9(11)V99           COMP-3.
           02  HIGH-PREC-1             PIC S9V9(13) VALUE 1.0  COMP-3.
           02  HIGH-PREC-0             PIC S9V9(13) VALUE 0.0  COMP-3.

           02  AG-CUR-TS               PIC S9(11)V99           COMP-3.
           02  T-CUR-TS                PIC S9(11)V99           COMP-3.
           02  T-FWT-TTL               PIC S9(11)V99           COMP-3.
           02  T-FWT-CUM               PIC S9(11)V99           COMP-3.
           02  AG-FWT-CUM              PIC S9(11)V99           COMP-3.
           02  AGT-CUR-TS              PIC S9(11)V99           COMP-3.

           02  AGT-CUM-TS              PIC S9(11)V99           COMP-3.
           02  PA-AG-CUM-TS            PIC S9(11)V99           COMP-3.
           02  PA-BLK-AG-CUM           PIC S9(11)V99           COMP-3.
           02  PA-BLK-DED-AMT          PIC S9(11)V99           COMP-3.

           02  AG-TTL-TS               PIC S9(11)V99           COMP-3.
           02  AG-NET-TS               PIC S9(11)V99           COMP-3.

           02  AGT-TTL-TS              PIC S9(11)V99           COMP-3.
           02  AGT-TTL-FWT             PIC S9(11)V99           COMP-3.
           02  AGT-TTL-SWT             PIC S9(11)V99           COMP-3.
           02  AGT-TTL-LWT             PIC S9(11)V99           COMP-3.

           02  AGT-TTL-FWT-NEG         PIC S9(11)V99           COMP-3.
           02  AGT-TTL-SWT-NEG         PIC S9(11)V99           COMP-3.
           02  AGT-TTL-LWT-NEG         PIC S9(11)V99           COMP-3.

           02  DEDCD-FOUND             PIC X.
               88  DEDCD-FOUND-YES                 VALUE 'Y'.
               88  DEDCD-FOUND-NO                  VALUE 'N'.
           02  SAV-RESIDENCE           PIC X(6).
           02  SAV-WORK                PIC X(6).
NOCBGN     02  SAV-LOCALITY.
               05  FIRST-CHAR-LOC      PIC X(1).
NOCEND         05  LAST-CHAR-LOC       PIC X(9).
           02  DED-AMT                 PIC S9(11)V99           COMP-3.
           02  MANUAL-FUT-AMT          PIC S9(11)V99           COMP-3.
           02  MANUAL-FUT-DED          PIC S9(11)V99           COMP-3.
           02  WK-DED-CUR              PIC S9(11)V99           COMP-3.
           02  WK-DED-YTD              PIC S9(11)V99           COMP-3.  HP9999
           02  WK-REG-DED              PIC S9(11)V99           COMP-3.
           02  WK-AMT                  PIC S9(11)V99           COMP-3.
           02  WK-AMT-ROUND            PIC S9(11)              COMP-3.
           02  WK-ARREARS              PIC S9(11)V99           COMP-3.
           02  SAV-DEDNET              PIC S9(11)V99           COMP-3.
           02  SAV-CHECK-NET           PIC S9(11)V99           COMP-3.
           02  SAV-TTLTAX              PIC S9(11)V99           COMP-3.
           02  SAV-TTLDED              PIC S9(11)V99           COMP-3.
           02  PARTIAL-DED             PIC S9(11)V99           COMP-3.
           02  TOT-ADDL-AMT            PIC S9(11)V99           COMP-3.
           02  SAV-TXARY-IDX           PIC 99                  COMP.
           02  CAL-TXARY-IDX           PIC 99                  COMP.
           02  RES-TXARY-IDX           PIC 99                  COMP.
           02  SAV-TARRY-IDX           PIC 99                  COMP.
           02  RES-TARRY-IDX           PIC 99                  COMP.
           02  SAV-LOCAL-CNT           PIC 99                  COMP.
           02  LOC-RES-COUNT           PIC 99                  COMP.
           02  STATE-COUNT             PIC 99                  COMP.
           02  RES-TAX                 PIC S9(11)V99           COMP-3.
           02  RES-TAX-TOTAL           PIC S9(11)V99           COMP-3.
           02  RES-TAX-RECALC          PIC S9(11)V99           COMP-3.
           02  RES-TAX-PRIOR           PIC S9(11)V99           COMP-3.
           02  ACTUAL-TAX-CUR-SAV      PIC S9(11)V99           COMP-3.
           02  RES-TAX-RECALC-SAV      PIC S9(11)V99           COMP-3.
           02  RES-TAX-PRIOR-CALC      PIC S9(11)V99           COMP-3.
           02  RES-TAX-SUPPL           PIC S9(11)V99           COMP-3.
           02  RES-TXGRS-RECALC        PIC S9(11)V99           COMP-3.
           02  SAV-WORK-TXGRS          PIC S9(11)V99           COMP-3.
           02  SAV-RES-TXGRS           PIC S9(11)V99           COMP-3.
           02  RES-TAX-LESS            PIC S9(11)V99           COMP-3.
           02  RES-TXBL                PIC S9(11)V99           COMP-3.
           02  SAV-TAX-ADJ             PIC S9(11)V99           COMP-3.
           02  REDUCE-PCT              PIC S9(3)V99            COMP-3.
           02  CREDIT-LIM-RT           PIC SV9(6)              COMP-3.
           02  CALCED-LIMIT            PIC S9(11)V99           COMP-3.
           02  CALCED-RCP-LIMIT        PIC S9(11)V99           COMP-3.
           02  RES-CALC                PIC X.
               88 RES-CALC-YES                     VALUE 'Y'.
               88 RES-CALC-NO                      VALUE 'N'.
           02  RES-LCLCALC             PIC X.
               88 RES-LCLCALC-YES                  VALUE 'Y'.
               88 RES-LCLCALC-NO                   VALUE 'N'.
           02  HOLD-EARN-END-DT        PIC X(10).
           02  EMPL-RCD-NO             PIC 999                 COMP.
           02  TAX-CALCD               PIC X.
               88  TAX-CALCD-YES                VALUE 'Y'.
               88  TAX-CALCD-NO                 VALUE 'N'.

           02  SAVE-PCT-NET            PIC S9(3)V99            COMP-3.
           02  SAV-RESIDENT            PIC X.
           02  TAX-ON-RES-EARNS        PIC S9(11)V99           COMP-3.
           02  TTL-DED-NOT-TAKEN       PIC S9(11)V99           COMP-3.
           02  REV-DEDNET              PIC S9(11)V99           COMP-3.
           02  ADJ-EARNINGS            PIC S9(11)V99           COMP-3.
           02  WK-LCLPCT               PIC 9(5)V9(10)          COMP-3.
           02  WK-LCLPCT-RES           PIC 9(5)V9(10)          COMP-3.
           02  W-ADJ                   PIC S9(11)V99           COMP-3.
           02  W-LCLTX                 PIC S9(11)V99           COMP-3.
           02  W-LCLGRS                PIC S9(11)V99           COMP-3.
           02  DED-EST                 PIC S9(11)V99           COMP-3.  HP90003
           02  FUTURE-DED-PERIOD       PIC 9999                COMP.    HP90003
           02  CONF-DED-PERIOD         PIC 9999                COMP.    HP90003
           02  CUM-DED-PERIOD          PIC 9999                COMP.    HP90003
           02  TOT-PAY-PERIOD          PIC 9999                COMP.    HP90003
           02  SARRY-PASS              PIC 9999                COMP.
           02  EMPL-RCD-NO-SPCL        PIC 999                 COMP.
           02  EMPL-RCD-NO-SPCL-HOLD   PIC 999                 COMP.
           02  HOLD-DED-NOT-TAKEN      PIC S9(11)V99           COMP-3.
           02  HOLD-DEDC1              PIC 999                 COMP.

           02  PAY-FREQ-TYPE           PIC X.
           02  TAX-METHOD              PIC X.
           02  TAX-METHOD1             PIC X       VALUE SPACES.
           02  WK-LCLPCT-RATE          PIC 9(5)V9(10)          COMP-3.
           02  WK-LCLPCT-USED-RATE     PIC 9(5)V9(10)          COMP-3.
           02  CMPR-LCL-RATE           PIC 9(5)V9(10)          COMP-3.

           02  RESFLAG-IDX             PIC 99                  COMP.
           02  CALENDAR-FILL           PIC X(06)   VALUE '-01-01'.
           02  WK-PA-OPT-YTD           PIC S9(11)V99           COMP-3.
           02  WK-CUR-PAYBK-ADJUST     PIC S9(11)V99           COMP-3.
           02  WK-DED-CUR-PAYBK-HOLD   PIC S9(11)V99           COMP-3.
           02  WK-DED-NOT-TAKEN        PIC S9(11)V99           COMP-3.
           02  WK-DED-CUR-ADJUST       PIC S9(11)V99           COMP-3.
           02  WK-REG-RECIP-TAX        PIC S9(11)V99           COMP-3.
           02  PRIOR-GRS-STATE-RCP     PIC S9(11)V99           COMP-3.
           02  GROSSUP-DEDNET          PIC S9(11)V99           COMP-3.
           02  SUPPL-BAL-TOTAL         PIC S9(11)V99           COMP-3.
           02  SUPPL-BAL-EXTRA         PIC S9(11)V99           COMP-3.
           02  SUPPL-NON-TAX           PIC S9(11)V99           COMP-3.
           02  SUPPL-TAXABLE           PIC S9(11)V99           COMP-3.
           02  TAX-CUR-SAV             PIC S9(11)V99           COMP-3.
           02  WK-SUPPL-LIMIT          PIC 9999999 VALUE 1000000 COMP.
           02  DARRY-HOLD              PIC 999                 COMP.
           02  DCLAS-HOLD              PIC 999                 COMP.
           02  PREV-DEDNET             PIC S9(11)V99           COMP-3.
           02  WK-RECIPROCITY-RULE     PIC X.
               88  WK-RECIPROCITY-RULE-NONE       VALUE SPACE.
               88  WK-RECIPROCITY-RULE-BOTH       VALUE 'B'.
               88  WK-RECIPROCITY-RULE-FACTOR     VALUE 'G'.
               88  WK-RECIPROCITY-RULE-REDWRK     VALUE 'K'.
               88  WK-RECIPROCITY-RULE-REDTTL     VALUE 'N'.
               88  WK-RECIPROCITY-RULE-REDTTL-A   VALUE 'P'.
               88  WK-RECIPROCITY-RULE-REDRES     VALUE 'R'.
               88  WK-RECIPROCITY-RULE-NOWORK     VALUE 'W'.
               88  WK-RECIPROCITY-RULE-NOWORK-A   VALUE 'Y'.
               88  WK-RECIPROCITY-RULE-NOWORK-ALL VALUE 'W', 'Y'.
               88  WK-RECIPROCITY-RULE-SPECRT     VALUE 'S'.
           02  BEFORE-TAX-NOT-APPLIED  PIC S9(11)V99           COMP-3.
           02  FUT-SUBTR-NOT-APPLIED   PIC S9(11)V99           COMP-3.
           02  FICA-SUBTR-NOT-APPLIED  PIC S9(11)V99           COMP-3.
           02  FIRST-DED-CLASS         PIC X.
               88  FIRST-DED-CLASS-ATAX           VALUE 'A'.
               88  FIRST-DED-CLASS-PTAX           VALUE 'P'.
           02  SECOND-DED-CLASS        PIC X.
               88  SECOND-DED-CLASS-ATAX          VALUE 'A'.
               88  SECOND-DED-CLASS-PTAX          VALUE 'P'.
           02  LOCAL-RCP-SAVE          PIC S9(8)V99            COMP-3.
           02  LOCAL-RCP-SAVE-ORIG     PIC S9(8)V99            COMP-3.
           02  HOLD-SUT-EXEMPT         PIC X.
           02  WK-DED-IDX1             PIC 9999 COMP.
           02  WK-DED-IDX2             PIC 9999 COMP.
           02  CT-SUPPC-SPCL-ACCUM-CD  PIC X(10)   VALUE 'CTS'.
           02  WTARRY-IDX              PIC 9999                COMP.
           02  LAST-PA-LOC             PIC 9999                COMP.
           02  PA-LOC-ADJUST           PIC 9999                COMP.
           02  SUBJ-1042-DED           PIC S9(8)V99            COMP-3.  HP99994
           02  1042-GRS-TMP            PIC S9(8)V99            COMP-3.  HP99994
           02  WK-EARNS-TTL            PIC S9(11)V99           COMP-3.  HP99994
           02  WK-1042-EARNS           PIC S9(11)V99           COMP-3.  HP99994
           02  WK-1042-DED-AMT         PIC S9(11)V99           COMP-3.  HP99994
           02  WK-REMAIN-DED-AMT       PIC S9(11)V99           COMP-3.  HP99994
           02  WK-FIRST                PIC 9999 COMP VALUE 1.           HP99994

           02  STATE-FOUND             PIC X.                           HP99994
               88  STATE-FOUND-NO              VALUE 'N'.               HP99994
               88  STATE-FOUND-YES             VALUE 'Y'.               HP99994
           02  1042-ERN-CNT            PIC 99  VALUE 0.                 HP99994
               88  1042-ERN-MAX                VALUE 20.                HP99994
           02  1042-ERN-TABLE                 OCCURS 20                 HP99994
                                                   INDEXED BY           HP99994
                                                   1042-IDX.            HP99994
               03  1042-ERN-STATE      PIC X(6).                        HP99994
               03  1042-ERN-AMT        PIC S9(11)V99           COMP-3.  HP99994
               03  ST-TTL-AMT          PIC S9(11)V99           COMP-3.  HP99994
           02  PRIOR-RECIPROCITY       PIC S9(11)V99           COMP-3.
           02  PRE-FACTOR-TAX-CUR      PIC S9(11)V99           COMP-3.
           02  FACTOR-TAX-CUR          PIC S9(11)V99           COMP-3.
           02  FACTOR-TXGRS-CUR        PIC S9(11)V99           COMP-3.
           02  PRIOR-TAX-SUPP-CUR      PIC S9(11)V99           COMP-3.
           02  PA-LOC-TXGRS-TXARY      PIC S9(11)V99           COMP-3.
           02  PA-PHL-LOC-GRS-ADJ      PIC S9(8)V99            COMP-3.
           02  PA-TOT-125-DED          PIC S9(8)V99            COMP-3.
           02  PA-DED-ERN              PIC S9(8)V99            COMP-3.
           02  PA-DED-NON-PA-ERN       PIC S9(8)V99            COMP-3.
           02  PA-PHL-PERC-DED         PIC S9(8)V9999          COMP-3.
           02  PA-PERC-DED             PIC S9(8)V9999          COMP-3.
           02  PA-LOC-RES-TXGRS-ADJ    PIC S9(8)V9999          COMP-3.
           02  PA-LOC-RES-TXGRS-CUR    PIC S9(8)V9999          COMP-3.
           02  PA-LOC-RES-TXGRS        PIC S9(8)V99            COMP-3.
           02  PA-PREV-LOCALITY        PIC X(10)   VALUE SPACE.
           02  PA-RES-LOCALITY         PIC X(10)   VALUE SPACE.
           02  PA-PREV-TAX-METHOD      PIC X       VALUE SPACE.
           02  PA-PREV-TAX-METHOD-ANN  PIC X       VALUE SPACE.
           02  PA-PREV-TAX-METHOD-OTH  PIC X       VALUE SPACE.
           02  PA-WORK-PSD-CD-SAV      PIC X(6)    VALUE SPACE.
           02  SUT-STATE               PIC X(6)    VALUE SPACE.
           02  TAX-METHOD-SAV          PIC X       VALUE SPACE.
               88  TAX-METHOD-SAV-SUPP             VALUE 'S','X'.
           02  MO-WRK-STATES-IDX       PIC 99                  COMP.
           02  MO-TARRY-IDX            PIC 99                  COMP.
           02  MO-RCP-WRK-AMT          PIC S9(11)V99           COMP-3.
           02  MO-GRSWK-AMT            PIC S9(11)V99           COMP-3.
           02  MO-COMP-WRK-AMT         PIC S9(11)V99           COMP-3.
           02  DARRY-SAV               PIC 9999                COMP.
           02  TAX-STATE-RCP-ANN       PIC S9(8)V99            COMP-3.
           02  TAX-STATE-RCP-SUP       PIC S9(8)V99            COMP-3.
           02  TAX-SUP-PRIOR-WRK       PIC S9(11)V99           COMP-3.
           02  ALL-GROSS-AMT           PIC S9(11)V99           COMP-3.
           02  ANN-ALL-GROSS-AMT       PIC S9(11)V99           COMP-3.
           02  ANNUAL-FACTOR           PIC S9(3)V9(6)          COMP-3.
           02  WORK-LOCALITY           PIC X(10).
           02  WORK-TAX-RATE           PIC S9(5)V9(10)         COMP-3.
           02  WORK-TEMP-RATE          PIC S9(5)V9(10)         COMP-3.
           02  WORK-TRANSIT-AMT        PIC S9(5)V9(6)          COMP-3.
           02  WORK-TRANSIT-RND        PIC S9(5)V99            COMP-3.
           02  WORK-LTC-AMT            PIC S9(5)V9(6)          COMP-3.
           02  WORK-LTC-RND            PIC S9(5)V99            COMP-3.
           02  WORK-LTC-ADJ            PIC S9(11)V99           COMP-3.
           02  WORK-LTC-GRS-YTD        PIC S9(11)V99           COMP-3.
           02  WORK-LTC-GRS-QTD        PIC S9(11)V99           COMP-3.
           02  WORK-LTC-GRS-MTD        PIC S9(11)V99           COMP-3.
           02  WORK-LTC-CUR-YTD        PIC S9(11)V99           COMP-3.
           02  WORK-SEATTLE-GRS-YTD    PIC S9(11)V99           COMP-3.
           02  WORK-SEATTLE-GRS-QTD    PIC S9(11)V99           COMP-3.
           02  WORK-SEATTLE-GRS-MTD    PIC S9(11)V99           COMP-3.
           02  WORK-SEATTLE-CUR-YTD    PIC S9(11)V99           COMP-3.
           02  WORK-SEATTLE-AMT        PIC S9(11)V99           COMP-3.
           02  WORK-SEATTLE-ADJ        PIC S9(11)V99           COMP-3.
           02  WORK-SEATTLE-RND        PIC S9(11)V99           COMP-3.
           02  WORK-TOTAL-AMT          PIC S9(11)V99           COMP-3.
           02  WORK-OTH-IDX            PIC 99                  COMP.
           02  FLI-EE-RATE             PIC SV9(10)             COMP-3.
           02  FLI-SPLIT-RT            PIC SV9(10)             COMP-3.
           02  MLI-SPLIT-RT            PIC SV9(10)             COMP-3.
           02  FLI-EE-RATIO            PIC 999V99              COMP-3.
           02  FLI-ER-RATIO            PIC 999V99              COMP-3.
           02  MLI-EE-RATIO            PIC 999V99              COMP-3.
           02  MLI-ER-RATIO            PIC 999V99              COMP-3.
           02  FLI-MLI-TOT-PREM        PIC S9(11)V99           COMP-3.
           02  FLI-TOT                 PIC S9(11)V99           COMP-3.
           02  MLI-TOT                 PIC S9(11)V99           COMP-3.
           02  FLI-EE-PREM             PIC S9(11)V99           COMP-3.
           02  FLI-ER-PREM             PIC S9(11)V99           COMP-3.
           02  MLI-EE-PREM             PIC S9(11)V99           COMP-3.
           02  MLI-ER-PREM             PIC S9(11)V99           COMP-3.
           02  FLI-MAX-GROSS           PIC S9(11)V99           COMP-3.
           02  FLI-EE-TXGRS-CUR        PIC S9(11)V99           COMP-3.
           02  MLI-EE-TXGRS-CUR        PIC S9(11)V99           COMP-3.
           02  FML-MAX-GROSS           PIC S9(11)V99           COMP-3.
           02  WORK-FML-AMT            PIC S9(5)V9(6)          COMP-3.
           02  WORK-FML-RND            PIC S9(5)V99            COMP-3.
           02  WORK-ADJUST-AMT         PIC S9(11)V99           COMP-3.
           02  WORK-DED-AMT            PIC S9(11)V99           COMP-3.


NOCBGN 01  W-MSGDATA.
           02  MSGDATA1.
               03  FILLER              PIC X(5)    VALUE 'Freq='.
               03  PAY-FREQ-TYPE       PIC X(1).
               03  FILLER              PIC X(12)   VALUE ' Tx-Periods='.
               03  TAX-PERIODS         PIC 9(2).
               03  FILLER              PIC X(9)    VALUE ' Tx-Meth='.
               03  TAX-METHOD          PIC X(1).
           02  MSGDATA2.
               03  FILLER              PIC X(6)    VALUE 'State='.
               03  STATE               PIC X(6).
               03  FILLER              PIC X(7)    VALUE ' Local='.
               03  LOCALITY            PIC X(10).
           02  MSGDATA3.
               03  FILLER              PIC X(17)   VALUE
                                                   'SUT-Jurisdiction='.
               03  SUT-JURISDICTION    PIC X(6).

           02  MSGDATA1-A.
               03  FILLER              PIC X(8)    VALUE 'Garn ID='.
               03  GARNID              PIC X(6).
           02  MSGDATA2-A.
               03  FILLER              PIC X(15)   VALUE
                                                 'Deduction Code='.
NOCEND         03  DEDCD               PIC X(10).

       01  W-TTL.
           02  TXBL-TTL                PIC S9(11)V99           COMP-3.
           02  HOLD-TXBL-TTL           PIC S9(11)V99           COMP-3.
           02  HOLD2-TXBL-TTL          PIC S9(11)V99           COMP-3.
           02  TXBL-FW-TTL             PIC S9(11)V99           COMP-3.
           02  EARNS-TTL               PIC S9(11)V99           COMP-3.
           02  SAVE-EARNS-PCT          PIC S999V99999          COMP-3.
           02  DED-REFUND-TTL          PIC S9(11)V99           COMP-3.

      /*****************************************************************
      *          W-PRORATE  FOR PRORATION PROCESS                      *
      ******************************************************************
       01  W-PRORATE.
           02  AVAIL-AMT               PIC S9(6)V99            COMP-3.
           02  GARN-DE-TTL             PIC S9(6)V99            COMP-3.
           02  PRORATE-RULE            PIC X(6).
           02  GARN-TYPE               PIC X.
               88  GARN-TYPE-SUPPORT            VALUE 'C' 'D' 'S'.
               88  GARN-TYPE-WRIT               VALUE 'W'.
           02  PRORATE-REASON          PIC X.
               88  INSUFF-DE                     VALUE 'D'.
               88  INSUFF-PAY                    VALUE 'P'.
           02  CUR-MAX-AMT             PIC S9(6)V99            COMP-3.
           02  ARR-MAX-AMT             PIC S9(6)V99            COMP-3.

      /*****************************************************************FEDMERG
      *          W-SPCL-EARNS FOR SPECIAL EARNINGS                     *FED0014
      ******************************************************************FED0014
       01  W-SPCL-EARNS.                                                FED0014
           02 BW-SPCL-EARNS           PIC S9(11)V99 VALUE ZEROS COMP-3. FED0014
                                                                        FED0014
      /*****************************************************************FED0014
      *          W-PAY-LIMITS FOR PAY LIMITS                           *FED0014
      ******************************************************************FED0014
       01  W-PAY-LIMITS.                                                FED0014
           02 LEO-LOC-LIMIT            PIC S9(8)V99 VALUE ZEROS COMP-3. FED0014
           02 LEO-NLOC-LIMIT           PIC S9(8)V99 VALUE ZEROS COMP-3. FED0014
           02 BW-PAY-LIMIT             PIC S9(8)V99 VALUE ZEROS COMP-3. FED0014
                                                                        FEDMERG
           02  EARN-LIM-FOUND          PIC X.                           ENH0001
               88  EARN-LIM-FOUND-YES               VALUE 'Y'.          ENH0001
               88  EARN-LIM-FOUND-NO                VALUE 'N'.          ENH0001
      /*****************************************************************
      *            WITHHOLDING TAX ADJUSTMENTS                         *
      ******************************************************************
       01  WHADJ.                      COPY PSCWHADJ.


      /*****************************************************************
      *               PASSED  DATA                                     *
      ******************************************************************

       01  W-PASS.
           02  DARRY-PASS              PIC 9999                COMP.
           02  DCLAS-PASS              PIC 9999                COMP.
           02  ADJ-PAY-PRD             PIC X.
           02  ADJ-PAY-PRD-NUM REDEFINES ADJ-PAY-PRD
                                       PIC 9.

       01  W-PASSX.
           02  DARRY-PASS              PIC 9999                COMP.
           02  DCLAS-PASS              PIC 9999                COMP.
           02  DCALC-PASS              PIC 9999                COMP.
           02  SAV-ATAX-CALC-PASS      PIC X(3)    VALUE 'N'.
               88  SAV-ATAX-CALC-NO                VALUE 'N'.
               88  SAV-ATAX-CALC-YES               VALUE 'Y'.
           02  DARRY-SAV-PASS          PIC 9999                COMP.

       01  TGCTB-PASS.
           02  SUBJECT-FWT             PIC X.
               88  SUBJECT-FWT-YES                 VALUE 'Y'.
               88  SUBJECT-FWT-NO                  VALUE 'N'.
               88  SUBJECT-FWT-NA                  VALUE SPACE.
           02  SUBJECT-FUT             PIC X.
               88  SUBJECT-FUT-YES                 VALUE 'Y'.
               88  SUBJECT-FUT-NO                  VALUE 'N'.
               88  SUBJECT-FUT-NA                  VALUE SPACE.
           02  SUBJECT-FICA            PIC X.
               88  SUBJECT-FICA-YES                VALUE 'Y'.
               88  SUBJECT-FICA-NO                 VALUE 'N'.
               88  SUBJECT-FICA-NA                 VALUE SPACE.
           02  FICA-EFFECT             PIC X.
               88  FICA-EFFECT-ADD                 VALUE 'A'.
               88  FICA-EFFECT-SUBTRACT            VALUE 'S'.
               88  FICA-EFFECT-NA                  VALUE SPACE.
           02  FUT-EFFECT              PIC X.
               88  FUT-EFFECT-ADD                  VALUE 'A'.
               88  FUT-EFFECT-SUBTRACT             VALUE 'S'.
               88  FUT-EFFECT-NA                   VALUE SPACE.
           02  TAX-GRS-COMPNT          PIC X(10).
           02  PLAN-TYPE               PIC XX.
           02  ERNCD                   PIC X(10).
           02  DEDCD                   PIC X(10).
           02  DED-CLASS               PIC X.
           02  STATE-TG                PIC X(6).
           02  LOCALITY-TG             PIC X(10).
           02  TXBL-BEN-SW             PIC X.
               88  TXBL-BEN-YES                    VALUE 'Y'.
               88  TXBL-BEN-NO                     VALUE 'N'.
           02  WITHHOLD-FWT            PIC X.
               88  WITHHOLD-FWT-YES                VALUE 'Y'.
               88  WITHHOLD-FWT-NO                 VALUE 'N'.
               88  WITHHOLD-FWT-NA                 VALUE SPACE.
           02  PA-EARN-SW             PIC X.
               88  PA-EARN-YES                     VALUE 'Y'.
               88  PA-EARN-NO                      VALUE 'N'.
           02  WORK-IDX                PIC 9999                COMP.
           02  RES-IDX                 PIC 9999                COMP.
           02  BEFORE-TAX-NOT-APPLIED  PIC S9(8)V99            COMP-3.
           02  FUT-SUBTR-NOT-APPLIED   PIC S9(8)V99            COMP-3.
           02  FICA-SUBTR-NOT-APPLIED  PIC S9(8)V99            COMP-3.
           02  FUT-EARNINGS            PIC S9(8)V99            COMP-3.
           02  FICA-EARNINGS           PIC S9(8)V99            COMP-3.

       01  TGBTB-PASS.
           02  PASS-PGM                PIC X(5).
               88  PASS-PGM-PYNET                  VALUE 'PYNET'.
               88  PASS-PGM-TCALC                  VALUE 'TCALC'.
           02  WORK-IDX                PIC 9999                COMP.
           02  RES-IDX                 PIC 9999                COMP.

      /*****************************************************************
      *           DEARY - FOR DEDUCTIONS APPLIED TO DISPOSABLE EARNINGS*
      ******************************************************************
       01  DEARY.
           02  GARN-DE-DATA                    OCCURS 20
                                               INDEXED BY DEARY-IDX.
               03  GARNID                PIC X(6).
               03  GARN-DED-CALC         PIC X.
                   88  GARN-DED-CALC-MAX           VALUE 'M'.
                   88  GARN-DED-CALC-PCT-AMT       VALUE 'P'.
                   88  GARN-DED-CALC-GRTR          VALUE 'G'.
                   88  GARN-DED-CALC-PCT-GRS       VALUE 'R'.
                   88  GARN-DED-CALC-GRT-GRS       VALUE 'H'.


               03  GARN-RULE-DATA              OCCURS  3
                                               INDEXED BY DEARY-RL-IDX.
                   04  GARN-RULE-ID      PIC X(10).
                   04  GARN-DEDUCT-DATA        OCCURS 40
                                               INDEXED BY DEARY-DED-IDX.
                       05  DEDCD         PIC X(10).

      /*****************************************************************FEDMERG
      *            PAY_EARNINGS SQL BUFFER AND STMT                    *FED0014
      ******************************************************************FED0014
       01  PSPPYNET-U-ERN.                                              FED0014
           02  SQL-CURSOR              PIC 9999    VALUE ZERO  COMP.    FED0014
       01  U-ERN.                                                       FED0014
           02  SQL-STMT                PIC X(18)   VALUE                FED0014
                                                   'PSPPYNET_U_ERN'.    FED0014
           02  BIND-SETUP.                                              FED0014
               03  FILLER              PIC X       VALUE ALL 'H'.       FED0014
               03  FILLER              PIC X(10)   VALUE ALL 'C'.       FED0014
               03  FILLER              PIC X(10)   VALUE ALL 'H'.       FED0014
               03  FILLER              PIC X(10)   VALUE ALL 'D'.       FED0014
               03  FILLER              PIC X       VALUE ALL 'C'.       FED0014
               03  FILLER              PIC XXXX    VALUE ALL 'I'.       FED0014
               03  FILLER              PIC XX      VALUE ALL 'M'.       FED0014
               03  FILLER              PIC XX      VALUE ALL 'S'.       FED0014
               03  FILLER              PIC X       VALUE 'Z'.           FED0014
                                                                        FED0014
           02  BIND-DATA.                                               FED0014
               03  OK-TO-PAY           PIC X       VALUE SPACES.        FED0014
               03  COMPANY             PIC X(10)   VALUE SPACES.        FED0014
               03  PAYGROUP            PIC X(10)   VALUE SPACES.        FED0014
               03  PAY-END-DT          PIC X(10)   VALUE SPACES.        FED0014
               03  OFF-CYCLE           PIC X       VALUE SPACES.        FED0014
               03  PAGE-NO             PIC 99999               COMP.    FED0014
               03  LINE-NO             PIC 999                 COMP.    FED0014
               03  ADDL-NO             PIC 999                 COMP.    FED0014
               03  FILLER              PIC X       VALUE 'Z'.           FED0014
                                                                        FEDMERG
      /*****************************************************************
      *        S_RES_STATE SQL BUFFER AND STMT                         *
      ******************************************************************

       01  S-RES-STATE.
           02  SQL-STMT                PIC X(18)   VALUE
                                                   'PSPPYNET_S_RES_ST'.

           02  BIND-SETUP.
               03  FILLER              PIC X(10)   VALUE ALL 'C'.
               03  FILLER              PIC X(10)   VALUE ALL 'H'.
               03  FILLER              PIC X(10)   VALUE ALL 'D'.
               03  FILLER              PIC X       VALUE ALL 'C'.
               03  FILLER              PIC X(20)   VALUE ALL 'H'.
               03  FILLER              PIC X(6)    VALUE ALL 'C'.
               03  FILLER              PIC X(6)    VALUE ALL 'H'.
               03  FILLER              PIC X       VALUE 'Z'.

           02  BIND-DATA.
               03  COMPANY             PIC X(10).
               03  PAYGROUP            PIC X(10).
               03  PAY-END-DT          PIC X(10).
               03  OFF-CYCLE           PIC X.
               03  EMPLID              PIC X(20).
               03  STATE               PIC X(6).
                   88  STATE-FEDERAL               VALUE '$U'.

               03  AGGR-ID             PIC X(6).
               03  FILLER              PIC X       VALUE 'Z'.

           02  SELECT-SETUP.
               03  FILLER              PIC X       VALUE 'C'.
               03  FILLER              PIC X       VALUE 'Z'.

           02  SELECT-DATA.
               03  FILLER-X            PIC X.
               03  FILLER              PIC X       VALUE 'Z'.


      /*****************************************************************
      *        S_REV_ADJ_PAY_PRD SQL BUFFER AND STMT                         *
      ******************************************************************

       01  S-REV-ADJ.
           02  SQL-STMT                PIC X(18)   VALUE
                                                   'PSPPYNET_S_REV_ADJ'.

           02  BIND-SETUP.
               03  FILLER              PIC X(10)   VALUE ALL 'C'.
               03  FILLER              PIC X(10)   VALUE ALL 'H'.
               03  FILLER              PIC X(10)   VALUE ALL 'D'.
               03  FILLER              PIC X       VALUE 'Z'.

           02  BIND-DATA.
               03  COMPANY             PIC X(10).
               03  PAYGROUP            PIC X(10).
               03  PAY-END-DT          PIC X(10).
               03  FILLER              PIC X       VALUE 'Z'.

           02  SELECT-SETUP.
               03  FILLER              PIC X       VALUE ALL 'C'.
               03  FILLER              PIC X(10)   VALUE ALL 'D'.
               03  FILLER              PIC X       VALUE 'Z'.

           02  SELECT-DATA.
               03  PAY-PERIOD          PIC X.
               03  PAY-END-DATE        PIC X(10).
               03  FILLER              PIC X       VALUE 'Z'.

      /*****************************************************************
      *            PAY_DEDUCTION SQL BUFFER AND STMT                   *
      ******************************************************************
       01  S-CUR.
           02  SQL-STMT                PIC X(18)   VALUE
                                                   'PSPPYNET_S_CUR'.
           02  SQL-STMT-MOD            PIC X(18)   VALUE
                                                   'PSPPYNET_S_CURM'.

           02  BIND-SETUP.
               03  FILLER              PIC X(20)   VALUE ALL 'C'.
               03  FILLER              PIC XX      VALUE ALL 'H'.
               03  FILLER              PIC X(10)   VALUE ALL 'C'.
               03  FILLER              PIC X(10)   VALUE ALL 'H'.
               03  FILLER              PIC X       VALUE ALL 'C'.
               03  FILLER              PIC X       VALUE 'Z'.

           02  BIND-DATA.
               03  EMPLID              PIC X(20).
               03  PLAN-TYPE           PIC XX.
               03  BENEFIT-PLAN        PIC X(10).
               03  DEDCD               PIC X(10).
               03  DED-CLASS           PIC X.
               03  FILLER              PIC X       VALUE 'Z'.

           02  SELECT-SETUP.
               03  FILLER              PIC X(6)    VALUE '2PPPPP'.
               03  FILLER              PIC X(6)    VALUE '2PPPPP'.
               03  FILLER              PIC X(6)    VALUE '2PPPPP'.
               03  FILLER              PIC X(6)    VALUE '2PPPPP'.
               03  FILLER              PIC X(10)   VALUE ALL 'D'.
               03  FILLER              PIC XX      VALUE ALL 'S'.
               03  FILLER              PIC X       VALUE 'Z'.

           02  SELECT-DATA.

               03  DED-CUR             PIC S9(8)V99            COMP-3.
               03  DED-NOT-TAKEN       PIC S9(8)V99            COMP-3.
               03  DED-CUR-PAYBK       PIC S9(8)V99            COMP-3.
               03  DED-CUR-REFUND      PIC S9(8)V99            COMP-3.
               03  PAY-END-DT          PIC X(10).
               03  SEPCHK              PIC 99                  COMP.
               03  FILLER              PIC X       VALUE 'Z'.

      /*****************************************************************
      *    INDIANA LOCAL_TAX_DATA BUFFER AND STMT                      *
      ******************************************************************
       01  S-LCLIND.
           02  FETCH-LCLIND-SW         PIC X       VALUE 'S'.
               88  FETCH-LCLIND-END                VALUE 'E'.
               88  FETCH-LCLIND-START              VALUE 'S'.

           02  SQL-CURSOR              PIC 9999    VALUE ZERO  COMP.
           02  SQL-STMT                PIC X(18)   VALUE
                                                   'PSPPYNET_S_LCLIND'.

           02  BIND-SETUP.
               03  FILLER              PIC X(20)   VALUE ALL 'C'.
               03  FILLER              PIC X(10)   VALUE ALL 'H'.
               03  FILLER              PIC X       VALUE ALL 'C'.
               03  FILLER              PIC X(6)    VALUE ALL 'H'.
               03  FILLER              PIC X(10)   VALUE ALL 'D'.
               03  FILLER              PIC X       VALUE 'Z'.

           02  BIND-DATA.
               03  EMPLID              PIC X(20).
               03  COMPANY             PIC X(10).
               03  RESIDENT            PIC X.
               03  WORK-STATE          PIC X(6).
               03  CHECK-DT            PIC X(10).
               03  FILLER              PIC X       VALUE 'Z'.

           02  SELECT-SETUP.
               03  FILLER              PIC X(6)    VALUE ALL 'C'.
               03  FILLER              PIC X(10)   VALUE ALL 'H'.
               03  FILLER              PIC X(10)   VALUE ALL 'D'.
               03  FILLER              PIC X       VALUE 'Z'.

           02  SELECT-DATA.
               03  STATE               PIC X(6).
               03  LOCALITY            PIC X(10).
               03  EFFCT-DT            PIC X(10).
               03  FILLER              PIC X       VALUE 'Z'.

      /*****************************************************************
      *    INDIANA LOCAL_TAX_DATA BUFFER AND STMT                      *
      ******************************************************************
       01  S-LCLINDP.
           02  FETCH-LCLINDP-SW         PIC X       VALUE 'S'.
               88  FETCH-LCLINDP-END                VALUE 'E'.
               88  FETCH-LCLINDP-START              VALUE 'S'.

           02  SQL-CURSOR              PIC 9999    VALUE ZERO  COMP.
           02  SQL-STMT                PIC X(18)   VALUE
                                                   'PSPPYNET_S_LCLINDP'.

           02  BIND-SETUP.
               03  FILLER              PIC X(20)   VALUE ALL 'C'.
               03  FILLER              PIC X(10)   VALUE ALL 'H'.
               03  FILLER              PIC X       VALUE ALL 'C'.
               03  FILLER              PIC X(6)    VALUE ALL 'H'.
               03  FILLER              PIC X(10)   VALUE ALL 'D'.
               03  FILLER              PIC X       VALUE 'Z'.

           02  BIND-DATA.
               03  EMPLID              PIC X(20).
               03  COMPANY             PIC X(10).
               03  RESIDENT            PIC X.
               03  WORK-STATE          PIC X(6).
               03  CHECK-DT            PIC X(10).
               03  FILLER              PIC X       VALUE 'Z'.

           02  SELECT-SETUP.
               03  FILLER              PIC X(6)    VALUE ALL 'C'.
               03  FILLER              PIC X(10)   VALUE ALL 'H'.
               03  FILLER              PIC X(10)   VALUE ALL 'D'.
               03  FILLER              PIC X       VALUE 'Z'.

           02  SELECT-DATA.
               03  STATE               PIC X(6).
               03  LOCALITY            PIC X(10).
               03  EFFCT-DT            PIC X(10).
               03  FILLER              PIC X       VALUE 'Z'.
      ******************************************************************
      *            PAY_CHECK ARREARS SQL BUFFER AND STMT               *
      ******************************************************************
       01  S-CHKCNT.
           02  SQL-CURSOR              PIC 9999    VALUE ZERO  COMP.
           02  SQL-STMT                PIC X(18)   VALUE
                                                   'PSPPYNET_S_CHKCNT'.

           02  BIND-SETUP.
               03  FILLER              PIC X(10)   VALUE ALL 'C'.
               03  FILLER              PIC X(10)   VALUE ALL 'D'.
               03  FILLER              PIC X(20)   VALUE ALL 'C'.
               03  FILLER              PIC X       VALUE ALL 'H'.
               03  FILLER              PIC XX      VALUE ALL 'C'.
               03  FILLER              PIC X(10)   VALUE ALL 'H'.
               03  FILLER              PIC X(10)   VALUE ALL 'C'.
               03  FILLER              PIC X       VALUE ALL 'H'.
               03  FILLER              PIC X       VALUE 'Z'.

           02  BIND-DATA.
               03  COMPANY             PIC X(10).
               03  PAY-END-DT          PIC X(10).
               03  EMPLID              PIC X(20).
               03  OFF-CYCLE           PIC X.
               03  PLAN-TYPE           PIC XX.
               03  BENEFIT-PLAN        PIC X(10).
               03  DEDCD               PIC X(10).
               03  DED-CLASS           PIC X.
               03  FILLER              PIC X       VALUE 'Z'.

           02  SELECT-SETUP.
               03  FILLER              PIC X       VALUE ALL 'C'.
               03  FILLER              PIC X       VALUE 'Z'.

           02  SELECT-DATA.
               03  CHK-YES             PIC X.
               03  FILLER              PIC X       VALUE 'Z'.

      ******************************************************************
      *            SUPPL BALANCES COUNT  BUFFER AND STMT               *
      ******************************************************************
       01  S-CNTBAL.
           02  SQL-CURSOR              PIC 9999    VALUE ZERO  COMP.
           02  SQL-STMT                PIC X(18)   VALUE
                                                   'PSPPYNET_S_CNTBAL'.

           02  BIND-SETUP.
               03  FILLER              PIC X(20)   VALUE ALL 'C'.
               03  FILLER              PIC X(2)    VALUE ALL 'S'.
               03  FILLER              PIC X(10)   VALUE ALL 'C'.
               03  FILLER              PIC X(2)    VALUE ALL 'H'.
               03  FILLER              PIC X       VALUE 'Z'.

           02  BIND-DATA.
               03  EMPLID              PIC X(20).
               03  BAL-YEAR            PIC 9(4)                COMP.
               03  SPCL-ERNCD          PIC X(10).
               03  BAL-ID              PIC X(2).
               03  FILLER              PIC X       VALUE 'Z'.

           02  SELECT-SETUP.
               03  FILLER              PIC X(07)    VALUE '2PPPPPP'.
               03  FILLER              PIC X       VALUE 'Z'.

           02  SELECT-DATA.
               03  GRS-YTD             PIC S9(11)V99           COMP-3.
               03  FILLER              PIC X       VALUE 'Z'.

      ******************************************************************
      *            CURR CHECK SUPPL BAL  BUFFER AND STMT               *
      ******************************************************************
       01  S-SPCLBAL.
           02  SQL-CURSOR              PIC 9999    VALUE ZERO  COMP.
           02  SQL-STMT                PIC X(18)   VALUE
                                                   'PSPPYNET_S_SPCLBAL'.
           02  BIND-SETUP.
               03  FILLER              PIC X(20)   VALUE ALL 'C'.
               03  FILLER              PIC X       VALUE ALL 'H'.
               03  FILLER              PIC X(10)   VALUE ALL 'C'.
               03  FILLER              PIC X(10)   VALUE ALL 'H'.
               03  FILLER              PIC X(10)   VALUE ALL 'C'.
               03  FILLER              PIC X(10)   VALUE ALL 'D'.
               03  FILLER              PIC X       VALUE 'Z'.

           02  BIND-DATA.
               03  EMPLID              PIC X(20).
               03  OFF-CYCLE           PIC X.
               03  SPCL-ERNCD          PIC X(10).
               03  COMPANY             PIC X(10).
               03  PAYGROUP            PIC X(10).
               03  PAY-END-DT          PIC X(10).
               03  FILLER              PIC X       VALUE 'Z'.

           02  SELECT-SETUP.
               03  FILLER              PIC X(06)    VALUE '2PPPPP'.
               03  FILLER              PIC X       VALUE 'Z'.

           02  SELECT-DATA.
               03  TXGRS-CUR           PIC S9(8)V99            COMP-3.
               03  FILLER              PIC X       VALUE 'Z'.

      ******************************************************************
      *            CURR CHECK SUPPL BAL  BUFFER AND STMT               *
      ******************************************************************
       01  S-FICADED.
           02  SQL-CURSOR              PIC 9999    VALUE ZERO  COMP.
           02  SQL-STMT                PIC X(18)   VALUE
                                                   'PSPPYNET_S_FICADED'.
           02  BIND-SETUP.
               03  FILLER              PIC X(20)   VALUE ALL 'C'.
               03  FILLER              PIC X(2)    VALUE ALL 'S'.
               03  FILLER              PIC X(2)    VALUE ALL 'H'.
               03  FILLER              PIC X       VALUE 'Z'.

           02  BIND-DATA.
               03  EMPLID              PIC X(20).
               03  BAL-YEAR            PIC 9(4)                COMP.
               03  BAL-ID              PIC X(2).
               03  FILLER              PIC X       VALUE 'Z'.

           02  SELECT-SETUP.
               03  FILLER              PIC X(07)    VALUE '2PPPPPP'.
               03  FILLER              PIC X       VALUE 'Z'.

           02  SELECT-DATA.
               03  DED-YTD             PIC S9(11)V99           COMP-3.
               03  FILLER              PIC X       VALUE 'Z'.


      ******************************************************************
      *            SUPPL BALANCES FOR CONNECTICUT                      *
      ******************************************************************

       01  S-CTBAL.
           02  SQL-CURSOR              PIC 9999    VALUE ZERO  COMP.
           02  SQL-STMT                PIC X(18)   VALUE
                                                   'PSPPYNET_S_CTBAL'.

           02  BIND-SETUP.
               03  FILLER              PIC X(20)   VALUE ALL 'C'.
               03  FILLER              PIC X(2)    VALUE ALL 'S'.
               03  FILLER              PIC X(10)   VALUE ALL 'C'.
               03  FILLER              PIC X(2)    VALUE ALL 'H'.
               03  FILLER              PIC X       VALUE 'Z'.

           02  BIND-DATA.
               03  EMPLID              PIC X(20).
               03  BAL-YEAR            PIC 9(4)                COMP.
               03  SPCL-ERNCD          PIC X(10).
               03  BAL-ID              PIC X(2).
               03  FILLER              PIC X       VALUE 'Z'.

           02  SELECT-SETUP.
               03  FILLER              PIC X(07)    VALUE '2PPPPPP'.
               03  FILLER              PIC X       VALUE 'Z'.

           02  SELECT-DATA.
               03  GRS-YTD             PIC S9(11)V99           COMP-3.
               03  FILLER              PIC X       VALUE 'Z'.

      ******************************************************************
      *            CONNECTICUT HIRE DATE                               *
      ******************************************************************

       01  S-CTHIRE.
           02  SQL-CURSOR              PIC 9999    VALUE ZERO  COMP.
           02  SQL-STMT                PIC X(18)   VALUE
                                                   'PSPPYNET_S_CTHIRE'.

           02  BIND-SETUP.
               03  FILLER              PIC X(20)   VALUE ALL 'C'.
               03  FILLER              PIC X(10)   VALUE ALL 'H'.
               03  FILLER              PIC X(10)   VALUE ALL 'D'.
               03  FILLER              PIC X       VALUE 'Z'.

           02  BIND-DATA.
               03  EMPLID              PIC X(20).
               03  COMPANY             PIC X(10).
               03  EFFDT               PIC X(10).
               03  FILLER              PIC X       VALUE 'Z'.

           02  SELECT-SETUP.
               03  FILLER              PIC X       VALUE ALL 'C'.
               03  FILLER              PIC X       VALUE 'Z'.

           02  SELECT-DATA.
               03  HIRE-YES            PIC X       VALUE SPACE.
               03  FILLER              PIC X       VALUE 'Z'.

      /*****************************************************************
      *            LOCAL_TAX_DATA BUFFER AND STMT                      *
      ******************************************************************
       01  S-SEATTLE.
           02  SQL-CURSOR              PIC 9999    VALUE ZERO  COMP.
           02  SQL-STMT                PIC X(18)   VALUE
                                                   'PSPPYNET_S_SEATTLE'.

           02  BIND-SETUP.
               03  FILLER              PIC X(20)   VALUE ALL 'C'.
               03  FILLER              PIC X(10)   VALUE ALL 'H'.
               03  FILLER              PIC X(6)    VALUE ALL 'C'.
               03  FILLER              PIC X(10)   VALUE ALL 'H'.
               03  FILLER              PIC X(10)   VALUE ALL 'D'.
               03  FILLER              PIC X       VALUE 'Z'.

           02  BIND-DATA.
               03  EMPLID              PIC X(20).
               03  COMPANY             PIC X(10).
               03  STATE               PIC X(6).
               03  LOCALITY            PIC X(10).
               03  CHECK-DT            PIC X(10).
               03  FILLER              PIC X       VALUE 'Z'.

           02  SELECT-SETUP.
               03  FILLER              PIC X(10)   VALUE ALL 'C'.
               03  FILLER              PIC X       VALUE 'Z'.

           02  SELECT-DATA.
               03  LOCALITY            PIC X(10)    VALUE SPACES.
               03  FILLER              PIC X       VALUE 'Z'.


      /*****************************************************************
      *        S_AGRS_AC    BUFFER AND STMT                            *
      ******************************************************************

       01  S-AGRS-ACCUM.
           02  SQL-STMT                PIC X(18)   VALUE
                                               'PSPPYNET_S_AGRS'.

           02  BIND-SETUP.
               03  FILLER              PIC X       VALUE ALL 'C'.
               03  FILLER              PIC X       VALUE 'Z'.

           02  BIND-DATA.
               03  ACCM-USAGE          PIC X.
               03  FILLER              PIC X       VALUE 'Z'.

           02  SELECT-SETUP.
               03  FILLER              PIC X(03)   VALUE ALL 'C'.
               03  FILLER              PIC X       VALUE ALL 'Z'.

           02  SELECT-DATA.
               03  ERNCD-SPCL          PIC X(03).
               03  FILLER              PIC X       VALUE 'Z'.

      /*****************************************************************
      *        S_BAL_SEA    BUFFER AND STMT                            *
      ******************************************************************

       01  S-BAL-SEA.
           02  SQL-STMT                PIC X(18)   VALUE
                                               'PSPPYNET_S_BAL_SEA'.

           02  BIND-SETUP.
               03  FILLER              PIC X(20)   VALUE ALL 'C'.
               03  FILLER              PIC XX      VALUE ALL 'S'.
               03  FILLER              PIC XX      VALUE ALL 'M'.
               03  FILLER              PIC X       VALUE 'Z'.

           02  BIND-DATA.
               03  EMPLID              PIC X(20).
               03  BALANCE-YEAR        PIC 9999                COMP.
               03  BALANCE-PERIOD      PIC 999                 COMP.
               03  FILLER              PIC X       VALUE 'Z'.

           02  SELECT-SETUP.
               03  FILLER              PIC X(7)    VALUE '2PPPPPP'.
               03  FILLER              PIC X(7)    VALUE '2PPPPPP'.
               03  FILLER              PIC X(7)    VALUE '2PPPPPP'.
               03  FILLER              PIC X(7)    VALUE '2PPPPPP'.
               03  FILLER              PIC X       VALUE ALL 'Z'.

           02  SELECT-DATA.
               03  TXGRS-YTD           PIC S9(11)V99           COMP-3.
               03  TXGRS-QTD           PIC S9(11)V99           COMP-3.
               03  TXGRS-MTD           PIC S9(11)V99           COMP-3.
               03  TAX-YTD             PIC S9(11)V99           COMP-3.
               03  FILLER              PIC X       VALUE 'Z'.

      /*****************************************************************
      *        S_BAL_SEA    BUFFER AND STMT                            *
      ******************************************************************

       01  S-BAL-WALTC.
           02  SQL-STMT                PIC X(18)   VALUE
                                               'PSPPYNET_S_BAL_LTC'.

           02  BIND-SETUP.
               03  FILLER              PIC X(20)   VALUE ALL 'C'.
               03  FILLER              PIC XX      VALUE ALL 'S'.
               03  FILLER              PIC XX      VALUE ALL 'M'.
               03  FILLER              PIC X       VALUE 'Z'.

           02  BIND-DATA.
               03  EMPLID              PIC X(20).
               03  BALANCE-YEAR        PIC 9999                COMP.
               03  BALANCE-PERIOD      PIC 999                 COMP.
               03  FILLER              PIC X       VALUE 'Z'.

           02  SELECT-SETUP.
               03  FILLER              PIC X(7)    VALUE '2PPPPPP'.
               03  FILLER              PIC X(7)    VALUE '2PPPPPP'.
               03  FILLER              PIC X(7)    VALUE '2PPPPPP'.
               03  FILLER              PIC X(7)    VALUE '2PPPPPP'.
               03  FILLER              PIC X       VALUE ALL 'Z'.

           02  SELECT-DATA.
               03  TXGRS-YTD           PIC S9(11)V99           COMP-3.
               03  TXGRS-QTD           PIC S9(11)V99           COMP-3.
               03  TXGRS-MTD           PIC S9(11)V99           COMP-3.
               03  TAX-YTD             PIC S9(11)V99           COMP-3.
               03  FILLER              PIC X       VALUE 'Z'.

      /*****************************************************************
      *            LOCAL_TAX_DATA BUFFER AND STMT                      *
      ******************************************************************
       01  S-WALTC.
           02  SQL-CURSOR              PIC 9999    VALUE ZERO  COMP.
           02  SQL-STMT                PIC X(18)   VALUE
                                                   'PSPPYNET_S_WALTC'.

           02  BIND-SETUP.
               03  FILLER              PIC X(20)   VALUE ALL 'C'.
               03  FILLER              PIC X(10)   VALUE ALL 'H'.
               03  FILLER              PIC X(10)   VALUE ALL 'D'.
               03  FILLER              PIC X       VALUE 'Z'.

           02  BIND-DATA.
               03  EMPLID              PIC X(20).
               03  COMPANY             PIC X(10).
               03  CHECK-DT            PIC X(10).
               03  FILLER              PIC X       VALUE 'Z'.

           02  SELECT-SETUP.
               03  FILLER              PIC X       VALUE ALL 'C'.
               03  FILLER              PIC X       VALUE 'Z'.

           02  SELECT-DATA.
               03  LTC-IND             PIC X       VALUE SPACES.
               03  FILLER              PIC X       VALUE 'Z'.


      /*****************************************************************
      *            FRAMEWORK DATA                                      *
      ******************************************************************
       01  S-FRMWKS.
           02  SQL-CURSOR              PIC 9999    VALUE ZERO  COMP.
           02  SQL-STMT                PIC X(18)   VALUE
                                                   'PSPPYNET_S_FRMWKS'.

           02  BIND-SETUP.
               03  FILLER              PIC X(20)   VALUE ALL 'C'.
               03  FILLER              PIC X(10)   VALUE ALL 'H'.
               03  FILLER              PIC X(10)   VALUE ALL 'D'.
               03  FILLER              PIC X       VALUE 'Z'.

           02  BIND-DATA.
               03  EMPLID              PIC X(20).
               03  COMPANY             PIC X(10).
               03  CHECK-DT            PIC X(10).
               03  FILLER              PIC X       VALUE 'Z'.

           02  SELECT-SETUP.
               03  FILLER              PIC X(6)    VALUE ALL 'C'.
               03  FILLER              PIC X(4)    VALUE ALL 'H'.
               03  FILLER              PIC X       VALUE ALL 'C'.
               03  FILLER              PIC X       VALUE 'Z'.

           02  SELECT-DATA.
               03  PY-STATE            PIC X(6).
               03  PY-PFF-PROGID       PIC X(4).
               03  PY-PFF-PLAN         PIC X.
               03  FILLER              PIC X       VALUE 'Z'.


      /*****************************************************************
      *            PAY MESSAGE INTERFACE                               *
      ******************************************************************
       01  PYMSG.                      COPY PSCPYMSG.

      /*****************************************************************
      *            LOCAL RECIPROCITY INDICES                          *
      ******************************************************************
       01  L-PASS.
           02  LCLWK-PASS              PIC 99                 COMP.
           02  LCLWH-PASS              PIC 99                 COMP.
           02  LRCWK-PASS              PIC 99                 COMP.
           02  LCL-RCP-PASS            PIC 99                 COMP.
           02  TARRY-PASS              PIC 99                 COMP.


      /*****************************************************************
      *            PCT-OF-FWT STATE                                    *
      ******************************************************************
       01  PCTST.
           02  STATE                   PIC X(10).
           02  PCT-OF-FWT                                      COMP-1.


      /*****************************************************************
      *            SUPPLEMENTAL PCT                                    *
      ******************************************************************
       01  SUPPC.
           02  HIGH-SUPPL-AMT          PIC X       VALUE 'N'.
               88  HIGH-SUPPL-AMT-YES              VALUE 'Y'.
               88  HIGH-SUPPL-AMT-NO               VALUE 'N'.
           02  CT-NEW-HIRE-SW          PIC X       VALUE 'N'.
               88  CT-NEW-HIRE-YES                 VALUE 'Y'.
               88  CT-NEW-HIRE-NO                  VALUE SPACE.
           02  SUPPL-AMOUNT            PIC S9(11)V99           COMP-3.
           02  SUPPL-AMT-EXTRA         PIC S9(11)V99           COMP-3.
           02  SUPPL-BAL-YTD           PIC S9(11)V99           COMP-3.
           02  SUPPL-TXGRS             PIC S9(11)V99           COMP-3.
           02  SUPPL-TXGRS-TOT         PIC S9(11)V99           COMP-3.
           02  SUPPL-AMT-EXTRA-ST      PIC S9(11)V99           COMP-3.
           02  SUPPL-AMT-LESS-ST       PIC S9(11)V99           COMP-3.
           02  SUPPL-TXGRS-AGG         PIC S9(11)V99           COMP-3.
           02  SUPPL-TAX-AGG           PIC S9(11)V99           COMP-3.
           02  SUPPL-TXGRS-ACCUM       PIC S9(11)V99           COMP-3.
           02  CT-SUPPL-BAL            PIC S9(11)V99           COMP-3.
           02  CT-SUPPL-CUR            PIC S9(11)V99           COMP-3.
           02  SAV-WKST-TAX-CUR        PIC S9(8)V99            COMP-3.
           02  SAV-RECIPROCITY         PIC X        VALUE SPACE.
           02  SAV-STATE               PIC X(10)    VALUE SPACE.
           02  VT-HIGH-SUPPL-SW        PIC X.
               88  VT-HIGH-SUPPL-SW-YES           VALUE 'Y'.
               88  VT-HIGH-SUPPL-SW-NO            VALUE 'N'.
           02  VT-RECIPROCITY-SW       PIC X.
               88  VT-RECIPROCITY-SW-YES          VALUE 'Y'.
               88  VT-RECIPROCITY-SW-NO           VALUE 'N'.


      /*****************************************************************
      *            ADD-TO-GROSS SWITCH                                 *
      ******************************************************************
       01  ATGSW.
           02  ADD-TO-GROSS-SW         PIC X.
               88  ADD-TO-GROSS-NO                 VALUE 'N'.
               88  ADD-TO-GROSS-YES                VALUE 'Y'.


      /*****************************************************************
      *    AFTER TAX DEDUCTION REFUNDS                                 *
      ******************************************************************
       01  RFNTL.
           02  AFT-TAX-DED-REFUND-TTL  PIC S9(8)V99            COMP-3.


      /*****************************************************************
      *            BENEFIT RATE MANAGEMENT INTERFACE                   *
      ******************************************************************
       01  BENRT-PASS.                 COPY PSCBENRT.


      /*****************************************************************
      *            ADDITIONAL LIBRARY FUNCTION PARAMETERS              *
      ******************************************************************
       01  LIBWK.                      COPY PTCLIBWK.


      /*****************************************************************
      *            TAX CALC WORK DATA                                  *
      ******************************************************************
       01  TXWRK.                      COPY PSCTXWRK.


      /*****************************************************************
      *            TAX CALC LINKAGE DATA                               *
      ******************************************************************
       01  TXLNK.                      COPY PSCTXLNK.

      /*****************************************************************
      *            CALL TO TCALC ONLY FOR FRMWK                        *
      ******************************************************************
       01  PYNET-PASS.
           02  PYNET-FRMWK-PROCESS     PIC X.
               88  PYNET-FRMWK-PROCESS-YES         VALUE 'Y'.
               88  PYNET-FRMWK-PROCESS-NO          VALUE 'N'.
           02  PYNET-PROGID            PIC X(4)    VALUE SPACE.


      /*****************************************************************
      *                                                                *
       LINKAGE SECTION.


      /*****************************************************************
      *            SQL COMMUNICATION                                   *
      ******************************************************************
       01  SQLRT.                      COPY PTCSQLRT.


      /*****************************************************************
      *            PAYROLL SELECTION                                   *
      ******************************************************************
       01  PSLCT.                      COPY PSCPSLCT.


      /*****************************************************************
      *            EARNINGS ARRAY                                      *
      ******************************************************************
       01  EARRY.                      COPY PSCEARRY.


      /*****************************************************************
      *            DEDUCTION ARRAY                                     *
      ******************************************************************
       01  DARRY.                      COPY PSCDARRY.


      /*****************************************************************
      *            DEDUCTION TABLE 1                                   *
      ******************************************************************
       01  DEDT1.                      COPY PSCDEDT1.


      /*****************************************************************
      *            DEDUCTION TABLE 2                                   *
      ******************************************************************
       01  DEDT2.                      COPY PSCDEDT2.


      /*****************************************************************
      *            DEDUCTION TABLE 3                                   *
      ******************************************************************
       01  DEDT3.                      COPY PSCDEDT3.


      /*****************************************************************
      *            DEDUCTION TABLE 4                                   *
      ******************************************************************
       01  DEDT4.                      COPY PSCDEDT4.


      /*****************************************************************
      *            DEDUCTION TABLE 5                                   *
      ******************************************************************
       01  DEDT5.                      COPY PSCDEDT5.


      /*****************************************************************
      *            DEDUCTION TABLE 6                                   *
      ******************************************************************
       01  DEDT6.                      COPY PSCDEDT6.


      /*****************************************************************
      *            PAY GROUP                                           *
      ******************************************************************
       01  PYGRP.                      COPY PSCPYGRP.


      /*****************************************************************
      *            CHECK DATA                                          *
      ******************************************************************
       01  CHECK.                      COPY PSCCHECK.


      /*****************************************************************
      *            EARNINGS TYPE TABLE                                 *
      ******************************************************************
       01  ERNTB.                      COPY PSCERNTB.


      /*****************************************************************
      *            LIMIT RULES TABLE                                   *
      ******************************************************************
       01  LMTTB.                      COPY PSCLMTTB.


      /*****************************************************************
      *            PENSION RATE TABLE                                  *
      ******************************************************************
       01  PENRT.                      COPY PSCPENRT.


      /*****************************************************************
      *            SERVICE STEP TABLE                                  *
      ******************************************************************
       01  SVCST.                      COPY PSCSVCST.


      /*****************************************************************
      *            GARNISHMENT DEDUCTION ARRAY                         *
      ******************************************************************
       01  GARRY.                      COPY PSCGARRY.


      /*****************************************************************
      *            GARNISHMENT DISPOSABLE EARNINGS DEFINITION          *
      ******************************************************************
       01  GDERN.                      COPY PSCGDERN.


      /*****************************************************************
      *            GARNISHMENT RULES                                   *
      ******************************************************************
       01  GRULE.                      COPY PSCGRULE.


      /*****************************************************************
      *            SPECIAL EARNINGS ARRAY                              *
      ******************************************************************
       01  SARRY.                      COPY PSCSARRY.


      /*****************************************************************
      *            TAX ARRAY                                           *
      ******************************************************************
       01  TARRY.                      COPY PSCTARRY.


      /*****************************************************************
      *            TAX WORK DATA                                       *
      ******************************************************************
       01  TAXWK.                      COPY PSCTAXWK.


      /*****************************************************************
      *            TAX WORK DATA SAVE FOR GROSSUP LOOP                 *
      ******************************************************************
       01  TAXWK-SAVE                  PIC X(1200).


      /*****************************************************************
      *            TAX DATA                                            *
      ******************************************************************
       01  TAXDT.                      COPY PSCTAXDT.


      /*****************************************************************
      *            TAX DATA SAVE FOR GROSSUP LOOP                      *
      ******************************************************************
       01  TAXDT-SAVE                  PIC X(460).


      /*****************************************************************
      *            GROSS WORK                                          *
      ******************************************************************
       01  GRSWK.                      COPY PSCGRSWK.


      /*****************************************************************
      *            STATE TAX RATE TABLE                                *
      ******************************************************************
       01  STTRT.                      COPY PSCSTTRT.


      /*****************************************************************
      *            LOCAL TAX RATE TABLE                                *
      ******************************************************************
       01  LCLRT.                      COPY PSCLCLRT.


      /*****************************************************************
      *            SUT RATE TABLE                                      *
      ******************************************************************
       01  SUTRT.                      COPY PSCSUTRT.


      /*****************************************************************
      *            SDI RATE TABLE                                      *
      ******************************************************************
       01  SDIRT.                      COPY PSCSDIRT.


      /*****************************************************************
      *            VDI RATE TABLE                                      *
      ******************************************************************
       01  VDIRT.                      COPY PSCVDIRT.


      /*****************************************************************
      *            FLI RATE TABLE                                      *
      ******************************************************************
       01  FLIRT.                      COPY PSCFLIRT.


      /*****************************************************************
      *            SUT RECIPROCITY LOOKUP TABLE                        *
      ******************************************************************
       01  SUTTB.                      COPY PSCSUTTB.


      /*****************************************************************
      *            STATE RECIPROCITY RULE TABLE                        *
      ******************************************************************
       01  SRCTB.                      COPY PSCSRCTB.


      /*****************************************************************
      *            LOCAL RECIPROCITY RULE TABLE                        *
      ******************************************************************
       01  LRCTB.                      COPY PSCLRCTB.


      /*****************************************************************
      *            LOCAL RECIPROCITY RULE TABLE                        *
      ******************************************************************
       01  LWKRC.                      COPY PSCLWKRC.


      /*****************************************************************
      *            TAXABLE GROSS BASE TABLE                            *
      ******************************************************************
       01  TGBTB.                      COPY PSCTGBTB.


      /*****************************************************************
      *            TAXABLE GROSS COMPONENT TABLE                       *
      ******************************************************************
       01  TGCTB.                      COPY PSCTGCTB.


      /*****************************************************************
      *            CALCULATED TAX SET DATA                             *
      ******************************************************************
       01  TXARY.                      COPY PSCTXARY.


      /*****************************************************************HP99999
      *            TAX 1042 ARRAY                                      *HP99999
      ******************************************************************HP99999
       01  YARRY.                      COPY PSCYARRY.                   HP99999


      /*****************************************************************HP99999
      *            TAX 1042 TABLE                                      *HP99999
      ******************************************************************HP99999
       01  TRTTB.                      COPY PSCTRTTB.                   HP99999

      /*****************************************************************
      *            COMPANY/COMMON PAY ID TAX BALANCES                  *
      ******************************************************************
       01  CMPTT.                      COPY PSCCMPTT.


      /*****************************************************************
      *            FML RATE TABLE                                      *
      ******************************************************************
       01  FMLRT.                      COPY PSCFMLRT.

      /*****************************************************************
      *            FRAMEWORK DATA                                      *
      ******************************************************************
       01  FRMWK.                      COPY PSCFRMWK.


      /*****************************************************************
      *                                                                *
       PROCEDURE DIVISION USING    SQLRT
                                   PSLCT
                                   EARRY
                                   DARRY
                                   DEDT1
                                   DEDT2
                                   DEDT3
                                   DEDT4
                                   DEDT5
                                   DEDT6
                                   PYGRP
                                   CHECK
                                   ERNTB
                                   LMTTB
                                   PENRT
                                   SVCST
                                   GARRY
                                   GDERN
                                   GRULE
                                   SARRY
                                   TARRY
                                   TAXWK
                                   TAXWK-SAVE
                                   TAXDT
                                   TAXDT-SAVE
                                   GRSWK
                                   STTRT
                                   LCLRT
                                   SUTRT
                                   SDIRT
                                   VDIRT
                                   FLIRT
                                   SUTTB
                                   SRCTB
                                   LRCTB
                                   LWKRC
                                   TGBTB
                                   TGCTB
                                   TXARY
                                   YARRY                                HP99999
                                   TRTTB                                HP99999
                                   CMPTT
                                   FMLRT
                                   FRMWK
                                   .
      *                                                                *
      ******************************************************************
      *                                                                *
       AA000-MAIN SECTION.
       AA000.
      *                                                                *
      ******************************************************************

           COPY PTCLIBFX.

           SET PREPAY-NO OF W-SW  TO  TRUE                              HP00010
           SET HIGH-SUPPL-AMT-NO TO TRUE
           MOVE ZERO TO SUPPL-BAL-TOTAL OF W-WK
           MOVE ZERO TO SUPPL-BAL-EXTRA OF W-WK
           MOVE ZERO TO SUPPL-NON-TAX   OF W-WK
           MOVE ZERO TO SUPPL-TAXABLE   OF W-WK
           MOVE ZERO TO SUPPL-AMT-EXTRA-ST OF SUPPC
           MOVE ZERO TO SUPPL-AMT-LESS-ST  OF SUPPC
           MOVE ZERO TO CT-SUPPL-CUR OF SUPPC
           MOVE ZERO TO CT-SUPPL-BAL OF SUPPC
           SET VT-HIGH-SUPPL-SW-NO OF SUPPC TO TRUE
           MOVE ZERO TO PREV-DEDNET OF W-WK
           MOVE SPACE TO SAV-RECIPROCITY
           MOVE ZERO TO SAV-WKST-TAX-CUR
           MOVE ZERO TO RES-TAX-PRIOR OF W-WK
           SET PROC-SUPPL-FOR-CT-NO OF W-SW TO TRUE
           SET CT-NEW-HIRE-NO OF SUPPC       TO TRUE
           SET NEGATIVE-TAX-REQ-NO OF W-SW   TO TRUE
           SET TAX-CUR-ADDL-AMT-NOTADDED OF W-SW TO TRUE
           SET MO-TAX-ADDL-AMT-NOTADDED OF W-SW TO TRUE
           SET BALID-IDX TO BALID-IDX-FOR-CAL-YR
           MOVE ZERO TO PA-RES-LWT  OF GRSWK
           MOVE ZERO TO PA-RES-LOC-ADJ OF GRSWK
           MOVE ZERO TO PA-RES-FWT OF GRSWK
           MOVE ZERO TO PA-BLK-DED-AMT OF W-WK
           MOVE SPACE TO PA-WORK-PSD-CD-SAV OF W-WK
           SET PA-EARN-YES OF TGCTB-PASS  TO  TRUE
           INITIALIZE PA-LGRS-ACCUM-LEVEL OF GRSWK(1)
           INITIALIZE PA-LGRS-ACCUM-LEVEL OF GRSWK(2)
           MOVE ZERO TO PA-TOT-125-DED OF W-WK
           MOVE ZERO TO PA-PERC-DED OF W-WK
           MOVE ZERO TO PA-LOC-RES-TXGRS-ADJ OF W-WK
           MOVE ZERO TO TAX-CUR-SAV OF W-WK
           MOVE ZERO TO PRETAX-DED-NOT-APPLIED OF GRSWK
           SET TXGRS-EFF-SUBTR-NO OF GRSWK TO TRUE
           SET PHIL-RES-WRK-OSTATE-NO OF GRSWK TO TRUE
           SET TAX-TIPS-OASDI-EE-NO OF TAXWK TO TRUE
           SET TAX-TIPS-MHI-EE-NO OF TAXWK TO TRUE
           MOVE ZERO TO TAX-SUPP-PRIOR-RES OF GRSWK
           MOVE ZERO TO TAX-SUPP-PRIOR-WORK OF GRSWK
           MOVE ZERO TO TAX-SUPP-REG-AGG    OF GRSWK
           MOVE ZERO TO TAX-SUPP-TAX-WITHHELD OF GRSWK
           MOVE ZERO TO TAX-AGG-RES-RECALC OF GRSWK
           MOVE ZERO TO TXGRS-PRIOR-RES OF GRSWK
           MOVE ZERO TO TXGRS-PRIOR-AGG-RES OF GRSWK
           MOVE ZERO TO TXGRS-PRIOR-SUPP-WORK OF GRSWK
           MOVE ZERO TO LOC-NET-WAGES OF GRSWK 
           MOVE ZERO TO WK-REG-RECIP-TAX OF W-WK
           MOVE SPACE TO SUPPL-METHOD-USED-SW OF GRSWK
           MOVE SPACE TO TAX-METHOD-SAV OF W-WK
           MOVE ZERO TO NBR-OF-STATES OF GRSWK
           MOVE ZERO TO PRIOR-GRS-STATE-RCP OF W-WK
           MOVE ZERO TO PRIOR-RECIPROCITY OF W-WK
           MOVE ZERO TO PRE-FACTOR-TAX-CUR OF W-WK
           MOVE ZERO TO FACTOR-TAX-CUR OF W-WK
           MOVE ZERO TO FACTOR-TXGRS-CUR OF W-WK
           SET FIRST-TAX-METHOD-FOUND-NO OF W-SW TO TRUE
           MOVE ZERO TO PRIOR-TAX-SUPP-CUR OF W-WK
           SET STATE-RECIP-WITH-LOCAL-NO OF GRSWK TO TRUE
           SET STATE-RCP-TAKEN-NO OF W-SW TO TRUE
           SET LOCAL-RCP-TO-BE-TAKEN-NO OF W-SW TO TRUE
           SET TXGRS-AGG-THIS-CHK-NO  OF GRSWK TO TRUE
           SET MULTIPLE-SUPPL-FOUND-NO OF W-SW TO TRUE
           SET MO-SUPPLE-NO OF W-SW TO TRUE
           SET MO-WRK-OUTSIDE-YES OF W-SW TO TRUE
           SET MO-WRK-CALCED-NO  OF W-SW TO TRUE
           SET REDTTL-ADJ-GR0SS-NO OF GRSWK TO TRUE
           SET TAX-METHOD-ANN-NO OF TAXWK TO TRUE
           SET TAX-METHOD-SUP-NO OF TAXWK TO TRUE
           SET MULTI-TAX-METHOD-NO OF TAXWK TO TRUE
           SET CALC-TRANSIT-NO OF TAXWK TO TRUE
           SET CALC-LTC-NO     OF TAXWK TO TRUE
           SET CALC-WA-FLI-NO  OF TAXWK TO TRUE           
           SET CALC-CT-FML-NO  OF TAXWK TO TRUE
           SET WS-FML-MULTI-ST-NO OF TAXWK TO TRUE
           SET WA-FLI-MULTI-ST-NO OF TAXWK TO TRUE
           SET WA-FLI-EARNS-NO OF TAXWK TO TRUE
           SET STATE-NEG-RCP-NO OF W-SW TO TRUE
           MOVE ZERO TO WA-FLI-TOT-PREM OF TAXWK
           MOVE ZERO TO WA-FLI-TOT      OF TAXWK
           MOVE ZERO TO WA-MLI-TOT      OF TAXWK
           MOVE ZERO TO WA-FLI-EE-PREM  OF TAXWK
           MOVE ZERO TO WA-FLI-ER-PREM  OF TAXWK
           MOVE ZERO TO WA-VFLI-EE-PREM OF TAXWK
           MOVE ZERO TO WA-VFLI-ER-PREM OF TAXWK
           MOVE ZERO TO WA-FLI-SPLIT-RT OF TAXWK
           MOVE ZERO TO WA-MLI-SPLIT-RT OF TAXWK
           MOVE ZERO TO WA-MLI-EE-PREM  OF TAXWK
           MOVE ZERO TO WA-MLI-ER-PREM  OF TAXWK
           MOVE ZERO TO WA-VMLI-EE-PREM OF TAXWK
           MOVE ZERO TO WA-VMLI-ER-PREM OF TAXWK           
           MOVE ZERO TO TAX-STATE-RCP-ANN OF W-WK
           MOVE ZERO TO TAX-STATE-RCP-SUP OF W-WK
           MOVE ZERO TO TAX-SUP-PRIOR-WRK OF W-WK
           MOVE ZERO TO MO-RCP-WRK-AMT OF W-WK
           MOVE ZERO TO MO-COMP-WRK-AMT OF W-WK
           MOVE ZERO TO MO-GRSWK-AMT OF W-WK
           MOVE ZERO TO STATE-COUNT OF GRSWK
           MOVE ZERO TO T-FWT-TTL OF W-WK
           MOVE ZERO TO T-FWT-CUM OF W-WK
           MOVE ZERO TO AG-FWT-CUM OF W-WK
           SET SPEC-SWT-EXEMPT-NO OF W-SW TO TRUE
           SET AGT-FWT-NEG-NO OF W-SW TO TRUE
           MOVE 50   TO STATES-COUNT-MAX OF GRSWK
           MOVE SPACE TO PAY-FREQ-TYPE OF W-WK
           SET MULTI-PAY-FREQ-NO OF PYGRP TO TRUE
           SET FRMWK-STATE-TAX-NO OF TAXWK TO TRUE
           SET FRMWK-STATE-CALCED-NO OF TAXWK TO TRUE
           SET PYNET-FRMWK-PROCESS-NO OF PYNET-PASS TO TRUE
           MOVE ZERO TO MANUAL-FUT-AMT OF W-WK
           MOVE ZERO TO MANUAL-FUT-DED OF W-WK

           INITIALIZE WK-PFF-CALC OF TAXWK

           PERFORM VARYING STATES-IDX FROM 1 BY 1
                       UNTIL STATES-IDX > STATES-COUNT-MAX OF GRSWK
                  INITIALIZE WORK-STATE-DATA OF GRSWK(STATES-IDX)
           END-PERFORM

           IF TAXGRS-EMPLID OF GRSWK NOT = EMPLID OF CHECK

              MOVE ZERO TO TAXGRS-CMP-COUNT OF GRSWK
              MOVE 5000 TO TAXGRS-COUNT-MAX   OF GRSWK
              MOVE EMPLID OF CHECK TO  TAXGRS-EMPLID OF GRSWK
              PERFORM VARYING L-TAXGRS-IDX  FROM  1  BY  1
                          UNTIL L-TAXGRS-IDX
                                  >  TAXGRS-COUNT-MAX OF GRSWK
                     INITIALIZE TAXGRS-CMP-DATA OF GRSWK(L-TAXGRS-IDX)
              END-PERFORM
           END-IF

           IF DEDUCTION-COUNT OF DARRY  >  ZERO                         HP00010

               SET DARRY-IDX  TO  DEDUCTION-COUNT OF DARRY              HP00010

               IF DEDCD OF DARRY(DARRY-IDX)  =  'PREPAY'                HP00010

                  SUBTRACT 1  FROM  DEDUCTION-COUNT OF DARRY            HP00010
                  SET PREPAY-YES OF W-SW  TO  TRUE                      HP00010
               END-IF                                                   HP00010

           END-IF                                                       HP00010

           MOVE ZERO  TO  FUT-EARNINGS OF TGCTB-PASS
           MOVE ZERO  TO  FICA-EARNINGS OF TGCTB-PASS

           PERFORM DA000-INITIALIZE
           PERFORM LA100-INIT-DED-SOURCE-CNTRS
           PERFORM LA200-INIT-DED-ALLOC-CNTRS
           PERFORM LA400-TOTAL-ALL-TAX-SETS

           IF CALC-TAX-INITIAL OF GRSWK

               PERFORM JA100-SAVE-STARTING-DATA
           END-IF

           IF CALC-TAX-YES OF GRSWK

               PERFORM JA200-RESTORE-START-DATA
           END-IF
           SET DED-FOUND-NO OF W-SW TO TRUE
           PERFORM HA000-CALC-DEDUCTIONS
           PERFORM JA000-INIT-GROSS-WORK
           SET PROCESS-NON-TIPS OF GRSWK TO TRUE
           SUBTRACT TXBL-TIPS OF GRSWK FROM TXBL-TTL OF GRSWK

           PERFORM LA000-CALC-TAXES
           PERFORM MA000-ADJ-NEG-TAXES

           PERFORM TA000-SAVE-NON-TIPS-CALC

           SET TIPS-ERNS-NO OF W-SW TO TRUE
           SET TIPS-WA-NO OF W-SW TO TRUE

           PERFORM VARYING TARRY-IDX  FROM  1  BY  1
                   UNTIL TARRY-IDX  >  TAXCALC-COUNT OF TARRY
                           OR TIPS-WA-YES OF W-SW
                           OR TIPS-ERNS-YES OF W-SW

              IF STATE OF TARRY(TARRY-IDX) = 'WA'

                 SET TIPS-WA-YES OF W-SW TO TRUE

                 PERFORM VARYING EARRY-IDX  FROM  1  BY  1
                   UNTIL EARRY-IDX  >  EARNINGS-COUNT OF EARRY
                           OR PAY-LINE-STATUS-ERROR OF CHECK
                           OR TIPS-ERNS-YES OF W-SW

                   SET ERNTB-IDX  TO  ERNTB-PTR OF EARRY(EARRY-IDX)

                   IF TIPS-CATEGORY-TIPS OF ERNTB(ERNTB-IDX)
                     SET TIPS-ERNS-YES OF W-SW  TO TRUE
                   END-IF

                 END-PERFORM

              END-IF

           END-PERFORM
           
           IF TIPS-WA-NO OF W-SW
              SET TIPS-ERNS-YES OF W-SW  TO TRUE
           END-IF             

           IF TIPS-FOUND OF CHECK AND TIPS-ERNS-YES OF W-SW
               ADD TXBL-TIPS OF GRSWK TO TXBL-TTL OF GRSWK
               PERFORM DA000-INITIALIZE
               PERFORM PD100-CLEAR-TAX-EXCEPT-TIPS
               PERFORM JA000-INIT-GROSS-WORK
               SET PROCESS-ALL OF GRSWK TO TRUE
               PERFORM LA000-CALC-TAXES
               PERFORM MA000-ADJ-NEG-TAXES
               PERFORM TB000-SAVE-TIPS-CALC

           END-IF

           PERFORM NA000-DEDUCT-TAXES

           PERFORM SA100-INITIALIZE-DEARY

           IF GARN-FOUND-YES OF W-SW

               PERFORM SA000-GARNISHMENT-PROCESS
           END-IF

           PERFORM SB000-SAVE-PREV-DED-AMT

           IF GROSSUP-WORK-YES OF GRSWK

               PERFORM PA000-GROSSUP-LOOP
           END-IF

           IF GROSSUP-WORK-YES OF GRSWK
               PERFORM PD000-CLEAR-TAX-CALC
               PERFORM PD200-CLEAR-DED-CALC
           ELSE

               PERFORM RA000-TAKE-DEDUCTIONS
           END-IF

           IF PREPAY-YES OF W-SW                                        HP00010

               ADD 1  TO  DEDUCTION-COUNT OF DARRY                      HP00010
           END-IF                                                       HP00010

           .
       MAIN-EXIT.
           EXIT PROGRAM.

      /*****************************************************************
      *                                                                *
       DA000-INITIALIZE SECTION.
       DA000.
      *                                                                *
      ******************************************************************

           MOVE ZERO  TO  BEFORE-TAX-NOT-APPLIED OF TGCTB-PASS
           MOVE ZERO  TO  FUT-SUBTR-NOT-APPLIED OF TGCTB-PASS
           MOVE ZERO  TO  FICA-SUBTR-NOT-APPLIED OF TGCTB-PASS
           MOVE ZERO  TO  BEFORE-TAX-NOT-APPLIED OF W-WK
           MOVE ZERO  TO  FUT-SUBTR-NOT-APPLIED OF W-WK
           MOVE ZERO  TO  FICA-SUBTR-NOT-APPLIED OF W-WK
           SET EARNS-NEG-NO OF W-SW  TO  TRUE
           MOVE 100  TO  REDUCE-PCT OF W-WK
           MOVE ZERO  TO  CREDIT-LIM-RT OF W-WK
           SET REDRES-NO-ADJ-TARRY TO TRUE
           MOVE ZERO TO TAX-ON-RES-EARNS OF W-WK
           SET STATE-RCP-TAKEN-NO OF W-SW TO TRUE
           SET LOCAL-RCP-TO-BE-TAKEN-NO OF W-SW TO TRUE
           MOVE ZERO  TO  TTL-DED-NOT-TAKEN OF W-WK
           SET FICA-CREDIT-APPLIED-NO OF TAXWK TO TRUE                  HP90003
           MOVE ZERO TO SUPPL-AMOUNT OF SUPPC
           MOVE ZERO TO SUPPL-AMT-EXTRA OF SUPPC
           MOVE ZERO TO SUPPL-BAL-YTD OF SUPPC
           MOVE ZERO TO SUPPL-TXGRS OF SUPPC
           MOVE ZERO TO SUPPL-TXGRS-TOT OF SUPPC
           MOVE ZERO TO SUPPL-TXGRS-AGG OF SUPPC
           MOVE ZERO TO SUPPL-TAX-AGG OF SUPPC
           MOVE ZERO TO SUPPL-TXGRS-ACCUM OF SUPPC
           MOVE ZERO TO CT-SUPPL-CUR OF SUPPC
           MOVE ZERO TO LOCAL-RCP-SAVE OF W-WK
           MOVE ZERO TO PAID-LEAVE-TXGRS OF GRSWK
           MOVE ZERO TO PAID-LEAVE-NLGRS OF GRSWK
           SET SUPPL-LOCAL-RCP-NO  TO  TRUE
           SET NEG-LOCAL-RCP-NO    TO  TRUE
           .
       INITIALIZE-EXIT.

      /*****************************************************************
      *                                                                *
       HA000-CALC-DEDUCTIONS SECTION.
       HA000.
      *                                                                *
      ******************************************************************

           SET GARN-FOUND-NO OF W-SW  TO  TRUE

           MOVE SPACE  TO  ADJ-PAY-PRD OF W-PASS

           IF PAYCHECK-ADJUST-YES OF CHECK

               MOVE SPACES  TO  HOLD-EARN-END-DT OF W-WK

               PERFORM VARYING EARRY-IDX FROM 1  BY  1
                       UNTIL EARRY-IDX
                               >  EARNINGS-COUNT OF EARRY

                   IF HOLD-EARN-END-DT OF W-WK  =  SPACES
                       OR  EARNS-END-DT OF EARRY(EARRY-IDX)
                               >  HOLD-EARN-END-DT OF W-WK

                       MOVE EARNS-END-DT OF EARRY(EARRY-IDX)
                               TO  HOLD-EARN-END-DT OF W-WK
                   END-IF
               END-PERFORM

               IF HOLD-EARN-END-DT OF W-WK
                       NOT =  PAY-END-DT OF PSLCT

                   PERFORM HA500-GET-ADJ-PERIOD

                   IF ADJ-PAY-PRD OF W-PASS  =  SPACE

                       MOVE PAY-PERIOD OF PYGRP
                               TO  ADJ-PAY-PRD OF W-PASS
                   END-IF
               ELSE

                   MOVE PAY-PERIOD OF PYGRP
                           TO  ADJ-PAY-PRD OF W-PASS
               END-IF
           END-IF

           MOVE ZERO TO PENSION-DED-CUR OF TAXWK                        HP90003
           MOVE ZERO TO PENSION-DED-YTD OF TAXWK                        HP90003
           MOVE ZERO TO PENSION-DED-EST OF TAXWK                        HP90003

           IF BEN-DED-CLS-P-THEN-A OF PSLCT

               SET FIRST-DED-CLASS-PTAX OF W-WK  TO  TRUE
               SET SECOND-DED-CLASS-ATAX OF W-WK  TO  TRUE
           ELSE

               SET FIRST-DED-CLASS-ATAX OF W-WK  TO  TRUE
               SET SECOND-DED-CLASS-PTAX OF W-WK  TO  TRUE
           END-IF

           PERFORM VARYING DARRY-IDX  FROM  1  BY  1
                   UNTIL DARRY-IDX  >  DEDUCTION-COUNT OF DARRY

               SET DEDT1-IDX  TO  DEDTB-PTR OF DARRY(DARRY-IDX)

               IF SPCL-PROCESS-GARN OF DEDT1(DEDT1-IDX)
                   AND (GNL-DED-TAKEN-TABLE OF CHECK
                   OR  DED-TAKEN-GNL OF CHECK  =  SPACE
                   OR (GNL-DED-TAKEN-SUBSET OF CHECK
                   AND DED-SUBSET-YES OF DEDT1(DEDT1-IDX)))

                   SET GARN-FOUND-YES OF W-SW  TO  TRUE

                   IF CALC-TAX-YES OF GRSWK

                       MOVE ZERO  TO  DED-CUR OF DARRY(DARRY-IDX 1)
                   END-IF
               ELSE

                   PERFORM VARYING DCLAS-IDX  FROM  1  BY  1
                           UNTIL DCLAS-IDX
                                   >  DEDUCTION-CLASS-MAX OF DARRY
                                   OR DED-CLASS
                                         OF DARRY(DARRY-IDX DCLAS-IDX)
                                           =  SPACE

                       IF DED-CLASS-BEFORE-TAX OF DARRY
                                                 (DARRY-IDX DCLAS-IDX)

                           SET DEDT1-IDX  TO  DEDTB-PTR OF DARRY
                                                          (DARRY-IDX)
                           SET DEDT5-IDX  TO  DEDTB-PTR OF DARRY
                                                          (DARRY-IDX)
                           SET DEDT6-IDX  TO  DEDTB-PTR OF DARRY
                                                          (DARRY-IDX)
                           SET DEDC1-IDX  TO  DCLAS-IDX
                           SET DEDC2-IDX  TO  DCLAS-IDX

                           PERFORM XA000-CALC-DEDUCTION

                           SET DCLAS-IDX  TO  DEDUCTION-CLASS-MAX
                                                       OF DARRY
                       END-IF
                   END-PERFORM

                   PERFORM VARYING DCLAS-IDX  FROM  1  BY  1
                           UNTIL DCLAS-IDX
                                   >  DEDUCTION-CLASS-MAX OF DARRY
                                   OR DED-CLASS
                                        OF DARRY(DARRY-IDX DCLAS-IDX)
                                           =  SPACE

                       IF DED-CLASS OF DARRY(DARRY-IDX DCLAS-IDX)
                          = FIRST-DED-CLASS OF W-WK

                           SET DEDT1-IDX  TO  DEDTB-PTR OF DARRY
                                                          (DARRY-IDX)
                           SET DEDT5-IDX  TO  DEDTB-PTR OF DARRY
                                                          (DARRY-IDX)
                           SET DEDT6-IDX  TO  DEDTB-PTR OF DARRY
                                                          (DARRY-IDX)
                           SET DEDC1-IDX  TO  DCLAS-IDX
                           SET DEDC2-IDX  TO  DCLAS-IDX

                           PERFORM XA000-CALC-DEDUCTION
                       END-IF
                   END-PERFORM

                   PERFORM VARYING DCLAS-IDX  FROM  1  BY  1
                           UNTIL DCLAS-IDX
                                   >  DEDUCTION-CLASS-MAX OF DARRY
                                   OR DED-CLASS
                                        OF DARRY(DARRY-IDX DCLAS-IDX)
                                           =  SPACE

                       IF DED-CLASS OF DARRY(DARRY-IDX DCLAS-IDX)
                          = SECOND-DED-CLASS OF W-WK

                           SET DEDT1-IDX  TO  DEDTB-PTR OF DARRY
                                                          (DARRY-IDX)
                           SET DEDT5-IDX  TO  DEDTB-PTR OF DARRY
                                                          (DARRY-IDX)
                           SET DEDT6-IDX  TO  DEDTB-PTR OF DARRY
                                                          (DARRY-IDX)
                           SET DEDC1-IDX  TO  DCLAS-IDX
                           SET DEDC2-IDX  TO  DCLAS-IDX

                           PERFORM XA000-CALC-DEDUCTION
                       END-IF
                   END-PERFORM

                   PERFORM VARYING DCLAS-IDX  FROM  1  BY  1
                           UNTIL DCLAS-IDX
                                   >  DEDUCTION-CLASS-MAX OF DARRY
                                   OR DED-CLASS
                                        OF DARRY(DARRY-IDX DCLAS-IDX)
                                           =  SPACE

                       IF DED-CLASS-BEFORE-TAX OF DARRY
                                                 (DARRY-IDX DCLAS-IDX)
                           OR DED-CLASS-AFTER-TAX OF DARRY
                                                 (DARRY-IDX DCLAS-IDX)
                           OR DED-CLASS-NON-TAX-BT OF DARRY
                                                 (DARRY-IDX DCLAS-IDX)

                           CONTINUE
                       ELSE

                           SET DEDT1-IDX  TO  DEDTB-PTR OF DARRY
                                                          (DARRY-IDX)
                           SET DEDT5-IDX  TO  DEDTB-PTR OF DARRY
                                                          (DARRY-IDX)
                           SET DEDT6-IDX  TO  DEDTB-PTR OF DARRY
                                                          (DARRY-IDX)
                           SET DEDC1-IDX  TO  DCLAS-IDX
                           SET DEDC2-IDX  TO  DCLAS-IDX

                           PERFORM XA000-CALC-DEDUCTION
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM

           IF DED-FOUND-NO  OF W-SW  AND                                HP90003
              ((PUBLIC-SECTOR-YES OF PSLCT AND                          HP90003
                FICA-CREDIT-YES OF PSLCT)                               HP90003
              OR (GOVERNMENT OF PSLCT AND                               FED0999
                  US-FEDERAL-GOVT OF PSLCT AND                          HP90003
                  FICA-CREDIT-YES OF PSLCT))                            FED0999
               PERFORM XE000-SEL-FICA-CR-DED-YTD                        HP90003
               PERFORM XF000-FICA-CREDIT-DED-AMT                        HP90003
           END-IF                                                       HP90003

           .
       CALC-DEDUCTIONS-EXIT.


      /*****************************************************************
      *                                                                *
       HA500-GET-ADJ-PERIOD SECTION.
       HA500.
      *                                                                *
      ******************************************************************

           MOVE COMPANY OF PSLCT  TO  COMPANY OF S-REV-ADJ
           MOVE PAYGROUP OF PSLCT  TO  PAYGROUP OF S-REV-ADJ
           MOVE HOLD-EARN-END-DT OF W-WK
                   TO  PAY-END-DT OF S-REV-ADJ

           CALL 'PTPSQLRT' USING   ACTION-SELECT OF SQLRT
                                   SQLRT
                                   SQL-CURSOR-COMMON OF SQLRT
                                   SQL-STMT OF S-REV-ADJ
                                   BIND-SETUP OF S-REV-ADJ
                                   BIND-DATA OF S-REV-ADJ
                                   SELECT-SETUP OF S-REV-ADJ
                                   SELECT-DATA OF S-REV-ADJ
           IF RTNCD-ERROR OF SQLRT

               MOVE 'GET-ADJ-PERIOD(SELECT)'
                       TO  ERR-SECTION OF SQLRT
               PERFORM ZZ000-SQL-ERROR
           END-IF

           INITIALIZE SELECT-DATA OF S-REV-ADJ

           CALL 'PTPSQLRT' USING   ACTION-FETCH OF SQLRT
                                   SQLRT
                                   SQL-CURSOR-COMMON OF SQLRT
           IF RTNCD-ERROR OF SQLRT

               MOVE 'GET-ADJ-PERIOD(FETCH)'
                       TO  ERR-SECTION OF SQLRT
               PERFORM ZZ000-SQL-ERROR
           END-IF

           MOVE PAY-PERIOD OF S-REV-ADJ  TO  ADJ-PAY-PRD OF W-PASS

           .
       GET-ADJ-PERIOD-EXIT.


      /*****************************************************************
      *                                                                *
       JA000-INIT-GROSS-WORK SECTION.
       JA000.
      *                                                                *
      ******************************************************************

           INITIALIZE ACCUM-LEVEL OF GRSWK(1)
           INITIALIZE ACCUM-LEVEL OF GRSWK(2)
           SET SUT-RECALC-NO OF GRSWK(1) TO TRUE
           SET SUT-RECALC-NO OF GRSWK(2) TO TRUE
           INITIALIZE STATE-RCP OF GRSWK
           MOVE LOCALITY-COUNT OF RESIDENCE OF TAXWK
                   TO  SAV-LOCAL-CNT OF W-WK
           SET LOCALITY-COUNT-MAX OF RESIDENCE OF TAXWK  TO  TRUE
           PERFORM VARYING LRCWK-IDX FROM  1  BY  1
                   UNTIL LRCWK-IDX >  LOCALITY-COUNT OF RESIDENCE
                                                  OF TAXWK

               INITIALIZE LOCAL-RCP OF GRSWK(LRCWK-IDX)
           END-PERFORM
           MOVE SAV-LOCAL-CNT OF W-WK
                   TO  LOCALITY-COUNT OF RESIDENCE OF TAXWK
           INITIALIZE WHADJ
           INITIALIZE TGBTB-PASS
           PERFORM JB000-INIT-TAX1042-GROSS-WORK                        HP00009

           .
       INIT-GROSS-WORK-EXIT.


      /*****************************************************************
      *                                                                *
       JA050-INIT-WORK-ACCUM SECTION.
       JA050.
      *                                                                *
      ******************************************************************

           INITIALIZE ACCUM-LEVEL OF GRSWK(1)
           SET SUT-RECALC-NO OF GRSWK(1) TO TRUE
           INITIALIZE ACCUM-LEVEL OF WHADJ(1)

           .
       INIT-WORK-ACCUM-EXIT.


      /*****************************************************************
      *                                                                *
       JA100-SAVE-STARTING-DATA SECTION.
       JA100.
      *                                                                *
      * SAVE EARNINGS TOTALS FOR POSSIBLE RECALC                       *
      *                                                                *
      ******************************************************************

           MOVE DEDNET OF GRSWK  TO  SAV-DEDNET OF W-WK
           MOVE CHECK-NET OF GRSWK  TO  SAV-CHECK-NET OF W-WK
           MOVE TTLDED OF GRSWK  TO  SAV-TTLDED OF W-WK
           MOVE TTLTAX OF GRSWK  TO  SAV-TTLTAX OF W-WK
           MOVE TXBL-TTL OF GRSWK  TO  HOLD-TXBL-TTL OF W-TTL
           MOVE PA-OPT-YTD OF TARRY TO WK-PA-OPT-YTD OF W-WK

           .
       SAVE-STARTING-DATA-EXIT.


      /*****************************************************************
      *                                                                *
       JA200-RESTORE-START-DATA SECTION.
       JA200.
      *                                                                *
      * RESTORE EARNINGS TOTALS FOR RECALC                             *
      *                                                                *
      ******************************************************************

           MOVE SAV-DEDNET OF W-WK  TO  DEDNET OF GRSWK
           MOVE SAV-CHECK-NET OF W-WK  TO  CHECK-NET OF GRSWK
           MOVE SAV-TTLDED OF W-WK  TO  TTLDED OF GRSWK
           MOVE SAV-TTLTAX OF W-WK  TO  TTLTAX OF GRSWK
           MOVE HOLD-TXBL-TTL OF W-TTL  TO  TXBL-TTL OF GRSWK

           .
       RESTORE-START-DATA-EXIT.


      /*****************************************************************HP00009
      *                                                                *HP00009
       JB000-INIT-TAX1042-GROSS-WORK SECTION.                           HP00009
       JB000.                                                           HP00009
      *                                                                *HP00009
      ******************************************************************HP00009
                                                                        HP00009
           INITIALIZE 1042-GRS OF GRSWK(1)                              HP00009
           INITIALIZE 1042-GRS OF GRSWK(2)                              HP00009
           INITIALIZE 1042-GRS OF GRSWK(3)                              HP00009
           INITIALIZE 1042-GRS OF GRSWK(4)                              HP00009
           INITIALIZE 1042-GRS OF GRSWK(5)                              HP00009
           INITIALIZE 1042-GRS OF GRSWK(6)                              HP00009
           INITIALIZE 1042-GRS OF GRSWK(7)                              HP00009
           INITIALIZE 1042-GRS OF GRSWK(8)                              HP00009
           INITIALIZE 1042-GRS OF GRSWK(9)                              HP00009
           INITIALIZE 1042-GRS OF GRSWK(10)                             HP00009
           INITIALIZE 1042-GRS OF GRSWK(11)                             HP00009
           INITIALIZE 1042-GRS OF GRSWK(12)                             HP00009
           INITIALIZE 1042-GRS OF GRSWK(13)                             HP00009
           INITIALIZE 1042-GRS OF GRSWK(14)                             HP00009
           INITIALIZE 1042-GRS OF GRSWK(15)                             HP00009
           INITIALIZE 1042-GRS OF GRSWK(16)                             HP00009
           INITIALIZE 1042-GRS OF GRSWK(17)                             HP00009
           INITIALIZE 1042-GRS OF GRSWK(18)                             HP00009
           INITIALIZE 1042-GRS OF GRSWK(19)                             HP00009
           INITIALIZE 1042-GRS OF GRSWK(20)                             HP00009
                                                                        HP00009
           MOVE ZERO TO 1042-GRS-COUNT OF GRSWK                         HP00009
           MOVE ZERO TO TX-DED-TTL OF GRSWK                             HP99995
           MOVE ZERO TO SUBJ-1042-DED OF GRSWK                          HP99994
                                                                        HP00009
           .                                                            HP00009
       INIT-TAX1042-GROSS-WORK-EXIT.                                    HP00009


      /*****************************************************************
      *                                                                *
       LA000-CALC-TAXES SECTION.
       LA000.
      *                                                                *
      * ACCUM GROSSES AND CALC TAXES FOR EACH TAX SET                  *
      *                                                                *
      ******************************************************************

      * CALCULATE FEDERAL TAXES

           PERFORM LA200-INIT-DED-ALLOC-CNTRS

           MOVE SPACES TO TAX-METHOD OF TAXWK
           MOVE SPACES TO WORK-LOCALITY OF W-WK
           SET CALC-TAX-NO OF GRSWK  TO  TRUE
           SET CALC-FED-WH OF TAXWK  TO  TRUE
           SET TAX-CALCD-YES OF W-WK TO TRUE
           SET CALC-FWT-ADDL-NO  OF TAXWK TO TRUE
           SET MULTI-STATES-NO OF GRSWK TO TRUE
           SET WA-SEA-TXGRS-EFF-NO OF GRSWK TO TRUE
           SET WA-SEA-TXGRS-FOUND-NO OF GRSWK TO TRUE
           MOVE ZERO TO WA-SEA-TXGRS-ADJ OF GRSWK
           MOVE ZERO  TO  ADJ-EARNINGS OF W-WK
           MOVE ZERO  TO  NRA-DED-ADJ OF GRSWK                          HP99995
           MOVE ZERO  TO WK-EARNS-TTL OF W-WK                           HP99994

           IF PUBLIC-SECTOR-YES OF PSLCT                                HP99994
               PERFORM LA700-PREPARE-1042-ARRAY                         HP99994
           END-IF                                                       HP99994

           IF EARNS-SUPPLE-YES OF W-SW AND EARNS-SPCL-YES OF W-SW
              AND AGT-TTL-FWT-NEG OF W-WK NOT = ZERO
              ADD AGT-TTL-FWT-NEG OF W-WK TO AGT-TTL-FWT OF W-WK
           END-IF

           MOVE AGT-TTL-FWT OF W-WK TO AGT-TTL-TS OF W-WK

           PERFORM VARYING EARRY-IDX  FROM  1  BY  1
                   UNTIL EARRY-IDX  >  EARNINGS-COUNT OF EARRY
                           OR PAY-LINE-STATUS-ERROR OF CHECK
               SET ERNTB-IDX  TO  ERNTB-PTR OF EARRY(EARRY-IDX)
               SET TAX-CALCD-NO OF W-WK TO TRUE
               SET EARNS-NOT-SUPPL-FOUND-NO TO TRUE

               IF NOT TAX-METHOD-SUPPLEMENTAL OF EARRY(EARRY-IDX)

                   SET EARNS-NOT-SUPPL-FOUND-YES TO TRUE
               END-IF

               IF TAX-METHOD-SUPPLEMENTAL OF EARRY(EARRY-IDX)
                  AND EARNS-END-DT OF EARRY(EARRY-IDX) >= '2005-01-01'

                  IF NOT PAYCHECK-MODELING-YES OF PSLCT

                      PERFORM LA450-GET-SUPPL-BAL-OTHR-CHK
                  END-IF

                  IF HIGH-SUPPL-AMT-YES OF SUPPC

                     COMPUTE SUPPL-AMT-EXTRA OF SUPPC =
                        SUPPL-AMT-EXTRA OF SUPPC +
                                   EARNS OF EARRY(EARRY-IDX)
                  ELSE

                     PERFORM LA460-GET-SUPPL-BAL-CAL-YR

                  END-IF

               END-IF

               IF DEDCD-PAYBACK OF ERNTB(ERNTB-IDX)
                       NOT =  SPACE

                   COMPUTE ADJ-EARNINGS OF W-WK
                           =  ADJ-EARNINGS OF W-WK
                           +  EARNS OF EARRY(EARRY-IDX)
               END-IF

               IF PROCESS-ALL OF GRSWK OR
                 NOT TIPS-CATEGORY-TIPS OF ERNTB(ERNTB-IDX)

                   MOVE TAX-METHOD OF EARRY(EARRY-IDX)
                   TO TAX-METHOD OF W-WK
                   IF TAX-METHOD OF EARRY(EARRY-IDX) = 'X'
                      MOVE 'S' TO TAX-METHOD OF EARRY(EARRY-IDX)
                   END-IF

                   IF TAX-SET OF EARRY(EARRY-IDX)  =  TAX-SET OF TAXWK
                     OR TAX-METHOD OF TAXWK  =  SPACES

                       IF EARRY-IDX = 1

                          MOVE TAX-SET OF EARRY(EARRY-IDX)
                               TO  TAX-SET OF TAXWK
                          MOVE EMPL-RCD-NO OF EARRY(EARRY-IDX)
                               TO EMPL-RCD-NO OF W-WK
                       END-IF

                       PERFORM LG000-GROSS-CALC

                   ELSE

                       IF FICA-STATUS-EE OF TAXWK                       HP99999
                               NOT = FICA-STATUS-EE OF EARRY(EARRY-IDX) HP99999

                               SET CHANGE-FICA-YES    OF TAXWK TO TRUE  HP99997
                               SET CALC-FED-FICA-ONLY OF TAXWK TO TRUE  HP99997
                               SET CHANGE-FICA-YES-SAV OF W-SW TO TRUE
                       ELSE                                             HP99999
                               SET CHANGE-FICA-NO     OF TAXWK TO TRUE  HP99997
                       END-IF                                           HP99997

                       IF PAY-FREQ-TYPE OF TAXWK
                           NOT =  PAY-FREQ-TYPE OF EARRY(EARRY-IDX)
                       OR TAX-PERIODS OF TAXWK
                           NOT =  TAX-PERIODS OF EARRY(EARRY-IDX)
                       OR (PAID-PRDS-PER-YEAR OF TAXWK                  HP99999
                           NOT =  PAID-PRDS-PER-YR OF                   HP99999
                                    EARRY(EARRY-IDX)                    HP99999
                           AND PUBLIC-SECTOR-YES OF PSLCT)              HP00001

                           SET CALC-FED-ALL OF TAXWK  TO  TRUE

                           IF  TAX-PERIODS OF TAXWK
                               NOT =  TAX-PERIODS OF
                                      EARRY(EARRY-IDX)

                               SET CALC-FWT-ADDL-YES OF TAXWK
                                                     TO TRUE

                           ELSE

                               SET CALC-FWT-ADDL-NO  OF TAXWK
                                                     TO TRUE       
    
                           END-IF

                           MOVE '$U'
                                  TO  STATE OF TAX-SET OF TAXWK
                           MOVE SPACES
                                  TO  LOCALITY OF TAX-SET OF TAXWK
                           PERFORM LD000-TAX-CHANGE


                       ELSE

                           SET CALC-FWT-ADDL-NO OF TAXWK TO TRUE

                           IF TAX-METHOD OF TAXWK
                               NOT =  TAX-METHOD OF EARRY
                                                   (EARRY-IDX)

                               SET CALC-FED-WH OF TAXWK  TO  TRUE
                               MOVE '$U'
                                  TO  STATE OF TAX-SET OF TAXWK
                               MOVE SPACES
                                  TO  LOCALITY OF TAX-SET OF TAXWK
                               PERFORM LD000-TAX-CHANGE
                           END-IF
                       END-IF

                       IF CALC-FED-FICA-ONLY OF TAXWK                   HP99997
                           MOVE '$U' TO STATE OF TAX-SET OF TAXWK       HP99997
                           MOVE SPACES TO LOCALITY OF TAX-SET OF TAXWK  HP99997
                           PERFORM LD000-TAX-CHANGE                     HP99997
                       END-IF                                           HP99997

                       MOVE TAX-SET OF EARRY(EARRY-IDX)
                               TO  TAX-SET OF TAXWK
                       MOVE EMPL-RCD-NO OF EARRY(EARRY-IDX)
                               TO EMPL-RCD-NO OF W-WK
                       PERFORM LG000-GROSS-CALC
                   END-IF

                   MOVE TAX-METHOD OF W-WK
                   TO TAX-METHOD OF EARRY(EARRY-IDX)

                   IF TAX-METHOD-SUPPLEMENTAL OF EARRY(EARRY-IDX)
                      AND STATE OF TAX-SET OF TAXWK = 'MO'

                      SET MO-SUPPLE-YES OF W-SW TO TRUE

                   END-IF

                   IF STATE OF RESIDENCE OF TAXWK = 'MO'

                      IF STATE OF TAX-SET OF TAXWK  = 'MO'

                         SET MO-WRK-OUTSIDE-NO OF W-SW TO TRUE

                       END-IF

                   END-IF

                   IF STATE OF TAX-SET OF TAXWK = 'OR'

                      SET CALC-TRANSIT-YES OF TAXWK TO TRUE

                   END-IF


                   IF STATE OF TAX-SET OF TAXWK = 'WA'

                      SET WA-FLI-EARNS-YES OF TAXWK TO TRUE
                      SET CALC-LTC-YES     OF TAXWK TO TRUE

                   END-IF


                   IF CALC-TRANSIT-NO OF TAXWK

                      IF STATE OF RESIDENCE OF TAXWK = 'OR'

                         SET CALC-TRANSIT-YES OF TAXWK TO TRUE
                      END-IF
                   END-IF

                   IF EARNS OF EARRY(EARRY-IDX) NOT EQUAL ZERO
                      AND (SUT-JURISDICTION OF EARRY(EARRY-IDX)
                           NOT = STATE OF RESIDENCE OF TAXWK
                           OR STATE OF EARRY(EARRY-IDX)
                              NOT = STATE OF RESIDENCE OF TAXWK)

                          SET MULTI-STATES-YES OF GRSWK TO TRUE
                   END-IF
               END-IF
           END-PERFORM

           SET CALC-FED-ALL OF TAXWK  TO  TRUE
           SET CHANGE-FICA-NO OF TAXWK TO TRUE                          HP99997
           SET CALC-FWT-ADDL-NO  OF TAXWK TO TRUE
           MOVE '$U'  TO  STATE OF TAX-SET OF TAXWK
           MOVE SPACES  TO  LOCALITY OF TAX-SET OF TAXWK
           SET TAX-CALCD-YES OF W-WK TO TRUE
           PERFORM LD000-TAX-CHANGE

           IF SUPPL-NON-TAX OF W-WK > 0
              IF SUPPL-TXGRS OF SUPPC <
                 SUPPL-NON-TAX OF W-WK

                 IF SUPPL-TAXABLE OF W-WK = 0
                    IF SUPPL-TXGRS OF SUPPC < 0
                        COMPUTE SUPPL-TXGRS OF SUPPC
                               = SUPPL-NON-TAX OF W-WK
                               + SUPPL-TXGRS OF SUPPC
                    ELSE

                         COMPUTE SUPPL-TXGRS OF SUPPC
                               =  SUPPL-NON-TAX OF W-WK
                               -  SUPPL-TXGRS OF SUPPC
                    END-IF
                 ELSE
                     COMPUTE SUPPL-TXGRS OF SUPPC
                     =  SUPPL-NON-TAX
                     +  SUPPL-TAXABLE OF W-WK
                     +  (SUPPL-TXGRS OF SUPPC
                     -  SUPPL-TAXABLE OF W-WK)
                 END-IF

              ELSE
                     COMPUTE SUPPL-TXGRS OF SUPPC
                         = SUPPL-TXGRS OF SUPPC
                         + SUPPL-NON-TAX OF W-WK
              END-IF
           END-IF

           IF SUPPL-TXGRS OF SUPPC NOT EQUAL 0
               IF SUPPL-AMOUNT OF SUPPC + SUPPL-AMT-EXTRA OF SUPPC
                  > SUPPL-TXGRS OF SUPPC

                  PERFORM LA500-ADJUST-TXGRS-SPCL-AMTS

               END-IF
           END-IF

           IF WA-SEA-TXGRS-FOUND-YES OF GRSWK
              SET WA-SEA-TXGRS-EFF-YES OF GRSWK TO TRUE
           END-IF

      * CALCULATE STATE TAXES

           PERFORM DA000-INITIALIZE
           PERFORM JA000-INIT-GROSS-WORK
           PERFORM LA200-INIT-DED-ALLOC-CNTRS

           MOVE SPACES  TO  STATE OF PCTST
           MOVE SPACES  TO  TAX-METHOD OF TAXWK
           SET RESIDENT-NO OF TAXWK  TO  TRUE
           SET CALC-ST-WH OF TAXWK  TO  TRUE
           SET ADDL-AMT-APPLIED-NO OF YARRY TO TRUE                     HP99998
           SET CALC-FWT-ADDL-NO OF TAXWK TO TRUE
           MOVE ZERO  TO  NRA-DED-ADJ OF GRSWK                          HP99995
           SET WA-LTC-CALCED-NO OF TAXWK TO TRUE

           MOVE AGT-TTL-SWT OF W-WK TO AGT-TTL-TS OF W-WK

           IF CHANGE-FICA-YES-SAV OF W-SW
              AND AGT-TTL-SWT OF W-WK > ZERO
              AND AGT-TTL-SWT-NEG OF W-WK < ZERO

              ADD AGT-TTL-SWT-NEG OF W-WK
               TO AGT-TTL-TS OF W-WK

           END-IF

           IF  CHECK-DT OF PYGRP < '2012-01-01'
               AND PROC-SUPPL-FOR-CT-NO  OF W-SW

               PERFORM LA470-GET-HIRE-DT

               IF  CT-NEW-HIRE-NO OF SUPPC

                   PERFORM LA480-GET-SUPPL-BAL-CT
               END-IF
           END-IF

           PERFORM VARYING EARRY-IDX  FROM  1  BY  1
                   UNTIL EARRY-IDX  >  EARNINGS-COUNT OF EARRY
                           OR PAY-LINE-STATUS-ERROR OF CHECK
               SET ERNTB-IDX  TO  ERNTB-PTR OF EARRY(EARRY-IDX)
               SET TAX-CALCD-NO OF W-WK TO TRUE



               IF PROCESS-ALL OF GRSWK OR
                 NOT TIPS-CATEGORY-TIPS OF ERNTB(ERNTB-IDX)

                   IF TAX-SET OF EARRY(EARRY-IDX)
                           NOT =  TAX-SET OF TAXWK

                       IF TAX-METHOD OF TAXWK   NOT =  SPACES

                           IF PAY-FREQ-TYPE OF TAXWK
                                   NOT =  PAY-FREQ-TYPE
                                       OF EARRY(EARRY-IDX)
                               OR TAX-PERIODS OF TAXWK
                                  NOT =  TAX-PERIODS OF EARRY(EARRY-IDX)
                               OR TAX-METHOD OF TAXWK
                                   NOT =  TAX-METHOD OF EARRY(EARRY-IDX)
                               OR (PAID-PRDS-PER-YEAR OF TAXWK          HP99999
                                   NOT =  PAID-PRDS-PER-YR OF           HP99999
                                            EARRY(EARRY-IDX)            HP99999
                                   AND PUBLIC-SECTOR-YES OF PSLCT)      HP00001
                               OR  STATE OF TAX-SET OF TAXWK
                                   NOT =  STATE OF EARRY(EARRY-IDX)

                               SET CALC-ST-ALL OF TAXWK  TO  TRUE
                               MOVE SPACES
                                       TO  LOCALITY OF TAX-SET OF TAXWK
                               IF STATE OF TAX-SET OF TAXWK
                                   NOT =  STATE OF EARRY(EARRY-IDX)

                                   PERFORM LA300-GET-BASE-ID
                                   MOVE ZERO TO NRA-DED-ADJ OF GRSWK    HP99995
                               END-IF
                               PERFORM LD000-TAX-CHANGE

                               PERFORM JA050-INIT-WORK-ACCUM
                               MOVE TAX-SET OF EARRY(EARRY-IDX)
                                       TO  TAX-SET OF TAXWK
                               MOVE EMPL-RCD-NO OF EARRY(EARRY-IDX)
                                    TO EMPL-RCD-NO OF W-WK
                               PERFORM LD300-GET-TAX-DATA

                               IF STATE OF TAX-SET OF TAXWK
                                       NOT = STATE OF RESIDENCE OF TAXWK
                                   AND STATE OF RESIDENCE OF TAXWK
                                           NOT = SPACE

                                   PERFORM LA050-CHECK-IF-NO-SWT
                               END-IF

                               PERFORM LG000-GROSS-CALC

                               IF PAY-FREQ-TYPE OF TAXWK
                                       NOT =  PAY-FREQ-TYPE OF EARRY
                                                             (EARRY-IDX)
                                   OR TAX-PERIODS OF TAXWK
                                       NOT =  TAX-PERIODS OF EARRY
                                                             (EARRY-IDX)
                                   OR TAX-METHOD OF TAXWK
                                       NOT =  TAX-METHOD OF EARRY
                                                             (EARRY-IDX)

                                   OR (PAID-PRDS-PER-YEAR OF TAXWK      HP99999
                                       NOT =  PAID-PRDS-PER-YR OF       HP99999
                                                EARRY(EARRY-IDX)        HP99999
                                       AND PUBLIC-SECTOR-YES OF PSLCT)  HP00001

                                   SET CALC-ST-RES OF TAXWK  TO  TRUE
                                   MOVE SPACES
                                           TO  LOCALITY OF TAX-SET
                                                           OF TAXWK
                                   PERFORM LD300-GET-TAX-DATA
                                   PERFORM LD050-CALCULATE-TAX
                                   PERFORM DA000-INITIALIZE
                                   PERFORM JA000-INIT-GROSS-WORK
                               END-IF
                           ELSE

                              PERFORM LG000-GROSS-CALC
                           END-IF
                       ELSE

                           MOVE TAX-SET OF EARRY(EARRY-IDX)
                                   TO  TAX-SET OF TAXWK
                           MOVE EMPL-RCD-NO OF EARRY(EARRY-IDX)
                                TO EMPL-RCD-NO OF W-WK
                           MOVE SPACES  TO  LOCALITY OF TAX-SET
                                                         OF TAXWK
                           PERFORM LA300-GET-BASE-ID
                           PERFORM LD300-GET-TAX-DATA

                           IF STATE OF TAX-SET OF TAXWK
                                       NOT = STATE OF RESIDENCE OF TAXWK
                                   AND STATE OF RESIDENCE OF TAXWK
                                           NOT = SPACE

                               PERFORM LA050-CHECK-IF-NO-SWT
                           END-IF

                           PERFORM LG000-GROSS-CALC
                       END-IF
                   ELSE

                      PERFORM LG000-GROSS-CALC
                   END-IF
               END-IF

           END-PERFORM

           MOVE SPACES  TO  LOCALITY OF TAX-SET OF TAXWK
           MOVE SPACES  TO INDIANA-RESIDENT OF TAXWK
           SET CALC-ST-ALL OF TAXWK  TO  TRUE
           SET TAX-CALCD-YES OF W-WK TO TRUE
           PERFORM LA300-GET-BASE-ID
           PERFORM LD000-TAX-CHANGE

           IF CT-SUPPL-CUR OF SUPPC NOT EQUAL ZERO
              AND CHECK-DT OF PYGRP > '2011-07-31'

                  PERFORM LA550-CREATE-SARRY-CT

           END-IF

      *    Port Authority Change 18c - Remove perform

      *    PERFORM LH155-CALC-TRANSIT-SELF-ADJ

           IF MANUAL-CHECK-NO OF CHECK
             IF CALC-LTC-NO OF TAXWK

                PERFORM LA230-GET-WA-LTC-DATA
             ELSE

                IF PY-LTC-STATUS-YES OF WORK OF TAXDT
                  PERFORM LH156-CALC-LTC-SELF-ADJ
                END-IF
             END-IF
           ELSE
              IF CALC-LTC-YES OF TAXWK AND
                 PY-LTC-STATUS-YES OF WORK OF TAXDT

                 SET TARRY-IDX  TO  1
                 SEARCH TAXCALC-DATA OF TARRY

                 WHEN TARRY-IDX  >
                      TAXCALC-COUNT OF TARRY

                      CONTINUE

                 WHEN  TAX-CLASS-LTC-EE
                            OF TARRY (TARRY-IDX)

                      IF TXGRS-CUR OF TARRY(TARRY-IDX)
                                 NOT =
                         NLGRS-CUR OF TARRY(TARRY-IDX)

                       MOVE TXGRS-CUR OF TARRY(TARRY-IDX)
                             TO
                            NLGRS-CUR OF TARRY(TARRY-IDX)

                      END-IF

                 END-SEARCH
              END-IF
           END-IF

           IF MANUAL-CHECK-NO OF CHECK
               PERFORM LH165-FLI-MLI-QTD-SELF-ADJ
               PERFORM LH160-CALC-FML-SELF-ADJ
           ELSE
               PERFORM LH169-PFML-TXGRS
           END-IF


      *Search through all States for Employee to find out if there are
      *PFF taxes that should be processed where there are no earnings
      *in that state
           PERFORM LH173-FRAMEWORK-STATES

      *Process Framework Defined deductions
           IF FRMWK-STATE-TAX-YES OF TAXWK
              AND FRMWK-STATE-CALCED-YES OF TAXWK
              AND MANUAL-CHECK-NO OF CHECK

              PERFORM VARYING TARRY-IDX FROM 1 BY 1
                   UNTIL TARRY-IDX  >  TAXCALC-COUNT

                 IF FRMWK-STATE-TAX-YES OF TARRY (TARRY-IDX)

                    PERFORM LH171-FRAMEWORK-TXGRS-CALC

                    IF NOT FRMWK-SLF-ADJ-NA
                                       OF TARRY (TARRY-IDX)
                       PERFORM LH170-FRAMEWORK-SELF-ADJ
                    END-IF

                    IF ONE-TIME-ADDITION OF TARRY(TARRY-IDX) OR
                       ONE-TIME-CD OF TARRY(TARRY-IDX) = 'P'

                       COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                          = TAX-CUR OF TARRY(TARRY-IDX)
                          + TAX-ONE-TIME OF TARRY(TARRY-IDX)
                    END-IF

                    IF ONE-TIME-OVERRIDE OF TARRY(TARRY-IDX)

                       MOVE TAX-ONE-TIME OF TARRY(TARRY-IDX)
                           TO TAX-CUR OF TARRY(TARRY-IDX)
                    END-IF

                 ELSE

                   IF STATE OF TARRY (TARRY-IDX) NOT = '$U'

                     PERFORM LH170-SET-FRAMEWORK-FLAGS

                   END-IF

                 END-IF

              END-PERFORM

           ELSE


      *Look through TARRY for Balances of PFF states and make sure
      *they are marked as PFF taxes
              PERFORM VARYING TARRY-IDX FROM 1 BY 1
                   UNTIL TARRY-IDX  >  TAXCALC-COUNT

                 IF STATE OF TARRY (TARRY-IDX) NOT = '$U'

                   PERFORM LH170-SET-FRAMEWORK-FLAGS

                 END-IF

              END-PERFORM

           END-IF


      *Framework Defined Taxes on a manual check
           IF FRMWK-STATE-TAX-YES OF TAXWK
              AND MANUAL-CHECK-YES OF CHECK

              PERFORM VARYING TARRY-IDX FROM 1 BY 1
                   UNTIL TARRY-IDX  >  TAXCALC-COUNT

                   SET ST-PGMID-IDX  TO  1

                   SEARCH STATE-PGMID-ARRAY OF FRMWK

                          AT END
                             CONTINUE

                      WHEN STATE OF TARRY(TARRY-IDX) =
                            STATE OF STATE-PGMID-ARRAY
                                  OF FRMWK (ST-PGMID-IDX)
                            AND
                            TAX-CLASS OF TARRY(TARRY-IDX)  =
                            TAX-CLASS OF STATE-PGMID-ARRAY
                                      OF FRMWK (ST-PGMID-IDX)

                            IF NOT ONE-TIME-NA OF TARRY(TARRY-IDX)

                               MOVE TAX-ONE-TIME OF TARRY(TARRY-IDX)
                                      TO TAX-CUR OF TARRY(TARRY-IDX)
                            END-IF

                            MOVE TAX-CUR OF TARRY(TARRY-IDX)
                                               TO TAX-CUR-SAV OF W-WK

                            PERFORM LH171-FRAMEWORK-TXGRS-CALC

                            IF NOT ONE-TIME-NA OF TARRY(TARRY-IDX)
                               MOVE TAX-CUR-SAV OF W-WK
                                    TO TAX-CUR OF TARRY(TARRY-IDX)
                            END-IF

                            IF FRMWK-EE-TAX-SW-YES OF TARRY(TARRY-IDX)
                               AND
                               NOT ONE-TIME-NA OF TARRY(TARRY-IDX)

                               COMPUTE DEDNET OF GRSWK =
                                    DEDNET OF GRSWK -
                                    TAX-CUR OF TARRY(TARRY-IDX)

                               COMPUTE TTLTAX OF GRSWK =
                                    TTLTAX OF GRSWK +
                                    TAX-CUR OF TARRY(TARRY-IDX)
                            ELSE
                               IF FRMWK-EE-TAX-SW-YES
                                           OF TARRY(TARRY-IDX) AND
                                  ONE-TIME-NA OF TARRY(TARRY-IDX)
                                  MOVE ZERO TO
                                      TAX-CUR OF TARRY(TARRY-IDX)
                                  SET FRMWK-EE-TAX-SW-NO
                                      OF TARRY(TARRY-IDX) TO TRUE
                               END-IF
                            END-IF
                   END-SEARCH

              END-PERFORM

           END-IF

           IF WA-SEA-TXGRS-FOUND-YES OF GRSWK
              SET WA-SEA-TXGRS-EFF-YES OF GRSWK TO TRUE
           END-IF

      * CALCULATE LOCAL TAXES

           SET NO-LCL-WK-TAX-CALCED OF TXARY  TO  TRUE
           SET PHIL-LOC-FOUND-NO    OF GRSWK  TO  TRUE
           SET PA-RES-880000-ROW-NO OF W-SW   TO  TRUE
           SET SEATTLE-TAX-CALC-NO  OF W-SW   TO  TRUE

           PERFORM DA000-INITIALIZE
           PERFORM JA000-INIT-GROSS-WORK
           PERFORM LA200-INIT-DED-ALLOC-CNTRS

           MOVE SPACES  TO  TAX-METHOD OF TAXWK

           MOVE AGT-TTL-LWT OF W-WK TO AGT-TTL-TS OF W-WK

           SET MTA-WORK-NO OF W-SW TO TRUE
           PERFORM VARYING EARRY-IDX  FROM  1  BY  1
                   UNTIL EARRY-IDX  >  EARNINGS-COUNT OF EARRY
                           OR PAY-LINE-STATUS-ERROR OF CHECK

               SET ERNTB-IDX  TO  ERNTB-PTR OF EARRY(EARRY-IDX)
               SET TAX-CALCD-NO OF W-WK TO TRUE
               SET IND-LWT-CALCULATE-YES OF W-SW   TO  TRUE

               IF PROCESS-ALL OF GRSWK OR
                 NOT TIPS-CATEGORY-TIPS OF ERNTB(ERNTB-IDX)

                   IF  (STATE OF WORK OF TAXWK  = 'IN'
                   AND STATE OF RESIDENCE OF TAXWK  NOT = 'IN')
                        OR
                       (STATE OF EARRY(EARRY-IDX) = 'IN'
                   AND STATE OF RESIDENCE OF TAXWK  NOT = 'IN')
                       PERFORM LA080-INDIANA-BEGYR-RES
                   END-IF

                   IF TAX-SET OF EARRY(EARRY-IDX)  NOT =
                       TAX-SET OF TAXWK

                       IF TAX-METHOD OF TAXWK  NOT =  SPACES

                           IF LOCALITY OF TAX-SET OF TAXWK
                                   NOT =  SPACES

                               IF IND-LWT-CALCULATE-YES OF W-SW
                               SET CALC-LOC OF TAXWK  TO  TRUE
                               END-IF

                               IF LOCALITY OF TAX-SET OF TAXWK =
                                  MTA-LOCALITY OF TAXWK

                                  SET MTA-WORK-YES  OF W-SW TO  TRUE
                               END-IF
                               IF LOCALITY OF RESIDENCE OF TAXWK(1)
                                       =  LOCALITY OF WORK OF TAXWK(1)
                                   AND STATE OF RESIDENCE OF TAXWK
                                       =  STATE OF WORK OF TAXWK

                                   MOVE LOCALITY-COUNT OF RESIDENCE
                                                       OF TAXWK
                                           TO  LOCALITY-COUNT OF WORK
                                                       OF TAXWK

                                   SET LOCALITY-COUNT-MAX OF RESIDENCE
                                                             OF TAXWK
                                           TO  TRUE

                                   PERFORM VARYING RESWK-IDX
                                       FROM  1  BY  1
                                           UNTIL RESWK-IDX
                                               >  LOCALITY-COUNT
                                                  OF RESIDENCE OF TAXWK

                                       SET WRKWK-IDX  TO  RESWK-IDX
                                       MOVE LOCALITY OF RESIDENCE
                                            OF TAXWK(RESWK-IDX)
                                               TO LOCALITY OF WORK
                                                  OF TAXWK(WRKWK-IDX)
                                   END-PERFORM
                                   MOVE LOCALITY-COUNT OF WORK OF TAXWK
                                           TO  LOCALITY-COUNT
                                             OF RESIDENCE OF TAXWK
                               END-IF

                               PERFORM LD000-TAX-CHANGE

                           END-IF

                           IF LOCALITY OF EARRY(EARRY-IDX) NOT = SPACES

                               IF IND-LWT-CALCULATE-YES OF W-SW
                               SET CALC-LOC OF TAXWK  TO  TRUE
                               END-IF

                               IF LOCALITY OF EARRY(EARRY-IDX) =
                                  MTA-LOCALITY OF TAXWK

                                  SET MTA-WORK-YES  OF W-SW TO  TRUE
                               END-IF
                               MOVE TAX-SET OF EARRY(EARRY-IDX)
                                       TO  TAX-SET OF TAXWK

                                   MOVE EMPL-RCD-NO OF EARRY(EARRY-IDX)
                                    TO EMPL-RCD-NO OF W-WK
                                   PERFORM LD300-GET-TAX-DATA
                                   SET RESIDENT-NO OF TAXWK  TO  TRUE
                                   MOVE ZERO TO PA-AG-CUM-TS OF W-WK
                                   PERFORM LG000-GROSS-CALC
                           ELSE

                               MOVE SPACE TO LOCALITY
                                               OF TAX-SET OF TAXWK
                           END-IF
                       ELSE

                           IF LOCALITY OF EARRY(EARRY-IDX) NOT = SPACES

                               IF IND-LWT-CALCULATE-YES OF W-SW
                               SET CALC-LOC OF TAXWK  TO  TRUE
                               END-IF

                               IF LOCALITY OF EARRY(EARRY-IDX) =
                                  MTA-LOCALITY OF TAXWK

                                  SET MTA-WORK-YES  OF W-SW TO  TRUE
                               END-IF
                               MOVE TAX-SET OF EARRY(EARRY-IDX)
                                       TO  TAX-SET OF TAXWK

                                  MOVE EMPL-RCD-NO OF EARRY(EARRY-IDX)
                                    TO EMPL-RCD-NO OF W-WK
                                  PERFORM LD300-GET-TAX-DATA
                                  SET RESIDENT-NO OF TAXWK  TO  TRUE
                                  PERFORM LG000-GROSS-CALC
                           ELSE

                               MOVE SPACE TO LOCALITY
                                               OF TAX-SET OF TAXWK
                           END-IF
                       END-IF
                   ELSE

                       IF LOCALITY OF EARRY(EARRY-IDX) NOT = SPACES

                          PERFORM LG000-GROSS-CALC

                       END-IF
                   END-IF
               END-IF
           END-PERFORM

           IF LOCALITY OF TAX-SET OF TAXWK  NOT =  SPACE

               SET CALC-LOC OF TAXWK  TO  TRUE

               IF LOCALITY OF TAX-SET OF TAXWK =
                  MTA-LOCALITY OF TAXWK

                   SET MTA-WORK-YES  OF W-SW     TO  TRUE
               END-IF

               IF LOCALITY OF RESIDENCE OF TAXWK(1)
                       =  LOCALITY OF WORK OF TAXWK(1)
                   AND STATE OF RESIDENCE OF TAXWK
                       =  STATE OF WORK OF TAXWK

                       MOVE LOCALITY-COUNT OF RESIDENCE
                                       OF TAXWK
                           TO  LOCALITY-COUNT OF WORK
                                       OF TAXWK

                   SET LOCALITY-COUNT-MAX OF RESIDENCE
                                       OF TAXWK
                           TO  TRUE

                   PERFORM VARYING RESWK-IDX FROM  1  BY  1
                           UNTIL RESWK-IDX
                               >  LOCALITY-COUNT
                                  OF RESIDENCE OF TAXWK

                       SET WRKWK-IDX  TO  RESWK-IDX
                       MOVE LOCALITY OF RESIDENCE
                            OF TAXWK(RESWK-IDX)
                               TO LOCALITY OF WORK
                                  OF TAXWK(WRKWK-IDX)
                   END-PERFORM
                   MOVE LOCALITY-COUNT OF WORK OF TAXWK
                           TO  LOCALITY-COUNT OF RESIDENCE
                                               OF TAXWK
               END-IF

               SET TAX-CALCD-YES OF W-WK TO TRUE
               PERFORM LD300-GET-TAX-DATA
               PERFORM LD000-TAX-CHANGE
           END-IF

           IF  MTA-LOCALITY OF TAX-SET OF TAXWK NOT = SPACE
               AND MTA-WORK-NO OF W-SW

               SET CALC-LOC OF TAXWK  TO  TRUE

               MOVE MTA-LOCALITY OF TAXWK
                      TO  LOCALITY OF TAX-SET OF TAXWK
               MOVE MTA-STATE OF TAXWK
                     TO STATE OF TAX-SET OF TAXWK

               PERFORM VARYING EARRY-IDX  FROM  1  BY  1
                   UNTIL EARRY-IDX  >  EARNINGS-COUNT OF EARRY
                           OR PAY-LINE-STATUS-ERROR OF CHECK

                   SET ERNTB-IDX  TO  ERNTB-PTR OF EARRY(EARRY-IDX)
                   SET TAX-CALCD-NO OF W-WK TO TRUE

                   IF PROCESS-ALL OF GRSWK OR
                     NOT TIPS-CATEGORY-TIPS OF ERNTB(ERNTB-IDX)


                       PERFORM LG000-GROSS-CALC

                   END-IF
               END-PERFORM

               SET TAX-CALCD-YES OF W-WK TO TRUE
               PERFORM LD300-GET-TAX-DATA
               PERFORM LD000-TAX-CHANGE
           END-IF

           MOVE ZERO TO PA-LOC-TXGRS-TXARY OF W-WK

           PERFORM VARYING TXARY-IDX FROM  1  BY  1
                   UNTIL TXARY-IDX  >  TAX-SET-CNT OF TXARY

               IF  STATE OF TXARY(TXARY-IDX)  = 'PA'
                   AND LOCALITY OF TXARY(TXARY-IDX) NOT = 'SPACE'
                   AND LOCALITY OF TXARY(TXARY-IDX) NOT = '510101'
                   AND LOCALITY OF TXARY(TXARY-IDX) =
                         LOCALITY OF RESIDENCE OF TAXWK(1)
                   AND TAX-CLASS-WITHHOLDING OF TXARY(TXARY-IDX)

                   COMPUTE PA-LOC-TXGRS-TXARY OF W-WK
                         = PA-LOC-TXGRS-TXARY OF W-WK
                         + TXGRS-CUR OF TXARY(TXARY-IDX)
               END-IF
           END-PERFORM

           IF STATE OF RESIDENCE OF TAXWK NOT = SPACE
               PERFORM LH000-CALC-RESIDENT-TAXES

               IF CT-SUPPL-CUR OF SUPPC NOT EQUAL ZERO
                  AND CHECK-DT OF PYGRP > '2011-07-31'

                      PERFORM LA550-CREATE-SARRY-CT

               END-IF
           END-IF


      *    CERTAIN STATES REQUIRE ROUNDING OF TAX AMOUNT TO DOLLARS ONLY
      *    MOVED FROM PSPTCALC AND LH400-CALC-RESIDENT-TAXES FOR CORRECT
      *    CALCULATION ON 10-1-99     - R-JSODER-5M3Q2

            PERFORM LH420-ROUND-TAXES VARYING TXARY-IDX FROM 1 BY 1
               UNTIL TXARY-IDX > TAX-SET-CNT OF TXARY

           PERFORM VARYING TXARY-IDX FROM 1 BY 1
                   UNTIL TXARY-IDX > TAX-SET-CNT OF TXARY

               IF TXGRS-CUR OF TXARY(TXARY-IDX) < ZERO
                       AND STATE OF TAX-SET OF TXARY(TXARY-IDX)
                           NOT = '$U'
                       AND ( SUT OF GRSWK(1) < ZERO
                               OR SDI OF GRSWK(1) < ZERO
                               OR SWT OF GRSWK(1) < ZERO )

                   SET MSGID-EARNINGS-WENT-NEG OF PYMSG TO TRUE

                   MOVE PAY-FREQ-TYPE OF TAX-SET OF TXARY(TXARY-IDX)
                                   TO PAY-FREQ-TYPE OF W-MSGDATA
                   MOVE TAX-PERIODS OF TAX-SET OF TXARY(TXARY-IDX)
                                   TO TAX-PERIODS OF W-MSGDATA
                   MOVE TAX-METHOD OF TAX-SET OF TXARY(TXARY-IDX)
                                   TO TAX-METHOD OF W-MSGDATA
                   MOVE STATE OF TAX-SET OF TXARY(TXARY-IDX)
                                   TO STATE OF W-MSGDATA
                   MOVE LOCALITY OF TAX-SET OF TXARY(TXARY-IDX)
                                   TO LOCALITY OF W-MSGDATA
                   MOVE SUT-JURISDICTION OF TAX-SET
                                                OF TXARY(TXARY-IDX)
                                   TO SUT-JURISDICTION OF W-MSGDATA

                   MOVE MSGDATA1 OF W-MSGDATA TO MSGDATA1 OF PYMSG
                   MOVE MSGDATA2 OF W-MSGDATA TO MSGDATA2 OF PYMSG
                   MOVE MSGDATA3 OF W-MSGDATA TO MSGDATA3 OF PYMSG

                   PERFORM ZM000-MESSAGE
               END-IF
           END-PERFORM

           PERFORM VARYING TARRY-IDX FROM 1 BY 1
                   UNTIL TARRY-IDX  >  TAXCALC-COUNT

               IF STATE OF TARRY(TARRY-IDX)  =  'PA'
                   AND TAX-CLASS-WITHHOLDING OF TARRY(TARRY-IDX)
                   AND LOCALITY OF TARRY(TARRY-IDX)  NOT =  SPACE
                   AND CHECK-DT OF CHECK  >=  '2012-01-01'

                   IF WORK-PSD-CD OF TARRY(TARRY-IDX)  NOT =  SPACE
                       AND RES-PSD-CD OF TARRY(TARRY-IDX)  NOT =  SPACE

                       IF MANUAL-CHECK-YES OF CHECK
                          AND ONE-TIME-NA OF TARRY(TARRY-IDX)

                             MOVE ZERO TO TAX-CUR OF TARRY(TARRY-IDX)
                       END-IF

                       IF  LOCALITY OF TARRY(TARRY-IDX) = '880000'
                           AND RES-PSD-CD OF TARRY(TARRY-IDX)
                                = LOCALITY OF TARRY(TARRY-IDX)
                           AND WORK-PSD-CD OF TARRY(TARRY-IDX)
                                = LOCALITY OF TARRY(TARRY-IDX)

                           MOVE ZERO TO TXGRS-CUR OF TARRY(TARRY-IDX)
                           MOVE ZERO TO TAX-CUR OF TARRY(TARRY-IDX)
                           MOVE ZERO TO TAX-CUR-ADDL-AMT
                                            OF TARRY(TARRY-IDX)
                       END-IF

                       IF LOCALITY OF TARRY(TARRY-IDX) = '880000'
                          AND WORK-PSD-CD OF TARRY(TARRY-IDX) = '880000'
                              MOVE RES-PSD-CD OF TARRY(TARRY-IDX) TO
                                   LOCALITY OF TARRY(TARRY-IDX)
                       END-IF

                       IF LOCALITY OF TARRY(TARRY-IDX)
                                =  RES-PSD-CD OF TARRY(TARRY-IDX)

                            SET RESIDENT-YES OF TARRY(TARRY-IDX)
                                        TO  TRUE
                       ELSE
                           MOVE SPACE  TO  RESIDENT OF TARRY(TARRY-IDX)
                       END-IF

                       SET  WTARRY-IDX  TO  TARRY-IDX
                       ADD  1           TO  WTARRY-IDX

                       PERFORM VARYING WTARRY-IDX FROM WTARRY-IDX BY 1
                               UNTIL WTARRY-IDX
                                        >  TAXCALC-COUNT OF TARRY

                           IF STATE OF TARRY(TARRY-IDX)
                                =  STATE OF TARRY(WTARRY-IDX)
                                AND LOCALITY OF TARRY(TARRY-IDX)
                                    =  LOCALITY OF TARRY(WTARRY-IDX)
                                AND TAX-CLASS OF TARRY(TARRY-IDX)
                                    =  TAX-CLASS OF TARRY(WTARRY-IDX)
                                AND WORK-PSD-CD OF TARRY(TARRY-IDX)
                                    =  WORK-PSD-CD OF TARRY(WTARRY-IDX)
                                AND RES-PSD-CD OF TARRY(TARRY-IDX)
                                    =  RES-PSD-CD OF TARRY(WTARRY-IDX)

                             IF ONE-TIME-OVERRIDE OF TARRY(WTARRY-IDX)
                               AND TXGRS-CUR OF TARRY(WTARRY-IDX) = ZERO

                               CONTINUE
                             ELSE
                               COMPUTE TAX-CUR OF TARRY(WTARRY-IDX)
                                       =  TAX-CUR OF TARRY(WTARRY-IDX)
                                       +  TAX-CUR OF TARRY(TARRY-IDX)
                             END-IF

                               COMPUTE NLGRS-CUR OF TARRY(WTARRY-IDX)
                                       =  NLGRS-CUR OF TARRY(WTARRY-IDX)
                                       +  NLGRS-CUR OF TARRY(TARRY-IDX)
                               COMPUTE TXGRS-CUR OF TARRY(WTARRY-IDX)
                                       =  TXGRS-CUR OF TARRY(WTARRY-IDX)
                                       +  TXGRS-CUR OF TARRY(TARRY-IDX)
                               COMPUTE TAX-ONE-TIME OF TARRY(WTARRY-IDX)
                                       =  TAX-ONE-TIME
                                                OF TARRY(WTARRY-IDX)
                                       +  TAX-ONE-TIME
                                                OF TARRY(TARRY-IDX)
                               COMPUTE TAX-NOT-TAKEN
                                                OF TARRY(WTARRY-IDX)
                                       =  TAX-NOT-TAKEN
                                                OF TARRY(WTARRY-IDX)
                                       +  TAX-NOT-TAKEN
                                                OF TARRY(TARRY-IDX)

                               IF TXGRS-CUR OF TARRY(TARRY-IDX) = ZERO
                                  AND TXGRS-AGG OF TARRY(TARRY-IDX)
                                                       = ZERO

                                    MOVE '880000' TO WORK-PSD-CD
                                             OF TARRY(WTARRY-IDX)

                               END-IF

                               MOVE ZERO  TO  TAX-CUR
                                                OF TARRY(TARRY-IDX)
                               MOVE ZERO  TO  NLGRS-CUR
                                                OF TARRY(TARRY-IDX)
                               MOVE ZERO  TO  TXGRS-CUR
                                                OF TARRY(TARRY-IDX)
                               MOVE ZERO  TO  TAX-ONE-TIME
                                                OF TARRY(TARRY-IDX)
                               MOVE ZERO  TO  TAX-NOT-TAKEN
                                                OF TARRY(TARRY-IDX)

                               IF RESIDENT-YES OF TARRY(TARRY-IDX)
                                   AND NOT RESIDENT-YES
                                        OF TARRY(WTARRY-IDX)
                                   AND TXGRS-AGG OF TARRY(TARRY-IDX)
                                       NOT =  ZERO
                                   AND TXGRS-AGG OF TARRY(TARRY-IDX)
                                       <  TXGRS-CUR
                                                OF TARRY(WTARRY-IDX)

                                  MOVE TXGRS-AGG OF TARRY(TARRY-IDX)
                                        TO  TXGRS-CUR
                                                OF TARRY(WTARRY-IDX)
                               END-IF
                               IF MANUAL-CHECK-YES OF CHECK
                                   AND NOT ONE-TIME-NA
                                                OF TARRY(TARRY-IDX)

                                   MOVE ONE-TIME-CD
                                                OF TARRY(TARRY-IDX)
                                        TO  ONE-TIME-CD
                                                OF TARRY(WTARRY-IDX)
                               END-IF
                           END-IF
                       END-PERFORM
                   ELSE

                       IF WORK-PSD-CD OF TARRY(TARRY-IDX) = SPACE
                           MOVE '880000' TO WORK-PSD-CD
                                         OF TARRY(TARRY-IDX)

                       END-IF

                       IF LOCALITY OF TARRY(TARRY-IDX)
                                =  RES-PSD-CD OF TARRY(TARRY-IDX)

                            SET RESIDENT-YES OF TARRY(TARRY-IDX)
                                        TO  TRUE
                       ELSE

                            MOVE SPACE TO RESIDENT OF TARRY(TARRY-IDX)
                       END-IF
                   END-IF
               ELSE
                   MOVE SPACES  TO  WORK-PSD-CD OF TARRY(TARRY-IDX)

                   MOVE SPACES  TO  RES-PSD-CD OF TARRY(TARRY-IDX)
               END-IF

               IF STATE OF TARRY(TARRY-IDX)  =  'WA'
                   AND LOCALITY OF TARRY(TARRY-IDX)  =  '63000'

                   SET SEATTLE-TAX-CALC-YES OF W-SW TO TRUE

               END-IF
           END-PERFORM

           MOVE SPACE TO PA-PREV-LOCALITY OF W-WK

           IF STATE OF RESIDENCE OF TAXWK = 'PA' AND
              LOCALITY OF RESIDENCE OF TAXWK(1)  = '510101'

              CONTINUE

           ELSE

            IF MULTI-PA-LOC-AND-OTHER-YES OF W-SW
              AND  PA-RES-LWT OF GRSWK  NOT EQUAL ZERO

               COMPUTE PA-PERC-DED OF W-WK =
                       PA-DED-ERN OF W-WK /
                       EARNS-TTL OF GRSWK
               COMPUTE PA-LOC-RES-TXGRS-ADJ OF W-WK =
                       PA-TOT-125-DED OF W-WK *
                       PA-PERC-DED OF W-WK
               PERFORM LH555-GET-LOCAL-RES-RATE

               PERFORM LH565-FIND-RES-TARRY
               PERFORM LH555-ACCUM-TXGRS
               PERFORM LH560-FIND-RES-88000-TARRY

            ELSE

               IF PA-RES-880000-ROW-YES OF W-SW

                IF PA-EARNS-TTL OF GRSWK NOT EQUAL ZERO

                   COMPUTE PA-PERC-DED OF W-WK =
                           PA-DED-NON-PA-ERN OF W-WK /
                            PA-EARNS-TTL OF GRSWK
                ELSE

                   COMPUTE PA-PERC-DED OF W-WK =
                            PA-DED-NON-PA-ERN OF W-WK /
                           EARNS-TTL OF GRSWK
                END-IF

                   COMPUTE PA-LOC-RES-TXGRS-ADJ OF W-WK =
                           PA-TOT-125-DED OF W-WK *
                           PA-PERC-DED OF W-WK
                   PERFORM LH555-GET-LOCAL-RES-RATE
                   PERFORM LH580-ADD-RES-88000-TARRY
               END-IF

            END-IF
           END-IF

           IF STATE OF TAX-SET OF TAXWK = 'PA'
              AND TAXGRS-CMP-COUNT OF GRSWK > ZERO

              PERFORM LH740-MATCH-PA-LOC-TGCID

           END-IF

           IF STATE OF TAX-SET OF TAXWK NOT = '$U'
              AND STATE OF TAX-SET OF TAXWK NOT = 'PA'
              AND TAXGRS-CMP-COUNT OF GRSWK > ZERO

              PERFORM LH750-MATCH-LAST-TGCID

           END-IF


           SET TXARY-IDX  TO  1

           SET PA-RES-LOCAL-UPDATE-NO OF W-SW TO TRUE
           MOVE ZERO  TO PA-LOC-RES-TXGRS-CUR OF W-WK
           MOVE SPACE TO PA-PREV-TAX-METHOD-ANN OF W-WK
           MOVE SPACE TO PA-PREV-TAX-METHOD-OTH OF W-WK

           PERFORM VARYING TXARY-IDX FROM  1  BY  1
                   UNTIL TXARY-IDX  >  TAX-SET-CNT OF TXARY

               IF  STATE OF TXARY(TXARY-IDX)  = 'PA'
                   AND LOCALITY OF TXARY(TXARY-IDX) NOT = '510101'
                   AND LOCALITY OF TXARY(TXARY-IDX) =
                         LOCALITY OF RESIDENCE OF TAXWK(1)
                   AND TAX-CLASS-WITHHOLDING OF TXARY(TXARY-IDX)
                   AND TAX-METHOD OF TXARY (TXARY-IDX)
                              NOT = PA-PREV-TAX-METHOD-ANN OF W-WK
                   AND TAX-METHOD OF TXARY (TXARY-IDX)
                              NOT = PA-PREV-TAX-METHOD-OTH OF W-WK

                   IF TAX-METHOD-ANNUALIZED OF TXARY(TXARY-IDX)

                         MOVE TAX-METHOD OF TXARY (TXARY-IDX)
                               TO PA-PREV-TAX-METHOD-ANN OF W-WK
                   ELSE

                         MOVE TAX-METHOD OF TXARY (TXARY-IDX)
                               TO PA-PREV-TAX-METHOD-OTH OF W-WK
                   END-IF

                   COMPUTE PA-LOC-RES-TXGRS-CUR OF W-WK =
                           PA-LOC-RES-TXGRS-CUR OF W-WK +
                           TXGRS-CUR OF TXARY (TXARY-IDX)

                   SET PA-RES-LOCAL-UPDATE-YES OF W-SW TO TRUE
               END-IF
           END-PERFORM


           SET TARRY-IDX TO 1

           SEARCH TAXCALC-DATA OF TARRY

                WHEN TARRY-IDX > TAXCALC-COUNT OF TARRY

                    CONTINUE

                WHEN STATE OF TARRY(TARRY-IDX) = 'PA'
                   AND LOCALITY OF TARRY(TARRY-IDX)
                       = LOCALITY OF RESIDENCE OF TAXWK(1)
                   AND RESIDENT-YES OF TARRY(TARRY-IDX)
                   AND TAX-CLASS-WITHHOLDING OF TARRY(TARRY-IDX)
                   AND TXGRS-CUR OF TARRY(TARRY-IDX) NOT = ZERO

                   IF NON-PA-ERN-FOUND-YES OF W-SW
                      AND PHIL-LOCAL-FOUND-NO OF W-SW
                      AND MULTI-PA-LOC-AND-OTHER-NO OF W-SW

                      IF PA-RES-ERN-FOUND-YES OF W-SW

                         IF PA-RES-LOCAL-UPDATE-YES OF W-SW
                            AND PA-LOC-RES-TXGRS-CUR OF W-WK = ZERO

                            MOVE '880000' TO WORK-PSD-CD OF TAXCALC-DATA
                                          OF TARRY(TARRY-IDX)
                            SET PA-RES-LOCAL-UPDATE-NO OF W-SW TO TRUE
                         END-IF
                      ELSE

                         IF WORK-PSD-CD OF TARRY(TARRY-IDX) NOT = SPACE
                            AND LOCALITY OF TARRY(TARRY-IDX)
                                = RES-PSD-CD OF TARRY(TARRY-IDX)

                            MOVE '880000' TO WORK-PSD-CD OF TAXCALC-DATA
                                                     OF TARRY(TARRY-IDX)
                         END-IF
                      END-IF
                   END-IF
           END-SEARCH

           PERFORM LH760-PA-LOC-RES-ROW
           PERFORM LH765-PA-LOC-RES-WRK-PSD

           IF STATE OF TARRY (TARRY-IDX) = 'PA'
              AND LOCALITY OF TARRY (TARRY-IDX) NOT = SPACE
              AND LOCALITY OF TARRY (TARRY-IDX) NOT = '510101'
              AND PA-BLANK-FOUND-NO OF W-SW

              IF PA-RES-ERN-FOUND-YES OF W-SW

                 IF NON-PA-ERN-FOUND-YES OF W-SW
                    AND PA-RES-WRK-PSD-FOUND-YES OF W-SW

                    IF TXGRS-CUR OF TARRY(TARRY-IDX)
                       NOT = PA-LOC-TXGRS-TXARY OF W-WK
                       AND TXGRS-CUR OF TARRY(TARRY-IDX) NOT = ZERO

                          MOVE PA-LOC-TXGRS-TXARY OF W-WK
                             TO TXGRS-CUR OF TARRY(TARRY-IDX)

                          PERFORM LH555-GET-LOCAL-RES-RATE

                          COMPUTE TAX-CUR OF TARRY(TARRY-IDX) ROUNDED
                                = TXGRS-CUR OF TARRY(TARRY-IDX)
                                * WK-LCLPCT-RES OF W-WK
                    END-IF

                    IF PA-880000-ROW-FOUND-NO OF W-SW
                       AND TXGRS-CUR OF TARRY(TARRY-IDX) NOT = ZERO

                        PERFORM LH770-CREATE-WRK-PSD-880000
                    END-IF
                 END-IF
              ELSE

                 IF NON-PA-ERN-FOUND-YES OF W-SW
                    AND PA-RES-WRK-PSD-FOUND-YES OF W-SW

                    IF LOC-RES-COUNT OF W-WK > 1

                          MOVE ZERO TO TAX-CUR OF TARRY(TARRY-IDX)
                          MOVE ZERO TO TXGRS-CUR OF TARRY(TARRY-IDX)
                    ELSE

                       IF PA-880000-ROW-FOUND-NO OF W-SW

                          MOVE '880000' TO WORK-PSD-CD OF TAXCALC-DATA
                                                  OF TARRY(TARRY-IDX)
                       END-IF
                    END-IF
                 END-IF
              END-IF
           END-IF

           PERFORM LH767-PHILLY-LOC-RES-ROW

           SET TARRY-IDX TO 1

           SEARCH TAXCALC-DATA OF TARRY

                WHEN TARRY-IDX > TAXCALC-COUNT OF TARRY

                    CONTINUE

                WHEN STATE OF TARRY(TARRY-IDX) = 'PA'
                   AND LOCALITY OF TARRY (TARRY-IDX) = '510101'
                   AND RES-PSD-CD OF TARRY(TARRY-IDX)
                       = LOCALITY OF TARRY (TARRY-IDX)
                   AND WORK-PSD-CD OF TARRY(TARRY-IDX)
                       NOT = RES-PSD-CD OF TARRY(TARRY-IDX)
                   AND RESIDENT-YES OF TARRY(TARRY-IDX)
                   AND PA-PHILLY-ROW-FOUND-YES OF W-SW

                   MOVE ZERO TO TXGRS-CUR OF TARRY(TARRY-IDX)
                   MOVE ZERO TO TAX-CUR OF TARRY(TARRY-IDX)

           END-SEARCH

      *Calculate Seattle ER tax if local for Seattle exists

           PERFORM LA220-GET-SEATTLE-DATA

           IF SEATTLE-TAX-CALC-YES OF W-SW

              PERFORM LA240-PROCESS-SEATTLE

           END-IF

            .
       CALC-TAXES-EXIT.


      /*****************************************************************
      *                                                                *
       LA050-CHECK-IF-NO-SWT SECTION.
       LA050.
      *                                                                *
      * IF THE RES STATE HAS SWT AND THE WORK STATE DOES NOT, AND IF   *
      * THE RES-WORK RECIPROCITY IS TO REDUCE THE RES STATE'S TAXABLE  *
      * GROSS (4TH RADIO BUTTON) BY THE WORK STATE'S TAXABLE GROSS     *
      * (WHICH WOULD BE ZERO IN THIS CASE), THEN MAKE THE RES STATE    *
      * THE WORK STATE FOR THESE EARNINGS.  THIS WILL FORCE THE RES    *
      * STATE'S TAXABLE GROSS COMPONENTS TO BE PROBERLY APPLIED.       *
      *                                                                *
      ******************************************************************

           MOVE CHECK-DT OF PYGRP TO CHECK-DT OF TXLNK
           MOVE SPACE TO LOCALITY OF TXWRK
           MOVE SPACE TO TAX-CLASS OF TXLNK
           MOVE BALANCE-YEAR OF BALID(BALID-IDX)
                             TO BALANCE-YEAR OF TXLNK

      *  GET RES STATE TAX RATE TABLE

           MOVE STATE OF RESIDENCE OF TAXWK TO STATE OF TXWRK

           CALL 'PSPSTTRT' USING   SQLRT
                                   STTRT
                                   TXWRK
                                   TXLNK

           IF RTNCD-ERROR OF SQLRT

               MOVE 'CHECK-IF-NO-SWT-STATE-1(PSPPYNET)'
                               TO ERR-SECTION OF SQLRT
               PERFORM ZZ000-SQL-ERROR
           END-IF

      *  IF RES STATE HAS SWT, CHECK WORK STATE

           SET STTRT-IDX TO STTRT-PTR OF TXWRK
           IF NOT ST-TAX-TYPE-NO-SWT OF STTRT(STTRT-IDX)

               MOVE STATE OF TAX-SET OF TAXWK TO STATE OF TXWRK

               CALL 'PSPSTTRT' USING   SQLRT
                                       STTRT
                                       TXWRK
                                       TXLNK

               IF RTNCD-ERROR OF SQLRT

                   MOVE 'CHECK-IF-NO-SWT-2(PSPPYNET)'
                                   TO ERR-SECTION OF SQLRT
                   PERFORM ZZ000-SQL-ERROR
               END-IF

      *  IF THE WORK STATE DOES NOT HAVE SWT, OR
      *  THE EMPLOYEE SPECAIL TAX STATUS IS SET TO
      *  "DO NOT MAINTAIN TAXABLE GROSS AND DO NOT WITHHOLD TAX"
      *  CHECK THE RES-WORK
      *  RECIPROCITY.  IF THE RULE IS TO REDUCE THE RES STATE'S
      *  TAXABLE TOTAL, THEN MAKE THE RES STATE THE WORK STATE
      *  FOR THESE EARNINGS AND RELOAD THE TAX DATA FOR THE NEW
      *  WORK STATE.

               SET STTRT-IDX TO STTRT-PTR OF TXWRK
               IF ST-TAX-TYPE-NO-SWT OF STTRT(STTRT-IDX)
                   IF RECIPROCITY-RULE-REDTTL OF STATE-RCP OF TAXWK

      *                FIRST CALL PSPTCALC TO PROCESS ANY FRAMEWORK
      *                TAXES FOR THE WORK STATE IF REQUIRED

                       PERFORM LD300-GET-TAX-DATA
                       SET PYNET-FRMWK-PROCESS-YES OF PYNET-PASS TO TRUE
                       MOVE SPACE TO PYNET-PROGID  OF PYNET-PASS
                       PERFORM LD050-CALCULATE-TAX
                       SET PYNET-FRMWK-PROCESS-NO OF PYNET-PASS TO TRUE

      *                NOW MAKE RES STATE THE WORK STATE

                       MOVE STATE OF RESIDENCE OF TAXWK
                               TO STATE OF TAX-SET OF TAXWK
                       MOVE SUT-EXEMPT OF WORK OF TAXDT TO
                                    HOLD-SUT-EXEMPT OF W-WK

                       PERFORM LD300-GET-TAX-DATA
                       MOVE HOLD-SUT-EXEMPT OF W-WK TO
                                    SUT-EXEMPT OF WORK OF TAXDT

                       MOVE STATE OF EARRY(EARRY-IDX)
                                  TO STATE OF TAX-SET OF TAXWK

                   END-IF
               ELSE
                   IF SPECIAL-SWT-STATUS-EXEMPT OF WORK OF TAXDT
                    IF RECIPROCITY-RULE-REDTTL OF STATE-RCP OF TAXWK
                    OR RECIPROCITY-RULE-REDTTL-A OF STATE-RCP OF TAXWK

      *                FIRST CALL PSPTCALC TO PROCESS ANY FRAMEWORK
      *                TAXES FOR THE WORK STATE IF REQUIRED

                       PERFORM LD300-GET-TAX-DATA
                       SET PYNET-FRMWK-PROCESS-YES OF PYNET-PASS TO TRUE
                       MOVE SPACE TO PYNET-PROGID  OF PYNET-PASS
                       PERFORM LD050-CALCULATE-TAX
                       SET PYNET-FRMWK-PROCESS-NO OF PYNET-PASS TO TRUE

      *                NOW MAKE RES STATE THE WORK STATE

                       MOVE STATE OF RESIDENCE OF TAXWK
                                  TO STATE OF TAX-SET OF TAXWK
                       MOVE SUT-EXEMPT OF WORK OF TAXDT TO
                                    HOLD-SUT-EXEMPT OF W-WK
                       SET SPEC-SWT-EXEMPT-YES OF W-SW TO TRUE

                       PERFORM LD300-GET-TAX-DATA
                       MOVE HOLD-SUT-EXEMPT OF W-WK TO
                                    SUT-EXEMPT OF WORK OF TAXDT
                    END-IF
                   END-IF
               END-IF
           END-IF

           .
       CHECK-IF-NO-SWT-EXIT.

      /*****************************************************************
      *                                                                *
       LA080-INDIANA-BEGYR-RES SECTION.
       LA080.
      *                                                                *
      * FOR INDIANA TAX THE RESIDENT LOCALITY AS OF 01-01 OF THE
      * CURRENT PAY YEAR IF IT  EXISTS
      * EVEN IF THE EMPLOYEE IS NO LONGER RESIDENT IN
      * INDIANA.
      *                                                                *
      ******************************************************************
           SET BALID-IDX               TO BALID-IDX-FOR-CAL-YR
           MOVE CALENDAR-FILL OF W-WK  TO CALENDAR-FILL OF TAXWK
           MOVE BALANCE-YEAR OF BALID(BALID-IDX)
                                       TO CALENDAR-YEAR  OF TAXWK
           MOVE EMPLID OF CHECK        TO EMPLID OF S-LCLIND
           MOVE COMPANY OF PSLCT       TO COMPANY OF S-LCLIND
           MOVE 'Y'                    TO RESIDENT OF S-LCLIND
           MOVE 'IN'                   TO WORK-STATE OF S-LCLIND
           MOVE BEGINYR-DT OF TAXWK    TO CHECK-DT OF S-LCLIND


           CALL 'PTPSQLRT' USING   ACTION-SELECT OF SQLRT
                                   SQLRT
                                   SQL-CURSOR-COMMON OF SQLRT
                                   SQL-STMT OF S-LCLIND
                                   BIND-SETUP OF S-LCLIND
                                   BIND-DATA OF S-LCLIND
                                   SELECT-SETUP OF S-LCLIND
                                   SELECT-DATA OF S-LCLIND
           IF RTNCD-ERROR OF SQLRT

               MOVE 'INDIANA-BEGYR-RES(EXEC)' TO ERR-SECTION OF SQLRT
               PERFORM ZZ000-SQL-ERROR
           END-IF

           INITIALIZE SELECT-DATA OF S-LCLIND

           CALL 'PTPSQLRT' USING   ACTION-FETCH OF SQLRT
                                   SQLRT
                                   SQL-CURSOR-COMMON   OF SQLRT
           IF RTNCD-ERROR OF SQLRT
                   AND NOT RTNCD-END OF SQLRT

               MOVE 'INDIANA-BEGYR-RES(FETCH)' TO ERR-SECTION OF SQLRT
               PERFORM ZZ000-SQL-ERROR
           ELSE
               IF RTNCD-END OF SQLRT
                    IF NOT BEGINYR-RESIDENT
                       PERFORM LA090-INDIANA-PRI-LCL
                    END-IF
               ELSE
                   IF STATE OF EARRY(EARRY-IDX) = 'IN'
                     MOVE LOCALITY OF S-LCLIND
                                 TO  LOCALITY OF EARRY(EARRY-IDX)
                     SET  BEGINYR-RESIDENT  TO TRUE
                   END-IF
               END-IF
           END-IF

           SET RTNCD-OK OF SQLRT  TO  TRUE
           .

       INDIANA-BEGYR-RES-EXIT.
      /*****************************************************************
      *                                                                *
       LA090-INDIANA-PRI-LCL SECTION.
       LA090.
      *                                                                *
      * FOR INDIANA TAX FIND THE COUNTY OF PRINCIPAL EMPLOYMENT AS OF
      * JANUARY 1st IF EMPLOYEE IS NOT A RESIDENT OF INDIANA
      *                                                                *
      ******************************************************************
           SET BALID-IDX               TO BALID-IDX-FOR-CAL-YR
           MOVE CALENDAR-FILL OF W-WK  TO CALENDAR-FILL OF TAXWK
           MOVE BALANCE-YEAR OF BALID(BALID-IDX)
                                       TO CALENDAR-YEAR  OF TAXWK
           MOVE EMPLID OF CHECK        TO EMPLID OF S-LCLINDP
           MOVE COMPANY OF PSLCT       TO COMPANY OF S-LCLINDP
           MOVE 'N'                    TO RESIDENT OF S-LCLINDP
           MOVE 'IN'                   TO WORK-STATE OF S-LCLINDP
           MOVE BEGINYR-DT OF TAXWK    TO CHECK-DT OF S-LCLINDP


           CALL 'PTPSQLRT' USING   ACTION-SELECT OF SQLRT
                                   SQLRT
                                   SQL-CURSOR-COMMON OF SQLRT
                                   SQL-STMT OF S-LCLINDP
                                   BIND-SETUP OF S-LCLINDP
                                   BIND-DATA OF S-LCLINDP
                                   SELECT-SETUP OF S-LCLINDP
                                   SELECT-DATA OF S-LCLINDP
           IF RTNCD-ERROR OF SQLRT

               MOVE 'INDIANA-BEGYR-RES(EXEC)' TO ERR-SECTION OF SQLRT
               PERFORM ZZ000-SQL-ERROR
           END-IF

           INITIALIZE SELECT-DATA OF S-LCLINDP

           CALL 'PTPSQLRT' USING   ACTION-FETCH OF SQLRT
                                   SQLRT
                                   SQL-CURSOR-COMMON   OF SQLRT
           IF RTNCD-ERROR OF SQLRT
                   AND NOT RTNCD-END OF SQLRT

               MOVE 'INDIANA-PRI-LCL(FETCH)' TO ERR-SECTION OF SQLRT
               PERFORM ZZ000-SQL-ERROR
           ELSE
               IF RTNCD-END OF SQLRT
                   SET IND-LWT-CALCULATE-NO OF W-SW TO TRUE
                   MOVE SPACE TO LOCALITY OF TAX-SET OF TAXWK
               ELSE
                   IF STATE OF EARRY(EARRY-IDX) = 'IN'
                       MOVE LOCALITY OF S-LCLINDP
                                 TO  LOCALITY OF EARRY(EARRY-IDX)
                   END-IF
               END-IF
           END-IF

           SET RTNCD-OK OF SQLRT  TO  TRUE
           .

       INDIANA-PRI-LCL-EXIT.

      /*****************************************************************
      *                                                                *
       LA100-INIT-DED-SOURCE-CNTRS SECTION.
       LA100.
      *                                                                *
      * INITIALIZE DEDUCTION-SOURCE COUNTERS                           *
      *                                                                *
      ******************************************************************

           MOVE ZERO  TO  DED-REFUND-TTL OF W-TTL
           MOVE ZERO  TO AFT-TAX-DED-REFUND-TTL OF RFNTL

           PERFORM VARYING DARRY-IDX  FROM  1  BY  1
                   UNTIL DARRY-IDX  >  DEDUCTION-COUNT OF DARRY

               PERFORM VARYING DCLAS-IDX FROM 1 BY 1
                   UNTIL DCLAS-IDX > DEDUCTION-CLASS-MAX

                       MOVE ZERO TO DED-CUR-FROM-AG-POS
                                   OF DARRY(DARRY-IDX DCLAS-IDX)
                       MOVE ZERO TO DED-CUR-FROM-AG-NEG
                                   OF DARRY(DARRY-IDX DCLAS-IDX)
               END-PERFORM
           END-PERFORM

           .
       INIT-DED-SOURCE-EXIT.


      /*****************************************************************
      *                                                                *
       LA200-INIT-DED-ALLOC-CNTRS SECTION.
       LA200.
      *                                                                *
      * INITIALIZE DEDUCTION-ALLOCATION COUNTERS                       *
      *                                                                *
      ******************************************************************

           MOVE ZERO  TO  AG-CUR-TS OF W-WK
           MOVE ZERO  TO  T-CUR-TS OF W-WK
           MOVE ZERO  TO  AGT-CUM-TS OF W-WK
           MOVE ZERO  TO  AG-TTL-TS OF W-WK
           MOVE ZERO  TO  AG-NET-TS OF W-WK
           MOVE ZERO  TO  PA-AG-CUM-TS OF W-WK
           MOVE ZERO  TO  PA-BLK-AG-CUM OF W-WK

           PERFORM VARYING DARRY-IDX  FROM  1  BY  1
                   UNTIL DARRY-IDX  >  DEDUCTION-COUNT OF DARRY

               PERFORM VARYING DCLAS-IDX FROM 1 BY 1
                   UNTIL DCLAS-IDX > DEDUCTION-CLASS-MAX

                       MOVE ZERO TO DED-CUM-FROM-AG-POS
                                   OF DARRY(DARRY-IDX DCLAS-IDX)
                       MOVE ZERO TO DED-CUM-FROM-AG-NEG
                                   OF DARRY(DARRY-IDX DCLAS-IDX)
                       MOVE ZERO TO DED-CUM-FROM-OTHER
                                   OF DARRY(DARRY-IDX DCLAS-IDX)
               END-PERFORM
           END-PERFORM

           .
       INIT-DED-ALLOC-EXIT.

      /*****************************************************************
      *                                                                *
       LA220-GET-SEATTLE-DATA SECTION.
       LA220.
      *                                                                *
      * Find out of employee has Seattle Local                         *
      *                                                                *
      ******************************************************************

           MOVE EMPLID OF CHECK  TO  EMPLID OF BIND-DATA OF S-SEATTLE
           MOVE COMPANY OF PSLCT TO  COMPANY OF BIND-DATA OF S-SEATTLE
           MOVE 'WA'             TO  STATE OF BIND-DATA OF S-SEATTLE
           MOVE '63000'          TO  LOCALITY OF BIND-DATA OF S-SEATTLE
           MOVE CHECK-DT OF PYGRP TO  CHECK-DT OF BIND-DATA OF S-SEATTLE


           CALL 'PTPSQLRT' USING   ACTION-SELECT OF SQLRT
                                   SQLRT
                                   SQL-CURSOR OF S-SEATTLE
                                   SQL-STMT OF S-SEATTLE
                                   BIND-SETUP OF S-SEATTLE
                                   BIND-DATA OF S-SEATTLE
                                   SELECT-SETUP OF S-SEATTLE
                                   SELECT-DATA OF S-SEATTLE
           IF RTNCD-ERROR OF SQLRT

               MOVE 'GET-SEATTLE-DATA(EXEC)'  TO  ERR-SECTION OF SQLRT
               PERFORM ZZ000-SQL-ERROR
           END-IF

           INITIALIZE SELECT-DATA OF S-SEATTLE

           SET SEATTLE-TAX-CALC-NO OF W-SW TO TRUE

           CALL 'PTPSQLRT' USING   ACTION-FETCH OF SQLRT
                                   SQLRT
                                   SQL-CURSOR OF S-SEATTLE

           IF RTNCD-ERROR OF SQLRT

              IF RTNCD-END OF SQLRT

                 SET RTNCD-OK OF SQLRT  TO  TRUE

              ELSE

                 MOVE 'GET-SEATTLE-DATA'  TO
                                ERR-SECTION OF SQLRT
                 PERFORM ZZ000-SQL-ERROR

              END-IF

           ELSE

              IF LOCALITY OF SELECT-DATA OF S-SEATTLE = '63000'

                 SET SEATTLE-TAX-CALC-YES OF W-SW TO TRUE
              ELSE

                 SET SEATTLE-TAX-CALC-NO OF W-SW TO TRUE 
              END-IF

           END-IF


           .
       GET-SEATTLE-DATA-EXIT.


      /*****************************************************************
      *                                                                *
       LA230-GET-WA-LTC-DATA SECTION.
       LA230.
      *                                                                *
      * Find out of employee has WA LTC selected                       *
      *                                                                *
      ******************************************************************

           MOVE EMPLID OF CHECK  TO  EMPLID OF BIND-DATA OF S-WALTC
           MOVE COMPANY OF PSLCT TO  COMPANY OF BIND-DATA OF S-WALTC
           MOVE CHECK-DT OF PYGRP TO  CHECK-DT OF BIND-DATA OF S-WALTC


           CALL 'PTPSQLRT' USING   ACTION-SELECT OF SQLRT
                                   SQLRT
                                   SQL-CURSOR OF S-WALTC
                                   SQL-STMT OF S-WALTC
                                   BIND-SETUP OF S-WALTC
                                   BIND-DATA OF S-WALTC
                                   SELECT-SETUP OF S-WALTC
                                   SELECT-DATA OF S-WALTC
           IF RTNCD-ERROR OF SQLRT

               MOVE 'GET-WA-LTC-DATA(EXEC)'  TO  ERR-SECTION OF SQLRT
               PERFORM ZZ000-SQL-ERROR
           END-IF

           INITIALIZE SELECT-DATA OF S-WALTC

           CALL 'PTPSQLRT' USING   ACTION-FETCH OF SQLRT
                                   SQLRT
                                   SQL-CURSOR OF S-WALTC

           IF RTNCD-ERROR OF SQLRT

              IF RTNCD-END OF SQLRT

                 SET RTNCD-OK OF SQLRT  TO  TRUE

              ELSE

                 MOVE 'GET-WA-LTC-DATA'  TO
                                ERR-SECTION OF SQLRT
                 PERFORM ZZ000-SQL-ERROR

              END-IF

           ELSE

              IF LTC-IND OF SELECT-DATA OF S-WALTC = 'Y'
                SET PY-LTC-STATUS-YES OF WORK OF TAXDT TO TRUE
                PERFORM LA245-PROCESS-WA-LTC
              END-IF

           END-IF

           .
       GET-WA-LTC-DATA-EXIT.
      /*****************************************************************
      *                                                                *
       LA240-PROCESS-SEATTLE SECTION.
       LA240.
      *                                                                *
      * Calculate Seattle ER tax                                       *
      *                                                                *
      ******************************************************************


           MOVE ZERO TO ANN-ALL-GROSS-AMT OF W-WK
           MOVE ZERO TO WORK-SEATTLE-AMT  OF W-WK
           MOVE ZERO TO WORK-SEATTLE-GRS-YTD OF W-WK
           MOVE ZERO TO WORK-SEATTLE-GRS-QTD OF W-WK
           MOVE ZERO TO WORK-SEATTLE-GRS-MTD OF W-WK
           MOVE ZERO TO WORK-SEATTLE-CUR-YTD OF W-WK
           MOVE ZERO TO WORK-SEATTLE-AMT  OF W-WK
           MOVE ZERO TO WORK-SEATTLE-ADJ  OF W-WK
           MOVE ZERO TO WORK-SEATTLE-RND  OF W-WK
           MOVE ZERO TO WORK-TAX-RATE OF W-WK

           PERFORM LA250-GET-ALL-GROSS

           MOVE 'ZM' TO STATE OF TXWRK
           PERFORM LA260-GET-TAX-RATES

           EVALUATE TRUE

               WHEN PAY-FREQ-TYPE-WEEKLY OF PYGRP

                   MOVE  52  TO  ANNUAL-FACTOR OF W-WK

               WHEN PAY-FREQ-TYPE-BIWEEKLY OF PYGRP

                   MOVE  26  TO  ANNUAL-FACTOR OF W-WK

               WHEN PAY-FREQ-TYPE-SEMIMONTHLY OF PYGRP

                   MOVE  24  TO  ANNUAL-FACTOR OF W-WK

               WHEN PAY-FREQ-TYPE-MONTHLY OF PYGRP

                   MOVE  12  TO  ANNUAL-FACTOR OF W-WK

               WHEN PAY-FREQ-TYPE-QUARTERLY OF PYGRP

                   MOVE   4  TO  ANNUAL-FACTOR OF W-WK

               WHEN PAY-FREQ-TYPE-ANNUALLY OF PYGRP

                   MOVE   1  TO  ANNUAL-FACTOR OF W-WK

               WHEN PAY-FREQ-TYPE-DAILY OF PYGRP

                   MOVE 260  TO  ANNUAL-FACTOR OF W-WK
           END-EVALUATE

           IF SEPCHK OF CHECK = ZERO

              OR

              (SEPCHK OF CHECK > ZERO AND
               SEP-JOB-PAY-YES   OF W-SW )

             COMPUTE ANN-ALL-GROSS-AMT OF W-WK =
                   ALL-GROSS-AMT OF W-WK * ANNUAL-FACTOR OF W-WK

             SET TAXRT-IDX  TO  1

             SEARCH TAX-RT-TABLE

               WHEN ANN-ALL-GROSS-AMT OF W-WK
                    >=  LOW-GROSS OF STTRT(STTRT-IDX TAXRT-IDX)

                    MOVE TAX-RT OF TAX-RT-TABLE
                               OF STTRT(STTRT-IDX TAXRT-IDX)
                         TO WORK-TAX-RATE OF W-WK


             END-SEARCH

             COMPUTE WORK-SEATTLE-AMT  OF W-WK  =
               (ANN-ALL-GROSS-AMT OF W-WK  * WORK-TAX-RATE OF W-WK)
                / ANNUAL-FACTOR OF W-WK

           END-IF

           SET TARRY-IDX TO 1

           SEARCH TAXCALC-DATA OF TARRY

                WHEN TARRY-IDX > TAXCALC-COUNT OF TARRY

                   PERFORM LA290-SEATTLE-BALANCES
                   IF ONE-TIME-NA OF TARRY(TARRY-IDX)
                      AND PAYCHECK-ADJUST-NO OF CHECK
                      PERFORM LA280-SEATTLE-SELF-ADJUST
                   END-IF
                   PERFORM LA270-ADD-SEATTLE-TARRY

                WHEN STATE OF TARRY(TARRY-IDX) = 'WA'
                   AND LOCALITY OF TARRY (TARRY-IDX) = '63000'
                   AND TAX-CLASS-LOCAL-ER OF TARRY(TARRY-IDX)

                   MOVE TXGRS-YTD OF TARRY(TARRY-IDX) TO
                        WORK-SEATTLE-GRS-YTD OF W-WK
                   MOVE TXGRS-QTD OF TARRY(TARRY-IDX) TO
                        WORK-SEATTLE-GRS-QTD OF W-WK
                   MOVE TXGRS-MTD OF TARRY(TARRY-IDX) TO
                        WORK-SEATTLE-GRS-MTD OF W-WK
                   MOVE TAX-YTD OF TARRY(TARRY-IDX) TO
                        WORK-SEATTLE-CUR-YTD OF W-WK

                   IF ONE-TIME-NA OF TARRY(TARRY-IDX)
                      AND PAYCHECK-ADJUST-NO OF CHECK
                      PERFORM LA280-SEATTLE-SELF-ADJUST
                   END-IF

                   MOVE ALL-GROSS-AMT TO TXGRS-CUR OF TARRY(TARRY-IDX)
                   MOVE ALL-GROSS-AMT TO NLGRS-CUR OF TARRY(TARRY-IDX)

                   IF ONE-TIME-NA OF TARRY(TARRY-IDX)
                      MOVE WORK-SEATTLE-AMT  OF W-WK
                                        TO TAX-CUR OF TARRY(TARRY-IDX)
                   END-IF

           END-SEARCH


           .
       PROCESS-SEATTLE-EXIT.

      /*****************************************************************
      *                                                                *
       LA245-PROCESS-WA-LTC SECTION.
       LA245.
      *                                                                *
      * Calculate Washington Long Term Care tax                        *
      *                                                                *
      ******************************************************************



           MOVE ZERO TO ANN-ALL-GROSS-AMT OF W-WK
           MOVE ZERO TO WORK-LTC-AMT  OF W-WK
           MOVE ZERO TO WORK-LTC-GRS-YTD OF W-WK
           MOVE ZERO TO WORK-LTC-GRS-QTD OF W-WK
           MOVE ZERO TO WORK-LTC-GRS-MTD OF W-WK
           MOVE ZERO TO WORK-LTC-CUR-YTD OF W-WK
           MOVE ZERO TO WORK-LTC-RND  OF W-WK
           MOVE ZERO TO WORK-TAX-RATE OF W-WK

           PERFORM LA250-GET-ALL-GROSS

           MOVE 'WA' TO STATE OF TXWRK
           PERFORM LA260-GET-TAX-RATES

           EVALUATE TRUE

               WHEN PAY-FREQ-TYPE-WEEKLY OF PYGRP

                   MOVE  52  TO  ANNUAL-FACTOR OF W-WK

               WHEN PAY-FREQ-TYPE-BIWEEKLY OF PYGRP

                   MOVE  26  TO  ANNUAL-FACTOR OF W-WK

               WHEN PAY-FREQ-TYPE-SEMIMONTHLY OF PYGRP

                   MOVE  24  TO  ANNUAL-FACTOR OF W-WK

               WHEN PAY-FREQ-TYPE-MONTHLY OF PYGRP

                   MOVE  12  TO  ANNUAL-FACTOR OF W-WK

               WHEN PAY-FREQ-TYPE-QUARTERLY OF PYGRP

                   MOVE   4  TO  ANNUAL-FACTOR OF W-WK

               WHEN PAY-FREQ-TYPE-ANNUALLY OF PYGRP

                   MOVE   1  TO  ANNUAL-FACTOR OF W-WK

               WHEN PAY-FREQ-TYPE-DAILY OF PYGRP

                   MOVE 260  TO  ANNUAL-FACTOR OF W-WK
           END-EVALUATE

           IF SEPCHK OF CHECK = ZERO

              OR

              (SEPCHK OF CHECK > ZERO AND
               SEP-JOB-PAY-YES   OF W-SW )

             COMPUTE ANN-ALL-GROSS-AMT OF W-WK =
                   ALL-GROSS-AMT OF W-WK * ANNUAL-FACTOR OF W-WK


             MOVE ZERO  TO  WORK-TAX-RATE OF W-WK

             PERFORM VARYING OTHRT-IDX2  FROM  1  BY  1
                   UNTIL OTHRT-IDX2  >  OTH-RT-TABLE-COUNT
                           OF STTRT(STTRT-IDX)

               IF TAX-CLASS-LTC-EE OF OTH-RT-TABLE
                           OF STTRT(STTRT-IDX OTHRT-IDX2)

                  COMPUTE WORK-TAX-RATE OF W-WK ROUNDED
                          =  WORK-TAX-RATE OF W-WK
                          +  TAX-RT OF OTH-RT-TABLE
                                    OF STTRT(STTRT-IDX OTHRT-IDX2)
               END-IF
             END-PERFORM

             COMPUTE WORK-LTC-AMT  OF W-WK  =
               (ANN-ALL-GROSS-AMT OF W-WK  * WORK-TAX-RATE OF W-WK)
                / ANNUAL-FACTOR OF W-WK

           END-IF

           SET TARRY-IDX TO 1

           SEARCH TAXCALC-DATA OF TARRY

                WHEN TARRY-IDX > TAXCALC-COUNT OF TARRY

                   PERFORM LA295-WA-BALANCES
                   IF ONE-TIME-NA OF TARRY(TARRY-IDX)
                      AND PAYCHECK-ADJUST-NO OF CHECK
                      PERFORM LH156-CALC-LTC-SELF-ADJ
                   END-IF
                   PERFORM LA275-ADD-LTC-TARRY

                WHEN STATE OF TARRY(TARRY-IDX) = 'WA'
                   AND TAX-CLASS-LTC-EE OF TARRY(TARRY-IDX)

                   MOVE TXGRS-YTD OF TARRY(TARRY-IDX) TO
                        WORK-LTC-GRS-YTD OF W-WK
                   MOVE TXGRS-QTD OF TARRY(TARRY-IDX) TO
                        WORK-LTC-GRS-QTD OF W-WK
                   MOVE TXGRS-MTD OF TARRY(TARRY-IDX) TO
                        WORK-LTC-GRS-MTD OF W-WK
                   MOVE TAX-YTD OF TARRY(TARRY-IDX) TO
                        WORK-LTC-CUR-YTD OF W-WK

                   IF ONE-TIME-NA OF TARRY(TARRY-IDX)
                      AND PAYCHECK-ADJUST-NO OF CHECK
                      PERFORM LH156-CALC-LTC-SELF-ADJ
                   END-IF

                   MOVE ALL-GROSS-AMT TO TXGRS-CUR OF TARRY(TARRY-IDX)
                   MOVE ALL-GROSS-AMT TO NLGRS-CUR OF TARRY(TARRY-IDX)

                   IF ONE-TIME-NA OF TARRY(TARRY-IDX)
                      MOVE WORK-LTC-AMT  OF W-WK
                                        TO TAX-CUR OF TARRY(TARRY-IDX)
                   END-IF

           END-SEARCH


           .
       PROCESS-WA-LTC-EXIT.

      /*****************************************************************
      *                                                                *
       LA250-GET-ALL-GROSS SECTION.
       LA250.
      *                                                                *
      * GET THE ALL GROSS ACCUMULATOR AMOUNT FOR SEATTLE               *
      *                                                                *
      ******************************************************************

           MOVE 'A'  TO ACCM-USAGE OF S-AGRS-ACCUM

           CALL 'PTPSQLRT' USING   ACTION-SELECT OF SQLRT
                                   SQLRT
                                   SQL-CURSOR-COMMON OF SQLRT
                                   SQL-STMT OF S-AGRS-ACCUM
                                   BIND-SETUP OF S-AGRS-ACCUM
                                   BIND-DATA OF S-AGRS-ACCUM
                                   SELECT-SETUP OF S-AGRS-ACCUM
                                   SELECT-DATA OF S-AGRS-ACCUM

           IF RTNCD-ERROR OF SQLRT

               MOVE 'ALL-GROSS-ACCUM(SELECT)'
                       TO  ERR-SECTION OF SQLRT
               PERFORM ZZ000-SQL-ERROR
           END-IF

           INITIALIZE SELECT-DATA OF S-AGRS-ACCUM

           CALL 'PTPSQLRT' USING ACTION-FETCH OF SQLRT
                                 SQLRT
                                 SQL-CURSOR-COMMON OF SQLRT

           IF RTNCD-ERROR OF SQLRT

               IF RTNCD-END OF SQLRT
                   SET RTNCD-OK OF SQLRT  TO  TRUE
               ELSE
                   MOVE 'ALL-GROSS-ACCUM(FETCH)'
                           TO  ERR-SECTION OF SQLRT
                   PERFORM ZZ000-SQL-ERROR
               END-IF

           END-IF


           SET SARRY-IDX  TO  1

           MOVE ZERO TO ALL-GROSS-AMT OF W-WK

           SEARCH SPCL-EARNS-DATA OF SARRY

               WHEN ERNCD-SPCL OF SARRY(SARRY-IDX) =
                       ERNCD-SPCL OF S-AGRS-ACCUM

                    MOVE SPCL-EARNS OF SARRY(SARRY-IDX)
                      TO ALL-GROSS-AMT OF W-WK

                    COMPUTE ALL-GROSS-AMT OF W-WK
                          = ALL-GROSS-AMT OF W-WK
                          + WA-SEA-TXGRS-ADJ OF GRSWK

           END-SEARCH
           .
       GET-ALL-GROSS-EXIT.


      /*****************************************************************
      *                                                                *
       LA260-GET-TAX-RATES SECTION.
       LA260.
      *                                                                *
      * GET THE RATES FOR CALCULATING SEATTLE ER TAX                   *
      *                                                                *
      ******************************************************************


           MOVE SPACE TO LOCALITY OF TXWRK
           SET TAX-CLASS-WITHHOLDING OF TXLNK TO TRUE
           MOVE CHECK-DT OF PYGRP TO CHECK-DT OF TXLNK

           CALL 'PSPSTTRT' USING   SQLRT
                                   STTRT
                                   TXWRK
                                   TXLNK

           IF RTNCD-ERROR OF SQLRT

              MOVE 'CHECK-IF-NO-SWT-STATE-1(PSPPYNET)'
                             TO ERR-SECTION OF SQLRT
              PERFORM ZZ000-SQL-ERROR
           END-IF

           SET STTRT-IDX TO STTRT-PTR OF TXWRK

           .
       GET-SEATTLE-RATES-EXIT.

      /*****************************************************************
      *                                                                *
       LA270-ADD-SEATTLE-TARRY SECTION.
       LA270.
      *                                                                *
      * ADD A TARRY ROW FOR SEATTLE ER TAX                             *
      *                                                                *
      ******************************************************************


           SET TAXCALC-COUNT OF TARRY  TO  TARRY-IDX
           INITIALIZE TAXCALC-DATA OF TARRY(TARRY-IDX)
           MOVE 'WA'    TO  STATE OF TARRY(TARRY-IDX)
           MOVE '63000' TO  LOCALITY OF TARRY(TARRY-IDX)
           MOVE 'R' TO  TAX-CLASS OF TARRY(TARRY-IDX)

           MOVE ALL-GROSS-AMT TO TXGRS-CUR OF TARRY(TARRY-IDX)
           MOVE ALL-GROSS-AMT TO NLGRS-CUR OF TARRY(TARRY-IDX)
           MOVE WORK-SEATTLE-AMT  OF W-WK
                              TO TAX-CUR OF TARRY(TARRY-IDX)

           .
       ADD-SEATTLE-TARRY-EXIT.

      /*****************************************************************
      *                                                                *
       LA275-ADD-LTC-TARRY SECTION.
       LA275.
      *                                                                *
      * ADD A TARRY ROW FOR WA LTC TAX                                 *
      *                                                                *
      ******************************************************************


           SET TAXCALC-COUNT OF TARRY  TO  TARRY-IDX
           INITIALIZE TAXCALC-DATA OF TARRY(TARRY-IDX)
           MOVE 'WA'    TO  STATE OF TARRY(TARRY-IDX)
           MOVE 'AE' TO  TAX-CLASS OF TARRY(TARRY-IDX)

           MOVE ALL-GROSS-AMT TO TXGRS-CUR OF TARRY(TARRY-IDX)
           MOVE ALL-GROSS-AMT TO NLGRS-CUR OF TARRY(TARRY-IDX)
           MOVE WORK-LTC-AMT  OF W-WK
                              TO TAX-CUR OF TARRY(TARRY-IDX)

           .
       ADD-LTC-TARRY-EXIT.


      /*****************************************************************
      *                                                                *
       LA280-SEATTLE-SELF-ADJUST SECTION.
       LA280.
      *                                                                *
      * EVALUATE WHETHER SELF-ADJUSTMENT IS NECESSARY FOR SEATTLE ON   *
      * YTD BASIS.                                                     *
      *                                                                *
      ******************************************************************

           MOVE ZERO TO WORK-TOTAL-AMT OF W-WK
           MOVE ZERO TO WORK-TEMP-RATE OF W-WK

           IF SEPCHK OF CHECK > ZERO
              OR EMPL-RCD-NO OF CHECK > ZERO

              IF WORK-SEATTLE-GRS-MTD OF W-WK > ZERO
                AND WORK-TAX-RATE OF W-WK = ZERO

                IF SEP-JOB-PAY-YES OF W-SW
                   OR EMPL-RCD-NO OF CHECK > ZERO

                  COMPUTE WORK-TOTAL-AMT OF W-WK =
                        ANN-ALL-GROSS-AMT OF W-WK +
                        (WORK-SEATTLE-GRS-MTD OF W-WK * 12 )
                ELSE

                  COMPUTE WORK-TOTAL-AMT OF W-WK =
                        ALL-GROSS-AMT OF W-WK +
                        (WORK-SEATTLE-GRS-MTD OF W-WK *
                                          ANNUAL-FACTOR OF W-WK)

                END-IF

                SET TAXRT-IDX  TO  1

                SEARCH TAX-RT-TABLE

                WHEN WORK-TOTAL-AMT OF W-WK

                    >=  LOW-GROSS OF STTRT(STTRT-IDX TAXRT-IDX)

                    MOVE TAX-RT OF TAX-RT-TABLE
                               OF STTRT(STTRT-IDX TAXRT-IDX)
                         TO WORK-TAX-RATE OF W-WK

                END-SEARCH

              ELSE

                IF SEP-JOB-PAY-YES OF W-SW
                    OR EMPL-RCD-NO OF CHECK > ZERO

                    IF WORK-SEATTLE-GRS-YTD OF W-WK > ZERO

                      COMPUTE WORK-TEMP-RATE OF W-WK ROUNDED =
                         WORK-SEATTLE-CUR-YTD OF W-WK /
                         WORK-SEATTLE-GRS-YTD OF W-WK

                    END-IF

                    IF WORK-TEMP-RATE OF W-WK >
                                       WORK-TAX-RATE OF W-WK

                       SET TAXRT-IDX  TO  1

                       SEARCH TAX-RT-TABLE

                       WHEN TAX-RT OF TAX-RT-TABLE
                               OF STTRT(STTRT-IDX TAXRT-IDX)

                           >  WORK-TAX-RATE OF W-WK

                           MOVE TAX-RT OF TAX-RT-TABLE
                               OF STTRT(STTRT-IDX TAXRT-IDX)
                           TO WORK-TAX-RATE OF W-WK

                       END-SEARCH

                    END-IF
                END-IF
              END-IF
           END-IF


           COMPUTE WORK-SEATTLE-RND OF W-WK ROUNDED
           = (WORK-SEATTLE-GRS-YTD OF W-WK + ALL-GROSS-AMT OF W-WK )
              * WORK-TAX-RATE OF W-WK

           COMPUTE WORK-SEATTLE-ADJ OF W-WK
           = WORK-SEATTLE-RND OF W-WK - WORK-SEATTLE-CUR-YTD OF W-WK

           IF WORK-SEATTLE-ADJ OF W-WK NOT EQUAL ZERO AND
              WORK-SEATTLE-ADJ OF W-WK  NOT EQUAL WORK-SEATTLE-AMT
                                                                OF W-WK

              MOVE WORK-SEATTLE-ADJ OF W-WK TO WORK-SEATTLE-AMT OF W-WK
           END-IF


           IF WORK-SEATTLE-AMT  OF W-WK  < ZERO
              AND ALL-GROSS-AMT OF W-WK NOT < ZERO

              MOVE ZERO TO WORK-SEATTLE-AMT  OF W-WK

           END-IF
           .
       SEATTLE-SELF-ADJUST-EXIT.

      /*****************************************************************
      *                                                                *
       LA290-SEATTLE-BALANCES SECTION.
       LA290.
      *                                                                *
      * GET THE SEATTLE BALANCES FOR NEW TARRY CREATIONS               *
      *                                                                *
      ******************************************************************


           MOVE EMPLID OF CHECK         TO EMPLID OF S-BAL-SEA
           MOVE CALENDAR-YEAR  OF TAXWK TO BALANCE-YEAR OF S-BAL-SEA
           MOVE CALENDAR-MM    OF TAXWK TO BALANCE-PERIOD OF S-BAL-SEA

           CALL 'PTPSQLRT' USING   ACTION-SELECT OF SQLRT
                                   SQLRT
                                   SQL-CURSOR-COMMON OF SQLRT
                                   SQL-STMT OF S-BAL-SEA
                                   BIND-SETUP OF S-BAL-SEA
                                   BIND-DATA OF S-BAL-SEA
                                   SELECT-SETUP OF S-BAL-SEA
                                   SELECT-DATA OF S-BAL-SEA

           IF RTNCD-ERROR OF SQLRT

               MOVE 'SEATTLE-BALANCES(SELECT)'
                       TO  ERR-SECTION OF SQLRT
               PERFORM ZZ000-SQL-ERROR
           END-IF

           INITIALIZE SELECT-DATA OF S-BAL-SEA

           CALL 'PTPSQLRT' USING ACTION-FETCH OF SQLRT
                                 SQLRT
                                 SQL-CURSOR-COMMON OF SQLRT

           IF RTNCD-ERROR OF SQLRT

               IF RTNCD-END OF SQLRT
                   SET RTNCD-OK OF SQLRT  TO  TRUE
               ELSE
                   MOVE 'SEATTLE-BALANCES(FETCH)'
                           TO  ERR-SECTION OF SQLRT
                   PERFORM ZZ000-SQL-ERROR
               END-IF

           ELSE
               MOVE TXGRS-YTD OF SELECT-DATA OF S-BAL-SEA
                    TO WORK-SEATTLE-GRS-YTD OF W-WK
               MOVE TXGRS-QTD OF SELECT-DATA OF S-BAL-SEA
                    TO WORK-SEATTLE-GRS-QTD OF W-WK
               MOVE TXGRS-MTD OF SELECT-DATA OF S-BAL-SEA
                    TO WORK-SEATTLE-GRS-MTD OF W-WK
               MOVE TAX-YTD OF SELECT-DATA OF S-BAL-SEA
                    TO WORK-SEATTLE-CUR-YTD OF W-WK
           END-IF



           .
       SEATTLE-BALANCES-EXIT.

      /*****************************************************************
      *                                                                *
       LA295-WA-BALANCES SECTION.
       LA295.
      *                                                                *
      * GET THE WA LTC BALANCES FOR NEW TARRY CREATIONS                *
      *                                                                *
      ******************************************************************


           MOVE EMPLID OF CHECK         TO EMPLID OF S-BAL-WALTC
           MOVE CALENDAR-YEAR  OF TAXWK TO BALANCE-YEAR OF S-BAL-WALTC
           MOVE CALENDAR-MM    OF TAXWK TO BALANCE-PERIOD OF S-BAL-WALTC

           CALL 'PTPSQLRT' USING   ACTION-SELECT OF SQLRT
                                   SQLRT
                                   SQL-CURSOR-COMMON OF SQLRT
                                   SQL-STMT OF S-BAL-WALTC
                                   BIND-SETUP OF S-BAL-WALTC
                                   BIND-DATA OF S-BAL-WALTC
                                   SELECT-SETUP OF S-BAL-WALTC
                                   SELECT-DATA OF S-BAL-WALTC

           IF RTNCD-ERROR OF SQLRT

               MOVE 'WA-BALANCES(SELECT)'
                       TO  ERR-SECTION OF SQLRT
               PERFORM ZZ000-SQL-ERROR
           END-IF

           INITIALIZE SELECT-DATA OF S-BAL-WALTC

           CALL 'PTPSQLRT' USING ACTION-FETCH OF SQLRT
                                 SQLRT
                                 SQL-CURSOR-COMMON OF SQLRT

           IF RTNCD-ERROR OF SQLRT

               IF RTNCD-END OF SQLRT
                   SET RTNCD-OK OF SQLRT  TO  TRUE
               ELSE
                   MOVE 'WA-BALANCES(FETCH)'
                           TO  ERR-SECTION OF SQLRT
                   PERFORM ZZ000-SQL-ERROR
               END-IF

           ELSE
               MOVE TXGRS-YTD OF SELECT-DATA OF S-BAL-WALTC
                    TO WORK-LTC-GRS-YTD OF W-WK
               MOVE TXGRS-QTD OF SELECT-DATA OF S-BAL-WALTC
                    TO WORK-LTC-GRS-QTD OF W-WK
               MOVE TXGRS-MTD OF SELECT-DATA OF S-BAL-WALTC
                    TO WORK-LTC-GRS-MTD OF W-WK
               MOVE TAX-YTD OF SELECT-DATA OF S-BAL-WALTC
                    TO WORK-LTC-CUR-YTD OF W-WK
           END-IF



           .
       WA-BALANCES-EXIT.

      /*****************************************************************
      *                                                                *
       LA300-GET-BASE-ID SECTION.
       LA300.
      *                                                                *
      * ID STARTING BASE FOR SUT AND SDI                               *
      *                                                                *
      ******************************************************************

           MOVE RESIDENT OF TAXWK  TO  SAV-RESIDENT OF W-WK

           SET PASS-PGM-PYNET OF TGBTB-PASS  TO  TRUE
           SET RESIDENT-NO OF TAXWK  TO  TRUE

           CALL 'PSPTGBTB' USING   SQLRT
                                   CHECK
                                   TAXWK
                                   GRSWK
                                   TGBTB
                                   TGBTB-PASS
           IF RTNCD-ERROR OF SQLRT
               MOVE 'LA300-PSPPYNET1(PSPTGBTB)'
                       TO  ERR-SECTION OF SQLRT
               PERFORM ZZ000-SQL-ERROR
           END-IF

           SET RESIDENT-YES OF TAXWK  TO  TRUE

           CALL 'PSPTGBTB' USING   SQLRT
                                   CHECK
                                   TAXWK
                                   GRSWK
                                   TGBTB
                                   TGBTB-PASS
           IF RTNCD-ERROR OF SQLRT
               MOVE 'LA300-PSPPYNET2(PSPTGBTB)'
                       TO  ERR-SECTION OF SQLRT
               PERFORM ZZ000-SQL-ERROR
           END-IF

           MOVE WORK-IDX OF TGBTB-PASS  TO  WORK-IDX OF TGCTB-PASS
           MOVE RES-IDX OF TGBTB-PASS  TO  RES-IDX OF TGCTB-PASS
           MOVE SAV-RESIDENT OF W-WK  TO  RESIDENT OF TAXWK
           .
       GET-BASE-ID-EXIT.


      /*****************************************************************
      *                                                                *
       LA400-TOTAL-ALL-TAX-SETS SECTION.
       LA400.
      *                                                                *
      *  CALC SUM OF TAX SETS WITH ADD-TO-GROSS TXBL (AGT) EARNINGS    *
      *                                                                *
      ******************************************************************

           MOVE ZERO TO AG-FWT OF W-WK
           MOVE ZERO TO AG-SWT OF W-WK
           MOVE ZERO TO AG-LWT OF W-WK

           MOVE ZERO TO T-FWT OF W-WK
           MOVE ZERO TO T-SWT OF W-WK
           MOVE ZERO TO T-LWT OF W-WK

           MOVE ZERO TO AGT-TTL-FWT OF W-WK
           MOVE ZERO TO AGT-TTL-SWT OF W-WK
           MOVE ZERO TO AGT-TTL-LWT OF W-WK

           MOVE ZERO TO AGT-TTL-FWT-NEG OF W-WK
           MOVE ZERO TO AGT-TTL-SWT-NEG OF W-WK
           MOVE ZERO TO AGT-TTL-LWT-NEG OF W-WK
           MOVE ZERO TO PA-DED-ERN OF W-WK
           MOVE ZERO TO PA-DED-NON-PA-ERN OF W-WK

           SET EARNS-SUPPLE-NO OF W-SW TO TRUE
           SET SEP-JOB-PAY-NO   OF W-SW TO TRUE
           SET EARNS-SPCL-NO OF W-SW TO TRUE
           SET ADD-TO-GROSS-NO OF ATGSW TO TRUE
           SET ADD-TO-GROSS-NO OF W-SW TO TRUE
           SET PHIL-LOCAL-FOUND-NO OF W-SW TO TRUE
           SET NON-PHIL-LOCAL-FOUND-NO OF W-SW TO TRUE
           SET PA-BLANK-FOUND-NO OF W-SW TO TRUE
           SET PA-RES-ERN-FOUND-NO OF W-SW TO TRUE
           SET NON-PA-ERN-FOUND-NO OF W-SW TO TRUE
           SET MULTI-PA-LOC-AND-OTHER-NO OF W-SW TO TRUE

           IF FICA-STATUS-EE-MEDICARE OF EARRY(1) OR                    HP90003
              FICA-STATUS-EE-SUBJECT  OF EARRY(1)                       HP90003
                                                                        HP90003
               SET  CAL-FICA-CREDIT-YES OF TAXWK TO TRUE                HP90003
           ELSE                                                         HP90003
               SET  CAL-FICA-CREDIT-NO  OF TAXWK TO TRUE                HP90003
           END-IF                                                       HP90003

           MOVE TAX-SET OF EARRY(1) TO TAX-SET OF TAXWK

           PERFORM VARYING EARRY-IDX  FROM  1  BY  1
                   UNTIL EARRY-IDX  >  EARNINGS-COUNT OF EARRY
                           OR PAY-LINE-STATUS-ERROR OF CHECK

               SET ERNTB-IDX  TO  ERNTB-PTR OF EARRY(EARRY-IDX)



               IF  STATE OF EARRY(EARRY-IDX) = 'PA'
                   AND LOCALITY OF EARRY(EARRY-IDX) = SPACE

                  SET PA-BLANK-FOUND-YES OF W-SW TO TRUE

                  COMPUTE PA-DED-ERN OF W-WK =
                    PA-DED-ERN OF W-WK + EARNS OF EARRY(EARRY-IDX)

                   MOVE '880000' TO LOCALITY OF EARRY(EARRY-IDX)
               END-IF

               IF STATE OF EARRY(EARRY-IDX) = 'PA'
                  AND LOCALITY OF EARRY(EARRY-IDX) =
                       LOCALITY OF RESIDENCE OF TAXWK(1)

                  SET PA-RES-ERN-FOUND-YES OF W-SW TO TRUE

               END-IF

               IF STATE OF EARRY(EARRY-IDX) = 'PA'
                   AND LOCALITY OF EARRY(EARRY-IDX) NOT = SPACE
                   AND LOCALITY OF EARRY(EARRY-IDX) NOT = '510101'

                   SET NON-PHIL-LOCAL-FOUND-YES OF W-SW TO TRUE

               END-IF

               IF STATE OF EARRY(EARRY-IDX) = 'PA'
                   AND LOCALITY OF EARRY(EARRY-IDX) = '510101'

                   SET PHIL-LOCAL-FOUND-YES OF W-SW TO TRUE

               END-IF

               IF STATE OF EARRY(EARRY-IDX) NOT = 'PA'

                   SET NON-PA-ERN-FOUND-YES OF W-SW TO TRUE

                   IF TAX-GRS-COMPNT OF ERNTB(ERNTB-IDX) = 'GTL'

                      CONTINUE

                   ELSE

                     COMPUTE PA-DED-ERN OF W-WK =
                    PA-DED-ERN OF W-WK + EARNS OF EARRY(EARRY-IDX)

                     IF SUBJECT-FWT-YES OF ERNTB(ERNTB-IDX)

                        COMPUTE PA-DED-NON-PA-ERN OF W-WK =
                        PA-DED-NON-PA-ERN + EARNS OF EARRY(EARRY-IDX)
                     END-IF
                   END-IF

               END-IF

               IF STATE OF EARRY(EARRY-IDX) NOT = SPACE

                   IF PAY-FREQ-TYPE OF W-WK = SPACE

                      MOVE PAY-FREQ-TYPE OF EARRY(EARRY-IDX)
                        TO PAY-FREQ-TYPE OF W-WK
                   ELSE

                   IF PAY-FREQ-TYPE OF EARRY(EARRY-IDX)
                        NOT = PAY-FREQ-TYPE OF W-WK
                        SET MULTI-PAY-FREQ-YES OF PYGRP TO TRUE
                     END-IF
                   END-IF

                   IF PAY-FREQ-TYPE OF EARRY(EARRY-IDX)
                           NOT =  PAY-FREQ-TYPE OF TAX-SET OF TAXWK
                       OR TAX-PERIODS OF EARRY(EARRY-IDX)
                               NOT = TAX-PERIODS OF TAX-SET OF TAXWK
                           OR TAX-METHOD OF EARRY(EARRY-IDX)
                                   NOT = TAX-METHOD OF TAX-SET OF TAXWK
                               OR PAID-PRDS-PER-YR OF EARRY(EARRY-IDX)
                                       NOT = PAID-PRDS-PER-YEAR
                                                   OF TAX-SET OF TAXWK
                                   OR FICA-STATUS-EE OF EARRY(EARRY-IDX)
                                           NOT = FICA-STATUS-EE
                                                   OF TAX-SET OF TAXWK

                       IF FICA-STATUS-EE-MEDICARE OF EARRY(EARRY-IDX)   HP90003
                         OR FICA-STATUS-EE-SUBJECT  OF EARRY(EARRY-IDX) HP90003
                                                                        HP90003
                           SET CAL-FICA-CREDIT-YES OF TAXWK TO TRUE     HP90003
                       END-IF                                           HP90003
                       MOVE ADD-GROSS OF ERNTB(ERNTB-IDX) TO
                            ADD-TO-GROSS-SW OF W-SW

                       PERFORM LA410-TOTAL-FED-TAX-SET

                       IF TAX-METHOD OF EARRY(EARRY-IDX) = 'S'
                          SET EARNS-SUPPLE-YES OF W-SW TO TRUE
                       END-IF

                       IF JOB-PAY-YES OF EARRY(EARRY-IDX)
                          AND SEPCHK OF CHECK > ZERO
                          SET SEP-JOB-PAY-YES   OF W-SW TO TRUE
                       END-IF

                       IF TAX-METHOD OF EARRY(EARRY-IDX) = 'X'
                          SET EARNS-SPCL-YES OF W-SW TO TRUE
                       END-IF

                       SET ADD-TO-GROSS-NO OF W-SW TO TRUE

                       MOVE HIGH-VALUES TO STATE OF TAX-SET OF TAXWK
                   END-IF

                   IF STATE OF EARRY(EARRY-IDX)
                           NOT = STATE OF TAX-SET OF TAXWK

                       PERFORM LA420-TOTAL-STATE-TAX-SET

                       MOVE HIGH-VALUES TO LOCALITY OF TAX-SET OF TAXWK
                   END-IF

                   IF LOCALITY OF EARRY(EARRY-IDX)
                           NOT = LOCALITY OF TAX-SET OF TAXWK

                       PERFORM LA430-TOTAL-LOCAL-TAX-SET
                   END-IF

                   IF ADD-GROSS-YES OF ERNTB(ERNTB-IDX)
                       OR (GROSSUP-YES OF CHECK
                           AND ADD-GROSS-NO OF ERNTB(ERNTB-IDX))

                       IF EARNS OF EARRY(EARRY-IDX) NOT = ZERO

                           SET ADD-TO-GROSS-YES OF ATGSW TO TRUE
                       END-IF

                       COMPUTE AG-FWT OF W-WK
                           =   AG-FWT OF W-WK
                           +   EARNS  OF EARRY(EARRY-IDX)
                       COMPUTE AG-SWT OF W-WK
                           =   AG-SWT OF W-WK
                           +   EARNS  OF EARRY(EARRY-IDX)
                       COMPUTE AG-LWT OF W-WK
                           =   AG-LWT OF W-WK
                           +   EARNS  OF EARRY(EARRY-IDX)
                   END-IF

                   IF ADD-GROSS-YES OF ERNTB(ERNTB-IDX)
                      AND NOT GROSSUP-YES OF CHECK
                      AND NOT SUBJECT-FWT-YES OF ERNTB(ERNTB-IDX)
                      AND EARNS OF EARRY(EARRY-IDX) < ZERO

                       COMPUTE AG-FWT OF W-WK
                           =   AG-FWT OF W-WK
                           -   EARNS  OF EARRY(EARRY-IDX)
                       COMPUTE AG-SWT OF W-WK
                           =   AG-SWT OF W-WK
                           -   EARNS  OF EARRY(EARRY-IDX)
                       COMPUTE AG-LWT OF W-WK
                           =   AG-LWT OF W-WK
                           -   EARNS  OF EARRY(EARRY-IDX)
                   END-IF

                   IF SUBJECT-FWT-YES OF ERNTB(ERNTB-IDX)

                       COMPUTE T-FWT OF W-WK
                           =   T-FWT OF W-WK
                           +   EARNS OF EARRY(EARRY-IDX)
                       COMPUTE T-SWT OF W-WK
                           =   T-SWT OF W-WK
                           +   EARNS OF EARRY(EARRY-IDX)
                       COMPUTE T-LWT OF W-WK
                           =   T-LWT OF W-WK
                           +   EARNS OF EARRY(EARRY-IDX)
                   END-IF

                   MOVE TAX-SET OF EARRY(EARRY-IDX)
                                       TO TAX-SET OF TAXWK
               END-IF
           END-PERFORM

           IF PHIL-LOCAL-FOUND-YES OF W-SW AND
              NON-PHIL-LOCAL-FOUND-YES OF W-SW AND
              PA-BLANK-FOUND-YES OF W-SW AND
              NON-PA-ERN-FOUND-YES OF W-SW

              SET MULTI-PA-LOC-AND-OTHER-YES OF W-SW TO TRUE

           END-IF

      *  ADD LAST TAX SETS TO TOTAL

           PERFORM LA410-TOTAL-FED-TAX-SET
           PERFORM LA420-TOTAL-STATE-TAX-SET
           PERFORM LA430-TOTAL-LOCAL-TAX-SET

      *  USE TOTAL OF NEG TAX SETS IF NO POS TAX SETS

           IF AGT-TTL-FWT OF W-WK = ZERO

               MOVE AGT-TTL-FWT-NEG OF W-WK
                       TO AGT-TTL-FWT OF W-WK
               MOVE AGT-TTL-SWT-NEG OF W-WK
                       TO AGT-TTL-SWT OF W-WK
               MOVE AGT-TTL-LWT-NEG OF W-WK
                       TO AGT-TTL-LWT OF W-WK
           END-IF

      *  CHECK FOR A DEDUCTION REFUND THAT COULD ADD TO THE GROSS OF AN
      *  OTHERWISE ZERO-NET CHECK

           PERFORM VARYING DARRY-IDX FROM 1 BY 1
                   UNTIL ADD-TO-GROSS-YES OF ATGSW
                       OR DARRY-IDX > DEDUCTION-COUNT
                                           OF DARRY

               PERFORM VARYING DCLAS-IDX FROM 1 BY 1
                       UNTIL ADD-TO-GROSS-YES OF ATGSW
                           OR DCLAS-IDX > DEDUCTION-CLASS-MAX
                                               OF DARRY
                               OR DED-CLASS OF DARRY
                                       (DARRY-IDX DCLAS-IDX)
                                               = SPACE

                   PERFORM VARYING DCALC-IDX FROM 1 BY 1
                           UNTIL ADD-TO-GROSS-YES OF ATGSW
                               OR DCALC-IDX > DEDUCTION-CALC-MAX
                                                   OF DARRY

                       IF ONE-TIME-REFUND OF DARRY
                                       (DARRY-IDX DCLAS-IDX DCALC-IDX)
                               AND (DED-ADDL-AMT OF DARRY
                                       (DARRY-IDX DCLAS-IDX DCALC-IDX)
                                               NOT = ZERO
                                   OR DED-RATE-PCT OF DARRY
                                       (DARRY-IDX DCLAS-IDX DCALC-IDX)
                                                   NOT = ZERO)

                           SET ADD-TO-GROSS-YES OF ATGSW TO TRUE
                       END-IF
                   END-PERFORM
               END-PERFORM
           END-PERFORM

      *  CHECK FOR A TAX REFUND THAT COULD ADD TO THE GROSS OF AN
      *  OTHERWISE ZERO-NET CHECK

           PERFORM VARYING TARRY-IDX FROM 1 BY 1
                   UNTIL ADD-TO-GROSS-YES OF ATGSW
                       OR TARRY-IDX > TAXCALC-COUNT OF TARRY

               IF TAX-ONE-TIME OF TARRY(TARRY-IDX) < ZERO

                   SET ADD-TO-GROSS-YES OF ATGSW TO TRUE
               END-IF
           END-PERFORM

           .
       TOTAL-ALL-TAX-SETS-EXIT.


      /*****************************************************************
      *                                                                *
       LA410-TOTAL-FED-TAX-SET SECTION.
       LA410.
      *                                                                *
      * TOTAL POS/NEG FED TAX SET                                      *
      *                                                                *
      ******************************************************************

           EVALUATE TRUE

               WHEN AG-FWT OF W-WK NOT < ZERO
                       AND T-FWT OF W-WK NOT < ZERO

                   IF AG-FWT OF W-WK < T-FWT OF W-WK

                       COMPUTE AGT-TTL-FWT OF W-WK
                           =   AGT-TTL-FWT OF W-WK
                           +   AG-FWT OF W-WK
                   ELSE

                       COMPUTE AGT-TTL-FWT OF W-WK
                           =   AGT-TTL-FWT OF W-WK
                           +   T-FWT OF W-WK
                   END-IF

               WHEN AG-FWT OF W-WK NOT > ZERO
                       AND T-FWT OF W-WK NOT > ZERO

                   IF AG-FWT OF W-WK > T-FWT OF W-WK

                      IF AG-FWT OF W-WK = ZERO
                         AND T-FWT OF W-WK < ZERO
                         AND EARNS-SUPPLE-YES OF W-SW
                         AND TAX-METHOD-SUPPLEMENTAL
                             OF TAX-SET OF TAXWK
                         COMPUTE AGT-TTL-FWT OF W-WK
                             =   AGT-TTL-FWT OF W-WK
                             +   T-FWT OF W-WK
                      ELSE
                         COMPUTE AGT-TTL-FWT-NEG OF W-WK
                             =   AGT-TTL-FWT-NEG OF W-WK
                             +   AG-FWT OF W-WK
                      END-IF

                   ELSE

                       IF EARNS-SUPPLE-YES OF W-SW
                          AND EARNS-SPCL-YES OF W-SW
                          AND ADD-TO-GROSS-YES OF W-SW
                          COMPUTE AGT-TTL-FWT-NEG OF W-WK
                              =   AGT-TTL-FWT-NEG OF W-WK
                              +   AG-FWT OF W-WK
                       ELSE
                          COMPUTE AGT-TTL-FWT-NEG OF W-WK
                              =   AGT-TTL-FWT-NEG OF W-WK
                              +   T-FWT OF W-WK
                       END-IF

                   END-IF

               WHEN AG-FWT OF W-WK NOT < ZERO
                       AND T-FWT OF W-WK < ZERO

                   IF EARNS-SUPPLE-YES OF W-SW
                      AND TAX-METHOD-SUPPLEMENTAL
                          OF TAX-SET OF TAXWK
                      COMPUTE AGT-TTL-FWT OF W-WK
                          =   AGT-TTL-FWT OF W-WK
                          +   T-FWT OF W-WK
                   END-IF

               WHEN AG-FWT OF W-WK < ZERO
                       AND T-FWT OF W-WK NOT < ZERO

                   IF EARNS-SUPPLE-YES OF W-SW
                      AND TAX-METHOD-SUPPLEMENTAL
                          OF TAX-SET OF TAXWK
                      COMPUTE AGT-TTL-FWT OF W-WK
                          =   AGT-TTL-FWT OF W-WK
                          +   AG-FWT OF W-WK
                   END-IF
           END-EVALUATE

           COMPUTE T-FWT-TTL OF W-WK
                 = T-FWT-TTL OF W-WK
                 + T-FWT OF W-WK

           MOVE ZERO TO AG-FWT OF W-WK
           MOVE ZERO TO T-FWT  OF W-WK

           .
       TOTAL-FED-TAX-SET-EXIT.

      /*****************************************************************
      *                                                                *
       LA420-TOTAL-STATE-TAX-SET SECTION.
       LA420.
      *                                                                *
      * TOTAL POS/NEG STATE TAX SET                                    *
      *                                                                *
      ******************************************************************

           EVALUATE TRUE

               WHEN AG-SWT OF W-WK NOT < ZERO
                       AND T-SWT OF W-WK NOT < ZERO

                   IF AG-SWT OF W-WK < T-SWT OF W-WK

                       COMPUTE AGT-TTL-SWT OF W-WK
                           =   AGT-TTL-SWT OF W-WK
                           +   AG-SWT  OF W-WK
                   ELSE

                       COMPUTE AGT-TTL-SWT OF W-WK
                           =   AGT-TTL-SWT OF W-WK
                           +   T-SWT OF W-WK
                   END-IF

               WHEN AG-SWT  OF W-WK NOT > ZERO
                       AND T-SWT OF W-WK NOT > ZERO

                   IF AG-SWT OF W-WK > T-SWT OF W-WK

                       COMPUTE AGT-TTL-SWT-NEG OF W-WK
                           =   AGT-TTL-SWT-NEG OF W-WK
                           +   AG-SWT  OF W-WK
                   ELSE

                       COMPUTE AGT-TTL-SWT-NEG OF W-WK
                           =   AGT-TTL-SWT-NEG OF W-WK
                           +   T-SWT  OF W-WK
                   END-IF
           END-EVALUATE

           MOVE ZERO TO AG-SWT OF W-WK
           MOVE ZERO TO T-SWT OF W-WK

           .
       TOTAL-STATE-TAX-SET-EXIT.

      /*****************************************************************
      *                                                                *
       LA430-TOTAL-LOCAL-TAX-SET SECTION.
       LA430.
      *                                                                *
      * TOTAL POS/NEG LOCAL TAX SET                                    *
      *                                                                *
      ******************************************************************

           EVALUATE TRUE

               WHEN AG-LWT OF W-WK NOT < ZERO
                       AND T-LWT OF W-WK NOT < ZERO

                   IF AG-LWT OF W-WK < T-LWT OF W-WK

                       COMPUTE AGT-TTL-LWT OF W-WK
                           =   AGT-TTL-LWT OF W-WK
                           +   AG-LWT  OF W-WK
                   ELSE

                       COMPUTE AGT-TTL-LWT OF W-WK
                           =   AGT-TTL-LWT OF W-WK
                           +   T-LWT OF W-WK
                   END-IF

               WHEN AG-LWT  OF W-WK NOT > ZERO
                       AND T-LWT OF W-WK NOT > ZERO

                   IF AG-LWT OF W-WK > T-LWT OF W-WK

                       COMPUTE AGT-TTL-LWT-NEG OF W-WK
                           =   AGT-TTL-LWT-NEG OF W-WK
                           +   AG-LWT  OF W-WK
                   ELSE

                       COMPUTE AGT-TTL-LWT-NEG OF W-WK
                           =   AGT-TTL-LWT-NEG OF W-WK
                           +   T-LWT  OF W-WK
                   END-IF
           END-EVALUATE

           MOVE ZERO TO AG-LWT OF W-WK
           MOVE ZERO TO T-LWT OF W-WK

           .
       TOTAL-LOCAL-TAX-SET-EXIT.

      /*****************************************************************
      *                                                                *
       LA450-GET-SUPPL-BAL-OTHR-CHK SECTION.
       LA450.
      *
      *  GET THE SPCL ACCUM BALANCES FOR OTHER CHECKS NOT CONFIRMED
      *                                                                *
      ******************************************************************

           MOVE EMPLID OF CHECK        TO EMPLID OF S-SPCLBAL
           MOVE OFF-CYCLE OF PSLCT     TO OFF-CYCLE OF S-SPCLBAL
           MOVE COMPANY OF PSLCT       TO COMPANY OF S-SPCLBAL
           MOVE PAYGROUP OF PSLCT      TO PAYGROUP OF S-SPCLBAL
           MOVE PAY-END-DT OF PSLCT    TO PAY-END-DT OF S-SPCLBAL
           MOVE SUPPC-SPCL-ACCUM-CD    OF PSLCT
                                       TO SPCL-ERNCD OF S-SPCLBAL

           CALL 'PTPSQLRT' USING   ACTION-SELECT OF SQLRT
                                   SQLRT
                                   SQL-CURSOR-COMMON OF SQLRT
                                   SQL-STMT OF S-SPCLBAL
                                   BIND-SETUP OF S-SPCLBAL
                                   BIND-DATA OF S-SPCLBAL
                                   SELECT-SETUP OF S-SPCLBAL
                                   SELECT-DATA OF S-SPCLBAL
           IF RTNCD-ERROR OF SQLRT

               MOVE 'GET-SPCL-BAL-OTHR-CHK' TO ERR-SECTION OF SQLRT
               PERFORM ZZ000-SQL-ERROR
           END-IF

           INITIALIZE SELECT-DATA OF S-SPCLBAL

           CALL 'PTPSQLRT' USING   ACTION-FETCH OF SQLRT
                                   SQLRT
                                   SQL-CURSOR-COMMON   OF SQLRT
           IF RTNCD-ERROR OF SQLRT
                   AND NOT RTNCD-END OF SQLRT

               MOVE 'GET-SPCL-BAL-OTHR-CHK(FETCH)'
                                            TO ERR-SECTION OF SQLRT
               PERFORM ZZ000-SQL-ERROR
           ELSE
               IF RTNCD-END OF SQLRT
                   MOVE TXGRS-CUR OF S-SPCLBAL TO
                                     SUPPL-TXGRS-TOT OF SUPPC
               ELSE
                   MOVE TXGRS-CUR OF S-SPCLBAL TO
                                     SUPPL-TXGRS-TOT OF SUPPC
               END-IF
           END-IF

           SET RTNCD-OK OF SQLRT  TO  TRUE

           .

       GET-SUPPL-BAL-OTHR-CHK.

      /*****************************************************************
      *                                                                *
       LA460-GET-SUPPL-BAL-CAL-YR SECTION.
       LA460.
      *
      *  GET ALL THE SUPPL BALANCES FOR THIS CALENDAR YEAR AND VERIFY
      *  WHETHER THE TOTAL IS OVER $1 MILLION.
      *                                                                *
      ******************************************************************
           SET BALID-IDX               TO BALID-IDX-FOR-CAL-YR
           MOVE EMPLID OF CHECK        TO EMPLID OF S-CNTBAL
           MOVE BALANCE-YEAR OF BALID(BALID-IDX)
                                       TO BAL-YEAR OF S-CNTBAL
           MOVE SUPPC-SPCL-ACCUM-CD OF PSLCT
                                       TO SPCL-ERNCD OF S-CNTBAL
           MOVE BAL-ID-FOR-CAL-YR OF PSLCT
                                       TO BAL-ID     OF S-CNTBAL
           MOVE ZEROS TO SUPPL-BAL-EXTRA
           SET HIGH-SUPPL-AMT-NO TO TRUE


           CALL 'PTPSQLRT' USING   ACTION-SELECT OF SQLRT
                                   SQLRT
                                   SQL-CURSOR-COMMON OF SQLRT
                                   SQL-STMT OF S-CNTBAL
                                   BIND-SETUP OF S-CNTBAL
                                   BIND-DATA OF S-CNTBAL
                                   SELECT-SETUP OF S-CNTBAL
                                   SELECT-DATA OF S-CNTBAL
           IF RTNCD-ERROR OF SQLRT

               MOVE 'GET-SUPPL-BAL-CAL-YR' TO ERR-SECTION OF SQLRT
               PERFORM ZZ000-SQL-ERROR
           END-IF

           INITIALIZE SELECT-DATA OF S-CNTBAL

           CALL 'PTPSQLRT' USING   ACTION-FETCH OF SQLRT
                                   SQLRT
                                   SQL-CURSOR-COMMON   OF SQLRT
           IF RTNCD-ERROR OF SQLRT
                   AND NOT RTNCD-END OF SQLRT

               MOVE 'GET-SUPPL-BAL-CAL-YR(FETCH)'
                                            TO ERR-SECTION OF SQLRT
               PERFORM ZZ000-SQL-ERROR
           ELSE
               IF RTNCD-END OF SQLRT
                   COMPUTE SUPPL-BAL-TOTAL = SUPPL-BAL-TOTAL +
                                         EARNS OF EARRY(EARRY-IDX)
               ELSE
                   COMPUTE SUPPL-BAL-TOTAL = SUPPL-BAL-TOTAL +
                                             GRS-YTD OF S-CNTBAL +
                                             EARNS OF EARRY(EARRY-IDX)
                   MOVE GRS-YTD OF S-CNTBAL TO SUPPL-BAL-YTD OF SUPPC
               END-IF

               IF SUPPL-BAL-TOTAL > WK-SUPPL-LIMIT
                  SET HIGH-SUPPL-AMT-YES TO TRUE
                  IF GRS-YTD OF S-CNTBAL >= WK-SUPPL-LIMIT
                     MOVE EARNS OF EARRY(EARRY-IDX)
                                          TO SUPPL-BAL-EXTRA
                  ELSE
                     COMPUTE SUPPL-BAL-EXTRA = SUPPL-BAL-TOTAL -
                                            WK-SUPPL-LIMIT
                  END-IF
                  COMPUTE SUPPL-AMOUNT OF SUPPC =
                          SUPPL-AMOUNT OF SUPPC +
                          EARNS OF EARRY(EARRY-IDX) - SUPPL-BAL-EXTRA
                  IF SUPPL-AMOUNT OF SUPPC < 0
                     MOVE ZEROS TO SUPPL-AMOUNT OF SUPPC
                  END-IF
               ELSE
                  COMPUTE SUPPL-AMOUNT OF SUPPC =
                          SUPPL-AMOUNT OF SUPPC +
                          EARNS OF EARRY(EARRY-IDX)
               END-IF

               MOVE SUPPL-BAL-EXTRA TO SUPPL-AMT-EXTRA OF SUPPC
           END-IF

           SET RTNCD-OK OF SQLRT  TO  TRUE
           .

       GET-SUPPL-BAL-CAL-YR-EXIT.


      /*****************************************************************
      *                                                                *
       LA470-GET-HIRE-DT SECTION.
       LA470.
      *
      *  GET HIRE DATE FOR EMPLOYEE WITH EARNINGS IN CT
      *                                                                *
      ******************************************************************

           MOVE EMPLID OF CHECK        TO EMPLID OF S-CTHIRE
           MOVE COMPANY OF PSLCT       TO COMPANY OF S-CTHIRE
           MOVE '2011-07-31'           TO EFFDT OF S-CTHIRE

           CALL 'PTPSQLRT' USING   ACTION-SELECT OF SQLRT
                                   SQLRT
                                   SQL-CURSOR-COMMON OF SQLRT
                                   SQL-STMT OF S-CTHIRE
                                   BIND-SETUP OF S-CTHIRE
                                   BIND-DATA OF S-CTHIRE
                                   SELECT-SETUP OF S-CTHIRE
                                   SELECT-DATA OF S-CTHIRE
           IF RTNCD-ERROR OF SQLRT

               MOVE 'GET-CT-HIRE-DT' TO ERR-SECTION OF SQLRT
               PERFORM ZZ000-SQL-ERROR
           END-IF

           INITIALIZE SELECT-DATA OF S-CTHIRE

           CALL 'PTPSQLRT' USING   ACTION-FETCH OF SQLRT
                                   SQLRT
                                   SQL-CURSOR-COMMON   OF SQLRT
           IF RTNCD-ERROR OF SQLRT
                   AND NOT RTNCD-END OF SQLRT


               MOVE HIRE-YES OF S-CTHIRE  TO CT-NEW-HIRE-SW OF SUPPC
               MOVE 'GET-CT-HIRE-DT(FETCH)'
                                            TO ERR-SECTION OF SQLRT
               PERFORM ZZ000-SQL-ERROR
           ELSE

               MOVE HIRE-YES OF S-CTHIRE  TO CT-NEW-HIRE-SW OF SUPPC
               SET PROC-SUPPL-FOR-CT-YES  TO TRUE

           END-IF

           SET RTNCD-OK OF SQLRT  TO  TRUE

           .

       GET-HIRE-DT-EXIT.



      /*****************************************************************
      *                                                                *
       LA480-GET-SUPPL-BAL-CT SECTION.
       LA480.
      *
      *  GET CT SUPPL BALANCES FOR THIS CALENDAR YEAR
      *   (INPUT BY CUSTOMER)
      *                                                                *
      ******************************************************************

           SET BALID-IDX               TO BALID-IDX-FOR-CAL-YR
           MOVE EMPLID OF CHECK        TO EMPLID OF S-CTBAL
           MOVE BALANCE-YEAR OF BALID(BALID-IDX)
                                       TO BAL-YEAR OF S-CTBAL
           MOVE CT-SUPPC-SPCL-ACCUM-CD OF W-WK
                                       TO SPCL-ERNCD OF S-CTBAL
           MOVE BAL-ID-FOR-CAL-YR OF PSLCT
                                       TO BAL-ID     OF S-CTBAL

           CALL 'PTPSQLRT' USING   ACTION-SELECT OF SQLRT
                                   SQLRT
                                   SQL-CURSOR-COMMON OF SQLRT
                                   SQL-STMT OF S-CTBAL
                                   BIND-SETUP OF S-CTBAL
                                   BIND-DATA OF S-CTBAL
                                   SELECT-SETUP OF S-CTBAL
                                   SELECT-DATA OF S-CTBAL
           IF RTNCD-ERROR OF SQLRT

               MOVE 'GET-CT-SUPPL-BAL' TO ERR-SECTION OF SQLRT
               PERFORM ZZ000-SQL-ERROR
           END-IF

           INITIALIZE SELECT-DATA OF S-CTBAL

           CALL 'PTPSQLRT' USING   ACTION-FETCH OF SQLRT
                                   SQLRT
                                   SQL-CURSOR-COMMON   OF SQLRT
           IF RTNCD-ERROR OF SQLRT
                   AND NOT RTNCD-END OF SQLRT

               MOVE 'GET-CT-SUPPL-BAL(FETCH)'
                                            TO ERR-SECTION OF SQLRT
               PERFORM ZZ000-SQL-ERROR
           ELSE

               MOVE  GRS-YTD OF S-CTBAL   TO CT-SUPPL-BAL OF SUPPC
               SET PROC-SUPPL-FOR-CT-YES  TO TRUE

           END-IF

           SET RTNCD-OK OF SQLRT  TO  TRUE


           .

       GET-SUPPL-BAL-CT-EXIT.

      /*****************************************************************
      *                                                                *
       LA500-ADJUST-TXGRS-SPCL-AMTS SECTION.
       LA500.
      *                                                                *
      * ADJUST TO TAXABLE GROSS ON SPECIAL ACCUMULATORS                *
      *                                                                *
      ******************************************************************

           PERFORM VARYING EARRY-SPEC-IDX  FROM  1 BY  1
                   UNTIL EARRY-SPEC-IDX  >  EARNINGS-COUNT OF EARRY


               SET ERNTB-IDX
                       TO  ERNTB-PTR OF EARRY(EARRY-SPEC-IDX)

               PERFORM VARYING ERNSP-IDX FROM 1  BY 1
                       UNTIL ERNSP-IDX
                       > SPCL-EARNS-TABLE-MAX OF ERNTB
                        OR ERNCD-SPCL
                             OF ERNTB(ERNTB-IDX
                                ERNSP-IDX) =  SPACE

                    IF SPCL-EFFECT-ADD-TXGRS OF
                           ERNTB(ERNTB-IDX ERNSP-IDX)

                         PERFORM LA510-GET-SARRY
                    END-IF
                END-PERFORM
           END-PERFORM

           .

       ADJUST-TXGRS-SPCL-AMTS-EXIT.


      /*****************************************************************
      *                                                                *
       LA510-GET-SARRY SECTION.
       LA510.
      *                                                                *
      * GET SARRY RECORD FOR ADJUSTMENT                                *
      *                                                                *
      ******************************************************************


           SET SARRY-IDX  TO  1

           SEARCH SPCL-EARNS-DATA OF SARRY

               WHEN ERNCD-SPCL OF SARRY(SARRY-IDX) =
                       ERNCD-SPCL OF ERNTB(ERNTB-IDX
                                ERNSP-IDX)

                   CONTINUE

           END-SEARCH


               IF NOT COUNTRY-CANADA OF PYGRP
                      COMPUTE SPCL-EARNS OF SARRY(SARRY-IDX)  ROUNDED
                                  =  SPCL-EARNS OF SARRY(SARRY-IDX)
                                  -  SPCL-EARNS OF SARRY(SARRY-IDX)
                                  +  SUPPL-TXGRS OF SUPPC

                      COMPUTE SPCL-EARNS-ACCUM OF
                                           SARRY(SARRY-IDX)  ROUNDED
                                  =  SPCL-EARNS-ACCUM OF
                                                  SARRY(SARRY-IDX)
                                  -  SPCL-EARNS-ACCUM OF
                                                  SARRY(SARRY-IDX)
                                  +  SUPPL-TXGRS OF SUPPC
               END-IF

           .

       GET-SARRY-EXIT.


      /*****************************************************************
      *                                                                *
       LA550-CREATE-SARRY-CT SECTION.
       LA550.
      *                                                                *
      * CREATE SARRY RECORD FOR CONNECTICUT SPECIAL ACCUMULATOR        *
      *                                                                *
      ******************************************************************

           SET CREATE-YES OF W-SW TO TRUE

           CALL 'PSPFNDSE' USING   SARRY
                                   SARRY-PASS OF W-WK
                                   CT-SUPPC-SPCL-ACCUM-CD OF W-WK
                                   EMPL-RCD-NO OF W-WK
                                   CREATE-SW OF W-SW

           SET SARRY-IDX  TO  SPCL-EARNS-COUNT OF SARRY

           IF NOT COUNTRY-CANADA OF PYGRP

               COMPUTE SPCL-EARNS OF SARRY(SARRY-IDX)  ROUNDED
                              =  SPCL-EARNS OF SARRY(SARRY-IDX)
                              -  SPCL-EARNS OF SARRY(SARRY-IDX)
                              +  CT-SUPPL-CUR OF SUPPC
               COMPUTE SPCL-EARNS-ACCUM OF SARRY(SARRY-IDX)  ROUNDED
                              =  SPCL-EARNS-ACCUM OF
                                              SARRY(SARRY-IDX)
                              -  SPCL-EARNS-ACCUM OF SARRY(SARRY-IDX)
                              +  CT-SUPPL-CUR OF SUPPC

           END-IF

           .

       CREATE-SARRY-CT-EXIT.


      /*****************************************************************HP99994
      *                                                                *HP99994
       LA700-PREPARE-1042-ARRAY SECTION.                                HP99994
       LA700.                                                           HP99994
      *                                                                *HP99994
      ******************************************************************HP99994
                                                                        HP99994
           MOVE ZERO  TO WK-EARNS-TTL OF W-WK                           HP99994
           MOVE 20    TO 1042-ERN-CNT OF W-WK                           HP99994
           PERFORM VARYING 1042-IDX FROM 1 BY 1                         HP99994
                       UNTIL 1042-IDX > 1042-ERN-CNT OF W-WK            HP99994
               MOVE SPACES TO 1042-ERN-STATE                            HP99994
                                           OF 1042-ERN-TABLE (1042-IDX) HP99994
               MOVE ZERO TO 1042-ERN-AMT   OF 1042-ERN-TABLE (1042-IDX) HP99994
               MOVE ZERO TO ST-TTL-AMT     OF 1042-ERN-TABLE (1042-IDX) HP99994
           END-PERFORM                                                  HP99994
           MOVE WK-FIRST TO 1042-ERN-CNT OF W-WK                        HP99994
           MOVE '$U' TO 1042-ERN-STATE OF 1042-ERN-TABLE (WK-FIRST)     HP99994
                                                                        HP99994
           PERFORM VARYING EARRY-IDX  FROM  1  BY  1                    HP99994
                       UNTIL EARRY-IDX  >  EARNINGS-COUNT OF EARRY      HP99994
                                                                        HP99994
               SET ERNTB-IDX  TO  ERNTB-PTR OF EARRY(EARRY-IDX)         HP99994
                                                                        HP99994
               SET 1042-IDX TO WK-FIRST                                 HP99994
               PERFORM LA710-CUMULATE-1042-ERN                          HP99994
                                                                        HP99994
               IF 1042-ERN-CNT OF W-WK = WK-FIRST                       HP99994
                   ADD 1 TO 1042-ERN-CNT OF W-WK                        HP99994
                   SET 1042-IDX TO 1042-ERN-CNT OF W-WK                 HP99994
                   MOVE STATE OF EARRY(EARRY-IDX)                       HP99994
                        TO 1042-ERN-STATE OF 1042-ERN-TABLE (1042-IDX)  HP99994
                                                                        HP99994
                   PERFORM LA710-CUMULATE-1042-ERN                      HP99994
               ELSE                                                     HP99994
                   SET STATE-FOUND-NO OF W-WK TO TRUE                   HP99994
                   PERFORM VARYING 1042-IDX FROM 2 BY 1                 HP99994
                               UNTIL 1042-IDX > 1042-ERN-CNT OF W-WK    HP99994
                                  OR STATE-FOUND-YES OF W-WK            HP99994
                                                                        HP99994
                       IF 1042-ERN-STATE OF 1042-ERN-TABLE (1042-IDX)   HP99994
                              = STATE OF EARRY(EARRY-IDX)               HP99994
                                                                        HP99994
                           SET STATE-FOUND-YES OF W-WK TO TRUE          HP99994
                           PERFORM LA710-CUMULATE-1042-ERN              HP99994
                       END-IF                                           HP99994
                   END-PERFORM                                          HP99994
                                                                        HP99994
                   IF STATE-FOUND-NO OF W-WK                            HP99994
                       ADD 1 TO 1042-ERN-CNT OF W-WK                    HP99994
                       SET 1042-IDX TO 1042-ERN-CNT OF W-WK             HP99994
                       MOVE STATE OF EARRY(EARRY-IDX)                   HP99994
                            TO 1042-ERN-STATE                           HP99994
                                       OF 1042-ERN-TABLE (1042-IDX)     HP99994
                       PERFORM LA710-CUMULATE-1042-ERN                  HP99994
                   END-IF                                               HP99994
               END-IF                                                   HP99994
           END-PERFORM                                                  HP99994
                                                                        HP99994
           .                                                            HP99994
       PREPARE-1042-ARRAY-EXIT.                                         HP99994


      /*****************************************************************HP99994
      *                                                                *HP99994
       LA710-CUMULATE-1042-ERN SECTION.                                 HP99994
       LA710.                                                           HP99994
      *                                                                *HP99994
      ******************************************************************HP99994
                                                                        HP99994
           IF SUBJECT-FWT-YES OF ERNTB(ERNTB-IDX)                       HP99994
                                                                        HP99994
               COMPUTE ST-TTL-AMT OF 1042-ERN-TABLE (1042-IDX)          HP99994
                     = ST-TTL-AMT OF 1042-ERN-TABLE (1042-IDX)          HP99994
                     + EARNS OF EARRY(EARRY-IDX)                        HP99994
                                                                        HP99994
               IF INCOME-CD-1042 OF ERNTB(ERNTB-IDX) NOT= SPACES        HP99994
                                                                        HP99994
                   COMPUTE 1042-ERN-AMT OF 1042-ERN-TABLE (1042-IDX)    HP99994
                         = 1042-ERN-AMT OF 1042-ERN-TABLE (1042-IDX)    HP99994
                         + EARNS OF EARRY(EARRY-IDX)                    HP99994
               END-IF                                                   HP99994
           END-IF                                                       HP99994
           .                                                            HP99994
       CUMULATE-1042-ERN-EXIT.                                          HP99994

      /*****************************************************************
      *                                                                *
       LD000-TAX-CHANGE SECTION.
       LD000.
      *                                                                *
      * CALC TAXES FOR THIS TAX ENTITY                                 *
      *                                                                *
      ******************************************************************

           MOVE ZERO TO TTLDED-PRETAX OF GRSWK

           EVALUATE TRUE

               WHEN AG-CUR-TS OF W-WK NOT < ZERO
                       AND T-CUR-TS OF W-WK NOT < ZERO

                   IF AG-CUR-TS OF W-WK < T-CUR-TS OF W-WK

                       MOVE AG-CUR-TS OF W-WK TO AGT-CUR-TS OF W-WK
                   ELSE

                       MOVE  T-CUR-TS OF W-WK TO AGT-CUR-TS OF W-WK
                   END-IF

               WHEN AG-CUR-TS OF W-WK NOT > ZERO
                       AND T-CUR-TS OF W-WK NOT > ZERO

                   IF AG-CUR-TS OF W-WK > T-CUR-TS OF W-WK

                       MOVE AG-CUR-TS OF W-WK TO AGT-CUR-TS OF W-WK
                   ELSE

                       MOVE  T-CUR-TS OF W-WK TO AGT-CUR-TS OF W-WK
                   END-IF

               WHEN OTHER

                       MOVE ZERO TO AGT-CUR-TS OF W-WK
           END-EVALUATE

           IF (AGT-TTL-TS OF W-WK > ZERO
               AND AGT-CUR-TS OF W-WK > ZERO)
                   OR  (AGT-TTL-TS OF W-WK < ZERO
                       AND AGT-CUR-TS OF W-WK < ZERO)

              IF (AGT-TTL-TS OF W-WK > ZERO
                 AND AGT-CUR-TS OF W-WK > ZERO)

                 IF AGT-CUR-TS OF W-WK > AGT-TTL-TS OF W-WK
                    COMPUTE AGT-CUM-TS OF W-WK ROUNDED
                        =   AGT-CUM-TS OF W-WK
                        +   AGT-TTL-TS OF W-WK
                 ELSE
                    COMPUTE AGT-CUM-TS OF W-WK ROUNDED
                        =   AGT-CUM-TS OF W-WK
                        +   AGT-CUR-TS OF W-WK
                 END-IF
              ELSE

                 IF AGT-CUR-TS OF W-WK < AGT-TTL-TS OF W-WK
                    COMPUTE AGT-CUM-TS OF W-WK ROUNDED
                        =   AGT-CUM-TS OF W-WK
                        +   AGT-TTL-TS OF W-WK
                 ELSE
                    COMPUTE AGT-CUM-TS OF W-WK ROUNDED
                        =   AGT-CUM-TS OF W-WK
                        +   AGT-CUR-TS OF W-WK
                 END-IF

              END-IF

           END-IF

           IF STATE OF TAX-SET OF TAXWK NOT = '$U'
              AND LOCALITY OF TAX-SET OF TAXWK = SPACE
              AND FWT OF GRSWK(1) > 0
              AND AGT-CUR-TS OF W-WK = ZERO
              AND SPEC-SWT-EXEMPT-YES OF W-SW

              COMPUTE AGT-CUM-TS OF W-WK
                    = AGT-CUM-TS OF W-WK 
                    + FWT OF GRSWK(1)
           END-IF

           IF NOT PAY-LINE-STATUS-ERROR OF CHECK

               IF PUBLIC-SECTOR-YES OF PSLCT                            HP99994
                   PERFORM LD400-GET-1042-ERN                           HP99994
               END-IF                                                   HP99994

               SET PA-EARN-NO OF TGCTB-PASS  TO  TRUE
               PERFORM VARYING DARRY-IDX  FROM  1  BY  1
                       UNTIL DARRY-IDX  >  DEDUCTION-COUNT OF DARRY

                   PERFORM VARYING DCLAS-IDX  FROM  1  BY  1
                           UNTIL DCLAS-IDX
                                   >  DEDUCTION-CLASS-MAX OF DARRY

                       IF DED-CLASS OF DARRY(DARRY-IDX DCLAS-IDX)
                               NOT =  SPACE AND
                          DED-CLASS-TAXABLE OF
                                          DARRY(DARRY-IDX DCLAS-IDX)

                           SET DEDT1-IDX
                                   TO  DEDTB-PTR OF DARRY(DARRY-IDX)
                           SET DEDT2-IDX
                                   TO  DEDTB-PTR OF DARRY(DARRY-IDX)
                           SET DEDT3-IDX
                                   TO  DEDTB-PTR OF DARRY(DARRY-IDX)
                           SET DEDT4-IDX
                                   TO  DEDTB-PTR OF DARRY(DARRY-IDX)
                           SET DEDT5-IDX
                                   TO  DEDTB-PTR OF DARRY(DARRY-IDX)
                           SET DEDT6-IDX
                                   TO  DEDTB-PTR OF DARRY(DARRY-IDX)
                           SET DEDC1-IDX  TO  DCLAS-IDX
                           SET DEDC2-IDX  TO  DCLAS-IDX
                           PERFORM LD100-APPLY-PERCENTAGE
                           PERFORM LD200-APPLY-DEDUCTIONS
                       END-IF
                   END-PERFORM
               END-PERFORM

               PERFORM VARYING DARRY-IDX  FROM  1  BY  1
                       UNTIL DARRY-IDX  >  DEDUCTION-COUNT OF DARRY

                   PERFORM VARYING DCLAS-IDX  FROM  1  BY  1
                           UNTIL DCLAS-IDX
                                   >  DEDUCTION-CLASS-MAX OF DARRY

                       IF DED-CLASS OF DARRY(DARRY-IDX DCLAS-IDX)
                               NOT =  SPACE AND
                          NOT DED-CLASS-TAXABLE OF
                                          DARRY(DARRY-IDX DCLAS-IDX)

                           SET DEDT1-IDX
                                   TO  DEDTB-PTR OF DARRY(DARRY-IDX)
                           SET DEDT2-IDX
                                   TO  DEDTB-PTR OF DARRY(DARRY-IDX)
                           SET DEDT3-IDX
                                   TO  DEDTB-PTR OF DARRY(DARRY-IDX)
                           SET DEDT4-IDX
                                   TO  DEDTB-PTR OF DARRY(DARRY-IDX)
                           SET DEDT5-IDX
                                   TO  DEDTB-PTR OF DARRY(DARRY-IDX)
                           SET DEDT6-IDX
                                   TO  DEDTB-PTR OF DARRY(DARRY-IDX)
                           SET DEDC1-IDX  TO  DCLAS-IDX
                           SET DEDC2-IDX  TO  DCLAS-IDX
                           PERFORM LD100-APPLY-PERCENTAGE

                           IF (TAX-GRS-COMPNT OF
                                          DEDT6(DEDT6-IDX DEDC2-IDX)
                                          = '125'
                               OR (TAX-GRS-COMPNT OF
                                          DEDT6(DEDT6-IDX DEDC2-IDX)
                                          = SPACES
                                    AND DED-CLASS-BEFORE-TAX OF
                                          DARRY(DARRY-IDX DCLAS-IDX)))
                           AND (CALC-FED-ALL OF TAXWK OR
                                CALC-FED-WH OF TAXWK)
                               COMPUTE PA-TOT-125-DED OF W-WK =
                                       PA-TOT-125-DED OF W-WK
                                        + DED-AMT OF W-WK
                           END-IF
                           PERFORM LD200-APPLY-DEDUCTIONS
                       END-IF
                   END-PERFORM
               END-PERFORM

               IF PUBLIC-SECTOR-YES OF PSLCT AND                        HP99994
                        SUBJ-1042-DED OF GRSWK NOT= ZERO                HP99994
                   PERFORM LD500-APPLY-1042-GRS                         HP99994
               END-IF                                                   HP99994

               MOVE ZERO TO FUT-SUBTR-NOT-APPLIED OF W-WK
               MOVE ZERO TO FICA-SUBTR-NOT-APPLIED OF W-WK
               MOVE ZERO TO BEFORE-TAX-NOT-APPLIED OF W-WK
      *  ISSUE A WARNING IF TXBL EARNINGS OF A TAX SET HAVE BECOME
      *  NEGATIVE (OR MORE NEGATIVE) AS A RESULT OF ALLOCATING
      *  BEFORE-TAX DEDUCTIONS (000138)


               IF MANUAL-CHECK-NO OF CHECK
                       AND PROCESS-NON-TIPS OF GRSWK

                   IF  EARNS-NEG-YES OF W-SW
                           AND AG-CUR-TS OF W-WK NOT < ZERO
                               AND AG-NET-TS OF W-WK NOT < ZERO

                       SET MSGID-EARNINGS-WENT-NEG OF PYMSG TO TRUE

                       MOVE PAY-FREQ-TYPE OF TAX-SET OF TAXWK
                                           TO PAY-FREQ-TYPE OF W-MSGDATA
                       MOVE TAX-PERIODS OF TAX-SET OF TAXWK
                                           TO TAX-PERIODS OF W-MSGDATA
                       MOVE TAX-METHOD OF TAX-SET OF TAXWK
                                           TO TAX-METHOD OF W-MSGDATA
                       MOVE STATE OF TAX-SET OF TAXWK
                                           TO STATE OF W-MSGDATA
                       MOVE LOCALITY OF TAX-SET OF TAXWK
                                           TO LOCALITY OF W-MSGDATA
                       MOVE SUT-JURISDICTION OF TAX-SET OF TAXWK
                                           TO SUT-JURISDICTION
                                                       OF W-MSGDATA
                       MOVE MSGDATA1 OF W-MSGDATA TO MSGDATA1 OF PYMSG
                       MOVE MSGDATA2 OF W-MSGDATA TO MSGDATA2 OF PYMSG
                       MOVE MSGDATA3 OF W-MSGDATA TO MSGDATA3 OF PYMSG

                       PERFORM ZM000-MESSAGE
                   END-IF
               END-IF

               IF TAX-CALCD-YES OF W-WK

                  IF STATE OF TAX-SET OF TAXWK = 'PA'
                     AND LOCALITY OF TAX-SET OF TAXWK = SPACE
                     AND TAX-METHOD-ANNUALIZED OF TAX-SET OF TAXWK
                     AND NOT RECIPROCITY-RULE-NOWORK
                                             OF STATE-RCP OF TAXWK
                     AND NON-PA-ERN-FOUND-YES OF W-SW
                     AND PA-RES-ERN-FOUND-NO OF W-SW

                     COMPUTE SWT OF GRSWK(1)
                           = SWT OF GRSWK(1)
                           + PA-BLK-DED-AMT OF W-WK
                  END-IF

                  PERFORM LD050-CALCULATE-TAX
               ELSE
                  IF TAX-SET OF EARRY(EARRY-IDX) NOT = TAX-SET OF TAXWK
                     IF CALC-FED-ALL OF TAXWK
                        MOVE '$U' TO  STATE OF TAX-SET OF TAXWK
                        MOVE SPACES TO  LOCALITY OF TAX-SET OF TAXWK
                     END-IF
                     PERFORM LD050-CALCULATE-TAX
                  END-IF
               END-IF

           END-IF

           MOVE ZERO  TO  AG-CUR-TS OF W-WK
           MOVE ZERO  TO  T-CUR-TS OF W-WK
           MOVE ZERO  TO  AG-TTL-TS OF W-WK
           MOVE ZERO  TO  AG-NET-TS OF W-WK
           MOVE ZERO  TO  TTLDED-PRETAX OF GRSWK

           IF STATE OF TAX-SET OF TAXWK NOT = '$U'
              AND TAXGRS-CMP-COUNT OF GRSWK > ZERO

              PERFORM LH700-MATCH-TARRY-TGCID

           END-IF

           IF CALC-FED-FICA-ONLY OF TAXWK                               HP99997
               CONTINUE                                                 HP99998
           ELSE                                                         HP99998
               PERFORM JB000-INIT-TAX1042-GROSS-WORK                    HP00009
           END-IF                                                       HP99998

           .
       TAX-CHANGE-EXIT.


      /*****************************************************************
      *                                                                *
       LD050-CALCULATE-TAX SECTION.
       LD050.
      *                                                                *
      ******************************************************************

           IF NOT PAY-LINE-STATUS-ERROR OF CHECK

               CALL 'PSPTCALC' USING   SQLRT
                                       TARRY
                                       GRSWK
                                       TAXWK
                                       TAXDT
                                       PYGRP
                                       CHECK
                                       STTRT
                                       LCLRT
                                       SUTRT
                                       SDIRT
                                       VDIRT
                                       FLIRT
                                       SUTTB
                                       TGBTB
                                       PSLCT
                                       WHADJ
                                       TXARY
                                       YARRY                            HP99999
                                       TRTTB                            HP99999
                                       EARRY
                                       ERNTB
                                       PCTST
                                       CMPTT
                                       SUPPC
                                       FMLRT
                                       FRMWK
                                       PYNET-PASS
               IF RTNCD-ERROR OF SQLRT

                   MOVE 'CALC-TAX(PSPPYNET)'  TO  ERR-SECTION
                                                    OF SQLRT
                   PERFORM ZZ000-SQL-ERROR
               END-IF
           END-IF

           .
       CALCULATE-TAX-EXIT.


      /*****************************************************************
      *                                                                *
       LD100-APPLY-PERCENTAGE SECTION.
       LD100.
      *                                                                *
      ******************************************************************

      *  ADJUST TAXABLE GROSSES BY ALLOCATING BEFORE-TAX DEDUCTIONS
      *  AND TAXABLE BENEFITS TO TAX SETS WITH EITHER NET POSITIVE OR
      *  NET NEGATIVE ADD-TO-GROSS TAXABLE (AGT) EARNINGS.

      *  DED-CUR INCLUDES PAYSHEET OVERRIDES AND REFUNDS.

           EVALUATE TRUE

               WHEN AGT-TTL-TS OF W-WK > ZERO
                       AND AGT-CUR-TS OF W-WK > ZERO

               WHEN AGT-TTL-TS OF W-WK > ZERO
                    AND AGT-CUR-TS OF W-WK = ZERO
                    AND STATE OF TAX-SET OF TAXWK NOT = '$U'
                    AND LOCALITY OF TAX-SET OF TAXWK = SPACE
                    AND FWT OF GRSWK(1) > 0
                    AND SPEC-SWT-EXEMPT-YES OF W-SW

                   COMPUTE WK-AMT  OF W-WK ROUNDED
                       =  ((DED-CUR OF DARRY(DARRY-IDX DCLAS-IDX)
                       +   DED-CUR-PAYBK
                                   OF DARRY(DARRY-IDX DCLAS-IDX))
                       *   AGT-CUM-TS OF W-WK
                       /   AGT-TTL-TS OF W-WK)
                       + HIGH-PREC-0

                   IF (DED-CUR OF DARRY(DARRY-IDX DCLAS-IDX) +
                       DED-CUR-PAYBK OF DARRY(DARRY-IDX DCLAS-IDX) > 0)
                      AND ((DED-CUR OF DARRY(DARRY-IDX DCLAS-IDX) +
                       DED-CUR-PAYBK OF DARRY(DARRY-IDX DCLAS-IDX)) <
                           WK-AMT  OF W-WK)

                     IF STATE OF TAX-SET OF TAXWK NOT = '$U'
                        AND SPEC-SWT-EXEMPT-YES OF W-SW

                       COMPUTE WK-AMT  OF W-WK
                          = DED-CUR-PAYBK OF DARRY(DARRY-IDX DCLAS-IDX)
                          + DED-CUR OF DARRY(DARRY-IDX DCLAS-IDX)
                     ELSE

                       MOVE DED-CUR OF DARRY(DARRY-IDX DCLAS-IDX)
                               TO  WK-AMT  OF W-WK
                     END-IF
                   END-IF

                   IF (DED-CUR OF DARRY(DARRY-IDX DCLAS-IDX) +
                       DED-CUR-PAYBK OF DARRY(DARRY-IDX DCLAS-IDX) < 0)
                      AND ((DED-CUR OF DARRY(DARRY-IDX DCLAS-IDX) +
                       DED-CUR-PAYBK OF DARRY(DARRY-IDX DCLAS-IDX)) >
                           WK-AMT  OF W-WK)

                       MOVE DED-CUR OF DARRY(DARRY-IDX DCLAS-IDX)
                               TO  WK-AMT  OF W-WK
                   END-IF

                   COMPUTE DED-AMT OF W-WK ROUNDED
                       =   WK-AMT  OF W-WK
                       -   DED-CUM-FROM-AG-POS
                                   OF DARRY(DARRY-IDX DCLAS-IDX)
                   MOVE WK-AMT OF W-WK
                       TO  DED-CUM-FROM-AG-POS
                                   OF DARRY(DARRY-IDX DCLAS-IDX)

               WHEN AGT-TTL-TS OF W-WK < ZERO
                       AND AGT-CUR-TS OF W-WK < ZERO

                   COMPUTE WK-AMT  OF W-WK ROUNDED
                       =  ((DED-CUR OF DARRY(DARRY-IDX DCLAS-IDX)
                       +   DED-CUR-PAYBK
                                   OF DARRY(DARRY-IDX DCLAS-IDX))
                       *   AGT-CUM-TS OF W-WK
                       /   AGT-TTL-TS OF W-WK)
                       + HIGH-PREC-0

                   COMPUTE DED-AMT OF W-WK ROUNDED
                       =   WK-AMT  OF W-WK
                       -   DED-CUM-FROM-AG-NEG
                                   OF DARRY(DARRY-IDX DCLAS-IDX)
                   MOVE WK-AMT OF W-WK
                       TO  DED-CUM-FROM-AG-NEG
                                   OF DARRY(DARRY-IDX DCLAS-IDX)

               WHEN AGT-TTL-TS OF W-WK = ZERO
                       AND AGT-CUR-TS OF W-WK = ZERO
                               AND DED-CUM-FROM-OTHER
                                   OF DARRY(DARRY-IDX DCLAS-IDX) = ZERO

                   COMPUTE DED-AMT OF W-WK
                       =   DED-CUR OF DARRY(DARRY-IDX DCLAS-IDX)
                       +   DED-CUR-PAYBK
                                   OF DARRY(DARRY-IDX DCLAS-IDX)

                   MOVE DED-AMT OF W-WK
                           TO DED-CUM-FROM-OTHER
                                   OF DARRY(DARRY-IDX DCLAS-IDX)

               WHEN AGT-TTL-TS OF W-WK < ZERO
                       AND AGT-CUR-TS OF W-WK = ZERO
                               AND DED-CUM-FROM-OTHER
                                   OF DARRY(DARRY-IDX DCLAS-IDX) = ZERO
                       AND EARNS-NOT-SUPPL-FOUND-YES OF W-SW

                   COMPUTE DED-AMT OF W-WK
                       =   DED-CUR OF DARRY(DARRY-IDX DCLAS-IDX)
                       +   DED-CUR-PAYBK
                                   OF DARRY(DARRY-IDX DCLAS-IDX)

                   MOVE DED-AMT OF W-WK
                           TO DED-CUM-FROM-OTHER
                                   OF DARRY(DARRY-IDX DCLAS-IDX)

               WHEN OTHER

                   MOVE ZERO  TO  DED-AMT OF W-WK
           END-EVALUATE

           IF STATE OF TAX-SET OF TAXWK = 'PA'
              AND LOCALITY OF TAX-SET  OF TAXWK NOT = SPACE
              AND LOCALITY OF TAX-SET  OF TAXWK NOT = '510101'
              AND PA-EARNS-TTL OF GRSWK NOT = EARNS-TTL OF GRSWK
              AND PA-EARNS-TTL OF GRSWK NOT EQUAL ZERO
              AND PA-AG-CUM-TS OF W-WK NOT EQUAL ZERO

              COMPUTE DED-AMT OF W-WK ROUNDED
                   =  ((DED-CUR OF DARRY(DARRY-IDX DCLAS-IDX)
                   +   DED-CUR-PAYBK OF DARRY(DARRY-IDX DCLAS-IDX))
                   *   PA-AG-CUM-TS OF W-WK
                   /   PA-EARNS-TTL OF GRSWK)
                   + HIGH-PREC-0
           END-IF

           IF STATE OF TAX-SET OF TAXWK = 'PA'
              AND LOCALITY OF TAX-SET  OF TAXWK = SPACE
              AND NON-PA-ERN-FOUND-YES OF W-SW
              AND PA-RES-ERN-FOUND-NO OF W-SW
              AND PA-EARNS-TTL OF GRSWK NOT EQUAL ZERO
              AND DED-CUR OF DARRY(DARRY-IDX DCLAS-IDX) NOT EQUAL ZERO

              COMPUTE PA-BLK-DED-AMT OF W-WK ROUNDED
                    = DED-AMT OF W-WK
                    - (DED-CUR OF DARRY(DARRY-IDX DCLAS-IDX)
                    * PA-BLK-AG-CUM OF W-WK
                    / PA-EARNS-TTL OF GRSWK)
           END-IF

           IF FWT OF GRSWK(1) > ZERO
              AND T-FWT-TTL OF W-WK > ZERO
              AND T-FWT-CUM OF W-WK > ZERO
              AND AG-FWT-CUM OF W-WK > ZERO
              AND SPEC-SWT-EXEMPT-YES OF W-SW
              AND NOT AGT-FWT-NEG-YES OF W-SW
              AND AGT-TTL-TS OF W-WK < ZERO
              AND STATE OF TAX-SET OF TAXWK NOT = '$U'

                  COMPUTE DED-AMT OF W-WK  ROUNDED
                       =  (DED-CUR OF DARRY(DARRY-IDX DCLAS-IDX)
                       +  DED-CUR-PAYBK OF DARRY(DARRY-IDX DCLAS-IDX))
                       *  WK-AMT OF GRSWK
                       /  T-FWT-TTL OF W-WK
           END-IF

           .
       APPLY-PERCENTAGE-EXIT.


      /*****************************************************************
      *                                                                *
       LD200-APPLY-DEDUCTIONS SECTION.
       LD200.
      *                                                                *
      * APPLY THIS DEDUCTION TO THE GROSSES AFFECTED                   *
      *                                                                *
      ******************************************************************

           IF PUBLIC-SECTOR-YES OF PSLCT                                HP99994
               IF WK-EARNS-TTL OF W-WK NOT= ZERO                        HP99994
                    AND WK-1042-EARNS OF W-WK NOT= ZERO                 HP99994
                                                                        HP99994
                   COMPUTE WK-1042-DED-AMT OF W-WK ROUNDED              HP99994
                         = DED-AMT OF W-WK                              HP99994
                         * (WK-1042-EARNS OF W-WK                       HP99994
                         / WK-EARNS-TTL OF W-WK)                        HP99994
                   COMPUTE WK-REMAIN-DED-AMT OF W-WK                    HP99994
                         = DED-AMT OF W-WK                              HP99994
                         - WK-1042-DED-AMT OF W-WK                      HP99994
               ELSE                                                     HP99994
                   MOVE ZERO TO WK-1042-DED-AMT OF W-WK                 HP99994
                   MOVE DED-AMT OF W-WK TO WK-REMAIN-DED-AMT OF W-WK    HP99994
               END-IF                                                   HP99994
           END-IF                                                       HP99994

           IF DED-CLASS-TAXABLE OF DARRY(DARRY-IDX DCLAS-IDX)

               IF PUBLIC-SECTOR-YES OF PSLCT                            HP99994
                   IF 1042-EFFECT-YES OF DEDT5(DEDT5-IDX DEDC1-IDX)     HP99994
                       COMPUTE SUBJ-1042-DED OF GRSWK                   HP99994
                             = SUBJ-1042-DED OF GRSWK                   HP99994
                             + WK-1042-DED-AMT OF W-WK                  HP99994
                   END-IF                                               HP99994
                   COMPUTE TX-DED-TTL OF GRSWK                          HP99994
                         = TX-DED-TTL OF GRSWK                          HP99994
                         + WK-REMAIN-DED-AMT OF W-WK                    HP99994
               END-IF                                                   HP99994

               COMPUTE FWT OF GRSWK(1)
                       =  FWT OF GRSWK(1)
                       +  DED-AMT OF W-WK

               IF WITHHOLD-FWT-NO OF DEDT5(DEDT5-IDX DEDC1-IDX)

                   COMPUTE TX-BEN-NOT-TAXED OF WHADJ(1)
                           =  TX-BEN-NOT-TAXED OF WHADJ(1)
                           +  DED-AMT OF W-WK
               END-IF
           ELSE

               IF DED-CLASS-BEFORE-TAX OF DARRY(DARRY-IDX DCLAS-IDX)

                   IF PUBLIC-SECTOR-YES OF PSLCT                        HP99994
                       IF 1042-EFFECT-YES OF DEDT5(DEDT5-IDX DEDC1-IDX) HP99994
                           COMPUTE SUBJ-1042-DED OF GRSWK               HP99994
                                 = SUBJ-1042-DED OF GRSWK               HP99994
                                 - WK-1042-DED-AMT OF W-WK              HP99994
                       END-IF                                           HP99994
                       COMPUTE TX-DED-TTL OF GRSWK                      HP99994
                             = TX-DED-TTL OF GRSWK                      HP99994
                             - WK-REMAIN-DED-AMT OF W-WK                HP99994
                   END-IF                                               HP99994

                  COMPUTE AG-NET-TS OF W-WK
                           =  AG-NET-TS OF W-WK
                           -  DED-AMT OF W-WK

                   COMPUTE NET OF GRSWK(1)
                           =  NET OF GRSWK(1)
                           -  DED-AMT OF W-WK

                   COMPUTE TTLDED-PRETAX OF GRSWK
                           =  TTLDED-PRETAX OF GRSWK
                           +  DED-AMT OF W-WK

                   IF FWT OF GRSWK(1)  >=  ZERO

                      IF DED-AMT OF W-WK  >  ZERO

                         IF FWT OF GRSWK(1)  >  DED-AMT OF W-WK
                               OR MANUAL-CHECK-YES OF CHECK

                            COMPUTE FWT OF GRSWK(1)
                                 =  FWT OF GRSWK(1)
                                 -  DED-AMT OF W-WK
                         ELSE

                            COMPUTE BEFORE-TAX-NOT-APPLIED OF TGCTB-PASS
                                 =  DED-AMT OF W-WK
                                 -  FWT OF GRSWK(1)
                            MOVE ZERO  TO  FWT OF GRSWK(1)

                            IF  STATE OF TAX-SET OF TAXWK  NOT = '$U'
                              AND LOCALITY OF TAX-SET  OF TAXWK = SPACE
                              AND DED-CUR OF DARRY(DARRY-IDX DCLAS-IDX)
                                    NOT EQUAL ZERO
                              AND DED-CUR-PAYBK
                                   OF DARRY(DARRY-IDX DCLAS-IDX) = ZERO

                              MOVE BEFORE-TAX-NOT-APPLIED OF TGCTB-PASS
                                   TO PRETAX-DED-NOT-APPLIED OF GRSWK
                            END-IF
                         END-IF
                      ELSE

                         IF DED-AMT OF W-WK  <  ZERO

                            COMPUTE FWT OF GRSWK(1)
                                 =  FWT OF GRSWK(1)
                                 -  DED-AMT OF W-WK

                            IF BEFORE-TAX-NOT-APPLIED OF W-WK NOT = ZERO
                               IF FWT OF GRSWK(1)
                                  > BEFORE-TAX-NOT-APPLIED OF W-WK
                                  COMPUTE FWT OF GRSWK(1)
                                       =  FWT OF GRSWK(1)
                                       -  BEFORE-TAX-NOT-APPLIED OF W-WK
                                  COMPUTE BEFORE-TAX-NOT-APPLIED
                                          OF TGCTB-PASS
                                       =  BEFORE-TAX-NOT-APPLIED OF W-WK
                                       *  (-1)
                                  MOVE ZERO
                                    TO BEFORE-TAX-NOT-APPLIED OF W-WK
                               ELSE
                                  COMPUTE BEFORE-TAX-NOT-APPLIED OF W-WK
                                       =  BEFORE-TAX-NOT-APPLIED OF W-WK
                                       -  FWT OF GRSWK(1)
                                  COMPUTE BEFORE-TAX-NOT-APPLIED
                                          OF TGCTB-PASS
                                       =  FWT OF GRSWK(1)
                                       *  (-1)
                                  MOVE ZERO TO FWT OF GRSWK(1)
                               END-IF
                            END-IF

                         END-IF
                      END-IF
                   ELSE
                      IF DED-AMT  <  ZERO
                               OR MANUAL-CHECK-YES OF CHECK

                         COMPUTE FWT OF GRSWK(1)
                              =  FWT OF GRSWK(1)
                              -  DED-AMT OF W-WK
                      ELSE

                         COMPUTE BEFORE-TAX-NOT-APPLIED OF TGCTB-PASS
                              =  DED-AMT OF W-WK

                         IF  STATE OF TAX-SET OF TAXWK  NOT = '$U'
                             AND LOCALITY OF TAX-SET  OF TAXWK = SPACE
                             AND DED-CUR OF DARRY(DARRY-IDX DCLAS-IDX)
                                     NOT EQUAL ZERO
                             AND DED-CUR-PAYBK
                                  OF DARRY(DARRY-IDX DCLAS-IDX) = ZERO

                             MOVE BEFORE-TAX-NOT-APPLIED OF TGCTB-PASS
                                  TO PRETAX-DED-NOT-APPLIED OF GRSWK
                         END-IF
                      END-IF
                   END-IF
               END-IF
           END-IF

           IF FUT-EFFECT-ADD OF DEDT5(DEDT5-IDX DEDC1-IDX)

               COMPUTE FUT OF GRSWK(1)
                       =  FUT OF GRSWK(1)
                       +  DED-AMT OF W-WK
           ELSE

               IF FUT-EFFECT-SUBTRACT OF DEDT5(DEDT5-IDX DEDC1-IDX)

                  IF FUT OF GRSWK(1)  >=  ZERO

                     IF DED-AMT OF W-WK  >  ZERO

                        IF FUT OF GRSWK(1)  >  DED-AMT OF W-WK
                               OR MANUAL-CHECK-YES OF CHECK

                           COMPUTE FUT OF GRSWK(1)
                                =  FUT OF GRSWK(1)
                                -  DED-AMT OF W-WK
                        ELSE

                           COMPUTE FUT-SUBTR-NOT-APPLIED OF TGCTB-PASS
                                =  DED-AMT OF W-WK
                                -  FUT OF GRSWK(1)
                           MOVE ZERO TO FUT OF GRSWK(1)
                        END-IF
                     ELSE
                        IF DED-AMT OF W-WK  <  ZERO

                           IF MANUAL-CHECK-YES OF CHECK AND
                              FUT OF GRSWK(1) = ZERO

                              MOVE DED-AMT OF W-WK TO
                                              MANUAL-FUT-DED OF W-WK
                           END-IF

                           COMPUTE FUT OF GRSWK(1)
                                =  FUT OF GRSWK(1)
                                -  DED-AMT OF W-WK

                            IF FUT-SUBTR-NOT-APPLIED OF W-WK NOT = ZERO
                               IF FUT OF GRSWK(1)
                                  > FUT-SUBTR-NOT-APPLIED OF W-WK
                                  COMPUTE FUT OF GRSWK(1)
                                       =  FUT OF GRSWK(1)
                                       -  FUT-SUBTR-NOT-APPLIED OF W-WK
                                  COMPUTE FUT-SUBTR-NOT-APPLIED
                                          OF TGCTB-PASS
                                       =  FUT-SUBTR-NOT-APPLIED OF W-WK
                                       *  (-1)
                                  MOVE ZERO
                                    TO FUT-SUBTR-NOT-APPLIED OF W-WK
                               ELSE
                                  COMPUTE FUT-SUBTR-NOT-APPLIED OF W-WK
                                       =  FUT-SUBTR-NOT-APPLIED OF W-WK
                                       -  FUT OF GRSWK(1)
                                  COMPUTE FUT-SUBTR-NOT-APPLIED
                                          OF TGCTB-PASS
                                       =  FUT OF GRSWK(1)
                                       *  (-1)
                                  MOVE ZERO TO FUT OF GRSWK(1)
                               END-IF
                            END-IF

                        END-IF
                     END-IF
                  ELSE
                     IF DED-AMT OF W-WK  <  ZERO
                               OR MANUAL-CHECK-YES OF CHECK

                        COMPUTE FUT OF GRSWK(1)
                             =  FUT OF GRSWK(1)
                             -  DED-AMT OF W-WK

                        IF MANUAL-CHECK-YES OF CHECK
                          MOVE DED-AMT OF W-WK TO
                                          MANUAL-FUT-AMT OF W-WK
                        END-IF
                     ELSE

                        COMPUTE FUT-SUBTR-NOT-APPLIED OF TGCTB-PASS
                             =  DED-AMT OF W-WK
                     END-IF
                  END-IF
               END-IF
           END-IF

           COMPUTE FUT-EARNINGS OF TGCTB-PASS
                 = FUT-EARNINGS OF TGCTB-PASS
                 + FUT OF GRSWK(1)

           IF FICA-EFFECT-ADD OF DEDT5(DEDT5-IDX DEDC1-IDX)

               COMPUTE FICA OF GRSWK(1)
                       =  FICA OF GRSWK(1)
                       +  DED-AMT OF W-WK
               COMPUTE FICA-WAGES OF GRSWK(1)
                       =  FICA-WAGES OF GRSWK(1)
                       +  DED-AMT OF W-WK
           ELSE

               IF FICA-EFFECT-SUBTRACT OF DEDT5(DEDT5-IDX DEDC1-IDX)

                  IF FICA OF GRSWK(1)  >=  ZERO

                     IF DED-AMT OF W-WK  >  ZERO

                        IF FICA-WAGES OF GRSWK(1)  >  DED-AMT OF W-WK
                               OR MANUAL-CHECK-YES OF CHECK

                           COMPUTE FICA OF GRSWK(1)
                                =  FICA OF GRSWK(1)
                                -  DED-AMT OF W-WK
                           COMPUTE FICA-WAGES OF GRSWK(1)
                                =  FICA-WAGES OF GRSWK(1)
                                -  DED-AMT OF W-WK
                        ELSE

                           COMPUTE FICA-SUBTR-NOT-APPLIED OF TGCTB-PASS
                                =  DED-AMT OF W-WK
                                -  FICA-WAGES OF GRSWK(1)
                           COMPUTE FICA OF GRSWK(1)
                                =  FICA OF GRSWK(1)
                                -  FICA-WAGES OF GRSWK(1)
                           MOVE ZERO TO FICA-WAGES OF GRSWK(1)
                        END-IF
                     ELSE
                        IF DED-AMT OF W-WK  <  ZERO

                           COMPUTE FICA OF GRSWK(1)
                                =  FICA OF GRSWK(1)
                                -  DED-AMT OF W-WK
                           COMPUTE FICA-WAGES OF GRSWK(1)
                                =  FICA-WAGES OF GRSWK(1)
                                -  DED-AMT OF W-WK

                            IF FICA-SUBTR-NOT-APPLIED OF W-WK NOT = ZERO
                               IF FICA-WAGES OF GRSWK(1)
                                  > FICA-SUBTR-NOT-APPLIED OF W-WK
                                  COMPUTE FICA OF GRSWK(1)
                                       =  FICA OF GRSWK(1)
                                       -  FICA-SUBTR-NOT-APPLIED OF W-WK
                                  COMPUTE FICA-WAGES OF GRSWK(1)
                                       =  FICA-WAGES OF GRSWK(1)
                                       -  FICA-SUBTR-NOT-APPLIED OF W-WK
                                  COMPUTE FICA-SUBTR-NOT-APPLIED
                                          OF TGCTB-PASS
                                       =  FICA-SUBTR-NOT-APPLIED OF W-WK
                                       *  (-1)
                                  MOVE ZERO
                                    TO FICA-SUBTR-NOT-APPLIED OF W-WK
                               ELSE
                                  COMPUTE FICA-SUBTR-NOT-APPLIED OF W-WK
                                       =  FICA-SUBTR-NOT-APPLIED OF W-WK
                                       -  FICA-WAGES OF GRSWK(1)
                                  COMPUTE FICA-SUBTR-NOT-APPLIED
                                          OF TGCTB-PASS
                                       =  FICA-WAGES OF GRSWK(1)
                                       *  (-1)
                                  MOVE ZERO TO FICA OF GRSWK(1)
                                  MOVE ZERO TO FICA-WAGES OF GRSWK(1)
                               END-IF
                            END-IF

                        END-IF
                     END-IF
                  ELSE
                     IF DED-AMT OF W-WK  <  ZERO
                               OR MANUAL-CHECK-YES OF CHECK

                        COMPUTE FICA OF GRSWK(1)
                             =  FICA OF GRSWK(1)
                             -  DED-AMT OF W-WK
                        COMPUTE FICA-WAGES OF GRSWK(1)
                             =  FICA-WAGES OF GRSWK(1)
                             -  DED-AMT OF W-WK
                     ELSE

                        COMPUTE FICA-SUBTR-NOT-APPLIED OF TGCTB-PASS
                             =  DED-AMT OF W-WK
                     END-IF
                  END-IF
               END-IF
           END-IF

           COMPUTE FICA-EARNINGS OF TGCTB-PASS
                 = FICA-EARNINGS OF TGCTB-PASS
                 + FICA OF GRSWK(1)

           IF  STATE OF TAX-SET OF TAXWK  NOT =  '$U'

               MOVE WK-AMT OF GRSWK  TO  WK-AMT OF W-WK
               MOVE DED-AMT OF W-WK  TO  WK-AMT OF GRSWK

               IF  STATE OF TAX-SET OF TAXWK = 'NY'
                   AND (LOCALITY OF TAX-SET OF TAXWK = 'P0023'
                       OR LOCALITY OF TAX-SET OF TAXWK = 'P0047')

                   MOVE DED-CUR OF DARRY(DARRY-IDX DCLAS-IDX)
                                     TO WK-AMT OF GRSWK
               END-IF

               MOVE TAX-GRS-COMPNT OF DEDT6(DEDT6-IDX DEDC2-IDX)
                   TO  TAX-GRS-COMPNT OF TGCTB-PASS
               MOVE PLAN-TYPE OF DARRY(DARRY-IDX)
                   TO  PLAN-TYPE OF TGCTB-PASS
               MOVE DEDCD OF DARRY(DARRY-IDX)  TO  DEDCD OF TGCTB-PASS
               MOVE DED-CLASS OF DARRY(DARRY-IDX DCLAS-IDX)  TO
                    DED-CLASS OF TGCTB-PASS
               MOVE SPACES  TO  ERNCD OF TGCTB-PASS
               MOVE STATE OF TAX-SET OF TAXWK
                            TO STATE-TG OF TGCTB-PASS
               MOVE LOCALITY OF TAX-SET OF TAXWK
                            TO LOCALITY-TG OF TGCTB-PASS

               IF DED-CLASS-TAXABLE OF DEDT5(DEDT5-IDX DEDC1-IDX)

                   SET SUBJECT-FWT-YES OF TGCTB-PASS  TO  TRUE
                   SET TXBL-BEN-YES OF TGCTB-PASS  TO  TRUE
                   IF WITHHOLD-FWT-YES OF DEDT5(DEDT5-IDX DEDC1-IDX)

                       SET WITHHOLD-FWT-YES OF TGCTB-PASS  TO  TRUE
                   ELSE

                       SET WITHHOLD-FWT-NO OF TGCTB-PASS  TO  TRUE
                   END-IF
               ELSE

                   SET TXBL-BEN-NO OF TGCTB-PASS  TO  TRUE
                   SET SUBJECT-FWT-NA OF TGCTB-PASS  TO  TRUE
                   SET WITHHOLD-FWT-NA OF TGCTB-PASS  TO  TRUE
               END-IF

               MOVE FUT-EFFECT OF DEDT5(DEDT5-IDX DEDC1-IDX)
                  TO FUT-EFFECT OF TGCTB-PASS

               MOVE FICA-EFFECT OF DEDT5(DEDT5-IDX DEDC1-IDX)
                  TO FICA-EFFECT OF TGCTB-PASS

               IF FUT-EFFECT-ADD OF DEDT5(DEDT5-IDX DEDC1-IDX)
                  OR FUT-EFFECT-SUBTRACT OF DEDT5(DEDT5-IDX DEDC1-IDX)

                  SET SUBJECT-FUT-YES OF TGCTB-PASS   TO TRUE
               ELSE

                  SET SUBJECT-FUT-NO OF TGCTB-PASS    TO TRUE
               END-IF

               IF FICA-EFFECT-ADD OF DEDT5(DEDT5-IDX DEDC1-IDX)
                  OR FICA-EFFECT-SUBTRACT OF DEDT5(DEDT5-IDX DEDC1-IDX)

                  SET SUBJECT-FICA-YES OF TGCTB-PASS  TO TRUE
               ELSE

                  SET SUBJECT-FICA-NO OF TGCTB-PASS   TO TRUE
               END-IF

      *    Access Rate Table and get applicable other rate for tax class
               IF DED-CALC-BENEFIT OF DARRY(DARRY-IDX 1 3)
                   AND RATE-TBL-ID OF DEDT1(DEDT1-IDX)  NOT =  SPACES
                   AND NOT PAY-LINE-STATUS-ERROR OF CHECK

                  PERFORM XG000-GET-BN-RATES
               END-IF

               IF TAX-GRS-COMPNT OF TGCTB-PASS NOT = SPACE
                 CALL 'PSPTGCTB' USING   SQLRT
                                         CHECK
                                         TAXWK
                                         GRSWK
                                         TGCTB
                                         WHADJ
                                         TGCTB-PASS
                                         TGBTB
                                         BENRT-PASS
                                         FRMWK
                                         TAXDT

                 IF RTNCD-ERROR OF SQLRT

                     MOVE 'APPLY-DEDUCTIONS(PSPTGCTB)'
                             TO  ERR-SECTION OF SQLRT
                     PERFORM ZZ000-SQL-ERROR
                 END-IF
               END-IF

      *    Process other TGCID's for same Deduction-Deduction Class
               PERFORM VARYING DEDC3-IDX FROM 1  BY  1
                       UNTIL DEDC3-IDX
                               >  DEDC3-COUNT
                                      OF DEDT6(DEDT6-IDX DEDC2-IDX)
                   MOVE TAX-GRS-CMP-OTH OF DEDT6(DEDT6-IDX DEDC2-IDX
                                                 DEDC3-IDX)
                        TO  TAX-GRS-COMPNT OF TGCTB-PASS
                   MOVE DED-AMT OF W-WK  TO  WK-AMT OF GRSWK

                   IF TAX-GRS-COMPNT OF TGCTB-PASS NOT = SPACE
                     CALL 'PSPTGCTB' USING   SQLRT
                                             CHECK
                                             TAXWK
                                             GRSWK
                                             TGCTB
                                             WHADJ
                                             TGCTB-PASS
                                             TGBTB
                                             BENRT-PASS
                                             FRMWK
                                             TAXDT

                     IF RTNCD-ERROR OF SQLRT

                         MOVE 'APPLY-DEDUCTIONS OTHER(PSPTGCTB)'
                             TO  ERR-SECTION OF SQLRT
                         PERFORM ZZ000-SQL-ERROR
                     END-IF
                     SET RTNCD-OK OF SQLRT TO TRUE
                   END-IF

               END-PERFORM

               MOVE WK-AMT OF W-WK  TO  WK-AMT OF GRSWK
           END-IF

           IF  BEFORE-TAX-NOT-APPLIED OF TGCTB-PASS NOT = ZERO

               SET EARNS-NEG-YES OF W-SW TO TRUE
           END-IF

           IF DED-AMT OF W-WK > ZERO
              COMPUTE FUT-SUBTR-NOT-APPLIED OF W-WK
                   =  FUT-SUBTR-NOT-APPLIED OF W-WK
                   +  FUT-SUBTR-NOT-APPLIED OF TGCTB-PASS
              COMPUTE FICA-SUBTR-NOT-APPLIED OF W-WK
                   =  FICA-SUBTR-NOT-APPLIED OF W-WK
                   +  FICA-SUBTR-NOT-APPLIED OF TGCTB-PASS
              COMPUTE BEFORE-TAX-NOT-APPLIED OF W-WK
                   =  BEFORE-TAX-NOT-APPLIED OF W-WK
                   +  BEFORE-TAX-NOT-APPLIED OF TGCTB-PASS
           END-IF

           MOVE ZERO TO DED-AMT OF W-WK
           MOVE ZERO TO FUT-SUBTR-NOT-APPLIED OF TGCTB-PASS
           MOVE ZERO TO FICA-SUBTR-NOT-APPLIED OF TGCTB-PASS
           MOVE ZERO TO BEFORE-TAX-NOT-APPLIED OF TGCTB-PASS

           .
       APPLY-DEDUCTIONS-EXIT.


      /*****************************************************************
      *                                                                *
       LD300-GET-TAX-DATA SECTION.
       LD300.
      *                                                                *
      * SET UP TAX DATA                                                *
      *                                                                *
      ******************************************************************

           IF CALC-FEDERAL OF TAXWK

               MOVE '$U' TO STATE OF TAX-SET OF TAXWK
               MOVE SPACE TO LOCALITY OF TAX-SET OF TAXWK
           END-IF

           IF CALC-STATE OF TAXWK

               MOVE SPACE TO LOCALITY OF TAX-SET OF TAXWK
           END-IF

           IF CALC-ST-RES OF TAXWK

               MOVE STATE OF RESIDENCE OF TAXWK
                       TO  STATE OF TAX-SET OF TAXWK
           END-IF

           IF CALC-LOC-RES OF TAXWK

               MOVE STATE OF RESIDENCE OF TAXWK
                       TO  STATE OF TAX-SET OF TAXWK
               MOVE LOCALITY OF RESIDENCE OF TAXWK(RESWK-IDX)
                       TO  LOCALITY OF TAX-SET OF TAXWK
           END-IF
      *Port Authority 18d - Locality being sent for NJ state - Start
           IF STATE OF TAX-SET OF TAXWK = 'NJ'
              MOVE SPACE TO LOCALITY OF TAX-SET OF TAXWK
           END-IF
      *Port Authority 18d - Locality being sent for NJ state - End

           SET TAX-DATA-ADDL OF TAXWK  TO  TRUE

           IF NOT CALC-FEDERAL OF TAXWK

               CALL 'PSPTAXDT' USING   SQLRT
                                       CHECK
                                       PYGRP
                                       TAXWK
                                       TAXDT
                                       SRCTB
                                       LRCTB
                                       LWKRC
                                       PSLCT
                                       EARRY
               IF RTNCD-ERROR OF SQLRT

                   MOVE 'CALC-TAXES(PSPTAXDT)'
                           TO  ERR-SECTION OF SQLRT
                   PERFORM ZZ000-SQL-ERROR
               END-IF
           END-IF

           MOVE ZERO  TO  BEFORE-TAX-NOT-APPLIED OF TGCTB-PASS
           MOVE ZERO  TO  FUT-SUBTR-NOT-APPLIED OF TGCTB-PASS
           MOVE ZERO  TO  FICA-SUBTR-NOT-APPLIED OF TGCTB-PASS
           SET EARNS-NEG-NO OF W-SW  TO  TRUE

           .
       GET-TAX-DATA-EXIT.

      /*****************************************************************HP99994
      *                                                                *HP99994
       LD400-GET-1042-ERN SECTION.                                      HP99994
       LD400.                                                           HP99994
      *                                                                *HP99994
      ******************************************************************HP99994
                                                                        HP99994
           MOVE ZERO TO WK-1042-EARNS OF W-WK                           HP99994
           SET STATE-FOUND-NO OF W-WK TO TRUE                           HP99994
           PERFORM VARYING 1042-IDX FROM 1 BY 1                         HP99994
                       UNTIL 1042-IDX > 1042-ERN-CNT OF W-WK            HP99994
                          OR STATE-FOUND-YES OF W-WK                    HP99994
                                                                        HP99994
               IF 1042-ERN-STATE OF 1042-ERN-TABLE (1042-IDX) =         HP99994
                                            STATE OF TAX-SET OF TAXWK   HP99994
                                                                        HP99994
                   SET STATE-FOUND-YES OF W-WK TO TRUE                  HP99994
                   MOVE ST-TTL-AMT OF 1042-ERN-TABLE (1042-IDX)         HP99994
                        TO WK-EARNS-TTL OF W-WK                         HP99994
                   MOVE 1042-ERN-AMT OF 1042-ERN-TABLE (1042-IDX)       HP99994
                        TO WK-1042-EARNS OF W-WK                        HP99994
               END-IF                                                   HP99994
           END-PERFORM                                                  HP99994
           .                                                            HP99994
       GET-1042-ERN-EXIT.                                               HP99994

      /*****************************************************************HP99994
      *                                                                *HP99994
       LD500-APPLY-1042-GRS SECTION.                                    HP99994
       LD500.                                                           HP99994
      *                                                                *HP99994
      * APPLY TO 1042 GROSS                                            *HP99994
      *                                                                *HP99994
      ******************************************************************HP99994
                                                                        HP99994
           MOVE SUBJ-1042-DED OF GRSWK TO SUBJ-1042-DED OF W-WK         HP99994
                                                                        HP99994
           PERFORM VARYING GR1WK-IDX  FROM  1  BY  1                    HP99994
                   UNTIL GR1WK-IDX  >  1042-GRS-COUNT OF GRSWK          HP99994
                      OR SUBJ-1042-DED OF W-WK = ZERO                   HP99994
                                                                        HP99994
               IF INCOME-CD-1042 OF 1042-GRS OF GRSWK(GR1WK-IDX)        HP99994
                                                        NOT = SPACES    HP99994
                   PERFORM LD510-LOOP-1042-GRSWK                        HP99994
               END-IF                                                   HP99994
           END-PERFORM                                                  HP99994
                                                                        HP99994
           .                                                            HP99994
       APPLY-1042-GRS-EXIT.                                             HP99994
                                                                        HP99994
      /*****************************************************************HP99994
      *                                                                *HP99994
       LD510-LOOP-1042-GRSWK SECTION.                                   HP99994
       LD510.                                                           HP99994
      *                                                                *HP99994
      ******************************************************************HP99994
                                                                        HP99994
           IF GRS OF 1042-GRS OF GRSWK(GR1WK-IDX) > ZERO                HP99994
                                                                        HP99994
               IF SUBJ-1042-DED OF W-WK > ZERO                          HP99994
                   COMPUTE GRS OF 1042-GRS OF GRSWK(GR1WK-IDX)          HP99994
                         = GRS OF 1042-GRS OF GRSWK(GR1WK-IDX)          HP99994
                         + SUBJ-1042-DED OF W-WK                        HP99994
                                                                        HP99994
                   MOVE ZERO TO SUBJ-1042-DED OF W-WK                   HP99994
               ELSE                                                     HP99994
                   COMPUTE 1042-GRS-TMP OF W-WK                         HP99994
                         = GRS OF 1042-GRS OF GRSWK(GR1WK-IDX)          HP99994
                         + SUBJ-1042-DED OF W-WK                        HP99994
                                                                        HP99994
                   IF 1042-GRS-TMP OF W-WK > ZERO                       HP99994
                       COMPUTE GRS OF 1042-GRS OF GRSWK(GR1WK-IDX)      HP99994
                             = GRS OF 1042-GRS OF GRSWK(GR1WK-IDX)      HP99994
                             + SUBJ-1042-DED OF W-WK                    HP99994
                                                                        HP99994
                       MOVE ZERO TO SUBJ-1042-DED OF W-WK               HP99994
                   ELSE                                                 HP99994
                       MOVE ZERO                                        HP99994
                           TO GRS OF 1042-GRS OF GRSWK(GR1WK-IDX)       HP99994
                       MOVE 1042-GRS-TMP OF W-WK                        HP99994
                           TO SUBJ-1042-DED OF W-WK                     HP99994
                   END-IF                                               HP99994
               END-IF                                                   HP99994
           ELSE                                                         HP99994
               IF GRS OF 1042-GRS OF GRSWK(GR1WK-IDX) < ZERO            HP99994
                                                                        HP99994
                   IF SUBJ-1042-DED OF W-WK < ZERO                      HP99994
                       COMPUTE GRS OF 1042-GRS OF GRSWK(GR1WK-IDX)      HP99994
                             = GRS OF 1042-GRS OF GRSWK(GR1WK-IDX)      HP99994
                             + SUBJ-1042-DED OF W-WK                    HP99994
                                                                        HP99994
                       MOVE ZERO TO SUBJ-1042-DED OF W-WK               HP99994
                   ELSE                                                 HP99994
                       COMPUTE 1042-GRS-TMP OF W-WK                     HP99994
                             = GRS OF 1042-GRS OF GRSWK(GR1WK-IDX)      HP99994
                             + SUBJ-1042-DED OF W-WK                    HP99994
                                                                        HP99994
                       IF 1042-GRS-TMP OF W-WK > ZERO                   HP99994
                           MOVE ZERO                                    HP99994
                             TO GRS OF 1042-GRS OF GRSWK(GR1WK-IDX)     HP99994
                           MOVE 1042-GRS-TMP OF W-WK                    HP99994
                             TO SUBJ-1042-DED OF W-WK                   HP99994
                       ELSE                                             HP99994
                           COMPUTE                                      HP99994
                                GRS OF 1042-GRS OF GRSWK(GR1WK-IDX)     HP99994
                              = GRS OF 1042-GRS OF GRSWK(GR1WK-IDX)     HP99994
                              + SUBJ-1042-DED OF W-WK                   HP99994
                                                                        HP99994
                           MOVE ZERO TO SUBJ-1042-DED OF W-WK           HP99994
                       END-IF                                           HP99994
                                                                        HP99994
                   END-IF                                               HP99994
               END-IF                                                   HP99994
           END-IF                                                       HP99994
                                                                        HP99994
           .                                                            HP99994
       LOOP-1042-GRSWK-EXIT.                                            HP99994


      /*****************************************************************
      *                                                                *
       LG000-GROSS-CALC SECTION.
       LG000.
      *                                                                *
      * ACCUMULATE EARNING AND APPLY CALCULATED DEDUCTION              *
      *                                                                *
      ******************************************************************


           COMPUTE WK-AMT OF GRSWK  ROUNDED
                   =  EARNS OF EARRY(EARRY-IDX)
                   *  1

           IF ADD-GROSS-YES OF ERNTB(ERNTB-IDX)
                   OR (GROSSUP-YES OF CHECK AND
                       ADD-GROSS-NO OF ERNTB(ERNTB-IDX))

               COMPUTE TTL OF GRSWK(1)
                       =  TTL OF GRSWK(1)
                       +  WK-AMT OF GRSWK
               COMPUTE NET OF GRSWK(1)
                       =  NET OF GRSWK(1)
                       +  WK-AMT OF GRSWK
               COMPUTE HRS OF GRSWK(1)
                       =  HRS OF GRSWK(1)
                       +  HRS OF EARRY(EARRY-IDX)
               COMPUTE AG-TTL-TS OF W-WK
                       =  AG-TTL-TS OF W-WK
                       +  WK-AMT OF GRSWK
               COMPUTE AG-NET-TS OF W-WK
                       =  AG-NET-TS OF W-WK
                       +  WK-AMT OF GRSWK
               COMPUTE AG-CUR-TS OF W-WK
                       =   AG-CUR-TS OF W-WK
                       +   WK-AMT OF GRSWK
           END-IF

           IF SUBJECT-FWT-YES OF ERNTB(ERNTB-IDX)
                   OR (GROSSUP-YES OF CHECK AND
                       ADD-GROSS-NO OF ERNTB(ERNTB-IDX))

               COMPUTE T-CUR-TS OF W-WK
                       =  T-CUR-TS OF W-WK
                       +  WK-AMT OF GRSWK
           END-IF

           IF ADD-GROSS-YES OF ERNTB(ERNTB-IDX)
              AND NOT GROSSUP-YES OF CHECK
              AND NOT SUBJECT-FWT-YES OF ERNTB(ERNTB-IDX)
              AND EARNS OF EARRY(EARRY-IDX) < ZERO

               COMPUTE AG-TTL-TS OF W-WK
                       =  AG-TTL-TS OF W-WK
                       -  WK-AMT OF GRSWK
               COMPUTE AG-NET-TS OF W-WK
                       =  AG-NET-TS OF W-WK
                       -  WK-AMT OF GRSWK
               COMPUTE AG-CUR-TS OF W-WK
                       =   AG-CUR-TS OF W-WK
                       -   WK-AMT OF GRSWK
           END-IF

           IF NOT GROSSUP-YES OF CHECK

               IF SUBJECT-FWT-YES OF ERNTB(ERNTB-IDX)
                  AND NOT ADD-GROSS-YES OF ERNTB(ERNTB-IDX)

                  COMPUTE T-FWT-CUM OF W-WK
                        = T-FWT-CUM OF W-WK
                        + WK-AMT OF GRSWK
               END-IF

               IF ADD-GROSS-YES OF ERNTB(ERNTB-IDX)
                  AND NOT SUBJECT-FWT-YES OF ERNTB(ERNTB-IDX)

                  COMPUTE AG-FWT-CUM OF W-WK
                        = AG-FWT-CUM OF W-WK
                        + WK-AMT OF GRSWK
               END-IF

               IF ADD-GROSS-YES OF ERNTB(ERNTB-IDX)
                  AND SUBJECT-FWT-YES OF ERNTB(ERNTB-IDX)
                  AND WK-AMT OF GRSWK < ZERO

                  SET AGT-FWT-NEG-YES OF W-SW TO TRUE
               END-IF
           END-IF

           IF EFFECT-ON-FLSA-HOURS OF ERNTB(ERNTB-IDX) OR
               EFFECT-ON-FLSA-BOTH  OF ERNTB(ERNTB-IDX)

               COMPUTE HRSWKD OF GRSWK(1)
                       =  HRSWKD OF GRSWK(1)
                       +  HRS OF EARRY(EARRY-IDX)
           END-IF

           IF SUBJECT-FWT-YES OF ERNTB(ERNTB-IDX)

               COMPUTE FWT OF GRSWK(1)
                       =  FWT OF GRSWK(1)
                       +  WK-AMT OF GRSWK
               COMPUTE FWG OF GRSWK(1)
                       =  FWG OF GRSWK(1)
                       +  WK-AMT OF GRSWK

               IF WITHHOLD-FWT-NO OF ERNTB(ERNTB-IDX)

                   COMPUTE TX-EARN-NOT-TAXED OF WHADJ(1)
                           =  TX-EARN-NOT-TAXED OF WHADJ(1)
                           +  WK-AMT OF GRSWK
                   IF  TAX-METHOD-SUPPLEMENTAL OF EARRY(EARRY-IDX)
                       COMPUTE SUPPL-NON-TAX OF W-WK
                           = WK-AMT OF GRSWK
                           + SUPPL-NON-TAX OF W-WK
                   END-IF
               ELSE

                   IF  TAX-METHOD-SUPPLEMENTAL OF EARRY(EARRY-IDX)
                       COMPUTE SUPPL-TAXABLE OF W-WK
                           = WK-AMT OF GRSWK
                   END-IF

               END-IF

               SET GR1WK-IDX TO 1                                       HP99999
                                                                        HP99999
               SEARCH 1042-GRS OF GRSWK                                 HP99999
                                                                        HP99999
                   AT END                                               HP99999
                                                                        HP99999
                       CONTINUE                                         HP99999
                                                                        HP99999
                   WHEN GR1WK-IDX > 1042-GRS-COUNT OF GRSWK             HP99999
                                                                        HP99999
                       SET 1042-GRS-COUNT OF GRSWK TO GR1WK-IDX         HP99999
                       MOVE INCOME-CD-1042 OF ERNTB(ERNTB-IDX)          HP99999
                               TO INCOME-CD-1042 OF GRSWK(GR1WK-IDX)    HP99999
                       MOVE WK-AMT OF GRSWK                             HP99999
                               TO GRS OF 1042-GRS OF GRSWK(GR1WK-IDX)   HP99999
                                                                        HP99999
                   WHEN INCOME-CD-1042 OF ERNTB(ERNTB-IDX)              HP99999
                           = INCOME-CD-1042 OF GRSWK(GR1WK-IDX)         HP99999
                                                                        HP99999
                       COMPUTE GRS OF 1042-GRS OF GRSWK(GR1WK-IDX)      HP99999
                               = GRS OF 1042-GRS OF GRSWK(GR1WK-IDX)    HP99999
                               + WK-AMT OF GRSWK                        HP99999
                                                                        HP99999
               END-SEARCH                                               HP99999

               IF STATE OF EARRY(EARRY-IDX) = 'OR'
                  AND LOCALITY OF EARRY(EARRY-IDX) = '23850'
                  AND NOT FLSA-CATEGORY-OVERTIME OF ERNTB(ERNTB-IDX)
                  AND CALC-FEDERAL OF TAXWK

                  COMPUTE LOC-NET-WAGES OF GRSWK
                        = LOC-NET-WAGES OF GRSWK
                        + WK-AMT OF GRSWK
               END-IF
           END-IF

           IF SUBJECT-FUT-YES OF ERNTB(ERNTB-IDX)

               COMPUTE FUT OF GRSWK(1)
                       =  FUT OF GRSWK(1)
                       +  WK-AMT OF GRSWK
           END-IF

           IF SUBJECT-FICA-YES OF ERNTB(ERNTB-IDX)

               COMPUTE FICA OF GRSWK(1)
                       =  FICA OF GRSWK(1)
                       +  WK-AMT OF GRSWK

               IF TIPS-CATEGORY-TIPS OF ERNTB(ERNTB-IDX)
                   COMPUTE FICA-TIPS OF GRSWK(1)
                           =  FICA-TIPS OF GRSWK(1)
                           +  WK-AMT OF GRSWK
               ELSE
                   COMPUTE FICA-WAGES OF GRSWK(1)
                           =  FICA-WAGES OF GRSWK(1)
                           +  WK-AMT OF GRSWK
               END-IF
           END-IF

           IF TIPS-CATEGORY-TIPS OF ERNTB(ERNTB-IDX)
               SET TIPS-IN-THIS-SET-YES OF GRSWK(1) TO TRUE
           END-IF

           IF STATE OF EARRY(EARRY-IDX) = 'PA'
              AND LOCALITY OF EARRY(EARRY-IDX) NOT = SPACE
              AND LOCALITY OF EARRY(EARRY-IDX) NOT = '510101'
              AND ADD-GROSS-YES OF ERNTB(ERNTB-IDX)
              AND SUBJECT-FWT-YES OF ERNTB(ERNTB-IDX)

                     COMPUTE PA-AG-CUM-TS OF W-WK
                           = PA-AG-CUM-TS OF W-WK
                           + WK-AMT OF GRSWK
           END-IF

           IF STATE OF EARRY(EARRY-IDX) = 'PA'
              AND ADD-GROSS-YES OF ERNTB(ERNTB-IDX)
              AND SUBJECT-FWT-YES OF ERNTB(ERNTB-IDX)

                     COMPUTE PA-BLK-AG-CUM OF W-WK
                           = PA-BLK-AG-CUM OF W-WK
                           + WK-AMT OF GRSWK
           END-IF

           IF TAX-GRS-COMPNT OF ERNTB(ERNTB-IDX)  NOT =  SPACE
               AND STATE OF TAX-SET OF TAXWK  NOT =  '$U'

               MOVE SUBJECT-FWT OF ERNTB(ERNTB-IDX)
                   TO  SUBJECT-FWT OF TGCTB-PASS
               MOVE SUBJECT-FUT OF ERNTB(ERNTB-IDX)
                   TO  SUBJECT-FUT OF TGCTB-PASS
               MOVE SUBJECT-FICA OF ERNTB(ERNTB-IDX)
                   TO  SUBJECT-FICA OF TGCTB-PASS
               MOVE TAX-GRS-COMPNT OF ERNTB(ERNTB-IDX)
                   TO  TAX-GRS-COMPNT OF TGCTB-PASS
               MOVE ERNCD OF ERNTB(ERNTB-IDX)  TO  ERNCD OF TGCTB-PASS
               MOVE SPACE  TO  PLAN-TYPE OF TGCTB-PASS
               MOVE SPACE  TO  DEDCD OF TGCTB-PASS
               MOVE SPACE  TO  DED-CLASS OF TGCTB-PASS
               MOVE STATE OF TAX-SET OF TAXWK
                            TO STATE-TG OF TGCTB-PASS
               MOVE LOCALITY OF TAX-SET OF TAXWK
                            TO LOCALITY-TG OF TGCTB-PASS
               SET TXBL-BEN-NO OF TGCTB-PASS  TO  TRUE

               IF WITHHOLD-FWT-YES OF ERNTB(ERNTB-IDX)

                   SET WITHHOLD-FWT-YES OF TGCTB-PASS  TO  TRUE
               ELSE

                   SET WITHHOLD-FWT-NO OF TGCTB-PASS  TO  TRUE
               END-IF

               CALL 'PSPTGCTB' USING   SQLRT
                                       CHECK
                                       TAXWK
                                       GRSWK
                                       TGCTB
                                       WHADJ
                                       TGCTB-PASS
                                       TGBTB
                                       BENRT-PASS
                                       FRMWK
                                       TAXDT

               IF RTNCD-ERROR OF SQLRT

                   MOVE 'GROSS-CALC(PSPTGCTB)'  TO  ERR-SECTION OF SQLRT
                   PERFORM ZZ000-SQL-ERROR
               END-IF
           END-IF

           .
       GROSS-CALC-EXIT.


      /*****************************************************************
      *                                                                *
       LH000-CALC-RESIDENT-TAXES SECTION.
       LH000.
      *                                                                *
      ******************************************************************

      *  ACCUMULATE TAX ON RES-STATE EARNINGS

           SET PROCESS-PA-RES-NO TO TRUE

           PERFORM VARYING TXARY-IDX FROM  1  BY  1
                   UNTIL TXARY-IDX  >  TAX-SET-CNT OF TXARY

               IF STATE OF TXARY(TXARY-IDX)
                           =  STATE OF RESIDENCE OF TAXWK
                       AND LOCALITY OF TXARY(TXARY-IDX) = SPACE
                           AND TAX-CLASS-WITHHOLDING OF TXARY(TXARY-IDX)
                               AND RESIDENT-TAX-NO OF TXARY(TXARY-IDX)

                   COMPUTE TAX-ON-RES-EARNS OF W-WK
                           =  TAX-ON-RES-EARNS OF W-WK
                           *  1
               END-IF
           END-PERFORM

           MOVE RECIPROCITY-RULE OF STATE-RCP OF TAXWK TO
                                        WK-RECIPROCITY-RULE OF W-WK

           SET CALC-ST-RES OF TAXWK  TO  TRUE
           SET RES-CALC-NO OF W-WK TO TRUE
           PERFORM LH100-RES-STATE-GROSS

           IF RES-CALC-NO OF W-WK

              PERFORM LD300-GET-TAX-DATA
              IF NOT SPECIAL-FWT-STATUS-NR-ALIEN OF TAXDT               HP99999
                       OR (SPECIAL-FWT-STATUS-NR-ALIEN OF TAXDT         HP99999
                       AND STATE OF TAX-SET OF TAXWK NOT = SPACES)      HP99999

                  PERFORM LD050-CALCULATE-TAX

                  IF TAX-METHOD-SUPPLEMENTAL OF TXARY(TXARY-IDX)
                     AND SPECIAL-SWT-STATUS-NONE OF TARRY(TARRY-IDX)
                     AND RESIDENT-YES OF TARRY(TARRY-IDX)
                     AND TAX-CUR-ADDL-AMT OF TARRY(TARRY-IDX)
                         NOT = ZERO
                     AND ADDL-TAX OF TXARY(TXARY-IDX) = ZERO
                     AND TAX-CUR-ADDL-AMT-ADDED OF W-SW


                         COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                               = TAX-CUR OF TARRY(TARRY-IDX)
                               - TAX-CUR-ADDL-AMT OF TARRY(TARRY-IDX)

                        IF STATE OF TARRY(TARRY-IDX) = 'MO'
                           AND LOCALITY OF TARRY(TARRY-IDX) = ' '

                           SET MO-TAX-ADDL-AMT-ADDED OF W-SW TO TRUE
                        END-IF
                  END-IF

                     SET TAX-CUR-ADDL-AMT-NOTADDED OF W-SW TO TRUE
              END-IF                                                    HP99999
           END-IF

           IF RES-CALC-NO OF W-WK
               OR TAX OF STATE-RCP OF GRSWK > ZERO

               IF MANUAL-CHECK-NO OF CHECK

                   PERFORM LH400-STATE-ADJ-RECIP
               ELSE

                   IF  WK-RECIPROCITY-RULE-FACTOR OF W-WK

                       PERFORM LH400-STATE-ADJ-RECIP

                   ELSE
                       PERFORM LH550-FIND-TARRY

                       IF MANUAL-CHECK-YES OF CHECK

                           COMPUTE TAX-SUPP-CUR OF TARRY(TARRY-IDX)
                                =  TAX-SUPP-CUR OF TARRY(TARRY-IDX)
                                -  TAX-SUPP-CUR OF TARRY(TARRY-IDX)
                                +  TAX-CUR OF TARRY(TARRY-IDX)
                       ELSE

                           COMPUTE TAX-SUPP-CUR OF TARRY(TARRY-IDX)
                                =  TAX-SUPP-CUR OF TARRY(TARRY-IDX)
                                +  TAX-CUR OF TARRY(TARRY-IDX)

                       END-IF
                   END-IF
               END-IF
           END-IF

           IF LOCALITY-COUNT OF RESIDENCE OF TAXWK  >  ZERO

               IF STATE OF RESIDENCE OF TAXWK = 'MD'
                       AND CHECK-DT OF CHECK > '1999-12-31'

                   PERFORM LH300-LOCAL-RECIP-FOR-MD-RES
               ELSE

                   PERFORM LH200-RES-LOCAL-GROSS
               END-IF
           END-IF

           IF STATE OF RESIDENCE OF TAXWK = 'PA'

              IF  LOCALITY-COUNT OF RESIDENCE = ZERO
                  AND LOCALITY-COUNT OF WORK = ZERO

                  SET PROCESS-PA-RES-YES TO TRUE
                  PERFORM LH190-GET-PA-WORK-TXARY
              ELSE

                  IF  LOCALITY-COUNT OF RESIDENCE = 1
                      AND LOCALITY-COUNT OF WORK = 1

                      SET PROCESS-PA-RES-YES TO TRUE
                      PERFORM LH192-GET-PA-RES-TARRY
                  END-IF
              END-IF
           END-IF

      *Port Authority 17d - Code causing abend
      *           IF (STATE OF TARRY(TARRY-IDX)  = 'MO'
      *              AND TAX-CLASS-WITHHOLDING OF TARRY(TARRY-IDX)
      *              AND RESIDENT-YES OF TARRY(TARRY-IDX)
      *              AND NBR-OF-STATES OF GRSWK = 1
      *              AND STATE OF WORK-STATE-DATA OF GRSWK(1)
      *                                  NOT EQUAL 'MO'
      *              AND MO-RCP-WRK-AMT OF W-WK > ZERO  )
      *              OR
      *              (STATE OF TXARY(TXARY-IDX) = 'MO'
      *              AND TAX-CLASS-WITHHOLDING OF TARRY(TARRY-IDX)
      *              AND RESIDENT-YES OF TARRY(TARRY-IDX)
      *              AND NBR-OF-STATES OF GRSWK > 1
      *              AND MO-SUPPLE-YES OF W-SW
      *              AND MO-RCP-WRK-AMT OF W-WK > ZERO )
      *
      *              PERFORM LH411-MO-STATE-WITHHOLD
      *
      *           END-IF
      *Port Change end 11-01-17

           IF LCL-WK-TAX-CALCED OF TXARY

               PERFORM LH175-LOCAL-WK-WK-RECIP
           END-IF

            .
       CALC-RESIDENT-TAXES-EXIT.


      /*****************************************************************
      *                                                                *
       LH100-RES-STATE-GROSS SECTION.
       LH100.
      *                                                                *
      ******************************************************************

           MOVE STATE OF RESIDENCE OF TAXWK
                   TO  STATE OF TAX-SET OF TAXWK
           MOVE SPACE  TO  LOCALITY OF TAX-SET OF TAXWK
           MOVE PAY-FREQ-TYPE OF TXARY(1)
                   TO  PAY-FREQ-TYPE OF TAX-SET OF TAXWK
           MOVE TAX-PERIODS OF TXARY(1)
                   TO  TAX-PERIODS OF TAX-SET OF TAXWK
           MOVE TAX-METHOD OF TXARY(1)
                   TO TAX-METHOD OF TAX-SET OF TAXWK
           MOVE BENEFIT-RCD-NO OF TXARY(1)
                   TO BENEFIT-RCD-NO OF TAXWK
           IF PUBLIC-SECTOR-YES OF PSLCT                                HP00001
                                                                        HP00001
               MOVE PAID-PRDS-PER-YEAR OF TXARY(1)                      HP99999
                       TO  PAID-PRDS-PER-YEAR OF TAX-SET OF TAXWK       HP99999
           END-IF                                                       HP00001
           INITIALIZE ACCUM-LEVEL OF GRSWK(2)
           SET SUT-RECALC-NO OF GRSWK(2) TO TRUE
           INITIALIZE ACCUM-LEVEL OF WHADJ(2)
           INITIALIZE STATE-RCP OF GRSWK
           PERFORM LH120-GET-RES-TGC

           PERFORM VARYING TXARY-IDX FROM  1  BY  1
                   UNTIL TXARY-IDX  >  TAX-SET-CNT OF TXARY

             IF RESIDENT-TAX-NO  OF TXARY(TXARY-IDX)
                 AND TAX-CLASS OF TXARY(TXARY-IDX) NOT = '6'

                 IF PAY-FREQ-TYPE OF TAX-SET OF TAXWK
                        NOT =  PAY-FREQ-TYPE OF TXARY(TXARY-IDX)
                        OR TAX-PERIODS OF TAX-SET OF TAXWK
                        NOT =  TAX-PERIODS OF TXARY(TXARY-IDX)
                        OR TAX-METHOD OF TAX-SET OF TAXWK
                        NOT =  TAX-METHOD OF TXARY(TXARY-IDX)
                        OR BENEFIT-RCD-NO OF TAXWK
                        NOT = BENEFIT-RCD-NO OF TXARY(TXARY-IDX)
                        OR (PAID-PRDS-PER-YEAR OF TAX-SET OF TAXWK      HP99999
                        NOT =  PAID-PRDS-PER-YEAR OF TXARY(TXARY-IDX)   HP99999
                        AND PUBLIC-SECTOR-YES OF PSLCT)                 HP00001


                     IF FWT OF GRSWK(2)  NOT =  ZERO
                             OR  ST-WH-ADJ OF WHADJ(2)  NOT =  ZERO

                         SET SAV-TXARY-IDX  TO  TXARY-IDX
                         PERFORM LD300-GET-TAX-DATA
                         PERFORM LD050-CALCULATE-TAX
                         PERFORM LH400-STATE-ADJ-RECIP
                         SET TXARY-IDX  TO  SAV-TXARY-IDX
                         SET RES-CALC-YES OF W-WK TO TRUE
                         INITIALIZE ACCUM-LEVEL OF GRSWK(2)
                         SET SUT-RECALC-NO OF GRSWK(2) TO TRUE
                         INITIALIZE ACCUM-LEVEL OF WHADJ(2)
                         INITIALIZE STATE-RCP OF GRSWK
                     END-IF


                     MOVE CORR TAX-SET OF TXARY(TXARY-IDX)
                                              TO TAX-SET OF TAXWK
                     MOVE BENEFIT-RCD-NO OF TXARY(TXARY-IDX)
                          TO BENEFIT-RCD-NO OF TAXWK
                     PERFORM LH120-GET-RES-TGC
                     MOVE SPACE  TO  LOCALITY OF TAX-SET OF TAXWK
                     MOVE SPACE  TO SUT-JURISDICTION OF TAX-SET OF TAXWK
                     SET TXARY-IDX-PASS OF TXARY  TO  TXARY-IDX
                     PERFORM LH150-STATE-RECIPROCITY
                 ELSE

                     SET TXARY-IDX-PASS OF TXARY  TO  TXARY-IDX
                     PERFORM LH150-STATE-RECIPROCITY

                     IF STATE OF TXARY(TXARY-IDX) = 'PA'
                        AND LOCALITY OF TXARY(TXARY-IDX) = SPACE
                        AND TAX-CLASS-WITHHOLDING OF TXARY(TXARY-IDX)
                        AND RESIDENT-TAX-NO OF TXARY(TXARY-IDX)
                        AND TAX-METHOD-ANNUALIZED OF TXARY(TXARY-IDX)
                        AND RECIPROCITY-RULE-REDTTL OF STATE-STATE
                                                  OF TXARY(TXARY-IDX)
                        AND NON-PA-ERN-FOUND-YES OF W-SW
                        AND PA-RES-ERN-FOUND-NO OF W-SW

                        COMPUTE ST-ADJ OF GRSWK(2)
                              = ST-ADJ OF GRSWK(2)
                              + PA-BLK-DED-AMT OF W-WK
                     END-IF

                     IF CHECK-GRS OF GRSWK = ZERO
                        AND TXGRS-CUR OF TXARY (TXARY-IDX) < ZERO
                        AND STATE OF TXARY (TXARY-IDX) = 'NY'
                        AND MTA-LOCALITY OF TAXWK = 'P0023'
                        AND CALC-STATE OF TAXWK

                         SET SAV-TXARY-IDX  TO  TXARY-IDX
                         PERFORM LD300-GET-TAX-DATA
                         PERFORM LD050-CALCULATE-TAX
                         PERFORM LH400-STATE-ADJ-RECIP
                         SET TXARY-IDX  TO  SAV-TXARY-IDX
                         SET RES-CALC-YES OF W-WK TO TRUE
                         INITIALIZE ACCUM-LEVEL OF GRSWK(2)
                         SET SUT-RECALC-NO OF GRSWK(2) TO TRUE
                         INITIALIZE ACCUM-LEVEL OF WHADJ(2)
                         INITIALIZE STATE-RCP OF GRSWK
                     END-IF

                 END-IF

             END-IF

           END-PERFORM
           IF FWT OF GRSWK(2)  NOT = ZERO
                     OR  ST-WH-ADJ OF WHADJ(2)  NOT = ZERO
              SET RES-CALC-NO OF W-WK TO TRUE
           END-IF

           .
       RES-STATE-GROSS-EXIT.


      /*****************************************************************
      *                                                                *
       LH120-GET-RES-TGC SECTION.
       LH120.
      *                                                                *
      ******************************************************************

           MOVE ZERO  TO  ST-ADJ OF GRSWK(2)
           MOVE ZERO  TO  ST-WH-ADJ OF WHADJ(2)

           PERFORM VARYING OTH-TXARY-IDX FROM 1  BY  1
                  UNTIL OTH-TXARY-IDX  >  TAX-SET-CNT OF TXARY

               IF STATE OF TAX-SET OF TXARY (OTH-TXARY-IDX)
                   NOT =  '$U'
                   AND TAX-CLASS-WITHHOLDING OF TXARY(OTH-TXARY-IDX)
                   AND PAY-FREQ-TYPE OF TAX-SET OF TAXWK
                     = PAY-FREQ-TYPE OF TXARY(OTH-TXARY-IDX)
                   AND TAX-PERIODS OF TAX-SET OF TAXWK
                     = TAX-PERIODS OF TXARY(OTH-TXARY-IDX)
                   AND TAX-METHOD OF TAX-SET OF TAXWK
                     = TAX-METHOD OF TXARY(OTH-TXARY-IDX)
                   AND BENEFIT-RCD-NO OF TAXWK
                     = BENEFIT-RCD-NO OF TXARY(OTH-TXARY-IDX)

                   IF RESIDENT-TAX-NO OF TXARY(OTH-TXARY-IDX)

                       COMPUTE  ST-ADJ OF GRSWK(2)
                             =  ST-ADJ OF GRSWK(2)
                             +  RES-TGC-ADJ-ST OF TXARY(OTH-TXARY-IDX)

                       COMPUTE ST-WH-ADJ OF WHADJ(2)
                             = ST-WH-ADJ OF WHADJ(2)
                             + RES-TGC-WH-ADJ-ST OF TXARY(OTH-TXARY-IDX)

                       COMPUTE ST-ADDL-TAX-SET OF WHADJ(2)
                             = ST-ADDL-TAX-SET OF WHADJ(2)
                             + TGC-ADJ-ADDL-TAX-SET
                                                 OF TXARY(OTH-TXARY-IDX)
                   ELSE

      *  IF TXARY RES HAS ADJ, THEN NON-RES ADJ HAS ALREADY BEEN APPLIED

                       COMPUTE  ST-ADJ OF GRSWK(2)
                             =  ST-ADJ OF GRSWK(2)
                             -  RES-TGC-ADJ-ST OF TXARY(OTH-TXARY-IDX)

                       COMPUTE ST-WH-ADJ OF WHADJ(2)
                             = ST-WH-ADJ OF WHADJ(2)
                             - RES-TGC-WH-ADJ-ST OF TXARY(OTH-TXARY-IDX)

                       COMPUTE ST-ADDL-TAX-SET OF WHADJ(2)
                             = ST-ADDL-TAX-SET OF WHADJ(2)
                             - TGC-ADJ-ADDL-TAX-SET
                                                 OF TXARY(OTH-TXARY-IDX)
                   END-IF
               END-IF
           END-PERFORM

           .
       GET-RES-TGC-EXIT.


      /*****************************************************************
      *                                                                *
       LH150-STATE-RECIPROCITY SECTION.
       LH150.
      *                                                                *
      ******************************************************************

           IF STATE OF TAX-SET OF TXARY(TXARY-IDX)  NOT =  '$U'
                   AND TAX-CLASS-WITHHOLDING OF TXARY(TXARY-IDX)

               CALL 'PSPTCSRC'  USING GRSWK
                                      WHADJ
                                      TXARY
                                      TARRY
                                      SRCTB
           END-IF

           .
       STATE-RECIPROCITY-EXIT.


      /*****************************************************************
      *                                                                *
       LH155-CALC-TRANSIT-SELF-ADJ SECTION.
       LH155.
      *                                                                *
      *    EVALUATE WHETHER SELF-ADJUSTMENT IS NECESSARY FOR TRANSIT   *
      *    TAX CALCULATED.                                             *
      *                                                                *
      ******************************************************************

           MOVE ZERO TO WORK-TAX-RATE OF W-WK
           MOVE ZERO TO WORK-TRANSIT-AMT OF W-WK
           MOVE ZERO TO WORK-TRANSIT-RND OF W-WK
           MOVE ZERO TO WORK-OTH-IDX OF W-WK

           SET TARRY-IDX  TO  1

           SEARCH TAXCALC-DATA OF TARRY

               WHEN TARRY-IDX  >  TAXCALC-COUNT OF TARRY

                    CONTINUE

               WHEN  TAX-CLASS OF TARRY (TARRY-IDX) = '5'

                    IF ONE-TIME-NA OF TARRY(TARRY-IDX) AND
                       PY-TRANSIT-TAX-NO OF WORK OF TAXDT AND
                       PY-TRANSIT-TAX-NO OF RESIDENCE OF TAXDT
                       SET STTRT-IDX  TO  1

                       SEARCH STATE-TAX-RT-TABLE OF STTRT

                        WHEN STTRT-IDX  >  STATE-TAX-RT-COUNT OF STTRT

                           CONTINUE

                        WHEN STATE OF STTRT(STTRT-IDX)
                            =  STATE OF TARRY(TARRY-IDX)

                           PERFORM VARYING WORK-OTH-IDX  FROM  1  BY  1
                               UNTIL WORK-OTH-IDX  >  OTH-RT-TABLE-COUNT
                               OF STTRT(STTRT-IDX)

                            IF TAX-CLASS-TRANSIT-EE OF OTH-RT-TABLE
                               OF STTRT(STTRT-IDX WORK-OTH-IDX)

                              COMPUTE WORK-TAX-RATE OF W-WK ROUNDED
                              =  WORK-TAX-RATE OF W-WK
                                 +  TAX-RT OF OTH-RT-TABLE
                                    OF STTRT(STTRT-IDX WORK-OTH-IDX)
                            END-IF
                           END-PERFORM

                           COMPUTE WORK-TRANSIT-RND OF W-WK ROUNDED
                           = (TXGRS-YTD OF TARRY(TARRY-IDX) +
                              TXGRS-CUR OF TARRY(TARRY-IDX) ) *
                             WORK-TAX-RATE OF W-WK

                           COMPUTE WORK-TRANSIT-AMT OF W-WK
                           = WORK-TRANSIT-RND OF W-WK
                           - TAX-YTD OF TARRY(TARRY-IDX)

                       END-SEARCH

                       IF WORK-TRANSIT-AMT OF W-WK NOT EQUAL ZERO AND
                         WORK-TRANSIT-AMT OF W-WK  NOT EQUAL
                          TAX-CUR OF TARRY(TARRY-IDX)

                       MOVE WORK-TRANSIT-AMT OF W-WK TO
                            TAX-CUR OF TARRY(TARRY-IDX)

                       END-IF

                       IF TAX-CUR OF TARRY(TARRY-IDX) < ZERO
                          AND TXGRS-CUR OF TARRY(TARRY-IDX) NOT < ZERO
                         MOVE ZERO TO TAX-CUR OF TARRY(TARRY-IDX)
                       END-IF

                    END-IF

                    IF ONE-TIME-ADDITION OF TARRY(TARRY-IDX) OR
                       ONE-TIME-REFUND OF TARRY(TARRY-IDX)

                       COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                       = TAX-CUR OF TARRY(TARRY-IDX)  +
                         TAX-ONE-TIME OF TARRY(TARRY-IDX)

                       IF TAX-CUR OF TARRY(TARRY-IDX) < ZERO
                          AND TXGRS-CUR OF TARRY(TARRY-IDX) NOT < ZERO
                         MOVE ZERO TO TAX-CUR OF TARRY(TARRY-IDX)
                       END-IF

                    END-IF

           END-SEARCH

           .
       CALC-TRANSIT-SELF-ADJ-EXIT.


      /*****************************************************************
      *                                                                *
       LH156-CALC-LTC-SELF-ADJ SECTION.
       LH156.
      *                                                                *
      *    EVALUATE WHETHER SELF-ADJUSTMENT IS NECESSARY FOR LONG TERM *
      *    CARE TAX CALCULATED.                                        *
      *                                                                *
      ******************************************************************

           MOVE ZERO TO WORK-TAX-RATE OF W-WK
           MOVE ZERO TO WORK-LTC-ADJ OF W-WK
           MOVE ZERO TO WORK-LTC-RND OF W-WK
           MOVE ZERO TO WORK-OTH-IDX OF W-WK

           SET TARRY-IDX  TO  1

           SEARCH TAXCALC-DATA OF TARRY

               WHEN TARRY-IDX  >  TAXCALC-COUNT OF TARRY

                    CONTINUE

               WHEN  TAX-CLASS-LTC-EE OF TARRY (TARRY-IDX)

                    IF ONE-TIME-NA OF TARRY(TARRY-IDX) AND
                       PY-LTC-STATUS-YES OF WORK OF TAXDT

                       PERFORM LA250-GET-ALL-GROSS


                       SET STTRT-IDX  TO  1

                       SEARCH STATE-TAX-RT-TABLE OF STTRT

                        WHEN STTRT-IDX  >  STATE-TAX-RT-COUNT OF STTRT

                           CONTINUE

                        WHEN STATE OF STTRT(STTRT-IDX)
                            =  STATE OF TARRY(TARRY-IDX)

                           PERFORM VARYING WORK-OTH-IDX  FROM  1  BY  1
                               UNTIL WORK-OTH-IDX  >  OTH-RT-TABLE-COUNT
                               OF STTRT(STTRT-IDX)

                            IF TAX-CLASS-LTC-EE OF OTH-RT-TABLE
                               OF STTRT(STTRT-IDX WORK-OTH-IDX)

                              COMPUTE WORK-TAX-RATE OF W-WK ROUNDED
                              =  WORK-TAX-RATE OF W-WK
                                 +  TAX-RT OF OTH-RT-TABLE
                                    OF STTRT(STTRT-IDX WORK-OTH-IDX)
                            END-IF
                           END-PERFORM

                           COMPUTE WORK-LTC-RND OF W-WK ROUNDED
                           = (TXGRS-QTD OF TARRY(TARRY-IDX) +
                              TXGRS-CUR OF TARRY(TARRY-IDX) ) *
                             WORK-TAX-RATE OF W-WK

                           COMPUTE WORK-LTC-ADJ OF W-WK
                           = WORK-LTC-RND OF W-WK
                           - TAX-QTD OF TARRY(TARRY-IDX)

                       END-SEARCH

      *                IF WORK-LTC-ADJ OF W-WK NOT EQUAL ZERO AND
      *                   WORK-LTC-ADJ OF W-WK  NOT EQUAL
      *                    TAX-CUR OF TARRY(TARRY-IDX)

      *                 MOVE WORK-LTC-ADJ OF W-WK TO
      *                      TAX-CUR OF TARRY(TARRY-IDX)

      *                 END-IF

                       IF TAX-CUR OF TARRY(TARRY-IDX) < ZERO
                          AND TXGRS-CUR OF TARRY(TARRY-IDX) NOT < ZERO
                         MOVE ZERO TO TAX-CUR OF TARRY(TARRY-IDX)
                       END-IF

                    END-IF

                    IF MANUAL-CHECK-YES OF CHECK  AND
                       PY-LTC-STATUS-YES OF WORK OF TAXDT

                       PERFORM LA250-GET-ALL-GROSS

                       IF TXGRS-CUR OF TARRY(TARRY-IDX) NOT =
                          ALL-GROSS-AMT OF W-WK

                          MOVE ALL-GROSS-AMT OF W-WK TO
                               TXGRS-CUR OF TARRY(TARRY-IDX)
                       END-IF

                    END-IF

                    IF TXGRS-CUR OF TARRY(TARRY-IDX) NOT =
                       NLGRS-CUR OF TARRY(TARRY-IDX)

                       MOVE TXGRS-CUR OF TARRY(TARRY-IDX)
                             TO
                            NLGRS-CUR OF TARRY(TARRY-IDX)
                    END-IF


           END-SEARCH

           .
       CALC-LTC-SELF-ADJ-EXIT.

      /*****************************************************************
      *                                                                *
       LH160-CALC-FML-SELF-ADJ SECTION.
       LH160.
      *                                                                *
      *    EVALUATE WHETHER SELF-ADJUSTMENT IS NECESSARY FOR FML       *
      *    TAX CALCULATED.                                             *
      *                                                                *
      ******************************************************************

           MOVE ZERO TO WORK-TAX-RATE OF W-WK
           MOVE ZERO TO WORK-FML-AMT OF W-WK
           MOVE ZERO TO WORK-FML-RND OF W-WK
           MOVE ZERO TO WORK-OTH-IDX OF W-WK
           SET PFML-CT-MAX-NO OF W-SW TO TRUE

           SET TARRY-IDX  TO  1

           SEARCH TAXCALC-DATA OF TARRY

               WHEN TARRY-IDX  >  TAXCALC-COUNT OF TARRY

                    CONTINUE

               WHEN  TAX-CLASS-FML-EE OF TARRY (TARRY-IDX)

                    IF ONE-TIME-NA OF TARRY(TARRY-IDX)
                       SET STTRT-IDX  TO  1
                       MOVE ZERO TO WORK-OTH-IDX OF W-WK

                       SEARCH STATE-TAX-RT-TABLE OF STTRT

                        WHEN STTRT-IDX  >  STATE-TAX-RT-COUNT OF STTRT

                           CONTINUE

                        WHEN STATE OF STTRT(STTRT-IDX)
                            =  STATE OF TARRY(TARRY-IDX)

                           PERFORM VARYING WORK-OTH-IDX  FROM  1  BY  1
                               UNTIL WORK-OTH-IDX  >  OTH-RT-TABLE-COUNT
                               OF STTRT(STTRT-IDX)

                            IF TAX-CLASS-FML-EE OF OTH-RT-TABLE
                               OF STTRT(STTRT-IDX WORK-OTH-IDX)

                              COMPUTE WORK-TAX-RATE OF W-WK ROUNDED
                              =  WORK-TAX-RATE OF W-WK
                                 +  TAX-RT OF OTH-RT-TABLE
                                    OF STTRT(STTRT-IDX WORK-OTH-IDX)

                                   MOVE MAX-GROSS OF OTH-RT-TABLE OF
                                     STTRT(STTRT-IDX WORK-OTH-IDX)
                                         TO  FML-MAX-GROSS OF W-WK
                            END-IF
                           END-PERFORM

                           IF (TXGRS-CUR OF TARRY(TARRY-IDX)
                               +  TXGRS-YTD OF TARRY(TARRY-IDX))
                               > FML-MAX-GROSS OF W-WK

                               COMPUTE WORK-FML-RND OF W-WK ROUNDED
                                   =  FML-MAX-GROSS OF W-WK
                                   *  WORK-TAX-RATE OF W-WK

                           ELSE

                               COMPUTE WORK-FML-RND OF W-WK ROUNDED
                                    = (TXGRS-QTD OF TARRY(TARRY-IDX)
                                    +  TXGRS-CUR OF TARRY(TARRY-IDX))
                                    *  WORK-TAX-RATE OF W-WK

                           END-IF

                           IF (TXGRS-CUR OF TARRY(TARRY-IDX)
                               +  TXGRS-YTD OF TARRY(TARRY-IDX))
                               >= FML-MAX-GROSS OF W-WK

                             SET PFML-CT-MAX-YES OF W-SW TO TRUE

                           END-IF

                          COMPUTE WORK-FML-AMT OF W-WK
                           = WORK-FML-RND OF W-WK
                           - TAX-QTD OF TARRY(TARRY-IDX)

                       END-SEARCH

                       IF WORK-FML-AMT OF W-WK NOT EQUAL ZERO AND
                         WORK-FML-AMT OF W-WK  NOT EQUAL
                          TAX-CUR OF TARRY(TARRY-IDX)

                         IF PFML-CT-MAX-YES OF W-SW  AND
                            WORK-FML-AMT OF W-WK < ZERO

                             CONTINUE

                         ELSE
                           MOVE WORK-FML-AMT OF W-WK TO
                            TAX-CUR OF TARRY(TARRY-IDX)

                         END-IF
                       END-IF

                    END-IF

                    IF ONE-TIME-ADDITION OF TARRY(TARRY-IDX)

                       COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                       = TAX-CUR OF TARRY(TARRY-IDX)
                       + TAX-ONE-TIME OF TARRY(TARRY-IDX)
                    ELSE
                       IF ONE-TIME-REFUND OF TARRY(TARRY-IDX)

                          IF WORK-FML-AMT OF W-WK NOT EQUAL ZERO

                             COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                                = TAX-CUR OF TARRY(TARRY-IDX)
                                + TAX-ONE-TIME OF TARRY(TARRY-IDX)

                          END-IF
                       END-IF

                    END-IF

           END-SEARCH

           .
       CALC-FML-SELF-ADJ-EXIT.


      /*****************************************************************
      *                                                                *
       LH165-FLI-MLI-QTD-SELF-ADJ SECTION.
       LH165.
      *                                                                *
      * EVALUATE WHETHER QTD SELF-ADJUSTMENT IS NECESSARY FOR          *
      * MASSACHUSETTS FLI AND MLI TAXES                                *
      *                                                                *
      ******************************************************************

           MOVE ZERO  TO  FLI-EE-RATE OF W-WK
           MOVE ZERO  TO  FLI-SPLIT-RT OF W-WK
           MOVE ZERO  TO  MLI-SPLIT-RT OF W-WK
           MOVE ZERO  TO  FLI-EE-RATIO OF W-WK
           MOVE ZERO  TO  FLI-ER-RATIO OF W-WK
           MOVE ZERO  TO  MLI-EE-RATIO OF W-WK
           MOVE ZERO  TO  MLI-ER-RATIO OF W-WK
           MOVE ZERO  TO  FLI-MLI-TOT-PREM OF W-WK
           MOVE ZERO  TO  FLI-TOT OF W-WK
           MOVE ZERO  TO  MLI-TOT OF W-WK
           MOVE ZERO  TO  FLI-EE-PREM OF W-WK
           MOVE ZERO  TO  FLI-ER-PREM OF W-WK
           MOVE ZERO  TO  MLI-EE-PREM OF W-WK
           MOVE ZERO  TO  MLI-ER-PREM OF W-WK
           MOVE ZERO  TO  FLI-MAX-GROSS OF W-WK
           MOVE ZERO  TO  FLI-EE-TXGRS-CUR OF W-WK
           MOVE ZERO  TO  MLI-EE-TXGRS-CUR OF W-WK

           PERFORM VARYING TARRY-IDX FROM 1 BY 1
                UNTIL TARRY-IDX  >  TAXCALC-COUNT OF TARRY

               IF STATE OF TARRY(TARRY-IDX) = 'MA'
                   IF TAX-CLASS-FLI-EE OF TARRY(TARRY-IDX)

                       MOVE TXGRS-CUR OF TARRY(TARRY-IDX)
                                TO  FLI-EE-TXGRS-CUR OF W-WK
                   END-IF

                   IF TAX-CLASS-MLI-EE OF TARRY(TARRY-IDX)

                       MOVE TXGRS-CUR OF TARRY(TARRY-IDX)
                                TO  MLI-EE-TXGRS-CUR OF W-WK
                   END-IF
               END-IF
           END-PERFORM

           PERFORM VARYING TARRY-IDX FROM 1 BY 1
                UNTIL TARRY-IDX  >  TAXCALC-COUNT OF TARRY

               IF STATE OF TARRY(TARRY-IDX) = 'MA'
                   IF TAX-CLASS-FLI-EE OF TARRY(TARRY-IDX)

      * GET FLI/MLI EMPLOYEE AND EMPOYER RATIOS
                       SET STTRT-IDX  TO  1

                       MOVE ZERO TO WORK-OTH-IDX OF W-WK

                       SEARCH STATE-TAX-RT-TABLE OF STTRT

                           WHEN STATE OF STTRT(STTRT-IDX)
                                     =  STATE OF TARRY(TARRY-IDX)

                               PERFORM VARYING WORK-OTH-IDX
                                   FROM 1 BY 1 UNTIL WORK-OTH-IDX
                                       >  OTH-RT-TABLE-COUNT
                                            OF STTRT(STTRT-IDX)

                                 IF TAX-CLASS-FLI-EE OF OTH-RT-TABLE
                                  OF STTRT(STTRT-IDX WORK-OTH-IDX)

                                   MOVE TAX-RT OF OTH-RT-TABLE OF
                                     STTRT(STTRT-IDX WORK-OTH-IDX)
                                         TO  FLI-EE-RATE OF W-WK
                                   MOVE PNA-WA-RATIO OF OTH-RT-TABLE OF
                                     STTRT(STTRT-IDX WORK-OTH-IDX)
                                         TO  FLI-SPLIT-RT OF W-WK

                                   MOVE MAX-GROSS OF OTH-RT-TABLE OF
                                     STTRT(STTRT-IDX WORK-OTH-IDX)
                                         TO  FLI-MAX-GROSS OF W-WK
                                 END-IF

                                 IF TAX-CLASS-MLI-EE OF OTH-RT-TABLE
                                  OF STTRT(STTRT-IDX WORK-OTH-IDX)

                                   MOVE PNA-WA-RATIO OF OTH-RT-TABLE OF
                                     STTRT(STTRT-IDX WORK-OTH-IDX)
                                         TO  MLI-SPLIT-RT OF W-WK
                                 END-IF
                               END-PERFORM
                       END-SEARCH

      * GET FLI/MLI RATES
                       SET FLIRT-IDX  TO  1

                       SEARCH FLI-EXPERIENCE-RT-TABLE OF FLIRT

                           WHEN STATE OF FLIRT(FLIRT-IDX)
                                       =  STATE OF TARRY(TARRY-IDX)

                               MOVE PNA-FLI-EE-PCT OF FLIRT(FLIRT-IDX)
                                      TO FLI-EE-RATIO OF W-WK
                               MOVE PNA-FLI-ER-PCT OF FLIRT(FLIRT-IDX)
                                      TO FLI-ER-RATIO OF W-WK
                               MOVE PNA-MLI-EE-PCT OF FLIRT(FLIRT-IDX)
                                      TO MLI-EE-RATIO OF W-WK
                               MOVE PNA-MLI-ER-PCT OF FLIRT(FLIRT-IDX)
                                      TO MLI-ER-RATIO OF W-WK
                       END-SEARCH

      * CALCULATE CURRENT + QTD FLI/EE, FLI/ER, MLI/EE AND MLI/ER TAXES
      * LOGIC NEEDS TO MATCH THAT OF DJ300-CALC-WA-FLI-SPLIT IN PSPTCALC

                       IF (TXGRS-CUR OF TARRY(TARRY-IDX)
                               +  TXGRS-YTD OF TARRY(TARRY-IDX))
                               > FLI-MAX-GROSS OF W-WK

                           IF TXGRS-QTD OF TARRY(TARRY-IDX)
                                  <  FLI-MAX-GROSS OF W-WK

                               COMPUTE FLI-MLI-TOT-PREM OF W-WK ROUNDED
                                       =  (TXGRS-CUR OF TARRY(TARRY-IDX)
                                       +  TXGRS-QTD OF TARRY(TARRY-IDX))
                                       *  FLI-EE-RATE OF W-WK
                           ELSE
                               COMPUTE FLI-MLI-TOT-PREM OF W-WK ROUNDED
                                   =  FLI-MAX-GROSS OF W-WK
                                   *  FLI-EE-RATE OF W-WK
                           END-IF
                       ELSE
                           COMPUTE FLI-MLI-TOT-PREM OF W-WK ROUNDED
                                   =  (TXGRS-CUR OF TARRY(TARRY-IDX)
                                   +  TXGRS-QTD OF TARRY(TARRY-IDX))
                                   *  FLI-EE-RATE OF W-WK
                       END-IF

                       COMPUTE FLI-TOT OF W-WK ROUNDED
                               =  FLI-MLI-TOT-PREM OF W-WK
                               *  FLI-SPLIT-RT OF W-WK

                       COMPUTE MLI-TOT OF W-WK
                               =  FLI-MLI-TOT-PREM OF W-WK
                               -  FLI-TOT  OF W-WK

                       COMPUTE FLI-EE-PREM OF W-WK ROUNDED
                               =  FLI-TOT OF W-WK
                               *  (FLI-EE-RATIO OF W-WK
                               /  100)

                       COMPUTE FLI-ER-PREM OF W-WK
                               =  FLI-TOT  OF W-WK
                               -  FLI-EE-PREM OF W-WK

                       COMPUTE MLI-EE-PREM OF W-WK ROUNDED
                               =  MLI-TOT OF W-WK
                               *  (MLI-EE-RATIO OF W-WK
                               /  100 )

                       IF MLI-ER-RATIO OF W-WK = ZERO
                           MOVE ZERO TO MLI-ER-PREM OF W-WK
                       ELSE
                           IF (MLI-EE-RATIO OF W-WK
                                   + MLI-ER-RATIO OF W-WK)  NOT =  100

                               COMPUTE MLI-ER-PREM OF W-WK ROUNDED
                                       =  MLI-TOT OF W-WK
                                       *  (MLI-ER-RATIO OF W-WK
                                       /  100)
                           ELSE
                               COMPUTE MLI-ER-PREM OF W-WK
                                       =  MLI-TOT OF W-WK
                                       -  MLI-EE-PREM OF W-WK
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-PERFORM

           PERFORM VARYING TARRY-IDX FROM 1 BY 1
                UNTIL TARRY-IDX  >  TAXCALC-COUNT OF TARRY

               IF STATE OF TARRY(TARRY-IDX) = 'MA'
                     AND (TAX-CLASS-FLI-EE OF TARRY(TARRY-IDX)
                           OR TAX-CLASS-FLI-ER OF TARRY(TARRY-IDX)
                           OR TAX-CLASS-MLI-EE OF TARRY(TARRY-IDX)
                           OR TAX-CLASS-MLI-ER OF TARRY(TARRY-IDX))

                   IF NOT ONE-TIME-NA OF TARRY(TARRY-IDX)

                       PERFORM LH167-FLI-MLI-ADJ-ONE-TIME
                   END-IF

                   IF NOT ONE-TIME-OVERRIDE OF TARRY(TARRY-IDX)
                       IF TAX-CLASS-FLI-EE OF TARRY(TARRY-IDX)
                             AND (TAX-QTD OF TARRY(TARRY-IDX)
                                + TAX-CUR OF TARRY(TARRY-IDX))
                                NOT =  FLI-EE-PREM OF W-WK


                           IF TXGRS-CUR OF TARRY(TARRY-IDX) = ZERO
                                        AND
                              NLGRS-CUR OF TARRY(TARRY-IDX) = ZERO

                              CONTINUE

                           ELSE
                             COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                                   =  FLI-EE-PREM OF W-WK
                                   -  TAX-QTD OF TARRY(TARRY-IDX)
                           END-IF
                       END-IF

                       IF TAX-CLASS-FLI-ER OF TARRY(TARRY-IDX)
                             AND (TAX-QTD OF TARRY(TARRY-IDX)
                                + TAX-CUR OF TARRY(TARRY-IDX))
                                NOT =  FLI-ER-PREM OF W-WK


                          IF TXGRS-CUR OF TARRY(TARRY-IDX) = ZERO
                                        AND
                             NLGRS-CUR OF TARRY(TARRY-IDX) = ZERO

                              CONTINUE

                           ELSE
                             COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                                   =  FLI-ER-PREM OF W-WK
                                   -  TAX-QTD OF TARRY(TARRY-IDX)
                          END-IF
                       END-IF

                       IF FLI-EE-TXGRS-CUR OF W-WK  NOT =
                                MLI-EE-TXGRS-CUR OF W-WK
                             AND FLI-EE-TXGRS-CUR OF W-WK  NOT =  ZERO
                             AND MLI-EE-TXGRS-CUR OF W-WK  =  ZERO
      * FLI STATUS SHOULD MATCH MLI STATUS FOR MASSACHUSETTS.  SKIPT MLI
      * SELF-ADJUSTMENT IF FLI STATUS IS SUBJECT BUT MLI STATUS IS NOT.

                           CONTINUE
                       ELSE
                           IF TAX-CLASS-MLI-EE OF TARRY(TARRY-IDX)
                               AND (TAX-QTD OF TARRY(TARRY-IDX)
                                + TAX-CUR OF TARRY(TARRY-IDX))
                                NOT =  MLI-EE-PREM OF W-WK

                               IF TXGRS-CUR OF TARRY(TARRY-IDX) = ZERO
                                        AND
                                  NLGRS-CUR OF TARRY(TARRY-IDX) = ZERO

                                  CONTINUE

                               ELSE
                                  COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                                       =  MLI-EE-PREM OF W-WK
                                       -  TAX-QTD OF TARRY(TARRY-IDX)
                               END-IF
                           END-IF

                           IF TAX-CLASS-MLI-ER OF TARRY(TARRY-IDX)
                               AND (TAX-QTD OF TARRY(TARRY-IDX)
                                    + TAX-CUR OF TARRY(TARRY-IDX))
                                    NOT =  MLI-ER-PREM OF W-WK

                               IF TXGRS-CUR OF TARRY(TARRY-IDX) = ZERO
                                        AND
                                  NLGRS-CUR OF TARRY(TARRY-IDX) = ZERO

                                  CONTINUE

                               ELSE
                                  COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                                       =  MLI-ER-PREM OF W-WK
                                       -  TAX-QTD OF TARRY(TARRY-IDX)
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF

               MOVE TAX-CLASS OF TARRY(TARRY-IDX)
                        TO  PFML-WA-SW OF W-SW
               MOVE TAX-CLASS OF TARRY(TARRY-IDX)
                        TO  PFML-MA-SW OF W-SW
                            PFML-CT-SW OF W-SW
               IF ( (STATE OF TARRY(TARRY-IDX) = 'WA'
                     AND PFML-WA-YES OF W-SW)
                     OR (STATE OF TARRY(TARRY-IDX) = 'CT'
                         AND PFML-CT-YES OF W-SW)
                     OR (STATE OF TARRY(TARRY-IDX) = 'MA'
                         AND PFML-MA-YES OF W-SW) )
                   AND (TAX-CUR OF TARRY(TARRY-IDX)  NOT =  ZERO
                        OR TAX-ONE-TIME OF TARRY(TARRY-IDX)
                                        NOT =  ZERO)
                   AND TXGRS-CUR OF TARRY(TARRY-IDX)  =  ZERO

                   MOVE PAID-LEAVE-TXGRS OF GRSWK
                        TO  TXGRS-CUR OF TARRY(TARRY-IDX)
                   MOVE PAID-LEAVE-NLGRS OF GRSWK
                        TO  NLGRS-CUR OF TARRY(TARRY-IDX)
               END-IF
           END-PERFORM

           .
       FLI-MLI-QTD-SELF-ADJ-EXIT.


      /*****************************************************************
      *                                                                *
       LH167-FLI-MLI-ADJ-ONE-TIME SECTION.
       LH167.
      *                                                                *
      ******************************************************************

           IF ONE-TIME-REFUND OF TARRY(TARRY-IDX)

               CONTINUE
           ELSE
               IF ONE-TIME-OVERRIDE OF TARRY(TARRY-IDX)
                   MOVE TAX-ONE-TIME OF TARRY(TARRY-IDX)
                         TO  TAX-CUR OF TARRY(TARRY-IDX)
               ELSE
                   COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                           =  TAX-CUR OF TARRY(TARRY-IDX)
                           -  TAX-ONE-TIME OF TARRY(TARRY-IDX)
               END-IF
           END-IF

           .
       FLI-MLI-ADJ-ONE-TIME-EXIT.


      /*****************************************************************
      *                                                                *
       LH169-PFML-TXGRS SECTION.
       LH169.
      *                                                                *
      ******************************************************************

           PERFORM VARYING TARRY-IDX FROM 1 BY 1
                UNTIL TARRY-IDX  >  TAXCALC-COUNT OF TARRY

               MOVE TAX-CLASS OF TARRY(TARRY-IDX)
                        TO  PFML-WA-SW OF W-SW
               MOVE TAX-CLASS OF TARRY(TARRY-IDX)
                        TO  PFML-MA-SW OF W-SW
                            PFML-CT-SW OF W-SW
               IF ( (STATE OF TARRY(TARRY-IDX) = 'WA'
                     AND PFML-WA-YES OF W-SW)
                     OR (STATE OF TARRY(TARRY-IDX) = 'CT'
                         AND PFML-CT-YES OF W-SW)
                     OR (STATE OF TARRY(TARRY-IDX) = 'MA'
                         AND PFML-MA-YES OF W-SW) )
                   AND (TAX-CUR OF TARRY(TARRY-IDX)  NOT =  ZERO
                        OR TAX-ONE-TIME OF TARRY(TARRY-IDX)
                                        NOT =  ZERO)
                   AND TXGRS-CUR OF TARRY(TARRY-IDX)  =  ZERO

                   MOVE PAID-LEAVE-TXGRS OF GRSWK
                        TO  TXGRS-CUR OF TARRY(TARRY-IDX)
                   MOVE PAID-LEAVE-NLGRS OF GRSWK
                        TO  NLGRS-CUR OF TARRY(TARRY-IDX)
               END-IF
           END-PERFORM

           .
       PFML-TXGRS-EXIT.

      /*****************************************************************
      *                                                                *
       LH170-FRAMEWORK-SELF-ADJ SECTION.
       LH170.
      *                                                                *
      ******************************************************************

           MOVE ZERO TO WORK-TAX-RATE OF W-WK
           MOVE ZERO TO WORK-FML-AMT OF W-WK
           MOVE ZERO TO WORK-FML-RND OF W-WK
           MOVE ZERO TO WORK-OTH-IDX OF W-WK

           IF FRMWK-TAX-RT OF TARRY(TARRY-IDX) > ZERO

              MOVE FRMWK-TAX-RT OF TARRY(TARRY-IDX)
                                           TO WORK-TAX-RATE OF W-WK

              IF ONE-TIME-NA OF TARRY(TARRY-IDX)

                 IF FRMWK-SLF-ADJ-QTD OF TARRY(TARRY-IDX)
                     COMPUTE WORK-FML-RND OF W-WK ROUNDED
                                = (TXGRS-QTD OF TARRY(TARRY-IDX)
                                +  TXGRS-CUR OF TARRY(TARRY-IDX))
                                *  WORK-TAX-RATE OF W-WK

                     COMPUTE WORK-FML-AMT OF W-WK
                                = WORK-FML-RND OF W-WK
                                - TAX-QTD OF TARRY(TARRY-IDX)

                 END-IF

                 IF FRMWK-SLF-ADJ-MTD OF TARRY(TARRY-IDX)
                    COMPUTE WORK-FML-RND OF W-WK ROUNDED
                                = (TXGRS-MTD OF TARRY(TARRY-IDX)
                                +  TXGRS-CUR OF TARRY(TARRY-IDX))
                                *  WORK-TAX-RATE OF W-WK

                    COMPUTE WORK-FML-AMT OF W-WK
                                = WORK-FML-RND OF W-WK
                                - TAX-MTD OF TARRY(TARRY-IDX)

                 END-IF

                 IF FRMWK-SLF-ADJ-YTD OF TARRY(TARRY-IDX)
                    COMPUTE WORK-FML-RND OF W-WK ROUNDED
                                = (TXGRS-YTD OF TARRY(TARRY-IDX)
                                +  TXGRS-CUR OF TARRY(TARRY-IDX))
                                *  WORK-TAX-RATE OF W-WK
                    COMPUTE WORK-FML-AMT OF W-WK
                                = WORK-FML-RND OF W-WK
                                - TAX-YTD OF TARRY(TARRY-IDX)

                 END-IF

                 IF WORK-FML-AMT OF W-WK NOT EQUAL ZERO AND
                         WORK-FML-AMT OF W-WK  NOT EQUAL
                          TAX-CUR OF TARRY(TARRY-IDX)

                    MOVE WORK-FML-AMT OF W-WK TO
                            TAX-CUR OF TARRY(TARRY-IDX)

                 END-IF

              END-IF
           END-IF
           .
       FRAMEWORK-SELF-ADJ-EXIT.

      /*****************************************************************
      *                                                                *
       LH170-SET-FRAMEWORK-FLAGS SECTION.
       LH170.
      *                                                                *
      ******************************************************************


           SET ST-PGMID-IDX  TO  1

           SEARCH STATE-PGMID-ARRAY OF FRMWK

              AT END
                 CONTINUE

              WHEN STATE OF TARRY(TARRY-IDX) =
                      STATE OF STATE-PGMID-ARRAY OF FRMWK (ST-PGMID-IDX)
                   AND
                   TAX-CLASS OF TARRY(TARRY-IDX)  =
                      TAX-CLASS OF STATE-PGMID-ARRAY
                                                 OF FRMWK (ST-PGMID-IDX)

                      SET FRMWK-STATE-TAX-YES
                                OF TARRY(TARRY-IDX) TO TRUE

                      SET PGMID-IDX  TO  1

                      SEARCH PGMID-ARRAY OF FRMWK

                           AT END
                               CONTINUE

                         WHEN PY-PFF-PROGID OF PGMID-ARRAY
                                   OF FRMWK (PGMID-IDX)  =
                                 PY-PFF-PROGID OF STATE-PGMID-ARRAY
                                      OF FRMWK (ST-PGMID-IDX)
                         AND TAX-CLASS OF TARRY(TARRY-IDX) =
                                 TAX-CLASS OF PGMID-ARRAY
                                       OF FRMWK (PGMID-IDX)

                          IF PY-PFF-TAX-CAT OF PGMID-ARRAY
                                            OF FRMWK (PGMID-IDX) = 'E'
                             SET FRMWK-EE-TAX-SW-YES
                                       OF TARRY(TARRY-IDX) TO TRUE
                          END-IF

                      END-SEARCH

           END-SEARCH
           .
       SET-FRAMEWORK-FLAGS-EXIT.

      /*****************************************************************
      *                                                                *
       LH171-FRAMEWORK-TXGRS-CALC SECTION.
       LH171.
      *                                                                *
      ******************************************************************

           MOVE ZERO TO WORK-TAX-RATE OF W-WK
           MOVE ZERO TO ANN-ALL-GROSS-AMT OF W-WK
           MOVE ZERO TO FML-MAX-GROSS OF W-WK
           SET PFF-MAX-LIM-NO TO TRUE

           IF FRMWK-TAX-RT OF TARRY(TARRY-IDX) > ZERO

              EVALUATE TRUE

                 WHEN PAY-FREQ-TYPE-WEEKLY OF PYGRP

                    MOVE  52  TO  ANNUAL-FACTOR OF W-WK

                 WHEN PAY-FREQ-TYPE-BIWEEKLY OF PYGRP

                    MOVE  26  TO  ANNUAL-FACTOR OF W-WK

                 WHEN PAY-FREQ-TYPE-SEMIMONTHLY OF PYGRP

                    MOVE  24  TO  ANNUAL-FACTOR OF W-WK

                 WHEN PAY-FREQ-TYPE-MONTHLY OF PYGRP

                    MOVE  12  TO  ANNUAL-FACTOR OF W-WK

                 WHEN PAY-FREQ-TYPE-QUARTERLY OF PYGRP

                    MOVE   4  TO  ANNUAL-FACTOR OF W-WK

                 WHEN PAY-FREQ-TYPE-ANNUALLY OF PYGRP

                    MOVE   1  TO  ANNUAL-FACTOR OF W-WK

                 WHEN PAY-FREQ-TYPE-DAILY OF PYGRP

                    MOVE 260  TO  ANNUAL-FACTOR OF W-WK
              END-EVALUATE

              MOVE FRMWK-TAX-RT OF TARRY(TARRY-IDX)
                                        TO WORK-TAX-RATE OF W-WK

              IF FRMWK-TXGRS-WITHHOLDING OF TARRY(TARRY-IDX)

                 MOVE FWT OF GRSWK(2) TO TXGRS-CUR OF TARRY(TARRY-IDX)
              END-IF

              IF FRMWK-TXGRS-DISABILITY OF TARRY(TARRY-IDX)

                 MOVE FICA OF GRSWK(2) TO TXGRS-CUR OF TARRY(TARRY-IDX)
              END-IF

              IF FRMWK-TXGRS-UNEMPLOYMENT OF TARRY(TARRY-IDX)

                 MOVE FUT OF GRSWK(2) TO TXGRS-CUR OF TARRY(TARRY-IDX)
              END-IF

              IF FRMWK-TXGRS-PAID-LEAVE OF TARRY(TARRY-IDX)

                  PERFORM LH172-FRAMEWORK-PAID-LEAVE
              END-IF

              SET STTRT-IDX  TO  1
              MOVE ZERO TO WORK-OTH-IDX OF W-WK

              SEARCH STATE-TAX-RT-TABLE OF STTRT

                    WHEN STTRT-IDX  >  STATE-TAX-RT-COUNT OF STTRT

                         CONTINUE

                    WHEN STATE OF STTRT(STTRT-IDX)
                            =  STATE OF TARRY(TARRY-IDX)

                         PERFORM VARYING WORK-OTH-IDX  FROM  1  BY  1
                           UNTIL WORK-OTH-IDX  >  OTH-RT-TABLE-COUNT
                               OF STTRT(STTRT-IDX)

                            IF TAX-CLASS OF OTH-RT-TABLE
                               OF STTRT(STTRT-IDX WORK-OTH-IDX) =
                               TAX-CLASS OF TARRY (TARRY-IDX)

                               MOVE MAX-GROSS OF OTH-RT-TABLE OF
                                     STTRT(STTRT-IDX WORK-OTH-IDX)
                                         TO  FML-MAX-GROSS OF W-WK
                            END-IF
                         END-PERFORM
              END-SEARCH

              MOVE TXGRS-CUR OF TARRY(TARRY-IDX)
                                    TO NLGRS-CUR OF TARRY(TARRY-IDX)

              IF TXGRS-CUR OF TARRY(TARRY-IDX) +
                 TXGRS-YTD OF TARRY(TARRY-IDX) > FML-MAX-GROSS OF W-WK

                   IF FML-MAX-GROSS OF W-WK >=
                                TXGRS-YTD OF TARRY(TARRY-IDX)

                      COMPUTE TXGRS-CUR OF TARRY(TARRY-IDX)
                             = FML-MAX-GROSS OF W-WK
                             - TXGRS-YTD OF TARRY(TARRY-IDX)
                      SET PFF-MAX-LIM-YES TO TRUE
                   ELSE

                      MOVE ZERO TO TXGRS-CUR OF TARRY(TARRY-IDX)

                   END-IF
              END-IF

              COMPUTE ANN-ALL-GROSS-AMT OF W-WK  =
                 ANNUAL-FACTOR OF W-WK *  TXGRS-CUR OF TARRY(TARRY-IDX)

              COMPUTE TAX-CUR OF TARRY(TARRY-IDX) ROUNDED =
                   (ANN-ALL-GROSS-AMT OF W-WK  * WORK-TAX-RATE OF W-WK)
                    / ANNUAL-FACTOR OF W-WK

              IF TAX-CUR OF TARRY(TARRY-IDX) = ZERO
                 IF PFF-MAX-LIM-YES
                    MOVE ZERO TO TXGRS-CUR OF TARRY(TARRY-IDX)
                 ELSE
                    MOVE ZERO TO NLGRS-CUR OF TARRY(TARRY-IDX)
                    MOVE ZERO TO TXGRS-CUR OF TARRY(TARRY-IDX)
                 END-IF
              END-IF

              COMPUTE NLGRS-YTD OF TARRY(TARRY-IDX) =
                      NLGRS-YTD OF TARRY(TARRY-IDX) +
                      NLGRS-CUR OF TARRY(TARRY-IDX)

           END-IF
           .
       FRMWORK-TXGRS-CALC-EXIT.

      /*****************************************************************
      *                                                                *
       LH172-FRAMEWORK-PAID-LEAVE SECTION.
       LH172.
      *                                                                *
      ******************************************************************

           MOVE ZERO TO WORK-ADJUST-AMT OF W-WK

           SET TGBTB-IDX  TO  1

           SEARCH TAX-GRS-BASE-TABLE OF TGBTB

               AT END

                   CONTINUE

               WHEN STATE OF TARRY(TARRY-IDX)
                           =  STATE OF TGBTB(TGBTB-IDX)
                       AND LOCALITY OF TGBTB(TGBTB-IDX) = SPACE

                   EVALUATE TRUE

                      WHEN TAX-GRS-BASE OF PDL(TGBTB-IDX) = 'W'

                           MOVE FWT OF GRSWK(2) TO
                                     TXGRS-CUR OF TARRY(TARRY-IDX)

                      WHEN TAX-GRS-BASE OF PDL(TGBTB-IDX) = 'U'

                           MOVE FUT OF GRSWK(2) TO
                                     TXGRS-CUR OF TARRY(TARRY-IDX)

                           IF MANUAL-CHECK-YES OF CHECK AND
                              FUT OF GRSWK(2) < ZERO AND
                              MANUAL-FUT-AMT OF W-WK =
                              MANUAL-FUT-DED OF W-WK

                              COMPUTE  TXGRS-CUR OF TARRY(TARRY-IDX)
                              = TXGRS-CUR OF TARRY(TARRY-IDX) +
                                MANUAL-FUT-DED OF W-WK

                           END-IF

                      WHEN TAX-GRS-BASE OF PDL(TGBTB-IDX) = 'D'

                           MOVE FICA OF GRSWK(2) TO
                                     TXGRS-CUR OF TARRY(TARRY-IDX)

                      WHEN TAX-GRS-BASE OF PDL(TGBTB-IDX) = 'G'

                           MOVE ZERO TO ALL-GROSS-AMT OF W-WK
                           PERFORM LA250-GET-ALL-GROSS
                           MOVE ALL-GROSS-AMT OF W-WK TO
                                     TXGRS-CUR OF TARRY(TARRY-IDX)

                   END-EVALUATE

           END-SEARCH

           SET PFF-TGC-ERNCD-NO OF W-SW TO TRUE
           SET PFF-TGC-DEDCD-NO OF W-SW TO TRUE


           PERFORM VARYING L-TAXGRS-IDX FROM 1 BY 1
                   UNTIL L-TAXGRS-IDX  >  TAXGRS-CMP-COUNT OF GRSWK

               IF TAX-CLASS-TG OF TAXGRS-CMP-DATA
                        OF GRSWK(L-TAXGRS-IDX)  =  'P'
                    AND STATE-TG OF TAXGRS-CMP-DATA
                        OF GRSWK(L-TAXGRS-IDX)
                                        =  STATE OF TARRY(TARRY-IDX)
                    AND LOCALITY-TG
                        OF TAXGRS-CMP-DATA OF GRSWK(L-TAXGRS-IDX)
                                        =  SPACE

                    IF ERNCD-TG OF TAXGRS-CMP-DATA
                        OF GRSWK(L-TAXGRS-IDX) NOT EQUAL SPACE

                       SET EARRY-IDX TO 1

                       SEARCH EARNINGS-DATA OF EARRY

                          AT END
                             CONTINUE

                          WHEN  ERNCD OF EARRY(EARRY-IDX)
                            = ERNCD-TG OF TAXGRS-CMP-DATA
                                       OF GRSWK(L-TAXGRS-IDX)

                            SET PFF-TGC-ERNCD-YES OF W-SW TO TRUE
                            MOVE TAX-GROSS-ADJ OF
                               TAXGRS-CMP-DATA OF GRSWK(L-TAXGRS-IDX)
                               TO WORK-DED-AMT OF W-WK

                       END-SEARCH

                    END-IF

                    IF DEDCD-TG OF TAXGRS-CMP-DATA
                        OF GRSWK(L-TAXGRS-IDX) NOT EQUAL SPACE

                       SET DARRY-IDX  TO  1
                       MOVE ZERO TO  WORK-DED-AMT OF W-WK

                       SEARCH DEDUCTION-DATA OF DARRY

                          AT END
                             CONTINUE

                          WHEN DEDCD OF DARRY(DARRY-IDX)
                             = DEDCD-TG OF TAXGRS-CMP-DATA
                               OF GRSWK(L-TAXGRS-IDX)

                            IF SEPCHK OF CHECK  NOT =  ZERO

                               SET DEDT5-IDX TO DEDTB-PTR
                                               OF DARRY(DARRY-IDX)
                               SET DEDC1-IDX TO DEDTB-PTR
                                               OF DARRY(DARRY-IDX)
                               IF SEPCHK-DED-YES OF
                                        DEDT5(DEDT5-IDX DEDC1-IDX)
                                    SET PFF-TGC-DEDCD-YES OF W-SW
                                                       TO TRUE
                               END-IF
                            ELSE
                               SET PFF-TGC-DEDCD-YES OF W-SW
                                                       TO TRUE

                               PERFORM VARYING DCLAS-IDX  FROM  1  BY  1
                                 UNTIL DCLAS-IDX
                                   >  DEDUCTION-CLASS-MAX OF DARRY
                                   OR DED-CLASS
                                         OF DARRY(DARRY-IDX DCLAS-IDX)
                                           =  SPACE

                                 IF DED-CLASS
                                       OF DARRY(DARRY-IDX DCLAS-IDX)
                                    = DED-CLASS-TG OF TAXGRS-CMP-DATA
                                       OF GRSWK(L-TAXGRS-IDX)

                                    MOVE DED-CUR
                                         OF DARRY(DARRY-IDX DCLAS-IDX)
                                         TO WORK-DED-AMT OF W-WK
                                 END-IF
                               END-PERFORM
                            END-IF

                       END-SEARCH

                    END-IF

                    IF TAX-GROSS-EFFECT OF
                               TAXGRS-CMP-DATA OF GRSWK(L-TAXGRS-IDX)
                       = 'A'

                      IF PFF-TGC-ERNCD-YES OF W-SW  OR
                         PFF-TGC-DEDCD-YES OF W-SW

                         COMPUTE WORK-ADJUST-AMT OF W-WK =
                            WORK-ADJUST-AMT OF W-WK +
                            WORK-DED-AMT OF W-WK
                      END-IF
                    END-IF

                    IF TAX-GROSS-EFFECT OF
                               TAXGRS-CMP-DATA OF GRSWK(L-TAXGRS-IDX)
                       = 'S'

                      IF PFF-TGC-ERNCD-YES OF W-SW  OR
                         PFF-TGC-DEDCD-YES OF W-SW

                         COMPUTE WORK-ADJUST-AMT OF W-WK =
                            WORK-ADJUST-AMT OF W-WK -
                            WORK-DED-AMT OF W-WK
                      END-IF
                    END-IF
               END-IF
           END-PERFORM

           COMPUTE TXGRS-CUR OF TARRY(TARRY-IDX) =
                   TXGRS-CUR OF TARRY(TARRY-IDX) +
                   WORK-ADJUST-AMT OF W-WK


           .
       FRAMEWORK-PAID-LEAVE-EXIT.


      /*****************************************************************
      *                                                                *
       LH173-FRAMEWORK-STATES SECTION.
       LH173.
      *                                                                *
      ******************************************************************

           MOVE EMPLID   OF CHECK  TO  EMPLID OF BIND-DATA OF S-FRMWKS
           MOVE COMPANY  OF PSLCT  TO  COMPANY
                                              OF BIND-DATA OF S-FRMWKS
           MOVE CHECK-DT OF PYGRP  TO  CHECK-DT
                                              OF BIND-DATA OF S-FRMWKS

           CALL 'PTPSQLRT' USING   ACTION-SELECT OF SQLRT
                                   SQLRT
                                   SQL-CURSOR OF S-FRMWKS
                                   SQL-STMT OF S-FRMWKS
                                   BIND-SETUP OF S-FRMWKS
                                   BIND-DATA OF S-FRMWKS
                                   SELECT-SETUP OF S-FRMWKS
                                   SELECT-DATA OF S-FRMWKS
           IF RTNCD-ERROR OF SQLRT

               MOVE 'FRAMEWORK-DATA(EXEC)'  TO  ERR-SECTION OF SQLRT
               PERFORM ZZ000-SQL-ERROR
           END-IF

           INITIALIZE SELECT-DATA OF S-FRMWKS

           PERFORM UNTIL RTNCD-END OF SQLRT

              CALL 'PTPSQLRT' USING   ACTION-FETCH OF SQLRT
                                   SQLRT
                                   SQL-CURSOR OF S-FRMWKS

              IF RTNCD-ERROR OF SQLRT

                 IF RTNCD-END OF SQLRT

                    CONTINUE

                 ELSE

                   MOVE 'FETCH-FRAMEWORK-STATES'
                                             TO ERR-SECTION OF SQLRT
                   PERFORM ZZ000-SQL-ERROR

                 END-IF
              ELSE

                SET PFF-STATE-ERN-NO OF W-SW TO TRUE
                SET PFF-PROCESS-STATE-NO OF W-SW TO TRUE

                SET EARRY-IDX TO 1

                SEARCH EARNINGS-DATA OF EARRY

                       AT END
                             CONTINUE
                   WHEN  STATE OF EARRY(EARRY-IDX)
                            = PY-STATE OF SELECT-DATA OF S-FRMWKS

                            SET PFF-STATE-ERN-YES OF W-SW TO TRUE

                END-SEARCH

                IF PFF-STATE-ERN-NO OF W-SW

                   SET ST-PGMID-IDX  TO  1

                   SEARCH STATE-PGMID-ARRAY OF FRMWK

                          AT END
                             CONTINUE

                      WHEN PY-STATE OF SELECT-DATA OF S-FRMWKS
                           = STATE OF STATE-PGMID-ARRAY
                                  OF FRMWK (ST-PGMID-IDX)
                            AND PY-PFF-PROGID OF SELECT-DATA
                                              OF S-FRMWKS
                                = PY-PFF-PROGID OF STATE-PGMID-ARRAY
                                  OF FRMWK (ST-PGMID-IDX)
                            AND
                            PY-PFF-PRC-YES OF STATE-PGMID-ARRAY
                                      OF FRMWK (ST-PGMID-IDX)

                            SET PFF-PROCESS-STATE-YES OF W-SW TO TRUE

                   END-SEARCH

                END-IF

                IF PFF-PROCESS-STATE-YES OF W-SW

                   MOVE PY-STATE OF SELECT-DATA OF S-FRMWKS
                        TO STATE OF TAX-SET OF TAXWK

                   PERFORM LD300-GET-TAX-DATA

                   SET PYNET-FRMWK-PROCESS-YES OF PYNET-PASS TO TRUE
                   MOVE PY-PFF-PROGID OF SELECT-DATA OF S-FRMWKS
                        TO PYNET-PROGID  OF PYNET-PASS
                   PERFORM LD050-CALCULATE-TAX
                   SET PYNET-FRMWK-PROCESS-NO OF PYNET-PASS TO TRUE

                END-IF
              END-IF

           END-PERFORM

           SET RTNCD-OK OF SQLRT  TO TRUE


           .
       FRAMEWORK-STATES-EXIT.



      /*****************************************************************
      *                                                                *
       LH175-LOCAL-WK-WK-RECIP SECTION.
       LH175.
      *                                                                *
      ******************************************************************

      *    SEARCH TXARY FOR WORK LOCALITIES WITH WORK-WORK RECIPROCITY
      *    AND REDUCE IF NO TAX OVERRIDES

           PERFORM VARYING TXARY-IDX FROM  1  BY  1
                   UNTIL TXARY-IDX  >  TAX-SET-CNT

               IF RESIDENT-TAX-NO  OF TXARY(TXARY-IDX)
                       AND  WK-WK-RCP-COUNT OF TXARY(TXARY-IDX) > ZERO

                   PERFORM LH550-FIND-TARRY
                   IF  SPECIAL-LWT-STATUS-NONE OF TARRY(TARRY-IDX)
                           AND ONE-TIME-NA OF TARRY(TARRY-IDX)

                       PERFORM VARYING WKWKRCP-IDX FROM  1  BY  1
                               UNTIL WKWKRCP-IDX  >
                                   WK-WK-RCP-COUNT OF TXARY (TXARY-IDX)

                           PERFORM LH185-FIND-WKWK-LOCALITY
                       END-PERFORM
                   END-IF
               END-IF
           END-PERFORM

           .
       LOCAL-WK-WK-RECIP-EXIT.


      /*****************************************************************
      *                                                                *
       LH185-FIND-WKWK-LOCALITY SECTION.
       LH185.
      *                                                                *
      ******************************************************************

           SET WK-WK-NO-ADJ-TARRY OF W-SW  TO  TRUE

           PERFORM VARYING OTH-TXARY-IDX FROM  1  BY  1
                   UNTIL OTH-TXARY-IDX  >  TAX-SET-CNT OF TXARY

               IF  PAY-FREQ-TYPE OF TXARY (OTH-TXARY-IDX)  =
                   PAY-FREQ-TYPE OF TXARY (TXARY-IDX)
               AND TAX-PERIODS   OF TXARY (OTH-TXARY-IDX) =
                   TAX-PERIODS   OF TXARY (TXARY-IDX)
               AND TAX-METHOD    OF TXARY (OTH-TXARY-IDX) =
                   TAX-METHOD    OF TXARY (TXARY-IDX)
               AND BENEFIT-RCD-NO OF TXARY (OTH-TXARY-IDX) =
                   BENEFIT-RCD-NO OF TXARY (TXARY-IDX)
               AND (PAID-PRDS-PER-YEAR OF TXARY (OTH-TXARY-IDX) =       HP99999
                   PAID-PRDS-PER-YEAR OF TXARY (TXARY-IDX)              HP99999
               OR  PUBLIC-SECTOR-NO OF PSLCT)                           HP00001
               AND TAX-CLASS     OF TXARY (OTH-TXARY-IDX) =
                   TAX-CLASS     OF TXARY (TXARY-IDX)
               AND STATE         OF TXARY (OTH-TXARY-IDX) =
                   WKWK-STATE    OF TXARY (TXARY-IDX WKWKRCP-IDX)
               AND LOCALITY      OF TXARY (OTH-TXARY-IDX) =
                   WKWK-LOCALITY OF TXARY (TXARY-IDX WKWKRCP-IDX)

                   IF RECIPROCITY-RULE-REDWRK OF WK-WK-RCP-DATA
                           OF TXARY (TXARY-IDX WKWKRCP-IDX)

      *            REDUCE LOCALITY OF EMPLOYMENT (TXARY-IDX) BY
      *            LOCALITY OF OTHER JURISDICTION (OTH-TXARY-IDX)

                     IF STATE OF TXARY(TXARY-IDX) = 'KY'

                       CONTINUE
                     ELSE

                       COMPUTE ACTUAL-TAX-CUR OF TAX-SET-DATA
                               OF TXARY (TXARY-IDX)
                            =  (ACTUAL-TAX-CUR OF TAX-SET-DATA
                               OF TXARY (TXARY-IDX)
                            -  ((ACTUAL-TAX-CUR OF TAX-SET-DATA
                               OF TXARY (OTH-TXARY-IDX)
                            *  REDUCE-PCT OF WK-WK-RCP-DATA
                               OF TXARY (TXARY-IDX WKWKRCP-IDX))
                            /  100))
                            + HIGH-PREC-0
                     END-IF

                       COMPUTE CALCED-TAX-CUR OF TAX-SET-DATA
                               OF TXARY (TXARY-IDX)
                            =  (CALCED-TAX-CUR OF TAX-SET-DATA
                               OF TXARY (TXARY-IDX)
                            -  ((CALCED-TAX-CUR OF TAX-SET-DATA
                               OF TXARY (OTH-TXARY-IDX)
                            *  REDUCE-PCT OF WK-WK-RCP-DATA
                               OF TXARY (TXARY-IDX WKWKRCP-IDX))
                            /  100))
                            + HIGH-PREC-0

                       IF ACTUAL-TAX-CUR OF TAX-SET-DATA
                               OF TXARY(TXARY-IDX) < 0

                           MOVE ZERO TO ACTUAL-TAX-CUR OF TAX-SET-DATA
                                   OF TXARY(TXARY-IDX)
                       END-IF

                       IF CALCED-TAX-CUR OF TAX-SET-DATA
                               OF TXARY(TXARY-IDX) < 0

                           MOVE ZERO TO CALCED-TAX-CUR OF TAX-SET-DATA
                               OF TXARY(TXARY-IDX)
                       END-IF

                       SET WK-WK-ADJ-TARRY OF W-SW  TO  TRUE
                       PERFORM LH600-ADJUST-TARRY
                   END-IF
               END-IF
           END-PERFORM

           .
       FIND-WKWK-LOCALITY-EXIT.

      /*****************************************************************
      *                                                                *
       LH190-GET-PA-WORK-TXARY SECTION.
       LH190.
      *                                                                *
      ******************************************************************

           PERFORM VARYING TXARY-IDX FROM  1  BY  1
                   UNTIL TXARY-IDX  >  TAX-SET-CNT

               IF  RESIDENT-TAX-NO OF TXARY(TXARY-IDX)
                   AND  STATE OF TXARY(TXARY-IDX) = 'PA'
                   AND  LOCALITY OF TXARY(TXARY-IDX)
                        NOT = SPACE
                   AND  TAX-CLASS-WITHHOLDING OF TXARY(TXARY-IDX)

                   PERFORM VARYING LCL-RCP-IDX FROM 1  BY  1
                       UNTIL LCL-RCP-IDX > 1

                       IF  LOCALITY OF TXARY(TXARY-IDX) = '880000'
                           AND RES-PSD-CD
                               OF TXARY(TXARY-IDX, LCL-RCP-IDX)
                           = LOCALITY OF TXARY(TXARY-IDX)

                           MOVE LOCALITY OF TXARY(TXARY-IDX)
                                TO WORK-PSD-CD
                                     OF TXARY(TXARY-IDX,LCL-RCP-IDX)

                           MOVE ZERO TO ACTUAL-TAX-CUR
                                     OF TXARY(TXARY-IDX)
                           MOVE ZERO TO CALCED-TAX-CUR
                                     OF TXARY(TXARY-IDX)

                       END-IF

                       SET TARRY-FOR-WORK-NO OF W-SW TO TRUE

                       PERFORM LH536-FIND-WORK-PA-TARRY

                       IF  TARRY-FOR-WORK-YES OF W-SW

                           MOVE LOCALITY OF TXARY(TXARY-IDX)
                                TO WORK-PSD-CD
                                        OF TARRY(TARRY-IDX)
                           MOVE LOCALITY OF TXARY(TXARY-IDX)
                                TO WORK-PSD-CD
                                     OF TXARY(TXARY-IDX,LCL-RCP-IDX)

                           IF  LOCALITY OF TARRY(TARRY-IDX)
                               = '880000'
                               AND RES-PSD-CD
                                   OF TARRY(TARRY-IDX)
                               = LOCALITY OF TARRY(TARRY-IDX)
                               AND WORK-PSD-CD OF TARRY(TARRY-IDX)
                               =  LOCALITY OF TARRY(TARRY-IDX)

                                  MOVE ZERO TO TAX-CUR
                                            OF TARRY(TARRY-IDX)
                                  MOVE ZERO TO TXGRS-CUR
                                            OF TARRY(TARRY-IDX)
                                  MOVE ZERO TO TAX-CUR-ADDL-AMT
                                            OF TARRY(TARRY-IDX)
                           END-IF
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM

           .
       GET-PA-WORK-TXARY-EXIT.

      /*****************************************************************
      *                                                                *
       LH192-GET-PA-RES-TARRY SECTION.
       LH192.
      *                                                                *
      ******************************************************************


            SET TARRY-FOR-RES-NO OF W-SW TO TRUE

            SET TARRY-IDX TO 1

            SEARCH TAXCALC-DATA OF TARRY

                WHEN TARRY-IDX > TAXCALC-COUNT OF TARRY

                    CONTINUE

                WHEN STATE OF TARRY(TARRY-IDX) = 'PA'
                   AND LOCALITY OF TARRY(TARRY-IDX) NOT = SPACE
                   AND RESIDENT-YES OF TARRY(TARRY-IDX)
                   AND TAX-CLASS-WITHHOLDING OF TARRY(TARRY-IDX)

                       SET TARRY-FOR-RES-YES OF W-SW TO TRUE

                       IF  (NON-PA-ERN-FOUND-YES OF W-SW
                            OR PA-BLANK-FOUND-YES OF W-SW)
                           AND PHIL-LOCAL-FOUND-NO OF W-SW
                           AND MULTI-PA-LOC-AND-OTHER-NO OF W-SW

                           MOVE '880000' TO WORK-PSD-CD OF TAXCALC-DATA
                                         OF TARRY(TARRY-IDX)
                       END-IF


            END-SEARCH

           .

       GET-PA-RES-TARRY-EXIT.


      /*****************************************************************
      *                                                                *
       LH200-RES-LOCAL-GROSS SECTION.
       LH200.
      *                                                                *
      ******************************************************************

           IF (STATE OF TARRY(TARRY-IDX)  = 'MO'
              AND TAX-CLASS-WITHHOLDING OF TARRY(TARRY-IDX)
              AND RESIDENT-YES OF TARRY(TARRY-IDX)
              AND MO-WRK-OUTSIDE-YES OF W-SW
              AND MO-RCP-WRK-AMT OF W-WK > ZERO  )
              OR
              (STATE OF TARRY(TARRY-IDX) = 'MO'
              AND TAX-CLASS-WITHHOLDING OF TARRY(TARRY-IDX)
              AND RESIDENT-YES OF TARRY(TARRY-IDX)
              AND NBR-OF-STATES OF GRSWK > 1
              AND MO-SUPPLE-YES OF W-SW
              AND MO-RCP-WRK-AMT OF W-WK > ZERO )

              PERFORM LH411-MO-STATE-WITHHOLD

           END-IF

           PERFORM VARYING RESWK-IDX FROM  1  BY  1
                    UNTIL RESWK-IDX
                    >  LOCALITY-COUNT OF RESIDENCE OF TAXWK

               SET CALC-LOC-RES OF TAXWK  TO  TRUE
               SET RES-LCLCALC-NO OF W-WK TO TRUE

               MOVE STATE OF RESIDENCE OF TAXWK
                       TO  STATE OF TAX-SET OF TAXWK
               MOVE LOCALITY OF RESIDENCE OF TAXWK(RESWK-IDX)
                       TO  LOCALITY OF TAX-SET OF TAXWK

               SET LCLWK-IDX  TO  RESWK-IDX
               SET LCLWH-IDX  TO  RESWK-IDX
               SET LRCWK-IDX  TO  RESWK-IDX
               SET LCL-RCP-IDX  TO  RESWK-IDX
               INITIALIZE ACCUM-LEVEL OF GRSWK(2)
               SET SUT-RECALC-NO OF GRSWK(2) TO TRUE
               INITIALIZE ACCUM-LEVEL OF WHADJ(2)

               MOVE PAY-FREQ-TYPE OF TXARY(1)
                       TO  PAY-FREQ-TYPE OF TAX-SET OF TAXWK
               MOVE TAX-PERIODS OF TXARY(1)
                       TO  TAX-PERIODS OF TAX-SET OF TAXWK
               MOVE TAX-METHOD OF TXARY(1)
                       TO  TAX-METHOD OF TAX-SET OF TAXWK
               MOVE BENEFIT-RCD-NO OF TXARY(1)
                       TO BENEFIT-RCD-NO OF TAXWK
               IF CHANGE-FICA-YES-SAV
                  MOVE FICA-STATUS-EE OF TAX-SET OF TXARY(1)
                    TO FICA-STATUS-EE OF TAX-SET OF TAXWK
               END-IF

               IF PUBLIC-SECTOR-YES OF PSLCT                            HP00001
                                                                        HP00001
                   MOVE PAID-PRDS-PER-YEAR OF TXARY(1)                  HP99999
                           TO  PAID-PRDS-PER-YEAR OF TAX-SET OF TAXWK   HP99999
               END-IF                                                   HP00001

               PERFORM LH221-ACCUM-LOC-RES-TGC

               PERFORM VARYING OTH-TXARY-IDX FROM  1  BY  1
                       UNTIL OTH-TXARY-IDX > TAX-SET-CNT OF TXARY

                   IF STATE OF TXARY (OTH-TXARY-IDX) = '$U'  AND
                           (PAY-FREQ-TYPE OF TAX-SET OF TAXWK
                           NOT =  PAY-FREQ-TYPE OF TXARY(OTH-TXARY-IDX)
                           OR  TAX-PERIODS OF TAX-SET OF TAXWK
                           NOT =  TAX-PERIODS OF TXARY(OTH-TXARY-IDX)
                           OR  TAX-METHOD OF TAX-SET OF TAXWK
                           NOT =  TAX-METHOD OF TXARY(OTH-TXARY-IDX)
                           OR BENEFIT-RCD-NO OF TAXWK
                           NOT = BENEFIT-RCD-NO OF TXARY(OTH-TXARY-IDX)
                           OR  (PAID-PRDS-PER-YEAR OF TAX-SET OF TAXWK  HP99999
                           NOT =  PAID-PRDS-PER-YEAR OF                 HP99999
                                   TXARY(OTH-TXARY-IDX)                 HP99999
                           AND PUBLIC-SECTOR-YES OF PSLCT)              HP00001
                           AND  TAX-CLASS-WITHHOLDING OF
                                   TXARY(OTH-TXARY-IDX))

                       PERFORM LH225-ACCUM-ADJUSTMENTS

                       IF FWT OF GRSWK(2)  NOT =  ZERO
                               OR LOC-WH-ADJ OF WHADJ(2 LCLWH-IDX)
                               NOT =  ZERO
                               OR LOC-ADJ OF GRSWK(2 LCLWK-IDX)
                               NOT = ZERO

                           SET CALC-LOC-RES OF TAXWK  TO  TRUE
                           SET PASS-RESWK-IDX OF TAXWK  TO  RESWK-IDX
                           PERFORM LD050-CALCULATE-TAX
                           SET RES-LCLCALC-YES OF W-WK TO TRUE

                       END-IF

                       INITIALIZE ACCUM-LEVEL OF GRSWK(2)
                       SET SUT-RECALC-NO OF GRSWK(2) TO TRUE
                       INITIALIZE ACCUM-LEVEL OF WHADJ(2)

                       MOVE PAY-FREQ-TYPE OF TXARY(OTH-TXARY-IDX)
                               TO  PAY-FREQ-TYPE OF TAXWK
                       MOVE TAX-PERIODS OF TXARY(OTH-TXARY-IDX)
                               TO  TAX-PERIODS OF TAXWK
                       MOVE TAX-METHOD OF TXARY(OTH-TXARY-IDX)
                               TO  TAX-METHOD OF TAXWK
                       MOVE BENEFIT-RCD-NO OF TXARY(OTH-TXARY-IDX)
                               TO  BENEFIT-RCD-NO OF TAXWK
                       MOVE FICA-STATUS-EE OF TXARY(OTH-TXARY-IDX)
                               TO FICA-STATUS-EE OF TAX-SET OF TAXWK
                       IF PUBLIC-SECTOR-YES OF PSLCT                    HP00001
                                                                        HP00001
                           MOVE PAID-PRDS-PER-YEAR                      HP00001
                                   OF TXARY(OTH-TXARY-IDX)              HP00001
                                       TO  PAID-PRDS-PER-YEAR OF TAXWK  HP00001
                       END-IF                                           HP00001
                       MOVE SPACE  TO  SUT-JURISDICTION
                               OF TAX-SET OF TAXWK

                       PERFORM LH221-ACCUM-LOC-RES-TGC
                   END-IF
               END-PERFORM

               PERFORM LH225-ACCUM-ADJUSTMENTS

               IF FWT OF GRSWK(2)  NOT =  ZERO
                       OR LOC-ADJ OF GRSWK(2 LCLWK-IDX)
                                       NOT = ZERO
                           OR LOC-WH-ADJ OF WHADJ(2 LCLWH-IDX)
                                           NOT =  ZERO

                   SET CALC-LOC-RES OF TAXWK  TO  TRUE
                   SET PASS-RESWK-IDX OF TAXWK  TO  RESWK-IDX
                   PERFORM LD050-CALCULATE-TAX
                   SET CAL-TXARY-IDX OF W-WK TO TXARY-IDX
                   SET RES-LCLCALC-YES OF W-WK TO TRUE

                   IF STATE OF TXARY(TXARY-IDX) = 'PA'
                      AND LOCALITY OF TXARY(TXARY-IDX) NOT EQUAL SPACE
                      AND LOCALITY OF TXARY(TXARY-IDX)
                          NOT EQUAL '510101'
                      AND  TAX-CLASS-WITHHOLDING OF TAX-SET-DATA
                                  OF TXARY(TXARY-IDX)

                      MOVE LOCALITY OF TXARY(TXARY-IDX)
                                 TO PA-RES-LOCALITY OF W-WK
                   END-IF
               END-IF

               PERFORM LH500-LOCAL-ADJ-RECIP
               PERFORM LH555-GET-LOCAL-RES-RATE

               SET TARRY-IDX TO 1
               SEARCH TAXCALC-DATA OF TARRY

                   WHEN TARRY-IDX > TAXCALC-COUNT OF TARRY

                       CONTINUE

                   WHEN STATE OF TARRY(TARRY-IDX) = 'PA'
                        AND LOCALITY OF TARRY(TARRY-IDX) NOT = SPACE
                        AND LOCALITY OF TARRY(TARRY-IDX) NOT = '510101'
                        AND RESIDENT-YES OF TARRY(TARRY-IDX)
                        AND TAX-CLASS-WITHHOLDING OF TARRY(TARRY-IDX)
                        AND LOCALITY OF TARRY(TARRY-IDX)
                            = PA-RES-LOCALITY OF W-WK
                        AND WORK-PSD-CD OF TARRY(TARRY-IDX)
                            = PA-RES-LOCALITY OF W-WK
                        AND WK-LCLPCT-RES OF W-WK EQUAL ZERO
                        AND PA-RES-LWT OF GRSWK  NOT EQUAL ZERO
                        AND PA-LOC-RES-TXGRS OF GRSWK NOT EQUAL ZERO

                        MOVE PA-LOC-RES-TXGRS OF GRSWK
                             TO TXGRS-CUR OF TARRY(TARRY-IDX)
               END-SEARCH

               MOVE ZERO TO PA-LOC-RES-TXGRS OF GRSWK

               PERFORM VARYING RESFLAG-IDX FROM  1  BY  1
                   UNTIL RESFLAG-IDX  > TAXCALC-COUNT OF TARRY

                   IF TAX-ONE-TIME OF TARRY(RESFLAG-IDX) < ZERO
                       AND TAX-CLASS-WITHHOLDING
                           OF TARRY(RESFLAG-IDX)
                       AND LOCALITY OF TARRY(RESFLAG-IDX)
                           = LOCALITY OF RESIDENCE
                             OF TAXWK(RESWK-IDX)

                           SET RESIDENT-YES OF TARRY(RESFLAG-IDX)
                               TO TRUE
                   END-IF
              END-PERFORM

              SET TARRY-IDX TO 1
              SEARCH TAXCALC-DATA OF TARRY

                   WHEN TARRY-IDX > TAXCALC-COUNT OF TARRY

                       CONTINUE

                   WHEN STATE OF TARRY(TARRY-IDX) = 'PA'
                        AND LOCALITY OF TARRY(TARRY-IDX) NOT = SPACE
                        AND LOCALITY OF TARRY(TARRY-IDX) NOT = '510101'
                        AND RESIDENT-YES OF TARRY(TARRY-IDX)
                        AND TAX-CLASS-WITHHOLDING OF TARRY(TARRY-IDX)
                        AND LOCALITY OF TARRY(TARRY-IDX)
                            = PA-RES-LOCALITY OF W-WK
                        AND WORK-PSD-CD OF TARRY(TARRY-IDX)
                            = PA-RES-LOCALITY OF W-WK
                        AND RES-PSD-CD OF TARRY(TARRY-IDX)
                            = PA-RES-LOCALITY OF W-WK
                        AND PA-BLANK-FOUND-NO OF W-SW
                        AND PA-RES-ERN-FOUND-NO OF W-SW
                        AND NON-PA-ERN-FOUND-NO OF W-SW
                        AND PHIL-LOCAL-FOUND-YES OF W-SW

                        MOVE ZERO TO TXGRS-CUR OF TARRY(TARRY-IDX)
                        MOVE ZERO TO TAX-CUR OF TARRY(TARRY-IDX)
              END-SEARCH

              SET TARRY-IDX TO 1
              SEARCH TAXCALC-DATA OF TARRY

                   WHEN TARRY-IDX > TAXCALC-COUNT OF TARRY

                       CONTINUE

                   WHEN STATE OF TARRY(TARRY-IDX) = 'PA'
                        AND LOCALITY OF TARRY(TARRY-IDX) NOT = SPACE
                        AND LOCALITY OF TARRY(TARRY-IDX) NOT = '510101'
                        AND RESIDENT-YES OF TARRY(TARRY-IDX)
                        AND TAX-CLASS-WITHHOLDING OF TARRY(TARRY-IDX)
                        AND LOCALITY OF TARRY(TARRY-IDX)
                            = PA-RES-LOCALITY OF W-WK
                        AND WORK-PSD-CD OF TARRY(TARRY-IDX) = '880000'
                        AND RES-PSD-CD OF TARRY(TARRY-IDX)
                            = PA-RES-LOCALITY OF W-WK
                        AND PA-BLANK-FOUND-NO OF W-SW
                        AND PA-RES-ERN-FOUND-NO OF W-SW
                        AND NON-PA-ERN-FOUND-NO OF W-SW

                        MOVE ZERO TO TXGRS-CUR OF TARRY(TARRY-IDX)
                        MOVE ZERO TO TAX-CUR OF TARRY(TARRY-IDX)
              END-SEARCH

           END-PERFORM

            .
       RES-LOCAL-GROSS-EXIT.


      /*****************************************************************
      *                                                                *
       LH221-ACCUM-LOC-RES-TGC SECTION.
       LH221.
      *                                                                *
      /*****************************************************************

           MOVE ZERO  TO  LOC-ADJ OF GRSWK(2 LCLWK-IDX)
           MOVE ZERO  TO  LOC-WH-ADJ OF WHADJ(2 LCLWH-IDX)

           PERFORM VARYING TXARY-IDX FROM 1 BY 1 UNTIL
                   TXARY-IDX > TAX-SET-CNT OF TXARY

               MOVE TAX-METHOD OF TXARY(TXARY-IDX) TO
                    TAX-METHOD OF W-WK
               IF TAX-METHOD OF TXARY(TXARY-IDX) = 'X'
                  MOVE 'S' TO TAX-METHOD OF W-WK
               END-IF

               IF STATE OF TAX-SET OF TXARY (TXARY-IDX) NOT =  '$U'
                  AND TAX-CLASS-WITHHOLDING OF TXARY(TXARY-IDX)
                  AND PAY-FREQ-TYPE OF TAX-SET OF TAXWK
                    = PAY-FREQ-TYPE OF TXARY(TXARY-IDX)
                  AND TAX-PERIODS OF TAX-SET OF TAXWK
                    = TAX-PERIODS OF TXARY(TXARY-IDX)
                  AND TAX-METHOD OF TAX-SET OF TAXWK
                    = TAX-METHOD OF W-WK
                  AND BENEFIT-RCD-NO OF TAXWK
                    = BENEFIT-RCD-NO OF TXARY(TXARY-IDX)
                  AND (PAID-PRDS-PER-YEAR OF TAX-SET OF TAXWK
                    =  PAID-PRDS-PER-YEAR OF TXARY(TXARY-IDX)
                  AND FICA-STATUS-EE OF TAX-SET OF TAXWK
                    =  FICA-STATUS-EE OF TXARY(TXARY-IDX))

                  IF LOCALITY OF TXARY(TXARY-IDX) = SPACES
                     ADD RES-TGC-ADJ-LOC OF
                         TXARY(TXARY-IDX LCL-RCP-IDX) TO
                         LOC-ADJ OF GRSWK(2 LCLWK-IDX)
                     ADD RES-TGC-WH-ADJ-LOC OF
                         TXARY(TXARY-IDX LCL-RCP-IDX) TO
                         LOC-WH-ADJ OF WHADJ(2 LCLWH-IDX)
                  END-IF

               END-IF

           END-PERFORM

           .
       ACCUM-LOC-RES-TGC-EXIT.


      /*****************************************************************
      *                                                                *
       LH225-ACCUM-ADJUSTMENTS SECTION.
       LH225.
      *                                                                *
      ******************************************************************

           PERFORM VARYING TXARY-IDX FROM 1  BY  1
                   UNTIL TXARY-IDX  >  TAX-SET-CNT

               MOVE TAX-METHOD OF TXARY(TXARY-IDX) TO
                    TAX-METHOD OF W-WK
               IF TAX-METHOD OF TXARY(TXARY-IDX) = 'X'
                  MOVE 'S' TO TAX-METHOD OF W-WK
               END-IF

               IF PAY-FREQ-TYPE OF TAX-SET OF TAXWK
                       =  PAY-FREQ-TYPE OF TXARY(TXARY-IDX)
                       AND TAX-PERIODS OF TAX-SET OF TAXWK
                       =  TAX-PERIODS OF TXARY(TXARY-IDX)
                       AND TAX-METHOD OF TAX-SET OF TAXWK
                       =  TAX-METHOD OF W-WK
                       AND BENEFIT-RCD-NO OF TAXWK
                       = BENEFIT-RCD-NO OF TXARY(TXARY-IDX)
                       AND (PAID-PRDS-PER-YEAR OF TAX-SET OF TAXWK      HP00001
                       =  PAID-PRDS-PER-YEAR OF TXARY(TXARY-IDX)        HP99999
                       AND FICA-STATUS-EE OF TAX-SET OF TAXWK           HP99999
                       =  FICA-STATUS-EE OF TXARY(TXARY-IDX))           HP99999
                       AND TAX-CLASS-WITHHOLDING OF TXARY(TXARY-IDX)
                       AND STATE OF TXARY (TXARY-IDX) NOT =  '$U'
                       AND RESIDENT-TAX-NO OF TXARY(TXARY-IDX)

                   SET TXARY-IDX-PASS OF TXARY  TO  TXARY-IDX
                   PERFORM LH250-LOCAL-RECIPROCITY
               END-IF
           END-PERFORM

           .
       ACCUM-ADJUSTMENTS-EXIT.


      /*****************************************************************
      *                                                                *
       LH250-LOCAL-RECIPROCITY SECTION.
       LH250.
      *                                                                *
      ******************************************************************

           IF STATE OF TAX-SET OF TXARY(TXARY-IDX)
                   NOT =  '$U'
                   AND TAX-CLASS-WITHHOLDING OF TXARY(TXARY-IDX)
                   AND RESIDENT-TAX-NO OF TXARY(TXARY-IDX)

               SET LCLWK-PASS OF L-PASS  TO  LCLWK-IDX
               SET LCLWH-PASS OF L-PASS  TO  LCLWH-IDX
               SET LRCWK-PASS OF L-PASS  TO  LRCWK-IDX
               SET LCL-RCP-PASS OF L-PASS  TO  LCL-RCP-IDX
               SET TARRY-PASS OF L-PASS  TO  TARRY-IDX

               CALL 'PSPTCLRC'  USING GRSWK
                                      WHADJ
                                      TXARY
                                      TARRY
                                      L-PASS
           END-IF

           .
       LOCAL-RECIPROCITY-EXIT.

      /*****************************************************************
      *                                                                *
       LH300-LOCAL-RECIP-FOR-MD-RES SECTION.
       LH300.
      *                                                                *
      ******************************************************************

      *  CHECK FOR RECIPROCITY BETWEEN A RESIDENT MD-LOCALITY AND A
      *  WORK LOCALITY.  NOTE THAT MD RESIDENTS ARE NOT TAXED DIRECTLY
      *  BY THEIR LOCALITIES.

           PERFORM VARYING RESWK-IDX FROM  1  BY  1
                    UNTIL RESWK-IDX
                    >  LOCALITY-COUNT OF RESIDENCE OF TAXWK

               SET LRCWK-IDX  TO  RESWK-IDX
               PERFORM LH500-LOCAL-ADJ-RECIP

           END-PERFORM

            .
       LOCAL-RECIP-FOR-MD-RES-EXIT.

      /*****************************************************************
      *                                                                *
       LH400-STATE-ADJ-RECIP SECTION.
       LH400.
      *                                                                *
      ******************************************************************


           SET TXARY-IDX  TO  TAX-SET-CNT   OF TXARY

           IF TAX-SET-CNT OF TXARY > ZERO

               IF TAX-METHOD OF TXARY(TXARY-IDX) =
                    TAX-METHOD-SAV OF W-WK
                   AND STATE-RECIP-WITH-LOCAL-YES OF GRSWK

                   SET LOCAL-RCP-TO-BE-TAKEN-YES OF W-SW TO TRUE

               END-IF

               IF  (TAX-METHOD-SUPPLEMENTAL OF TXARY(TXARY-IDX)
                   AND TAX-METHOD-SAV-SUPP OF W-WK
                   AND TAX-METHOD OF TXARY(TXARY-IDX) NOT EQUAL
                                     TAX-METHOD-SAV OF W-WK  )
                   OR
                   (TAX-METHOD OF TXARY(TXARY-IDX) = 'X'
                    AND TAX-METHOD OF TXARY(TXARY-IDX) NOT EQUAL
                                     TAX-METHOD-SAV OF W-WK
                    AND STATE OF TXARY(TXARY-IDX) = 'NJ' )

                   IF TAX-METHOD OF TXARY(TXARY-IDX) = 'X'
                    AND TAX-METHOD OF TXARY(TXARY-IDX) NOT EQUAL
                                     TAX-METHOD-SAV OF W-WK
                    AND STATE OF TXARY(TXARY-IDX) = 'NJ'


                    IF TAX-METHOD-SAV OF W-WK = 'S'
                       OR SEPCHK OF CHECK = ZERO
                      SET MULTIPLE-SUPPL-FOUND-YES OF W-SW TO TRUE
                    ELSE
                      SET MULTIPLE-SUPPL-FOUND-NO OF W-SW TO TRUE
                    END-IF

                   ELSE

                     SET MULTIPLE-SUPPL-FOUND-YES OF W-SW TO TRUE

                   END-IF

                   SET STATE-RCP-TAKEN-NO OF W-SW TO TRUE
                   SET FIRST-TAX-METHOD-FOUND-NO OF W-SW TO TRUE
                   MOVE TAX-METHOD OF TXARY(TXARY-IDX) TO
                                     TAX-METHOD-SAV OF W-WK

               END-IF

               IF  TAX-METHOD-SUPPLEMENTAL OF TXARY(TXARY-IDX)

                   MOVE TAX-METHOD OF TXARY(TXARY-IDX)
                                   TO TAX-METHOD-SAV OF W-WK
               END-IF

               IF  STATE-RECIP-WITH-LOCAL-YES OF GRSWK
                   AND LOCAL-RCP-TO-BE-TAKEN-YES OF W-SW
                   AND STATE-RCP-TAKEN-YES OF W-SW

                   SET STATE-RCP-TAKEN-NO OF W-SW TO TRUE
                   IF  NBR-OF-STATES OF GRSWK > 1

                       MOVE ZERO TO TAX-SUPP-PRIOR-WORK OF GRSWK
                       MOVE ZERO TO TAX-SUPP-REG-AGG OF GRSWK
                       MOVE ZERO TO TAX-SUPP-TAX-WITHHELD OF GRSWK

                   END-IF
               END-IF

               PERFORM LH550-FIND-TARRY

               IF TAX-CUR OF TARRY(TARRY-IDX) < ZERO
                  IF STATE OF TARRY(TARRY-IDX) = 'PA'
                     AND LOCALITY OF TARRY(TARRY-IDX) = SPACE
                     AND TAX-AGG OF TARRY(TARRY-IDX) < ZERO
                     AND TXGRS-AGG OF TARRY(TARRY-IDX) < ZERO

                     MOVE ZERO TO TAX-AGG OF TARRY(TARRY-IDX)
                     MOVE ZERO TO TXGRS-AGG OF TARRY(TARRY-IDX)
                  ELSE

                     MOVE TAX-CUR OF TARRY(TARRY-IDX)
                                TO TAX-CUR-SAV OF W-WK
                  END-IF
               END-IF

               IF  TAX OF STATE-RCP OF GRSWK > ZERO
      *        Port Authority Change
      *        Work around for 15B.
      *            AND STATE-STATE OF TXARY(TXARY-IDX) = SPACE
      *             AND LOCAL-STATE OF TXARY(TXARY-IDX) = SPACE
      *             AND STATE-RECIP-WITH-LOCAL-YES OF GRSWK
      *             AND LOCAL-RCP-TO-BE-TAKEN-YES OF W-SW
      *             AND STATE-RCP-TAKEN-NO OF W-SW
      *        End Port Authority Change

                   IF  (TAX-CUR OF TARRY(TARRY-IDX)
                           + TAX-SUPP-REG-AGG OF GRSWK
                           + TAX-SUPP-TAX-WITHHELD OF GRSWK)
                           > (TAX OF STATE-RCP OF GRSWK
                              + TAX-SUPP-PRIOR-WORK OF GRSWK)

                       IF (TAX-SUPP-REG-AGG OF GRSWK
                            + TAX-SUPP-PRIOR-RES OF GRSWK
                            < TAX-SUPP-PRIOR-WORK)

  
      *        Port Authority Change   
                     
                          COMPUTE ACTUAL-TAX-CUR OF TXARY(TXARY-IDX)
                                =  (ACTUAL-TAX-CUR OF TXARY(TXARY-IDX)
                                -   TAX OF STATE-RCP OF GRSWK)
                                +  HIGH-PREC-0

                          IF ACTUAL-TAX-CUR OF TXARY(TXARY-IDX) < 0 
                             MOVE 0 TO
                                ACTUAL-TAX-CUR OF TXARY(TXARY-IDX)
                          END-IF


                          COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                                = (TAX-CUR OF TARRY(TARRY-IDX)
                                -  TAX OF STATE-RCP OF GRSWK)
                                +  HIGH-PREC-0

      *        End Port Authority Change 
 
                       ELSE


                           COMPUTE ACTUAL-TAX-CUR OF TXARY(TXARY-IDX)
                                =  (ACTUAL-TAX-CUR OF TXARY(TXARY-IDX)
                                -   TAX OF STATE-RCP OF GRSWK)
                                +  HIGH-PREC-0

                           COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                                = (TAX-CUR OF TARRY(TARRY-IDX)
                                -  TAX OF STATE-RCP OF GRSWK)
                                +  HIGH-PREC-0
                       END-IF

                       SET STATE-RCP-TAKEN-YES OF W-SW TO TRUE
      *        Port Authority Change
                       IF TAX-CUR OF TARRY(TARRY-IDX) < 0

                          MOVE 0 TO TAX-CUR OF TARRY(TARRY-IDX)
                          MOVE ADDL-TAX OF TXARY(TXARY-IDX)
                                 TO TAX-CUR OF TARRY(TARRY-IDX)
                       ELSE
                          IF TAX-CUR OF TARRY(TARRY-IDX) < 
                             ADDL-TAX OF TXARY(TXARY-IDX) OR 
                             TAX-CUR OF TARRY(TARRY-IDX) = 0

                             MOVE ADDL-TAX OF TXARY(TXARY-IDX)
                                TO TAX-CUR OF TARRY(TARRY-IDX)
                          END-IF
                       END-IF
      *        End Port Authority Change
                   ELSE

                       COMPUTE ACTUAL-TAX-CUR OF TXARY(TXARY-IDX)
                            =  (ACTUAL-TAX-CUR OF TXARY(TXARY-IDX)
                            -  ACTUAL-TAX-CUR OF TXARY(TXARY-IDX)
                            +  TAX-CUR-ADDL-AMT OF TARRY(TARRY-IDX))
                            +  HIGH-PREC-0
      *        Port Authority Change

                       IF ACTUAL-TAX-CUR OF TXARY(TXARY-IDX)
                          < 0
                          MOVE 0 TO
                              ACTUAL-TAX-CUR OF TXARY(TXARY-IDX)
                       END-IF
      *        End Port Authority Change

                       COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                            =  (TAX-CUR OF TARRY(TARRY-IDX)
                            -  TAX-CUR OF TARRY(TARRY-IDX)
                            +  TAX-CUR-ADDL-AMT OF TARRY(TARRY-IDX))
                            +  HIGH-PREC-0

                       SET STATE-RCP-TAKEN-YES OF W-SW TO TRUE
      *        Port Authority Change
                       IF TAX-CUR OF TARRY(TARRY-IDX) < 0

                          MOVE 0 TO TAX-CUR OF TARRY(TARRY-IDX)
                          MOVE ADDL-TAX OF TXARY(TXARY-IDX)
                                 TO TAX-CUR OF TARRY(TARRY-IDX)
                       ELSE
                          IF TAX-CUR OF TARRY(TARRY-IDX) < 
                             ADDL-TAX OF TXARY(TXARY-IDX) OR 
                             TAX-CUR OF TARRY(TARRY-IDX) = 0

                             MOVE ADDL-TAX OF TXARY(TXARY-IDX)
                                TO TAX-CUR OF TARRY(TARRY-IDX)
                          END-IF
                       END-IF
      *        End Port Authority Change
  
             ELSE

                   IF NBR-OF-STATES OF GRSWK = ZERO

                       CONTINUE
                   ELSE

                       IF  STATE OF TXARY(TXARY-IDX) = 'CT'
                           AND TAX OF STATE-RCP OF GRSWK > ZERO

                           PERFORM LH401-CT-ADDL-TAX
                       END-IF

                       IF NBR-OF-STATES OF GRSWK < 2
                           AND STATE-RCP-TAKEN-NO OF W-SW
                           AND TAX-CUR OF TARRY(TARRY-IDX) > ZERO
                           AND MULTIPLE-SUPPL-FOUND-NO OF W-SW


                           COMPUTE PRIOR-TAX-SUPP-CUR OF W-WK
                                =  PRIOR-TAX-SUPP-CUR OF W-WK
                                +  TAX-SUPP-CUR OF TARRY(TARRY-IDX)
                           MOVE ZERO TO TAX-SUPP-CUR OF TARRY(TARRY-IDX)
                       END-IF

                       IF TAX-METHOD-SUPPLEMENTAL OF TAX-SET OF TAXWK

                           IF TAX-SUPP-CUR OF TARRY(TARRY-IDX) = ZERO
                               OR MULTIPLE-SUPPL-FOUND-YES OF W-SW

                               IF  TAX-AGG OF TARRY(TARRY-IDX) = ZERO

                                   PERFORM
                                        LH402-TAX-SUPP-CUR-CALC-NO-AGG
                               ELSE

                                   PERFORM LH404-TAX-SUPP-CUR-CALC-AGG

                               END-IF
                           END-IF
                       END-IF

                       IF MULTI-TAX-METHOD-YES OF TAXWK
                          AND WK-RECIPROCITY-RULE-REDRES OF W-WK
                          AND TAX-METHOD OF TXARY(TXARY-IDX) = 'X'
                          AND MULTIPLE-SUPPL-FOUND-YES OF W-SW
                          AND CALCED-TAX-CUR OF TXARY(TXARY-IDX)
                              NOT EQUAL TAX-CUR OF TARRY(TARRY-IDX)

                            MOVE TAX-SUPP-PRIOR-WORK OF GRSWK
                                     TO TAX-SUP-PRIOR-WRK OF W-WK
                            MOVE ZERO TO WK-REG-RECIP-TAX OF W-WK
                            MOVE ZERO TO PRIOR-TAX-SUPP-CUR OF W-WK
                            MOVE ZERO TO TAX-SUPP-TAX-WITHHELD OF GRSWK
                            MOVE ZERO TO TAX-SUPP-PRIOR-WORK OF GRSWK

                           IF TAX-SUP-PRIOR-WRK OF W-WK > ZERO

                                  COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                                        = TAX-CUR OF TARRY(TARRY-IDX)
                                        - TAX-STATE-RCP-ANN OF W-WK
                                        - TAX-STATE-RCP-SUP OF W-WK
                                        + TAX-AGG OF TARRY(TARRY-IDX)
                           END-IF
                       END-IF

                       IF STATE OF TARRY(TARRY-IDX) = 'MO'
                          AND SPECIAL-SWT-STATUS-MAINTAIN 
                                         OF TARRY(TARRY-IDX) 
                          AND GRS OF STATE-RCP OF GRSWK > ZERO

                          COMPUTE TXGRS-CUR OF TARRY(TARRY-IDX)
                                = TXGRS-CUR OF TARRY(TARRY-IDX)
                                - GRS OF STATE-RCP OF GRSWK
                       END-IF

                       IF  TAX OF STATE-RCP OF GRSWK  >  ZERO
                              OR GRS OF STATE-RCP OF GRSWK  >  ZERO
                              OR FACTOR-TXGRS-CUR OF W-WK > ZERO

                           IF SPECIAL-SWT-STATUS-NONE
                                OF TARRY(TARRY-IDX)
                               AND NOT ONE-TIME-OVERRIDE
                                               OF TARRY(TARRY-IDX)

                               PERFORM LH410-STATE-RECIP

                           END-IF

                       ELSE

                           IF (RECIPROCITY-RULE-REDTTL-ALL
                                   OF STATE-STATE OF TXARY(TXARY-IDX)
                               OR RECIPROCITY-RULE-REDTTL OF LOCAL-STATE
                                          OF TXARY(TXARY-IDX)
                               AND NBR-OF-STATES OF GRSWK > 1)

                                   CONTINUE

                           ELSE

                               IF STATE-RCP-TAKEN-NO OF W-SW
                                   AND SPECIAL-SWT-STATUS-NONE
                                                 OF TARRY(TARRY-IDX)
                                   AND (NOT SUPPL-METHOD-USED-NOANN
                                                 OF GRSWK
                                   OR NOT SUPPL-METHOD-USED-BOTH
                                          OF GRSWK)

                                   PERFORM LH415-FURTHER-RECIP-CALC
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF

           IF  TAX-CUR-SAV OF W-WK < ZERO
               AND TAX-CUR OF TARRY(TARRY-IDX) > ZERO
               AND MULTIPLE-SUPPL-FOUND-NO OF W-SW

               IF TXGRS-AGG-THIS-CHK-YES OF GRSWK
                  AND SUPPL-METHOD-USED-AGG OF GRSWK

                     CONTINUE
               ELSE

               COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                    =  TAX-CUR OF TARRY(TARRY-IDX)
                    +  TAX-CUR-SAV OF W-WK
                    +  TAX-SUPP-PRIOR-RES OF GRSWK

               MOVE ZERO TO TAX-CUR-SAV OF W-WK
             END-IF
           END-IF

           IF TAX OF STATE-RCP OF GRSWK > ZERO
               AND MULTIPLE-SUPPL-FOUND-NO OF W-SW

                    COMPUTE PRIOR-RECIPROCITY OF W-WK
                         =  PRIOR-RECIPROCITY OF W-WK
                         +  TAX OF STATE-RCP OF GRSWK

           END-IF

            .
       STATE-ADJ-RECIP-EXIT.


      /*****************************************************************
      *                                                                *
       LH401-CT-ADDL-TAX SECTION.
       LH401.
      *                                                                *
      ******************************************************************


           IF ADDL-TAX OF TXARY(TXARY-IDX) NOT EQUAL ZERO

             IF SPECIAL-SWT-STATUS-MAINTAIN OF TARRY(TARRY-IDX)
                AND TAX-CUR OF TARRY(TARRY-IDX)
                    = ADDL-TAX OF TXARY(TXARY-IDX)

                     CONTINUE
             ELSE
               COMPUTE ACTUAL-TAX-CUR OF TXARY(TXARY-IDX)
                    =  ACTUAL-TAX-CUR OF TXARY(TXARY-IDX)
                    +  ADDL-TAX OF TXARY(TXARY-IDX)

               COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                    =  TAX-CUR OF TARRY(TARRY-IDX)
                    +  ADDL-TAX OF TXARY(TXARY-IDX)

               IF  TAX-AGG OF TARRY(TARRY-IDX) > ZERO

                   COMPUTE TAX-AGG OF TARRY(TARRY-IDX)
                        =  TAX-AGG OF TARRY(TARRY-IDX)
                        +  ADDL-TAX OF TXARY(TXARY-IDX)

                   IF  TAX-AGG OF TARRY(TARRY-IDX) < ZERO

                       MOVE ZERO TO TAX-AGG OF TARRY(TARRY-IDX)
                   END-IF
               END-IF
             END-IF
           ELSE

               IF  FLAT-AMT OF GRSWK NOT EQUAL ZERO

                   COMPUTE ACTUAL-TAX-CUR OF TXARY(TXARY-IDX)
                        =  ACTUAL-TAX-CUR OF TXARY(TXARY-IDX)
                        +  FLAT-AMT OF GRSWK

                   COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                        =  TAX-CUR OF TARRY(TARRY-IDX)
                        +  FLAT-AMT OF GRSWK

               END-IF
           END-IF

           .

       CT-ADDL-TAX-EXIT.



      /*****************************************************************
      *                                                                *
       LH402-TAX-SUPP-CUR-CALC-NO-AGG SECTION.
       LH402.
      *                                                                *
      ******************************************************************


           IF  SUPPL-METHOD-USED-AGG OF GRSWK

               IF TXGRS-PRIOR-SUPP-WORK OF GRSWK > ZERO
                  AND MULTIPLE-SUPPL-FOUND-NO OF W-SW

                   COMPUTE TAX-SUPP-CUR OF TARRY(TARRY-IDX)
                        = (TAX-SUPP-CUR OF TARRY(TARRY-IDX)
                        +  TAX-CUR OF TARRY(TARRY-IDX)
                        -  TAX-SUPP-PRIOR-RES OF GRSWK
                        -  TAX-CUR-ADDL-AMT OF TARRY(TARRY-IDX))
                        +  HIGH-PREC-0

               ELSE

                   IF  (TAX OF STATE-RCP OF GRSWK > ZERO
                       OR WK-RECIPROCITY-RULE-NOWORK-ALL OF W-WK
                       OR WK-RECIPROCITY-RULE OF W-WK = SPACE )
                       AND MULTIPLE-SUPPL-FOUND-NO OF W-SW

                       COMPUTE TAX-SUPP-CUR OF TARRY(TARRY-IDX)
                            = (TAX-SUPP-CUR OF TARRY(TARRY-IDX)
                            +  TAX-CUR OF TARRY(TARRY-IDX)
                            -  TAX-SUPP-PRIOR-RES OF GRSWK
                            -  TAX-CUR-ADDL-AMT OF TARRY(TARRY-IDX))
                            +  HIGH-PREC-0

                   ELSE
                       IF MULTIPLE-SUPPL-FOUND-NO OF W-SW

                           COMPUTE TAX-SUPP-CUR OF TARRY(TARRY-IDX)
                                =  (TAX-SUPP-CUR OF TARRY(TARRY-IDX)
                                +  TAX-CUR OF TARRY(TARRY-IDX))
                                +  HIGH-PREC-0

                       ELSE

                          IF TAX-METHOD-SAV-SUPP OF W-WK
                             AND TAX-METHOD-SAV OF W-WK NOT = 'X'

                           COMPUTE TAX-SUPP-CUR OF TARRY(TARRY-IDX)
                                =  (TAX-SUPP-CUR OF TARRY(TARRY-IDX)
                                +  TAX-CUR OF TARRY(TARRY-IDX))
                                +  HIGH-PREC-0

                          ELSE

                              IF TAX-METHOD-SAV-SUPP OF W-WK
                                 AND SEPCHK OF CHECK = ZERO
                                 AND STATE OF TXARY(TXARY-IDX) = 'NJ'

                                 CONTINUE

                              ELSE
                                COMPUTE TAX-SUPP-CUR OF TARRY(TARRY-IDX)
                                =  (TAX-SUPP-CUR OF TARRY(TARRY-IDX)
                                -  TAX-SUPP-CUR OF TARRY(TARRY-IDX)
                                +  TAX-CUR OF TARRY(TARRY-IDX)
                                +  TAX-SUPP-PRIOR-WORK OF GRSWK
                                -  TAX-SUPP-PRIOR-RES OF GRSWK)
                                +  HIGH-PREC-0
                              END-IF

                          END-IF
                       END-IF
                   END-IF
               END-IF
           ELSE

               IF MULTIPLE-SUPPL-FOUND-NO OF W-SW
                   OR TAX OF STATE-RCP OF GRSWK > ZERO

                   IF  PRIOR-RECIPROCITY OF W-WK > 0
                       AND TAX-SUPP-CUR OF TARRY(TARRY-IDX) <
                           PRIOR-RECIPROCITY OF W-WK

                       COMPUTE TAX-SUPP-CUR OF TARRY(TARRY-IDX)
                            = (TAX-SUPP-CUR OF TARRY(TARRY-IDX)
                            +  TAX-CUR OF TARRY(TARRY-IDX)
                            -  TAX-CUR-ADDL-AMT OF TARRY(TARRY-IDX))
                            +  HIGH-PREC-0
                   ELSE

                        COMPUTE TAX-SUPP-CUR OF TARRY(TARRY-IDX)
                             = (TAX-SUPP-CUR OF TARRY(TARRY-IDX)
                             -  TAX-SUPP-CUR OF TARRY(TARRY-IDX)
                             +  TAX-CUR OF TARRY(TARRY-IDX)
                             +  PRIOR-RECIPROCITY OF W-WK
                             -  TAX-CUR-ADDL-AMT OF TARRY(TARRY-IDX))
                             +  HIGH-PREC-0
                   END-IF

                ELSE
                   COMPUTE TAX-SUPP-CUR OF TARRY(TARRY-IDX)
                        = (TAX-SUPP-CUR OF TARRY(TARRY-IDX)
                        -  TAX-SUPP-CUR OF TARRY(TARRY-IDX)
                        +  TAX-CUR OF TARRY(TARRY-IDX)
                        -  TAX-CUR-ADDL-AMT OF TARRY(TARRY-IDX))
                        +  HIGH-PREC-0
                END-IF
           END-IF


           .

       TAX-SUPP-CUR-CALC-NO-AGG-EXIT.


      /*****************************************************************
      *                                                                *
       LH404-TAX-SUPP-CUR-CALC-AGG SECTION.
       LH404.
      *                                                                *
      ******************************************************************

           IF  MULTIPLE-SUPPL-FOUND-YES OF W-SW

               COMPUTE PRIOR-TAX-SUPP-CUR OF W-WK
                    =  PRIOR-TAX-SUPP-CUR OF W-WK
                    +  TAX-SUPP-CUR OF TARRY(TARRY-IDX)
           END-IF

           IF  SUPPL-METHOD-USED-AGG OF GRSWK

               IF  MULTIPLE-SUPPL-FOUND-YES OF W-SW
                   OR TAX-AGG OF TARRY(TARRY-IDX) <
                      WK-REG-RECIP-TAX OF W-WK

                   IF  TAX-SUPP-CUR OF TARRY(TARRY-IDX)
                       - (TAX-CUR OF TARRY(TARRY-IDX)
                       -  TAX-CUR-ADDL-AMT OF TARRY(TARRY-IDX)
                       +  HIGH-PREC-0)
                       =  ZERO

                       CONTINUE
                   ELSE
                       IF  MULTIPLE-SUPPL-FOUND-YES OF W-SW

                           IF  TAX OF STATE-RCP OF GRSWK > ZERO

                             IF TAX-METHOD-SAV OF W-WK NOT = 'X'

                                    COMPUTE TAX-SUPP-CUR
                                                    OF TARRY(TARRY-IDX)
                                    = (TAX-SUPP-CUR OF TARRY(TARRY-IDX)
                                    +  TAX-CUR OF TARRY(TARRY-IDX))
                                    +  HIGH-PREC-0

                             ELSE
                                 IF (NBR-OF-STATES OF GRSWK > 2
                                       AND SEPCHK OF CHECK = ZERO)
                                    OR TAX-METHOD-SAV-SUPP OF W-WK

                                    COMPUTE TAX-SUPP-CUR
                                                    OF TARRY(TARRY-IDX)
                                    = (TAX-SUPP-CUR OF TARRY(TARRY-IDX)
                                    +  TAX-CUR OF TARRY(TARRY-IDX))
                                    +  HIGH-PREC-0

                                 END-IF
                             END-IF
                           ELSE

                               COMPUTE TAX-SUPP-CUR OF TARRY(TARRY-IDX)
                                    = (TAX-SUPP-CUR OF TARRY(TARRY-IDX)
                                    -  TAX-SUPP-CUR OF TARRY(TARRY-IDX)
                                    +  TAX-CUR OF TARRY(TARRY-IDX)
                                    +  PRIOR-RECIPROCITY OF W-WK
                                    -  TAX-AGG OF TARRY(TARRY-IDX)
                                    -  TAX-CUR-ADDL-AMT
                                                   OF TARRY(TARRY-IDX))
                                    +  HIGH-PREC-0
                           END-IF

                       ELSE

                           IF TAX-METHOD-SAV-SUPP OF W-WK

                               COMPUTE TAX-SUPP-CUR OF TARRY(TARRY-IDX)
                                = (TAX-SUPP-CUR OF TARRY(TARRY-IDX)
                                +  TAX-CUR OF TARRY(TARRY-IDX)
                                +  PRIOR-TAX-SUPP-CUR OF W-WK
                                -  TAX-CUR-ADDL-AMT OF TARRY(TARRY-IDX))
                                +  HIGH-PREC-0

                           ELSE

                               COMPUTE TAX-SUPP-CUR OF TARRY(TARRY-IDX)
                                = (TAX-SUPP-CUR OF TARRY(TARRY-IDX)
                                +  TAX-CUR OF TARRY(TARRY-IDX)
                                -  TAX-CUR-ADDL-AMT OF TARRY(TARRY-IDX))
                                +  HIGH-PREC-0
                           END-IF
                       END-IF
                   END-IF
               ELSE

                   IF TAX-AGG OF TARRY(TARRY-IDX)
                                      =  PRIOR-RECIPROCITY OF W-WK

                       COMPUTE TAX-SUPP-CUR OF TARRY(TARRY-IDX)
                            = (TAX-SUPP-CUR OF TARRY(TARRY-IDX)
                            +  TAX-CUR OF TARRY(TARRY-IDX)
                            +  WK-REG-RECIP-TAX OF W-WK
                            -  TAX-CUR-ADDL-AMT OF TARRY(TARRY-IDX))
                            +  HIGH-PREC-0

                   ELSE

                       COMPUTE TAX-SUPP-CUR OF TARRY(TARRY-IDX)
                            = (TAX-SUPP-CUR OF TARRY(TARRY-IDX)
                            +  TAX-CUR OF TARRY(TARRY-IDX)
     .                      -  TAX-AGG OF TARRY(TARRY-IDX)
                            +  WK-REG-RECIP-TAX OF W-WK
                            -  TAX-CUR-ADDL-AMT OF TARRY(TARRY-IDX))
                            +  HIGH-PREC-0

                   END-IF
               END-IF

           ELSE

               COMPUTE TAX-SUPP-CUR OF TARRY(TARRY-IDX)
                    = (TAX-SUPP-CUR OF TARRY(TARRY-IDX)
                    +  TAX-CUR OF TARRY(TARRY-IDX)
                    +  WK-REG-RECIP-TAX OF W-WK
                    -  TAX-AGG OF TARRY(TARRY-IDX)
                    -  TAX-SUPP-PRIOR-RES OF GRSWK
                    -  TAX-CUR-ADDL-AMT OF TARRY(TARRY-IDX))
                    +  HIGH-PREC-0

           END-IF

           .

       TAX-SUPP-CUR-CALC-AGG-EXIT.

      /*****************************************************************
      *                                                                *
       LH410-STATE-RECIP SECTION.
       LH410.
      *                                                                *
      ******************************************************************


           IF TAX OF STATE-RCP OF GRSWK  >  ZERO

              COMPUTE ACTUAL-TAX-CUR OF TXARY(TXARY-IDX)
                   =  (ACTUAL-TAX-CUR OF TXARY(TXARY-IDX)
                   +  TAX-SUPP-REG-AGG OF GRSWK
                   -  TAX-SUPP-TAX-WITHHELD OF GRSWK
                   -  ADDL-TAX OF TXARY(TXARY-IDX)
                   -  TAX OF STATE-RCP OF GRSWK)
                   +  HIGH-PREC-0

               IF TAX-METHOD-ANNUALIZED OF TXARY(TXARY-IDX)

                  MOVE TAX OF STATE-RCP OF GRSWK
                          TO TAX-STATE-RCP-ANN OF W-WK
               END-IF

               IF TAX-METHOD OF TXARY(TXARY-IDX) = 'S'

                  MOVE TAX OF STATE-RCP OF GRSWK
                          TO TAX-STATE-RCP-SUP OF W-WK

                   IF TAX-CUR OF TARRY(TARRY-IDX)
                      > CALCED-TAX-CUR OF TXARY(TXARY-IDX)

                      MOVE TAX-AGG OF TARRY(TARRY-IDX)
                           TO TAX-STATE-RCP-ANN OF W-WK
                   END-IF
               END-IF
           ELSE

      *        RECIPROCITY RULE FACTOR

               IF GRS OF STATE-RCP OF GRSWK > ZERO

                   COMPUTE TXGRS-CUR OF TXARY(TXARY-IDX)
                        =  (TXGRS-CUR OF TXARY(TXARY-IDX)
                        -  GRS OF STATE-RCP OF GRSWK)
                        +  HIGH-PREC-0

                 IF (GRS OF STATE-RCP OF GRSWK
                        +  TXGRS-CUR OF TXARY(TXARY-IDX))
                      NOT EQUAL ZERO

                   COMPUTE ACTUAL-TAX-CUR OF TXARY(TXARY-IDX)
                        =  (TXGRS-CUR OF TXARY(TXARY-IDX)
                        /  (GRS OF STATE-RCP OF GRSWK
                        +  TXGRS-CUR OF TXARY(TXARY-IDX))
                        *  (CALCED-TAX-CUR OF TXARY(TXARY-IDX)
                        -  ADDL-TAX OF TXARY(TXARY-IDX)))
                        +  HIGH-PREC-0
                 END-IF
               END-IF
           END-IF

           IF TAX OF STATE-RCP OF GRSWK  >  ZERO

               IF (TAX-SUPP-PRIOR-WORK OF GRSWK > ZERO
                   OR TAX-SUPP-PRIOR-RES OF GRSWK > ZERO)

      *              EMPLID HAS PRIOR CHECKS IN THIS RUN

                   IF STATE-RCP-TAKEN-NO OF W-SW

                       IF NBR-OF-STATES OF GRSWK < 2

                           PERFORM LH412-ONE-STATE-RED-RES
                       ELSE

                           PERFORM LH414-MULT-STATE-RED-RES
                       END-IF

                       SET STATE-RCP-TAKEN-YES OF W-SW TO TRUE

                   END-IF

               ELSE
                   IF TAX-AGG OF TARRY(TARRY-IDX) =
                      TAX-CUR OF TARRY(TARRY-IDX)
                      OR NBR-OF-STATES OF GRSWK > 1

                       IF SUPPL-METHOD-USED-AGG OF GRSWK

                           COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                                =  (TAX-CUR OF TARRY(TARRY-IDX)
                                -  TAX-SUPP-PRIOR-RES OF GRSWK
                                -  ADDL-TAX OF TXARY(TXARY-IDX)
                                -  TAX OF STATE-RCP OF GRSWK)
                                +  HIGH-PREC-0

                       ELSE

                         IF TAX-METHOD-SUPPLEMENTAL OF TXARY(TXARY-IDX)
                            AND SUPPL-METHOD-USED-PCT OF GRSWK

                           COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                                =  TAX-CUR OF TARRY(TARRY-IDX)
                                -  ADDL-TAX OF TXARY(TXARY-IDX)
                                -  TAX OF STATE-RCP OF GRSWK
                         ELSE

                           COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                                =  TAX-CUR OF TARRY(TARRY-IDX)
                                +  TAX-SUPP-REG-AGG OF GRSWK
                                +  TAX-SUPP-PRIOR-WORK OF GRSWK
                                -  TAX-SUPP-TAX-WITHHELD OF GRSWK
                                -  ADDL-TAX OF TXARY(TXARY-IDX)
                                -  TAX OF STATE-RCP OF GRSWK
                         END-IF

                       END-IF
                   ELSE

                       IF  TAX-AGG OF TARRY(TARRY-IDX) <
                           WK-REG-RECIP-TAX OF W-WK

                         IF ACTUAL-TAX-CUR OF TXARY(TXARY-IDX) < ZERO
                            AND TAX OF STATE-RCP OF GRSWK
                                > CALCED-TAX-CUR OF TXARY(TXARY-IDX)
                            AND MULTI-PAY-FREQ-YES OF PYGRP

                           COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                                =  TAX-CUR OF TARRY(TARRY-IDX)
                                -  ADDL-TAX OF TXARY(TXARY-IDX)
                                -  CALCED-TAX-CUR OF TXARY(TXARY-IDX)

                         ELSE

                           IF MULTIPLE-SUPPL-FOUND-YES OF W-SW
                              AND SEPCHK OF CHECK = ZERO
                              AND SUPPL-METHOD-USED-PCT OF GRSWK

                           COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                                =  (TAX-CUR OF TARRY(TARRY-IDX)
                                -  TAX OF STATE-RCP OF GRSWK)
                                +  HIGH-PREC-0
                           ELSE

                           COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                                =  (TAX-CUR OF TARRY(TARRY-IDX)
                                -  WK-REG-RECIP-TAX OF W-WK
                                +  TAX-AGG OF TARRY(TARRY-IDX)
                                -  TAX-SUPP-PRIOR-RES OF GRSWK
                                -  ADDL-TAX OF TXARY(TXARY-IDX)
                                -  TAX OF STATE-RCP OF GRSWK)
                                +  HIGH-PREC-0
                           END-IF
                         END-IF
                       ELSE
                           IF MULTIPLE-SUPPL-FOUND-NO OF W-SW

                           COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                                =  (TAX-CUR OF TARRY(TARRY-IDX)
                                -  TAX-SUPP-PRIOR-RES OF GRSWK
                                -  ADDL-TAX OF TXARY(TXARY-IDX)
                                -  TAX OF STATE-RCP OF GRSWK)
                                +  HIGH-PREC-0

                           ELSE

                               COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                                    =  (TAX-CUR OF TARRY(TARRY-IDX)
                                    -  PRIOR-TAX-SUPP-CUR OF W-WK
                                    -  TAX-SUPP-PRIOR-RES OF GRSWK
                                    -  ADDL-TAX OF TXARY(TXARY-IDX)
                                    -  TAX OF STATE-RCP OF GRSWK)
                                    +  HIGH-PREC-0

                           END-IF
                       END-IF
                   END-IF

                   IF NOT TAX-METHOD-SUPPLEMENTAL OF TAX-SET OF TAXWK
                      AND TAX-CUR OF TARRY(TARRY-IDX) NOT ZERO

      *            SUPPLEMENTAL WITH REGULAR CHECK IS CALCULATED
      *            SEPARATELY AND RECIPROCITY HAS BEEN CALCULATED
      *            ON EACH PIECE. THIS IS AN OFFSET TO THE
      *            RECIPROCITY CALCULATION ON THE REGULAR PORTION

                       COMPUTE WK-REG-RECIP-TAX OF W-WK
                            =  WK-REG-RECIP-TAX OF W-WK
                            +  TAX-AGG OF TARRY(TARRY-IDX)
                            -  TAX-CUR OF TARRY(TARRY-IDX)

                   END-IF

                   SET STATE-RCP-TAKEN-YES OF W-SW TO TRUE

               END-IF
      *        Port Authority Change
      *        Work around for 15B.
               COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                       =  TAX-CUR OF TARRY(TARRY-IDX)
      *                 -  ADDL-TAX OF TXARY(TXARY-IDX))
                       -  TAX OF STATE-RCP OF GRSWK

               SET STATE-RCP-TAKEN-YES OF W-SW TO TRUE
               IF TAX-CUR OF TARRY(TARRY-IDX) < 0

                  MOVE 0 TO TAX-CUR OF TARRY(TARRY-IDX) 
               ELSE
                   IF TAX-CUR OF TARRY(TARRY-IDX) < 
                       ADDL-TAX OF TXARY(TXARY-IDX) OR 
                       TAX-CUR OF TARRY(TARRY-IDX) = 0

                       MOVE  ADDL-TAX OF TXARY(TXARY-IDX)
                          TO TAX-CUR OF TARRY(TARRY-IDX)
                   END-IF
               END-IF
      *        End Port Authority Change
           ELSE

               IF GRS OF STATE-RCP OF GRSWK > ZERO

                   COMPUTE TXGRS-CUR OF TARRY(TARRY-IDX)
                        =  (TXGRS-CUR OF TARRY(TARRY-IDX)
                        -  GRS OF STATE-RCP OF GRSWK)
                        +  HIGH-PREC-0
               END-IF


               IF FIRST-TAX-METHOD-FOUND-NO OF W-SW

                    COMPUTE PRE-FACTOR-TAX-CUR OF W-WK
                         =  PRE-FACTOR-TAX-CUR OF W-WK
                         +  TAX-CUR OF TARRY(TARRY-IDX)

               END-IF

               IF ((GRS OF STATE-RCP OF GRSWK
                      + TXGRS-CUR OF TARRY(TARRY-IDX))
                      NOT EQUAL ZERO)
                    OR FACTOR-TAX-CUR OF W-WK > ZERO

                   IF FACTOR-TAX-CUR OF W-WK = ZERO


                   IF STATE OF TXARY(TXARY-IDX) = 'MO'
                      AND TAX-CLASS-WITHHOLDING OF TARRY(TARRY-IDX)
                      AND RESIDENT-YES OF TARRY(TARRY-IDX)
                      AND MO-WRK-OUTSIDE-YES OF W-SW
                      AND MO-SUPPLE-NO OF W-SW

                      PERFORM LH411-MO-STATE-SPECIFIC

                   ELSE


                       COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                            =  (TXGRS-CUR OF TARRY(TARRY-IDX)
                            / (GRS OF STATE-RCP OF GRSWK
                            +  TXGRS-CUR OF TARRY(TARRY-IDX))
                            *  (TAX-CUR OF TARRY(TARRY-IDX)
                            +  FACTOR-TAX-CUR OF W-WK
                            -  ADDL-TAX OF TXARY(TXARY-IDX)))
                            +  HIGH-PREC-0

                       COMPUTE PRIOR-GRS-STATE-RCP OF W-WK
                            =  PRIOR-GRS-STATE-RCP OF W-WK
                            +  GRS OF STATE-RCP OF GRSWK
                   END-IF

                   ELSE

                       IF GRS OF STATE-RCP OF GRSWK > 0

                           IF TXGRS-CUR OF TARRY(TARRY-IDX) = ZERO

                               COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                                    = TXGRS-CUR OF TARRY(TARRY-IDX)
                                    / (GRS OF STATE-RCP OF GRSWK
                                    +  (FACTOR-TXGRS-CUR OF W-WK))
                                    *  (TAX-CUR OF TARRY(TARRY-IDX)
                                    +  FACTOR-TAX-CUR OF W-WK
                                    -  ADDL-TAX OF TXARY(TXARY-IDX))
                                    +  HIGH-PREC-0

                           ELSE

                               IF STATE OF TXARY(TXARY-IDX) = 'MO'
                                  AND TAX-CLASS-WITHHOLDING
                                                   OF TARRY(TARRY-IDX)
                                  AND RESIDENT-YES OF TARRY(TARRY-IDX)
                                  AND MO-WRK-OUTSIDE-NO OF W-SW
                                  AND (MO-SUPPLE-NO OF W-SW
                                   OR (MO-SUPPLE-YES OF W-SW
                                  AND TAX-AGG OF TARRY(TARRY-IDX) > 0))

                                IF MO-TAX-ADDL-AMT-ADDED OF W-SW

                                 COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                                    =  TXGRS-CUR OF TARRY(TARRY-IDX)
                                    / (GRS OF STATE-RCP OF GRSWK
                                    +  (TXGRS-CUR OF TARRY(TARRY-IDX)
                                    +  FACTOR-TXGRS-CUR OF W-WK))
                                    *  (TAX-CUR OF TARRY(TARRY-IDX)
                                    +  FACTOR-TAX-CUR OF W-WK
                                    -  TAX-CUR-ADDL-AMT
                                                 OF TARRY(TARRY-IDX))
                                    +  HIGH-PREC-0
                                ELSE

                                 COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                                    =  TAX-CUR OF TARRY(TARRY-IDX)
                                    -  TAX-CUR-ADDL-AMT
                                                 OF TARRY(TARRY-IDX)

                                 COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                                    =  TXGRS-CUR OF TARRY(TARRY-IDX)
                                    / (GRS OF STATE-RCP OF GRSWK
                                    +  (TXGRS-CUR OF TARRY(TARRY-IDX)
                                    +  FACTOR-TXGRS-CUR OF W-WK))
                                    *  (TAX-CUR OF TARRY(TARRY-IDX)
                                    +  FACTOR-TAX-CUR OF W-WK
                                    -  TAX-CUR-ADDL-AMT
                                                 OF TARRY(TARRY-IDX))
                                    +  HIGH-PREC-0

                                 COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                                    =  TAX-CUR OF TARRY(TARRY-IDX)
                                    +  TAX-CUR-ADDL-AMT
                                                 OF TARRY(TARRY-IDX)
                                END-IF
                               ELSE

                               COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                                    =  (TXGRS-CUR OF TARRY(TARRY-IDX)
                                    -  FACTOR-TXGRS-CUR OF W-WK)
                                    / (GRS OF STATE-RCP OF GRSWK
                                    +  (TXGRS-CUR OF TARRY(TARRY-IDX)
                                    -  FACTOR-TXGRS-CUR OF W-WK))
                                    *  (TAX-CUR OF TARRY(TARRY-IDX)
                                    +  FACTOR-TAX-CUR OF W-WK
                                    -  ADDL-TAX OF TXARY(TXARY-IDX))
                                    +  HIGH-PREC-0

                               END-IF
                           END-IF
                       ELSE

                           IF STATE OF TXARY(TXARY-IDX) = 'MO'
                             AND TAX-CLASS-WITHHOLDING
                                              OF TARRY(TARRY-IDX)
                             AND RESIDENT-YES OF TARRY(TARRY-IDX)
                             AND MO-WRK-OUTSIDE-NO OF W-SW
                             AND MO-SUPPLE-YES OF W-SW
                             AND FACTOR-TXGRS-CUR OF W-WK > 0
                             AND TAX-AGG OF TARRY(TARRY-IDX) > 0

                             COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                                  =  TXGRS-CUR OF TARRY(TARRY-IDX)
                                  /  (GRS OF STATE-RCP OF GRSWK
                                  +  (TXGRS-CUR OF TARRY(TARRY-IDX)
                                  +  FACTOR-TXGRS-CUR OF W-WK))
                                  *  (TAX-CUR OF TARRY(TARRY-IDX)
                                  +  FACTOR-TAX-CUR OF W-WK
                                  -  TAX-CUR-ADDL-AMT
                                               OF TARRY(TARRY-IDX))
                                  +  HIGH-PREC-0

                          ELSE

                           COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                                =  ((TAX-CUR OF TARRY(TARRY-IDX)
                                +   PRE-FACTOR-TAX-CUR OF W-WK)
                                *  (TXGRS-CUR OF TARRY(TARRY-IDX)
                                /  (FACTOR-TXGRS-CUR OF W-WK
                                +  (TXGRS-CUR OF TARRY(TARRY-IDX)))
                                -  ADDL-TAX OF TXARY(TXARY-IDX)))
                                +  HIGH-PREC-0

                          END-IF
                       END-IF
                   END-IF

                   SET STATE-RCP-TAKEN-YES OF W-SW TO TRUE

               ELSE

                   MOVE ZERO TO TAX-CUR OF TARRY(TARRY-IDX)
               END-IF

               COMPUTE FACTOR-TAX-CUR OF W-WK
                    =  FACTOR-TAX-CUR OF W-WK
                    +  PRE-FACTOR-TAX-CUR OF W-WK
                    -  TAX-CUR OF TARRY(TARRY-IDX)

               IF GRS OF STATE-RCP OF GRSWK > ZERO

                   COMPUTE FACTOR-TXGRS-CUR OF W-WK
                        =  FACTOR-TXGRS-CUR OF W-WK
                        +  GRS OF STATE-RCP OF GRSWK

               ELSE

                   COMPUTE FACTOR-TXGRS-CUR OF W-WK
                        =  FACTOR-TXGRS-CUR OF W-WK
                        +  TXGRS-CUR OF TARRY(TARRY-IDX)
               END-IF

               SET FIRST-TAX-METHOD-FOUND-YES OF W-SW TO TRUE

               SET STATE-RCP-TAKEN-YES OF W-SW TO TRUE

           END-IF

           IF TAX-CUR OF TARRY(TARRY-IDX) < ZERO

              MOVE ZERO TO TAX-CUR OF TARRY(TARRY-IDX)
              MOVE ZERO TO ACTUAL-TAX-CUR OF TXARY(TXARY-IDX)

           END-IF

           IF ADDL-TAX OF TXARY(TXARY-IDX) > ZERO
              AND STATE OF TXARY(TXARY-IDX) NOT EQUAL 'CT'

               COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                    =  (TAX-CUR OF TARRY(TARRY-IDX)
                    +  ADDL-TAX OF TXARY(TXARY-IDX))
                    +  HIGH-PREC-0

               COMPUTE ACTUAL-TAX-CUR OF TXARY(TXARY-IDX)
                    = (ACTUAL-TAX-CUR OF TXARY(TXARY-IDX)
                    +  ADDL-TAX OF TXARY(TXARY-IDX))
                    +  HIGH-PREC-0

               IF NOT TAX-METHOD-SUPPLEMENTAL OF TXARY(TXARY-IDX)
                  AND RESIDENT-YES OF TARRY(TARRY-IDX)

                      SET TAX-CUR-ADDL-AMT-ADDED OF W-SW TO TRUE
               END-IF

           ELSE

               IF TAX-CUR-ADDL-AMT OF TARRY(TARRY-IDX) > ZERO
                  AND STATE OF TXARY(TXARY-IDX) NOT EQUAL 'CT'

                 IF RESIDENT-YES OF TARRY(TARRY-IDX)
                    AND TAX-CUR-ADDL-AMT-ADDED OF W-SW
                    AND SWT-ADDL-PCT OF RESIDENCE OF TAXDT EQUAL ZERO
                    AND ADDL-TAX OF TXARY(TXARY-IDX) EQUAL ZERO

                    CONTINUE
                 ELSE

                   COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                        =  (TAX-CUR OF TARRY(TARRY-IDX)
                        +  TAX-CUR-ADDL-AMT OF TARRY(TARRY-IDX))
                        +  HIGH-PREC-0

                   COMPUTE ACTUAL-TAX-CUR OF TXARY(TXARY-IDX)
                        = (ACTUAL-TAX-CUR OF TXARY(TXARY-IDX)
                        +  TAX-CUR-ADDL-AMT OF TARRY(TARRY-IDX))
                        +  HIGH-PREC-0
                 END-IF
               END-IF
           END-IF

           .

       STATE-RECIP-EXIT.

      /*****************************************************************
      *                                                                *
       LH411-MO-STATE-SPECIFIC SECTION.
       LH411.
      *
      * When Resident is MO and the earnings are 100% in another state *
      * then find the tax for the work state and use it to re-calc MO  *
      * state withholding later on.                                    *
      ******************************************************************


           SET MO-TARRY-IDX TO TARRY-IDX

           IF MO-WRK-CALCED-NO  OF W-SW

            PERFORM VARYING MO-WRK-STATES-IDX FROM  1  BY  1
                   UNTIL MO-WRK-STATES-IDX  >  NBR-OF-STATES OF GRSWK

             SET TARRY-IDX TO 1

             SEARCH TAXCALC-DATA OF TARRY

                  WHEN TARRY-IDX > TAXCALC-COUNT OF TARRY

                      CONTINUE

                  WHEN STATE OF TARRY(TARRY-IDX) =
                       STATE OF WORK-STATE-DATA
                             OF GRSWK(MO-WRK-STATES-IDX)
                     AND TAX-CLASS-WITHHOLDING OF TARRY(TARRY-IDX)
                     AND TAX-CUR OF TARRY(TARRY-IDX) NOT = ZERO

                     IF TXGRS-CUR OF TARRY(TARRY-IDX) NOT =
                         MO-GRSWK-AMT OF W-WK

                        IF MO-SUPPLE-YES OF W-SW

                          COMPUTE MO-RCP-WRK-AMT OF W-WK =
                             MO-RCP-WRK-AMT OF W-WK +
                             TAX-AGG OF TARRY(TARRY-IDX)

                        ELSE

                          COMPUTE MO-RCP-WRK-AMT OF W-WK =
                             MO-RCP-WRK-AMT OF W-WK +
                             TAX-CUR OF TARRY(TARRY-IDX) -
                             TAX-CUR-ADDL-AMT OF TARRY(TARRY-IDX)

                        END-IF

                        MOVE TXGRS-CUR OF TARRY(TARRY-IDX)
                             TO MO-GRSWK-AMT OF W-WK

                     END-IF

                     SET MO-WRK-CALCED-YES  OF W-SW TO TRUE

             END-SEARCH

            END-PERFORM

           END-IF

           SET TARRY-IDX TO MO-TARRY-IDX


           COMPUTE PRIOR-GRS-STATE-RCP OF W-WK
                    =  PRIOR-GRS-STATE-RCP OF W-WK
                      +  GRS OF STATE-RCP OF GRSWK

           .

       MO-STATE-SPECIFIC-EXIT.

      /*****************************************************************
      *                                                                *
       LH411-MO-STATE-WITHHOLD SECTION.
       LH411.
      *
      * Re calc MO State Withholding                                   *
      ******************************************************************

           IF MO-SUPPLE-YES OF W-SW

              MOVE TAX-AGG OF TARRY(TARRY-IDX) TO
                                     MO-COMP-WRK-AMT OF W-WK

           ELSE

              IF TAX-CUR OF TARRY(TARRY-IDX) =
                 TAX-CUR-ADDL-AMT OF TARRY(TARRY-IDX)

                 MOVE TAX-CUR OF TARRY(TARRY-IDX)
                 TO MO-COMP-WRK-AMT OF W-WK

              ELSE
                 COMPUTE MO-COMP-WRK-AMT OF W-WK
                 = TAX-CUR OF TARRY(TARRY-IDX)  -
                   TAX-CUR-ADDL-AMT OF TARRY(TARRY-IDX)
              END-IF

           END-IF

           IF MO-WRK-OUTSIDE-YES OF W-SW

              COMPUTE TAX-CUR OF TARRY(TARRY-IDX) =
                 MO-COMP-WRK-AMT -  MO-RCP-WRK-AMT OF W-WK

           END-IF

           IF TAX-CUR OF TARRY(TARRY-IDX) < ZERO

              MOVE ZERO TO TAX-CUR OF TARRY(TARRY-IDX)

           END-IF

           IF TXGRS-CUR OF TARRY(TARRY-IDX) < ZERO

              MOVE ZERO TO TXGRS-CUR OF TARRY(TARRY-IDX)

           END-IF

           IF TAX-CUR OF TARRY(TARRY-IDX) = ZERO
              AND (MO-COMP-WRK-AMT OF W-WK =
              TAX-CUR-ADDL-AMT OF TARRY(TARRY-IDX)
                OR (MO-COMP-WRK-AMT OF W-WK =
                    TAX-AGG OF TARRY(TARRY-IDX) +
                    TAX-SUPP-CUR OF TARRY(TARRY-IDX) +
                    TAX-CUR-ADDL-AMT OF TARRY(TARRY-IDX)
                    AND MO-SUPPLE-NO OF W-SW))

              MOVE TAX-CUR-ADDL-AMT OF TARRY(TARRY-IDX)
              TO TAX-CUR OF TARRY(TARRY-IDX)

           END-IF
           .

       MO-STATE-WITHHOLD-EXIT.


      /*****************************************************************
      *                                                                *
       LH412-ONE-STATE-RED-RES SECTION.
       LH412.
      *                                                                *
      ******************************************************************


           IF SUPPL-METHOD-USED-AGG OF GRSWK

               IF PRIOR-RECIPROCITY OF W-WK = ZERO

                   COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                        = (TAX-CUR OF TARRY(TARRY-IDX)
                        +  TAX-SUPP-REG-AGG OF GRSWK
                        -  TAX-SUPP-TAX-WITHHELD OF GRSWK
                        -  TAX-SUPP-PRIOR-WORK OF GRSWK
                        -  ADDL-TAX OF TXARY(TXARY-IDX)
                        -  TAX OF STATE-RCP OF GRSWK)
                        +  HIGH-PREC-0

               ELSE

                   IF  TAX-SUPP-REG-AGG OF GRSWK = ZERO

                       COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                            = (TAX-CUR OF TARRY(TARRY-IDX)
                            +  TAX-AGG OF TARRY(TARRY-IDX)
                            +  TAX-SUPP-TAX-WITHHELD OF GRSWK
                            -  TAX-SUPP-PRIOR-WORK OF GRSWK
                            -  WK-REG-RECIP-TAX OF W-WK
                            -  ADDL-TAX OF TXARY(TXARY-IDX)
                            -  TAX OF STATE-RCP OF GRSWK)
                            +  HIGH-PREC-0

                   ELSE

                       COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                            = (TAX-CUR OF TARRY(TARRY-IDX)
                            +  TAX-SUPP-REG-AGG OF GRSWK
                            -  TAX-SUPP-TAX-WITHHELD OF GRSWK
                            -  TAX-SUPP-PRIOR-WORK OF GRSWK
                            -  PRIOR-RECIPROCITY OF W-WK
                            -  ADDL-TAX OF TXARY(TXARY-IDX)
                            -  TAX OF STATE-RCP OF GRSWK)
                            +  HIGH-PREC-0

                   END-IF
               END-IF
           ELSE
               IF SUPPL-METHOD-USED-PCT OF GRSWK

                   COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                        =  (TAX-CUR OF TARRY(TARRY-IDX)

                        -  TAX OF STATE-RCP OF GRSWK
                        -  ADDL-TAX OF TXARY(TXARY-IDX))
                        +  HIGH-PREC-0

               ELSE

                   COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                        =  (TAX-CUR OF TARRY(TARRY-IDX)
                        +  TAX-SUPP-REG-AGG OF GRSWK
                        +  TAX-SUPP-PRIOR-RES OF GRSWK
                        -  TAX-SUPP-TAX-WITHHELD OF GRSWK
                        -  TAX-SUPP-PRIOR-WORK OF GRSWK
                        -  TAX OF STATE-RCP OF GRSWK
                        -  ADDL-TAX OF TXARY(TXARY-IDX))
                        +  HIGH-PREC-0

               END-IF
           END-IF

           .

       ONE-STATE-RED-RES-EXIT.




      /*****************************************************************
      *                                                                *
       LH414-MULT-STATE-RED-RES SECTION.
       LH414.
      *                                                                *
      ******************************************************************


           IF SUPPL-METHOD-USED-AGG OF GRSWK
              AND NOT HIGH-SUPPL-AMT-YES OF SUPPC


               COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                    = (TAX-CUR OF TARRY(TARRY-IDX)
                    +  TAX-SUPP-REG-AGG OF GRSWK
                    -  TAX-SUPP-TAX-WITHHELD OF GRSWK
                    -  TAX-SUPP-PRIOR-WORK OF GRSWK
                    -  TAX-AGG-RES-RECALC OF GRSWK
                    -  ADDL-TAX OF TXARY(TXARY-IDX)
                    -  TAX OF STATE-RCP OF GRSWK)
                    +  HIGH-PREC-0

           ELSE

               COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                    =  (TAX-CUR OF TARRY(TARRY-IDX)
                    -  TAX OF STATE-RCP OF GRSWK
                    -  ADDL-TAX OF TXARY(TXARY-IDX))
                    +  HIGH-PREC-0

           END-IF


           .

       MULT-STATE-RED-RES-EXIT.


      /*****************************************************************
      *                                                                *
       LH415-FURTHER-RECIP-CALC SECTION.
       LH415.
      *                                                                *
      ******************************************************************


           IF  TAX-CUR OF TARRY(TARRY-IDX) < ZERO

               COMPUTE TAX-SUPP-PRIOR-RES OF GRSWK
                    =  TAX-SUPP-PRIOR-RES OF GRSWK
                    *  -1
           END-IF

           IF  STATE-RCP-TAKEN-NO OF W-SW

               IF WK-RECIPROCITY-RULE OF W-WK = SPACE

                   IF SUPPL-METHOD-USED-AGG OF GRSWK
                       AND NOT ONE-TIME-OVERRIDE OF TARRY(TARRY-IDX)

                       IF TXGRS-PRIOR-SUPP-WORK OF GRSWK = ZERO

                           COMPUTE ACTUAL-TAX-CUR OF TXARY(TXARY-IDX)
                                =  (ACTUAL-TAX-CUR OF TXARY(TXARY-IDX)
                                -  TAX-SUPP-PRIOR-RES OF GRSWK
                                +  ADDL-TAX OF TXARY(TXARY-IDX))
                                +  HIGH-PREC-0

                           COMPUTE CALCED-TAX-CUR OF TXARY(TXARY-IDX)
                                =  (CALCED-TAX-CUR OF TXARY(TXARY-IDX)
                                -  TAX-SUPP-PRIOR-RES OF GRSWK
                                +  ADDL-TAX OF TXARY(TXARY-IDX))
                                +  HIGH-PREC-0

                           COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                                =  TAX-CUR OF TARRY(TARRY-IDX)
                                -  TAX-SUPP-PRIOR-RES OF GRSWK

                         IF OFF-CYCLE-YES OF PSLCT
                            AND TAX-SUPP-PRIOR-RES OF GRSWK NOT = ZERO

                           IF RESIDENT-YES OF TARRY(TARRY-IDX)

                             CONTINUE
                           ELSE

                             COMPUTE ACTUAL-TAX-CUR OF TXARY(TXARY-IDX)
                                  =  ACTUAL-TAX-CUR OF TXARY(TXARY-IDX)
                                  +  TAX-SUPP-REG-AGG OF GRSWK

                             COMPUTE CALCED-TAX-CUR OF TXARY(TXARY-IDX)
                                  =  CALCED-TAX-CUR OF TXARY(TXARY-IDX)
                                  +  TAX-SUPP-REG-AGG OF GRSWK

                             COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                                  =  TAX-CUR OF TARRY(TARRY-IDX)
                                  +  TAX-SUPP-REG-AGG OF GRSWK
                           END-IF
                         END-IF

                           SET STATE-RCP-TAKEN-YES OF W-SW TO TRUE
                       ELSE

                           COMPUTE ACTUAL-TAX-CUR OF TXARY(TXARY-IDX)
                                =  (ACTUAL-TAX-CUR OF TXARY(TXARY-IDX)
                                +  TAX-SUPP-REG-AGG OF GRSWK
                                -  TAX-SUPP-TAX-WITHHELD OF GRSWK
                                +  ADDL-TAX OF TXARY(TXARY-IDX))
                                +  HIGH-PREC-0

                           COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                                =  TAX-CUR OF TARRY(TARRY-IDX)
                                -  TAX-SUPP-TAX-WITHHELD OF GRSWK
                                +  TAX-SUPP-REG-AGG OF GRSWK
                           SET STATE-RCP-TAKEN-YES OF W-SW TO TRUE

                       END-IF

                   ELSE

                       COMPUTE ACTUAL-TAX-CUR OF TXARY(TXARY-IDX)
                            =  (ACTUAL-TAX-CUR OF TXARY(TXARY-IDX)
                            +  ADDL-TAX OF TXARY(TXARY-IDX))
                            +  HIGH-PREC-0

                       COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                            =  TAX-CUR OF TARRY(TARRY-IDX)
                            *  1

                   END-IF

               ELSE

                   PERFORM LH418-FURTHER-AGG-RECIP

               END-IF
           END-IF


           .

       FURTHER-RECIP-CALC-EXIT.




      /*****************************************************************
      *                                                                *
       LH418-FURTHER-AGG-RECIP SECTION.
       LH418.
      *                                                                *
      ******************************************************************


           IF  SUPPL-METHOD-USED-AGG OF GRSWK

               IF WK-RECIPROCITY-RULE-NOWORK-ALL OF W-WK

                   IF MULTIPLE-SUPPL-FOUND-NO OF W-SW
                        AND NBR-OF-STATES OF GRSWK > 1

                     IF (NOT SUBJECT-FWT-YES OF TGCTB-PASS
                        AND TXGRS-CUR OF TARRY(TARRY-IDX) = ZERO)
                        OR (STATE-RECIP-WITH-LOCAL-YES OF GRSWK
                             AND TXGRS-AGG-THIS-CHK-NO OF GRSWK)

                        CONTINUE
                     ELSE
                       COMPUTE ACTUAL-TAX-CUR OF TXARY(TXARY-IDX)
                            =  (ACTUAL-TAX-CUR OF TXARY(TXARY-IDX)
                            +  TAX-SUPP-REG-AGG OF GRSWK
                            -  TAX-SUPP-PRIOR-WORK OF GRSWK
                            +  ADDL-TAX OF TXARY(TXARY-IDX)
                            -  TAX OF STATE-RCP OF GRSWK)
                            +  HIGH-PREC-0

                       COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                            =  (TAX-CUR OF TARRY(TARRY-IDX)
                            +  TAX-SUPP-REG-AGG OF GRSWK
                            -  TAX-SUPP-TAX-WITHHELD OF GRSWK
                            -  TAX-SUPP-PRIOR-WORK OF GRSWK
                            -  TAX OF STATE-RCP OF GRSWK)
                            +  HIGH-PREC-0
                     END-IF

                   ELSE

                       COMPUTE ACTUAL-TAX-CUR OF TXARY(TXARY-IDX)
                            =  (ACTUAL-TAX-CUR OF TXARY(TXARY-IDX)
                            -  TAX-SUPP-PRIOR-RES OF GRSWK
                            +  ADDL-TAX OF TXARY(TXARY-IDX))
                            +  HIGH-PREC-0

                       COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                            = (TAX-CUR OF TARRY(TARRY-IDX)
                            -  TAX-SUPP-PRIOR-RES OF GRSWK)
                            +  HIGH-PREC-0

                   END-IF

                   SET STATE-RCP-TAKEN-YES OF W-SW TO TRUE

               ELSE

                   IF  TAX-CUR OF TARRY(TARRY-IDX) > ZERO
                       AND NOT ONE-TIME-OVERRIDE OF TARRY(TARRY-IDX)

                       IF  WK-RECIPROCITY-RULE-REDTTL-A OF W-WK

                           COMPUTE ACTUAL-TAX-CUR OF TXARY(TXARY-IDX)
                                =  (ACTUAL-TAX-CUR OF TXARY(TXARY-IDX)
                                +  TAX-SUPP-REG-AGG OF GRSWK
                                -  TAX-SUPP-TAX-WITHHELD OF GRSWK
                                +  ADDL-TAX OF TXARY(TXARY-IDX)
                                -  TAX OF STATE-RCP OF GRSWK)
                                +  HIGH-PREC-0

                       ELSE

                           COMPUTE ACTUAL-TAX-CUR OF TXARY(TXARY-IDX)
                                =  (ACTUAL-TAX-CUR OF TXARY(TXARY-IDX)
                                +  TAX-SUPP-REG-AGG OF GRSWK
                                -  TAX-SUPP-TAX-WITHHELD OF GRSWK
                                +  ADDL-TAX OF TXARY(TXARY-IDX)
                                -  TAX OF STATE-RCP OF GRSWK)
                                +  HIGH-PREC-0

                           COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                                =  (TAX-CUR OF TARRY(TARRY-IDX)
                                +  TAX-SUPP-REG-AGG OF GRSWK
                                -  TAX-SUPP-TAX-WITHHELD OF GRSWK
                                -  TAX OF STATE-RCP OF GRSWK)
                                +  HIGH-PREC-0
                       END-IF

                       SET STATE-RCP-TAKEN-YES OF W-SW TO TRUE

                   END-IF
               END-IF

           ELSE

               IF (OFF-CYCLE-YES OF PSLCT
                  AND TXGRS-CUR OF TARRY(TARRY-IDX) EQUAL ZERO)
                       OR (GROSSUP-YES OF CHECK
                           AND ADD-GROSS-NO OF ERNTB(ERNTB-IDX))
      *    Port Authority Change 18c - Remove OR statement
      *            OR PAY-OFF-CYCLE-CAL OF PSLCT = 'Y'

                   CONTINUE

               ELSE

                   IF WK-RECIPROCITY-RULE-REDTTL-A OF W-WK

                     IF RESIDENT-TAX-YES OF  TXARY(TXARY-IDX)

                       CONTINUE
                     ELSE

                       COMPUTE ACTUAL-TAX-CUR OF TXARY(TXARY-IDX)
                            =  (ACTUAL-TAX-CUR OF TXARY(TXARY-IDX)

                            +  ADDL-TAX OF TXARY(TXARY-IDX))
                            +  HIGH-PREC-0

                       COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                            =  (TAX-CUR OF TARRY(TARRY-IDX)

                            +   ADDL-TAX OF TXARY(TXARY-IDX))
                            +   HIGH-PREC-0
                     END-IF
                   ELSE

                     IF TAX-METHOD-SUPPLEMENTAL OF TXARY(TXARY-IDX)
                        AND RESIDENT-TAX-YES OF  TXARY(TXARY-IDX)
                        AND ADDL-TAX OF TXARY(TXARY-IDX) = ZERO
                        AND ((SWT-ADDL-AMT OF RESIDENCE OF TAXDT
                                  NOT = ZERO
                              AND (SWT-ADDL-AMT OF RESIDENCE OF TAXDT
                                  = TAX-SUPP-TAX-WITHHELD OF GRSWK
                                  - TAX-SUPP-REG-AGG OF GRSWK
                                  - TAX-SUPP-PRIOR-RES OF GRSWK
                                 OR (WK-RECIPROCITY-RULE-FACTOR OF W-WK
                                     AND TAX-SUPP-TAX-WITHHELD OF GRSWK
                                       > TAX-SUPP-REG-AGG OF GRSWK)))
                          OR (SWT-ADDL-PCT OF RESIDENCE OF TAXDT
                                  NOT = ZERO
                              AND TAX-SUPP-TAX-WITHHELD OF GRSWK
                                  > TAX-SUPP-REG-AGG OF GRSWK))

                         CONTINUE
                     ELSE

                       IF WK-RECIPROCITY-RULE-FACTOR OF W-WK
                          AND TXGRS-CUR OF TARRY(TARRY-IDX) = ZERO
                          AND TAX-CUR OF TARRY(TARRY-IDX) = ZERO
                          AND ADDL-TAX OF TXARY(TXARY-IDX) = ZERO
                          AND FLAT-AMT OF GRSWK > ZERO
                          AND TAX-SUPP-TAX-WITHHELD OF GRSWK
                              < TAX-SUPP-REG-AGG OF GRSWK

                         CONTINUE

                       ELSE

                       COMPUTE ACTUAL-TAX-CUR OF TXARY(TXARY-IDX)
                            =  (ACTUAL-TAX-CUR OF TXARY(TXARY-IDX)
                            -  TAX-SUPP-REG-AGG OF GRSWK
                            -  TAX-SUPP-PRIOR-RES OF GRSWK
                            +  TAX-SUPP-TAX-WITHHELD OF GRSWK
                            +  ADDL-TAX OF TXARY(TXARY-IDX)
                            -  TAX OF STATE-RCP OF GRSWK)
                            +  HIGH-PREC-0

                       COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                            =  (TAX-CUR OF TARRY(TARRY-IDX)
                            -  TAX-SUPP-REG-AGG OF GRSWK
                            +  TAX-SUPP-TAX-WITHHELD OF GRSWK
                            -  TAX-SUPP-PRIOR-RES OF GRSWK
                            -  TAX OF STATE-RCP OF GRSWK)
                            +  HIGH-PREC-0

      *    Port Authority Change 18c - Remove IF statement
      *                 IF TAX OF STATE-RCP OF GRSWK < ZERO
      *
      *                      SET RES-TARRY-IDX TO TARRY-IDX
      *                      SET STATE-NEG-RCP-YES OF W-SW TO TRUE
      *                 END-IF
                     END-IF
                   END-IF
               END-IF
           END-IF

           .


       FURTHER-AGG-RECIP-EXIT.


      /*****************************************************************
      *                                                                *
       LH420-ROUND-TAXES SECTION.
       LH420.
      *                                                                *
      ******************************************************************
           IF (STATE OF TXARY(TXARY-IDX) = 'IA' OR
               STATE OF TXARY(TXARY-IDX) = 'CO' OR
               STATE OF TXARY(TXARY-IDX) = 'ID' OR
               STATE OF TXARY(TXARY-IDX) = 'KS' OR
               STATE OF TXARY(TXARY-IDX) = 'ME' OR
               STATE OF TXARY(TXARY-IDX) = 'MO' OR
               STATE OF TXARY(TXARY-IDX) = 'MS' OR
               STATE OF TXARY(TXARY-IDX) = 'NC' OR
               STATE OF TXARY(TXARY-IDX) = 'OK' OR
               STATE OF TXARY(TXARY-IDX) = 'WV' OR
      *        STATE OF TXARY(TXARY-IDX) = 'SC' OR
               STATE OF TXARY(TXARY-IDX) = 'ND' OR
               STATE OF TXARY(TXARY-IDX) = 'MT') AND
               TAX-CLASS-WITHHOLDING OF TXARY(TXARY-IDX) AND
               LOCALITY OF TXARY(TXARY-IDX)  = SPACE
                  COMPUTE WK-AMT-ROUND OF W-WK ROUNDED
                           =  ACTUAL-TAX-CUR OF TXARY(TXARY-IDX)
                  MOVE WK-AMT-ROUND OF W-WK
                           TO ACTUAL-TAX-CUR OF TXARY(TXARY-IDX)
                  PERFORM LH550-FIND-TARRY
                  COMPUTE WK-AMT-ROUND OF W-WK ROUNDED
                           =  TAX-CUR OF TARRY(TARRY-IDX)
                  MOVE WK-AMT-ROUND OF W-WK
                           TO TAX-CUR OF TARRY(TARRY-IDX)

           END-IF


           .
       ROUND-TAXES-EXIT.

      /*****************************************************************
      *                                                                *
       LH500-LOCAL-ADJ-RECIP SECTION.
       LH500.
      *                                                                *
      ******************************************************************

      *    DETERMINE IF WORK TAX SHOULD BE REDUCED BY RESIDENT TAX
      *    (RECIPROCITY-RULE-REDWRK 'K')
      *    DO NOT W/H FOR WK LOCALITY IF TAXED BY RESIDENT LOCALITY
      *    (RECIPROCITY-RULE-NOWORK 'W')
      *    LOCAL-RCP OF GRSWK IS USED FOR REDUCING RESIDENT TAX
      *    (RECIPROCITY-RULE-REDRES 'R')
      *    HIGHER OF WORK OR RESIDENCE ONLY
      *    (RECIPROCITY-RULE-HIGHER 'C')
      *    UNLESS, IN EACH CASE, THERE IS A TAX OVERRIDE


           SET RECIP-RULE-REDRTE-NO OF W-SW TO TRUE
           MOVE ZERO TO WK-LCLPCT-RATE OF W-WK
           MOVE ZERO TO CMPR-LCL-RATE  OF W-WK
           MOVE ZERO TO LOCAL-RCP-SAVE-ORIG OF W-WK

           PERFORM VARYING TXARY-IDX FROM 1  BY  1
                   UNTIL TXARY-IDX > TAX-SET-CNT OF TXARY

               IF LOCALITY OF TAX-SET OF TXARY (TXARY-IDX)
                       NOT =  SPACES
                       AND  RESIDENT-TAX-NO OF TAX-SET-DATA
                               OF TXARY(TXARY-IDX)
                       AND  TAX-CLASS-WITHHOLDING OF TXARY(TXARY-IDX)

                   MOVE ZEROS  TO  RES-TAX OF W-WK
                   MOVE ZEROS  TO  RES-TAX-TOTAL OF W-WK
                   MOVE ZEROS  TO  RES-TXBL OF W-WK
                   MOVE ZEROS  TO  RES-TAX-SUPPL OF W-WK
                   SET REDWRK-NO-ADJ-TARRY OF W-SW  TO  TRUE
                   SET NOWRK-NO-ADJ-TARRY OF W-SW  TO  TRUE
                   SET HIGHER-NO-ADJ-TARRY OF W-SW  TO  TRUE
                   SET ADJUST-PA-NO OF W-SW TO TRUE

                   PERFORM VARYING LCL-RCP-IDX FROM 1  BY  1
                       UNTIL LCL-RCP-IDX  >  LOCALITY-COUNT
                                  OF RESIDENCE OF TAXWK

                       IF RES-LOCALITY OF TXARY(TXARY-IDX LCL-RCP-IDX)
                           = LOCALITY OF RESIDENCE OF TAXWK(RESWK-IDX)

                           IF RECIPROCITY-RULE-REDRTE OF LOCAL-LOCAL
                                               (TXARY-IDX LCL-RCP-IDX)
                               SET RECIP-RULE-REDRTE-YES OF W-SW TO TRUE
                           END-IF

                           PERFORM LH510-CHECK-RECIP-RULE
                           PERFORM LH530-RECALC-RES-TAX
                           PERFORM LH540-GET-LOCAL-RATE
                           MOVE WK-LCLPCT OF W-WK TO
                                WK-LCLPCT-RATE OF W-WK
                       END-IF
                   END-PERFORM

                   IF REDWRK-ADJ-TARRY OF W-SW
                           OR  NOWRK-ADJ-TARRY OF W-SW
                       OR  HIGHER-ADJ-TARRY OF W-SW
                       OR  ADJUST-PA-YES OF W-SW

                       PERFORM LH600-ADJUST-TARRY
                   END-IF
               END-IF

               IF STATE OF TXARY(TXARY-IDX)  =  'PA'
                   AND LOCALITY OF TXARY(TXARY-IDX)  NOT =  SPACE
                   AND (TAX-CLASS-WITHHOLDING OF TXARY(TXARY-IDX)
                        OR TAX-CLASS-LOC-OCC OF TXARY(TXARY-IDX))
                   AND (TXGRS-CUR OF TXARY(TXARY-IDX)  =  0.01
                        OR TXGRS-CUR OF TXARY(TXARY-IDX) = -0.01)

                   PERFORM LH610-PA-ADJUST-TXGRS
               END-IF

           END-PERFORM

      *    REDUCE RESIDENT TAX BY WORK TAX IF RECIPROCITY EXISTS
      *    AND THERE ARE NO TAX OVERRIDES

           SET REDRES-NO-ADJ-TARRY OF W-SW  TO  TRUE
           SET APPLY-ONCE-NO OF W-SW TO TRUE

           IF TAX OF LOCAL-RCP OF GRSWK(LRCWK-IDX) NOT = ZERO

               SET TXARY-IDX  TO  TXARY-IDX-PASS OF TXARY

               PERFORM LH540-GET-LOCAL-RATE

               COMPUTE CMPR-LCL-RATE OF W-WK = ((WK-LCLPCT OF W-WK -
               (WK-LCLPCT OF W-WK * (REDUCE-PCT OF W-WK/100))))
               + HIGH-PREC-0

               IF CMPR-LCL-RATE OF W-WK NOT ZERO AND
                  CMPR-LCL-RATE OF W-WK > CREDIT-LIM-RT OF W-WK
                  MOVE CREDIT-LIM-RT OF W-WK TO CMPR-LCL-RATE OF W-WK
               ELSE
                  MOVE WK-LCLPCT OF W-WK TO CMPR-LCL-RATE OF W-WK
               END-IF

               IF TAX-METHOD-SUPPLEMENTAL
                  OF TAX-SET-DATA OF TXARY(TXARY-IDX)
                  AND RECIP-RULE-REDRTE-NO OF W-SW
                  AND ((CMPR-LCL-RATE OF W-WK < WK-LCLPCT-RATE OF W-WK)
                       OR
                       (CMPR-LCL-RATE OF W-WK
                       NOT > WK-LCLPCT-RATE OF W-WK
                       AND TXGRS-CUR OF TXARY(TXARY-IDX) = 0
                       AND TAX-AGG OF TXARY(TXARY-IDX) > 0))

                  PERFORM VARYING TXARY-IDX FROM 1  BY  1
                          UNTIL TXARY-IDX > TAX-SET-CNT OF TXARY

                       IF LOCALITY OF TAX-SET OF TXARY (TXARY-IDX)
                          NOT = SPACES
                          AND RESIDENT-TAX-YES OF TAX-SET-DATA
                              OF TXARY(TXARY-IDX)
                          AND TAX-CLASS-WITHHOLDING OF TXARY(TXARY-IDX)

                         PERFORM LH540-GET-LOCAL-RATE

                         IF LOCALITY OF TXARY(TXARY-IDX) NOT = SPACE
                            AND STATE OF TXARY(TXARY-IDX) = 'OH'
                            AND WK-LCLPCT OF W-WK  = ZERO

                            CONTINUE
                         ELSE

                          IF CMPR-LCL-RATE OF W-WK
                             < WK-LCLPCT-RATE OF W-WK
                             IF  TAX-METHOD-SUPPLEMENTAL
                               OF TAX-SET-DATA OF TXARY(TXARY-IDX)

                                 IF LOCAL-RCP-SAVE-ORIG OF W-WK = 0
                                    MOVE TAX OF LOCAL-RCP OF
                                         GRSWK(LRCWK-IDX)
                                      TO LOCAL-RCP-SAVE-ORIG OF W-WK
                                 END-IF

                                 MOVE CALCED-TAX-CUR  OF
                                      TXARY(TXARY-IDX) TO
                                      TAX OF LOCAL-RCP OF
                                      GRSWK(LRCWK-IDX)
                                 SET SUPPL-LOCAL-RCP-YES  TO  TRUE
                             ELSE

                                MOVE TAX OF LOCAL-RCP
                                         OF GRSWK(LRCWK-IDX)
                                     TO  LOCAL-RCP-SAVE OF W-WK
                                MOVE LOCAL-RCP-SAVE OF W-WK
                                  TO LOCAL-RCP-SAVE-ORIG OF W-WK

                                COMPUTE  TAX OF LOCAL-RCP OF
                                         GRSWK(LRCWK-IDX)
                                      =  CALCED-TAX-CUR OF
                                         TXARY(TXARY-IDX)
                                      -  FLAT-AMT OF GRSWK
                               END-IF
                          END-IF

                          PERFORM VARYING LCL-RCP-IDX FROM 1  BY  1
                              UNTIL LCL-RCP-IDX  >  LOCALITY-COUNT
                                         OF RESIDENCE OF TAXWK
                                 PERFORM LH510-CHECK-RECIP-RULE
                          END-PERFORM

                          PERFORM LH505-APPLY-LOCALITY-CREDIT

                         END-IF

                       END-IF

                  END-PERFORM



               ELSE
                  SET APPLY-ONCE-YES OF W-SW TO TRUE
                  MOVE TAX OF LOCAL-RCP OF GRSWK(LRCWK-IDX)
                    TO LOCAL-RCP-SAVE-ORIG OF W-WK
                  PERFORM LH505-APPLY-LOCALITY-CREDIT
               END-IF

           END-IF

           IF STATE OF TARRY(TARRY-IDX) = 'PA'
              AND TAX-CLASS-WITHHOLDING OF TARRY(TARRY-IDX)
              AND LOCALITY OF TARRY(TARRY-IDX) NOT EQUAL SPACES
              AND LOCALITY OF TARRY(TARRY-IDX) =
                    LOCALITY OF RESIDENCE OF TAXWK(1)
              AND WORK-PSD-CD OF TARRY(TARRY-IDX) = '880000'
              AND PA-RES-LWT OF GRSWK  NOT EQUAL ZERO
              AND PA-RES-LOC-ADJ OF GRSWK  = ZERO
              AND PHIL-LOCAL-FOUND-YES OF W-SW
              AND PA-BLANK-FOUND-NO OF W-SW
              AND PA-RES-ERN-FOUND-NO OF W-SW
              AND NON-PA-ERN-FOUND-YES OF W-SW

              SET PA-RES-880000-ROW-YES OF W-SW   TO  TRUE
           ELSE

              IF STATE OF TARRY(TARRY-IDX) = 'PA'
              AND TAX-CLASS-WITHHOLDING OF TARRY(TARRY-IDX)
              AND LOCALITY OF TARRY(TARRY-IDX) NOT EQUAL SPACES
              AND LOCALITY OF TARRY(TARRY-IDX) =
                    LOCALITY OF RESIDENCE OF TAXWK(1)
              AND WORK-PSD-CD OF TARRY(TARRY-IDX) NOT = '880000'
              AND PA-RES-LWT OF GRSWK  NOT EQUAL ZERO
              AND PHIL-LOCAL-FOUND-YES OF W-SW
              AND PA-BLANK-FOUND-NO OF W-SW
              AND PA-RES-ERN-FOUND-YES OF W-SW
              AND NON-PA-ERN-FOUND-YES OF W-SW

                SET PA-RES-880000-ROW-YES OF W-SW   TO  TRUE
              ELSE
                IF TAX-CLASS-WITHHOLDING OF TARRY(TARRY-IDX)
                AND STATE OF RESIDENCE OF TAXWK = 'PA'
                AND PA-RES-LWT OF GRSWK  NOT EQUAL ZERO
                AND PHIL-LOCAL-FOUND-YES OF W-SW
                AND PA-BLANK-FOUND-NO OF W-SW
                AND PA-RES-ERN-FOUND-NO OF W-SW
                AND NON-PA-ERN-FOUND-YES OF W-SW

                  SET PA-RES-880000-ROW-YES OF W-SW   TO  TRUE
                END-IF

              END-IF

           END-IF

           .
       LOCAL-ADJ-RECIP-EXIT.


      /*****************************************************************
      *                                                                *
       LH505-APPLY-LOCALITY-CREDIT SECTION.
       LH505.
      *                                                                *
      ******************************************************************
           PERFORM LH550-FIND-TARRY
           IF  SPECIAL-LWT-STATUS-NONE OF TARRY(TARRY-IDX)
                   AND  ONE-TIME-NA OF TARRY(TARRY-IDX)

               COMPUTE TAX OF LOCAL-RCP OF GRSWK(LRCWK-IDX)
                   =   TAX OF LOCAL-RCP OF GRSWK(LRCWK-IDX)
                   *   REDUCE-PCT OF W-WK
                   /   100

               IF SUPPL-LOCAL-RCP-NO OF W-SW
                   COMPUTE LOCAL-RCP-SAVE OF W-WK
                       =   LOCAL-RCP-SAVE OF W-WK
                       *   REDUCE-PCT OF W-WK
                       /   100
               END-IF

               PERFORM LH540-GET-LOCAL-RATE

               IF CREDIT-LIM-RT OF W-WK = 0
                  COMPUTE CREDIT-LIM-RT OF W-WK =
                          ((WK-LCLPCT-RES OF W-WK -
                  (WK-LCLPCT-RES OF W-WK * (REDUCE-PCT OF W-WK/100))))
                  + HIGH-PREC-0
               END-IF

               IF WK-LCLPCT OF W-WK > 0  AND
                   CREDIT-LIM-RT OF W-WK > 0

                   IF TAX-METHOD-SUPPLEMENTAL
                          OF TAX-SET-DATA OF TXARY(TXARY-IDX)
                      AND APPLY-ONCE-YES OF W-SW

                      COMPUTE CALCED-LIMIT OF W-WK
                           = (((CALCED-TAX-CUR OF TXARY(TXARY-IDX)
                              + TAX-AGG OF TXARY(TXARY-IDX))
                              - ADDL-TAX OF TXARY(TXARY-IDX))
                           *  CREDIT-LIM-RT OF W-WK
                           /  WK-LCLPCT OF W-WK)
                           + HIGH-PREC-0
                   ELSE
                      IF CREDIT-LIM-RT OF W-WK =
                         WK-LCLPCT OF W-WK       AND
                         WK-LCLPCT OF W-WK  <
                         WK-LCLPCT-RES OF W-WK

                          COMPUTE CALCED-LIMIT OF W-WK
                             = (((CALCED-TAX-CUR OF TXARY(TXARY-IDX)
                             - ADDL-TAX OF TXARY(TXARY-IDX) )
                             / WK-LCLPCT-RES OF W-WK)
                             *  CREDIT-LIM-RT OF W-WK)
                             + HIGH-PREC-0

                      ELSE
                          IF  TAX-METHOD-SUPPLEMENTAL      OF
                              TAX-SET-DATA OF TXARY(TXARY-IDX)

                              COMPUTE CALCED-LIMIT OF W-WK
                             =  (CALCED-TAX-CUR OF TXARY(TXARY-IDX)
                             *  CREDIT-LIM-RT OF W-WK
                             /  WK-LCLPCT OF W-WK)
                             + HIGH-PREC-0

                     ELSE

                          COMPUTE CALCED-LIMIT OF W-WK
                             = ((CALCED-TAX-CUR OF TXARY(TXARY-IDX)
                             - ADDL-TAX OF TXARY(TXARY-IDX) )
                             *  CREDIT-LIM-RT OF W-WK
                             /  WK-LCLPCT OF W-WK)
                             + HIGH-PREC-0

                          END-IF
                      END-IF
                   END-IF

                   IF TAX OF LOCAL-RCP OF GRSWK(LRCWK-IDX) >
                       CALCED-LIMIT OF W-WK
                      OR TAX OF LOCAL-RCP OF GRSWK(LRCWK-IDX) NOT > 0

                           MOVE CALCED-LIMIT OF W-WK TO
                               TAX OF LOCAL-RCP OF GRSWK(LRCWK-IDX)
                   END-IF
               END-IF

               IF TAX OF LOCAL-RCP OF GRSWK(LRCWK-IDX) < 0
                   AND (NEG-LOCAL-RCP-NO OF W-SW
                        OR (NEG-LOCAL-RCP-YES OF W-SW
                            AND TAX-CUR OF TARRY(TARRY-IDX) = ZERO))

                  MOVE 0 TO TAX OF LOCAL-RCP OF GRSWK(LRCWK-IDX)
               END-IF

               IF TAX-METHOD-SUPPLEMENTAL
                      OF TAX-SET-DATA OF TXARY(TXARY-IDX)
                  AND APPLY-ONCE-YES OF W-SW

                  COMPUTE ACTUAL-TAX-CUR OF TXARY(TXARY-IDX)
                       = (ACTUAL-TAX-CUR OF TXARY(TXARY-IDX)
                          + TAX-AGG OF TXARY(TXARY-IDX))
                       -  TAX OF LOCAL-RCP OF GRSWK(LRCWK-IDX)
               ELSE
                  IF  CREDIT-LIM-RT OF W-WK   =
                      WK-LCLPCT OF W-WK       AND
                      WK-LCLPCT OF W-WK       <
                      WK-LCLPCT-RES OF W-WK

                       COMPUTE ACTUAL-TAX-CUR OF TXARY(TXARY-IDX)
                           =  ACTUAL-TAX-CUR OF TXARY(TXARY-IDX)
                           -  ACTUAL-TAX-CUR OF TXARY(TXARY-IDX)
                           +  TAX OF LOCAL-RCP OF GRSWK(LRCWK-IDX)

                   ELSE
                       COMPUTE ACTUAL-TAX-CUR OF TXARY(TXARY-IDX)
                           =  ACTUAL-TAX-CUR OF TXARY(TXARY-IDX)
                           -  TAX OF LOCAL-RCP OF GRSWK(LRCWK-IDX)
                  END-IF
               END-IF

               IF ACTUAL-TAX-CUR OF TAX-SET-DATA
                       OF TXARY(TXARY-IDX) < 0

                   MOVE ZERO TO ACTUAL-TAX-CUR OF TAX-SET-DATA
                           OF TXARY(TXARY-IDX)
               END-IF

               IF WK-LCLPCT-RES OF W-WK
                  < WK-LCLPCT-RATE OF W-WK
                     MOVE WK-LCLPCT-RES OF W-WK
                       TO WK-LCLPCT-USED-RATE OF W-WK
               ELSE
                     MOVE WK-LCLPCT-RATE OF W-WK
                       TO WK-LCLPCT-USED-RATE OF W-WK
               END-IF

               IF CREDIT-LIM-RT OF W-WK > 0
                 AND WK-LCLPCT-USED-RATE OF W-WK > 0
                 AND LOCAL-RCP-SAVE-ORIG OF W-WK > 0

                  COMPUTE CALCED-RCP-LIMIT OF W-WK
                        = (LOCAL-RCP-SAVE-ORIG OF W-WK
                        *  CREDIT-LIM-RT OF W-WK
                        /  WK-LCLPCT-USED-RATE OF W-WK)
                        + HIGH-PREC-0

                  IF LOCAL-RCP-SAVE OF W-WK  >
                      CALCED-RCP-LIMIT OF W-WK

                       MOVE CALCED-RCP-LIMIT OF W-WK TO
                            LOCAL-RCP-SAVE OF W-WK
                  END-IF

                  IF TAX OF LOCAL-RCP OF GRSWK(LRCWK-IDX) >
                      CALCED-RCP-LIMIT OF W-WK
                     AND LOCALITY-COUNT OF RESIDENCE OF TAXWK = 1

                       MOVE CALCED-RCP-LIMIT OF W-WK TO
                            TAX OF LOCAL-RCP OF GRSWK(LRCWK-IDX)
                  END-IF

               END-IF

               SET REDRES-ADJ-TARRY OF W-SW  TO  TRUE
               SET WK-WK-NO-ADJ-TARRY OF W-SW   TO TRUE
               SET REDWRK-NO-ADJ-TARRY OF W-SW  TO  TRUE
               SET NOWRK-NO-ADJ-TARRY OF W-SW  TO  TRUE

               PERFORM LH600-ADJUST-TARRY
           END-IF

           .
       APPLY-LOCALITY-CREDIT-EXIT.


      /*****************************************************************
      *                                                                *
       LH510-CHECK-RECIP-RULE SECTION.
       LH510.
      *                                                                *
      ******************************************************************


           MOVE ZERO TO RES-TAX-RECALC OF W-WK
           MOVE ZERO TO RES-TAX-LESS OF W-WK
           MOVE ZERO TO RES-TXGRS-RECALC OF W-WK
           MOVE ZERO TO ACTUAL-TAX-CUR-SAV OF W-WK
           MOVE ZERO TO RES-TAX-RECALC-SAV OF W-WK
           MOVE ZERO TO RES-TAX-PRIOR-CALC OF W-WK

           IF  STATE OF TXARY(TXARY-IDX) = 'PA'
               AND TAX-CLASS-WITHHOLDING OF TXARY(TXARY-IDX)

               MOVE LOCALITY OF TXARY(TXARY-IDX)
                    TO SAV-WORK OF W-WK

               MOVE RES-LOCALITY OF LOCALITY-DATA
                               OF TXARY(TXARY-IDX LCL-RCP-IDX)
                    TO SAV-LOCALITY OF W-WK

               IF  RES-STATE OF LOCALITY-DATA
                               OF TXARY(TXARY-IDX LCL-RCP-IDX)
                     NOT = 'PA'
                   MOVE '880000'  TO SAV-RESIDENCE OF W-WK
               ELSE

                   IF FIRST-CHAR-LOC OF SAV-LOCALITY NOT EQUAL 'O'

                       MOVE RES-LOCALITY OF LOCALITY-DATA
                               OF TXARY(TXARY-IDX LCL-RCP-IDX)
                            TO SAV-RESIDENCE OF W-WK
                   END-IF
               END-IF
           ELSE
               IF RES-STATE OF LOCALITY-DATA
                            OF TXARY(TXARY-IDX, LCL-RCP-IDX) = 'PA'
                  AND RES-LOCALITY OF LOCALITY-DATA
                                OF TXARY(TXARY-IDX LCL-RCP-IDX) > SPACE

                   MOVE RES-LOCALITY OF LOCALITY-DATA
                               OF TXARY(TXARY-IDX LCL-RCP-IDX)
                               TO SAV-LOCALITY OF W-WK

                   IF  FIRST-CHAR-LOC OF SAV-LOCALITY NOT EQUAL 'O'

                       MOVE RES-LOCALITY OF LOCALITY-DATA
                               OF TXARY(TXARY-IDX LCL-RCP-IDX)
                            TO SAV-RESIDENCE OF W-WK
                   END-IF
               ELSE

                   MOVE SPACE TO SAV-WORK OF W-WK
                   MOVE SPACE TO SAV-RESIDENCE OF W-WK
               END-IF
           END-IF

           IF RECIPROCITY-RULE-BOTH
                               OF LOCAL-LOCAL(TXARY-IDX LCL-RCP-IDX)

               MOVE 0          TO REDUCE-PCT OF W-WK
               MOVE 0          TO CREDIT-LIM-RT OF W-WK

               IF  STATE OF TXARY(TXARY-IDX) NOT EQUAL 'PA'
                   AND RES-STATE OF TXARY(TXARY-IDX LCL-RCP-IDX) = 'PA'
                   AND RES-LOCALITY OF TXARY(TXARY-IDX LCL-RCP-IDX)
                                    NOT EQUAL SPACE

                   MOVE '880000' TO SAV-WORK OF W-WK
               END-IF

               IF STATE OF TXARY(TXARY-IDX) = 'PA'
                   AND TAX-CLASS-WITHHOLDING OF TXARY(TXARY-IDX)
                   AND CHECK-DT OF CHECK > '2011-12-31'

                   PERFORM LH536-FIND-WORK-PA-TARRY

                   IF  TARRY-FOR-WORK-NO OF W-SW

                       MOVE '880000' TO SAV-WORK OF W-WK
                   END-IF

                   IF  TARRY-FOR-WORK-YES OF W-SW

                       MOVE SAV-RESIDENCE OF W-WK
                            TO RES-PSD-CD OF TAXCALC-DATA
                            OF TARRY(TARRY-IDX)
                   END-IF
               ELSE
                   IF  RES-STATE OF LOCALITY-DATA
                               OF TXARY(TXARY-IDX LCL-RCP-IDX) = 'PA'
                       AND RES-LOCALITY OF LOCALITY-DATA
                               OF TXARY(TXARY-IDX LCL-RCP-IDX) > SPACE

                       PERFORM LH533-FIND-PA-RES-TARRY

                       IF  TARRY-FOR-RES-YES OF W-SW

                           MOVE '880000' TO SAV-WORK OF W-WK
                           MOVE SAV-WORK OF W-WK TO
                                WORK-PSD-CD OF TAXCALC-DATA
                                            OF TARRY(TARRY-IDX)

                       END-IF
                   END-IF

                   PERFORM LH550-FIND-TARRY
               END-IF
           END-IF

           IF RECIPROCITY-RULE-REDRES
                               OF LOCAL-LOCAL(TXARY-IDX LCL-RCP-IDX)

               MOVE REDUCE-PCT OF LOCALITY-DATA
                               OF TXARY(TXARY-IDX LCL-RCP-IDX)
                                       TO REDUCE-PCT OF W-WK
               MOVE CREDIT-LIM-RT OF LOCALITY-DATA
                               OF TXARY(TXARY-IDX LCL-RCP-IDX)
                                       TO CREDIT-LIM-RT OF W-WK
           END-IF

           IF RECIPROCITY-RULE-REDWRK
                               OF LOCAL-LOCAL (TXARY-IDX LCL-RCP-IDX)

               PERFORM LH550-FIND-TARRY
               IF  SPECIAL-LWT-STATUS-NONE OF TARRY(TARRY-IDX)
                       AND ONE-TIME-NA OF TARRY(TARRY-IDX)

                   MOVE REDUCE-PCT OF LOCALITY-DATA
                               OF TXARY(TXARY-IDX LCL-RCP-IDX)
                                       TO REDUCE-PCT OF W-WK
                   MOVE CREDIT-LIM-RT OF LOCALITY-DATA
                               OF TXARY(TXARY-IDX LCL-RCP-IDX)
                                       TO CREDIT-LIM-RT OF W-WK
                   PERFORM LH525-FIND-RES-TXARY

                   COMPUTE ACTUAL-TAX-CUR OF TAX-SET-DATA
                                       OF TXARY (TXARY-IDX)
                       =   ACTUAL-TAX-CUR OF TAX-SET-DATA
                                       OF TXARY (TXARY-IDX)
                       -   RES-TAX OF W-WK

                   COMPUTE CALCED-TAX-CUR OF TAX-SET-DATA
                                       OF TXARY (TXARY-IDX)
                       =   CALCED-TAX-CUR OF TAX-SET-DATA
                                       OF TXARY (TXARY-IDX)
                       -   RES-TAX OF W-WK

                   SET REDWRK-ADJ-TARRY OF W-SW  TO  TRUE

                   IF ACTUAL-TAX-CUR OF TAX-SET-DATA
                                       OF TXARY(TXARY-IDX) < 0

                       MOVE ZERO TO ACTUAL-TAX-CUR OF TAX-SET-DATA
                                       OF TXARY(TXARY-IDX)
                   END-IF

                   IF CALCED-TAX-CUR OF TAX-SET-DATA
                                       OF TXARY(TXARY-IDX) < 0

                       MOVE ZERO TO CALCED-TAX-CUR OF TAX-SET-DATA
                                       OF TXARY(TXARY-IDX)
                   END-IF

                   COMPUTE RES-TAX-TOTAL OF W-WK
                       =   RES-TAX-TOTAL OF W-WK
                       +   RES-TAX OF W-WK

                   MOVE ZEROS  TO  RES-TAX OF W-WK
               END-IF
           END-IF

           IF RECIPROCITY-RULE-HIGHER
                               OF LOCAL-LOCAL (TXARY-IDX LCL-RCP-IDX)
                 AND  TAX-CLASS-WITHHOLDING OF TXARY(TXARY-IDX)

               SET PA-WRK-TXGRS-FOUND-NO TO TRUE

               PERFORM LH534-FIND-PA-RES-TXARY

      *            LOOKING FOR ANOTHER PA LOCALITY AS RESIDENCE

               IF  TXARY-FOR-RES-YES OF W-SW

                   PERFORM LH545-GET-LOCAL-RATE-RES

                   IF STATE OF TXARY(TXARY-IDX) NOT EQUAL 'PA'
                       COMPUTE  RES-TAX-RECALC OF W-WK ROUNDED
                             =  TXGRS-CUR OF TAX-SET-DATA
                                      OF TXARY(TXARY-IDX)
                             *  WK-LCLPCT-RES
                   ELSE
                       COMPUTE  RES-TAX-RECALC OF W-WK ROUNDED
                             =  (TXGRS-CUR OF TAX-SET-DATA
                                      OF TXARY(TXARY-IDX)
                             -  WRK-TGC-WH-ADJ
                                      OF TXARY(TXARY-IDX))
                             *  WK-LCLPCT-RES
                   END-IF

                   COMPUTE  RES-TXGRS-RECALC OF W-WK ROUNDED
                         =  RES-TXGRS-RECALC OF W-WK
                         +  TXGRS-CUR OF TAX-SET-DATA
                               OF TXARY(TXARY-IDX)

                   SET TARRY-FOR-WORK-NO OF W-SW TO TRUE

                   PERFORM LH536-FIND-WORK-PA-TARRY

                   IF TARRY-FOR-WORK-YES OF W-SW

                       MOVE RES-TAX-RECALC OF W-WK
                            TO RES-TAX-RECALC-SAV OF W-WK
                       MOVE ACTUAL-TAX-CUR OF TAX-SET-DATA
                                              OF TXARY(TXARY-IDX)
                            TO ACTUAL-TAX-CUR-SAV OF W-WK

                       IF  SPECIAL-LWT-STATUS-NONE OF TARRY(TARRY-IDX)

                           IF  (RES-TAX-RECALC OF W-WK > ZERO AND
                                  RES-TAX-RECALC OF W-WK
                                       NOT < ACTUAL-TAX-CUR
                                  OF TAX-SET-DATA OF TXARY(TXARY-IDX))
                           OR
                               (RES-TAX-RECALC OF W-WK < ZERO AND
                                  RES-TAX-RECALC OF W-WK
                                       < ACTUAL-TAX-CUR
                                  OF TAX-SET-DATA OF TXARY(TXARY-IDX))

                               IF  ONE-TIME-NA OF TARRY(TARRY-IDX)

                                   MOVE ACTUAL-TAX-CUR OF TAX-SET-DATA
                                       OF TXARY(TXARY-IDX)
                                       TO RES-TAX-PRIOR OF W-WK

                                   MOVE RES-TAX-RECALC OF W-WK
                                       TO ACTUAL-TAX-CUR OF TAX-SET-DATA
                                       OF TXARY(TXARY-IDX)

                                   MOVE RES-TAX-RECALC OF W-WK
                                        TO CALCED-TAX-CUR
                                           OF TAX-SET-DATA
                                           OF TXARY(TXARY-IDX)

                                   COMPUTE ACTUAL-TAX-CUR
                                           OF TAX-SET-DATA
                                        OF TXARY(OTH-TXARY-IDX) ROUNDED
                                    =  ACTUAL-TAX-CUR OF TAX-SET-DATA
                                       OF TXARY(OTH-TXARY-IDX)
                                    -  RES-TAX-RECALC OF W-WK

                                   COMPUTE ACTUAL-TAX-CUR
                                            OF TAX-SET-DATA
                                       OF TXARY(OTH-TXARY-IDX) ROUNDED
                                    =  ACTUAL-TAX-CUR OF TAX-SET-DATA
                                       OF TXARY(OTH-TXARY-IDX)
                                    -  RES-TAX-RECALC OF W-WK

                                   COMPUTE TXGRS-CUR OF TAX-SET-DATA
                                       OF TXARY(OTH-TXARY-IDX) ROUNDED
                                     = TXGRS-CUR OF TAX-SET-DATA
                                           OF TXARY(OTH-TXARY-IDX)
                                     - RES-TXGRS-RECALC OF W-WK
                                ELSE

                                   COMPUTE ACTUAL-TAX-CUR
                                           OF TAX-SET-DATA
                                        OF TXARY(TXARY-IDX) ROUNDED
                                    =  ACTUAL-TAX-CUR OF TAX-SET-DATA
                                       OF TXARY(TXARY-IDX)
                                    -  ACTUAL-TAX-CUR OF TAX-SET-DATA
                                       OF TXARY(TXARY-IDX)

                                   COMPUTE TXGRS-CUR OF TAX-SET-DATA
                                       OF TXARY(OTH-TXARY-IDX) ROUNDED
                                     = TXGRS-CUR OF TAX-SET-DATA
                                           OF TXARY(OTH-TXARY-IDX)
                                     - RES-TXGRS-RECALC OF W-WK
                                  END-IF

                                COMPUTE RES-TAX-TOTAL OF W-WK
                                    =   RES-TAX-TOTAL OF W-WK
                                    +   RES-TAX OF W-WK

                                SET HIGHER-ADJ-TARRY OF W-SW  TO  TRUE
                           ELSE

                               IF TXGRS-CUR OF TAX-SET-DATA
                                            OF TXARY(TXARY-IDX)
                                        NOT EQUAL ZERO

                                   SET PA-WRK-TXGRS-FOUND-YES OF W-SW
                                                              TO TRUE
                               END-IF

                               IF  TXARY-FOR-RES-YES OF W-SW

                                   MOVE ZERO TO ACTUAL-TAX-CUR
                                           OF TAX-SET-DATA
                                           OF TXARY (OTH-TXARY-IDX)

                                   MOVE ZERO TO CALCED-TAX-CUR
                                           OF TAX-SET-DATA
                                           OF TXARY (OTH-TXARY-IDX)

                                   COMPUTE RES-TAX-LESS OF W-WK ROUNDED
                                         = RES-TAX-LESS OF W-WK
                                         + RES-TAX-RECALC OF W-WK

                                   COMPUTE TXGRS-CUR OF TAX-SET-DATA
                                              OF TXARY(OTH-TXARY-IDX)
                                         = TXGRS-CUR OF TAX-SET-DATA
                                              OF TXARY(OTH-TXARY-IDX)
                                         - RES-TXGRS-RECALC OF W-WK

                                   MOVE ZERO TO RES-TAX-RECALC OF W-WK

                                   SET HIGHER-ADJ-TARRY OF W-SW TO TRUE
                               END-IF
                           END-IF
                           MOVE ZEROS  TO  RES-TAX OF W-WK
                       END-IF
                           SET HIGHER-ADJ-TARRY OF W-SW TO TRUE
                   END-IF
               END-IF
           END-IF

           IF RECIPROCITY-RULE-NONE
                               OF LOCAL-LOCAL(TXARY-IDX LCL-RCP-IDX)
               IF  STATE OF TXARY(TXARY-IDX) = 'PA'
                   AND TAX-CLASS-WITHHOLDING OF TXARY(TXARY-IDX)
                   AND CHECK-DT OF CHECK > '2011-12-31'

                   IF  LOCALITY OF TXARY(TXARY-IDX)
                       = RES-LOCALITY OF TXARY(TXARY-IDX LCL-RCP-IDX)

                       SET TARRY-FOR-WORK-YES OF W-SW TO TRUE
                   ELSE

                       PERFORM LH536-FIND-WORK-PA-TARRY
                   END-IF

                   IF  TARRY-FOR-WORK-NO OF W-SW

                       MOVE '880000' TO SAV-WORK OF W-WK
                   END-IF

                   PERFORM LH534-FIND-PA-RES-TXARY

                   IF  TXARY-FOR-RES-YES OF W-SW

                       PERFORM LH532-FIND-PA-RES-TARRY

                       SET HIGHER-ADJ-TARRY OF W-SW TO TRUE

                       MOVE ZEROS  TO  RES-TAX OF W-WK

                   END-IF
               ELSE

                   IF  RES-STATE(TXARY-IDX LCL-RCP-IDX) = 'PA'
                       AND RES-LOCALITY(TXARY-IDX LCL-RCP-IDX) > SPACE
                       AND CHECK-DT OF CHECK > '2011-12-31'

                       MOVE '880000' TO SAV-WORK OF W-WK

                       PERFORM LH533-FIND-PA-RES-TARRY

                           MOVE SAV-WORK OF W-WK TO
                                WORK-PSD-CD OF TARRY(TARRY-IDX)

                           MOVE SAV-RESIDENCE OF W-WK TO
                                RES-PSD-CD OF TARRY(TARRY-IDX)
                   END-IF
               END-IF
           END-IF

           IF (RECIPROCITY-RULE-REDTTL
                               OF LOCAL-LOCAL(TXARY-IDX LCL-RCP-IDX)
               AND STATE OF TXARY(TXARY-IDX) = 'PA'
               AND TAX-CLASS-WITHHOLDING OF TXARY(TXARY-IDX)
               AND CHECK-DT OF CHECK > '2011-12-31')

               PERFORM LH536-FIND-WORK-PA-TARRY

               IF TARRY-FOR-WORK-YES OF W-SW

                  MOVE SAV-RESIDENCE OF W-WK TO
                           RES-PSD-CD OF TAXCALC-DATA
                                OF TARRY(TARRY-IDX)
                  MOVE TXGRS-CUR OF TARRY(TARRY-IDX)
                       TO SAV-WORK-TXGRS OF W-WK

               END-IF

               PERFORM LH532-FIND-PA-RES-TARRY

               MOVE ZERO TO SAV-RES-TXGRS OF W-WK

               IF TARRY-IDX  NOT >  TAXCALC-COUNT OF TARRY
                   IF TXGRS-CUR OF TARRY(TARRY-IDX) < ZERO

                      SET NEGATIVE-TAX-REQ-YES OF W-SW TO TRUE

                   END-IF
               END-IF

               IF TARRY-FOR-RES-YES OF W-SW

                   COMPUTE SAV-RES-TXGRS OF W-WK
                         = TXGRS-CUR OF TARRY(TARRY-IDX)
                         - SAV-WORK-TXGRS OF W-WK

                   IF (SAV-RES-TXGRS OF W-WK > .01
                       OR SAV-RES-TXGRS OF W-WK < .01)

                       AND TXGRS-CUR OF TARRY(TARRY-IDX)
                       NOT EQUAL SAV-WORK-TXGRS OF W-WK

      *            TAX AND TXGRS LESS THAN EARNINGS BECAUSE OF
      *            PRE-TAX DEDUCTIONS FROM RES BUT NOT WORK

                       IF  TXGRS-CUR OF TARRY(TARRY-IDX) <  ZERO


                           MOVE ZERO TO TXGRS-CUR OF TARRY(TARRY-IDX)
                       END-IF

                       IF TAX-CUR OF TARRY(TARRY-IDX) < ZERO

                           MOVE ZERO TO TAX-CUR OF TARRY(TARRY-IDX)
                       END-IF
                   END-IF
               END-IF
           END-IF


           IF RECIPROCITY-RULE-NOWORK
                               OF LOCAL-LOCAL (TXARY-IDX LCL-RCP-IDX)

               PERFORM LH550-FIND-TARRY
               IF  SPECIAL-LWT-STATUS-NONE OF TARRY(TARRY-IDX)
                       AND ONE-TIME-NA OF TARRY(TARRY-IDX)

                   PERFORM LH525-FIND-RES-TXARY

                   IF RES-TAX OF W-WK  NOT =  ZEROS
                           OR RES-TXBL OF W-WK NOT = ZERO

                       MOVE ZERO  TO  ACTUAL-TAX-CUR OF TAX-SET-DATA
                                       OF TXARY(TXARY-IDX)
                       MOVE ZERO  TO  CALCED-TAX-CUR OF TAX-SET-DATA
                                       OF TXARY(TXARY-IDX)
                       MOVE ZERO  TO  TXGRS-CUR OF TAX-SET-DATA
                                       OF TXARY(TXARY-IDX)
                       SET NOWRK-ADJ-TARRY OF W-SW TO TRUE
                   END-IF
               END-IF
           END-IF

            .
       CHECK-RECIP-RULE-EXIT.

      /*****************************************************************
      *                                                                *
       LH520-FIND-RES-TARRY SECTION.
       LH520.
      *                                                                *
      ******************************************************************


            SET TARRY-IDX TO 1

            SEARCH TAXCALC-DATA OF TARRY

                WHEN TARRY-IDX > TAXCALC-COUNT OF TARRY

                    CONTINUE

                WHEN STATE OF TARRY(TARRY-IDX)
                   = STATE OF TXARY(TXARY-IDX)
                   AND LOCALITY OF TARRY(TARRY-IDX) NOT = SPACE
                   AND RESIDENT-YES OF TARRY(TARRY-IDX)

                CONTINUE

            END-SEARCH

            .
        FIND-RES-TARRY-EXIT.
      /*****************************************************************
      *                                                                *
       LH525-FIND-RES-TXARY SECTION.
       LH525.
      *                                                                *
      ******************************************************************

           PERFORM VARYING OTH-TXARY-IDX FROM 1  BY  1
                   UNTIL OTH-TXARY-IDX > TAX-SET-CNT OF TXARY

               IF PAY-FREQ-TYPE OF TXARY (TXARY-IDX)
                       = PAY-FREQ-TYPE OF TXARY (OTH-TXARY-IDX)
                       AND  TAX-PERIODS OF TXARY (TXARY-IDX)
                       =    TAX-PERIODS OF TXARY (OTH-TXARY-IDX)
                       AND  BENEFIT-RCD-NO OF TXARY (TXARY-IDX)
                       =    BENEFIT-RCD-NO OF TXARY (OTH-TXARY-IDX)
                       AND  (PAID-PRDS-PER-YEAR OF TXARY (TXARY-IDX)    HP00001
                       =    PAID-PRDS-PER-YEAR OF TXARY (OTH-TXARY-IDX) HP99999
                       AND  FICA-STATUS-EE OF TXARY (TXARY-IDX)         HP99999
                       =    FICA-STATUS-EE OF TXARY (OTH-TXARY-IDX))    HP99999
                       AND  TAX-CLASS OF TXARY (TXARY-IDX)
                       =    TAX-CLASS OF TXARY (OTH-TXARY-IDX)
                       AND  RES-STATE OF LOCALITY-DATA
                            OF TXARY (TXARY-IDX LCL-RCP-IDX)
                       =    STATE OF TXARY (OTH-TXARY-IDX)
                       AND (RES-LOCALITY OF LOCALITY-DATA
                            OF TXARY (TXARY-IDX LCL-RCP-IDX)
                       =    LOCALITY OF TXARY (OTH-TXARY-IDX)

      *  MD LOCALITY (BUT NOT RES-LOCALITY) IS SPACE AFTER 12/31/99

                       OR  (RES-STATE OF LOCALITY-DATA
                            OF TXARY (TXARY-IDX LCL-RCP-IDX)
                       =   'MD'
                       AND  LOCALITY OF TXARY(OTH-TXARY-IDX)
                       =    SPACE))

                   IF (TAX-METHOD OF TXARY (TXARY-IDX) =
                       TAX-METHOD OF TXARY (OTH-TXARY-IDX)) OR
                      (TAX-METHOD-SUPPLEMENTAL OF TXARY (TXARY-IDX) AND
                       TAX-METHOD-SUPPLEMENTAL OF TXARY (OTH-TXARY-IDX))
                      MOVE ACTUAL-TAX-CUR OF TXARY (OTH-TXARY-IDX)
                           TO RES-TAX OF W-WK
                      MOVE TXGRS-CUR OF TXARY(OTH-TXARY-IDX)
                           TO RES-TXBL OF W-WK
                   END-IF

               END-IF
           END-PERFORM

           COMPUTE RES-TAX OF W-WK = ( RES-TAX OF W-WK  *
                                      REDUCE-PCT OF W-WK ) / 100


            .
       FIND-RES-TXARY-EXIT.


      /*****************************************************************
      *                                                                *
       LH530-RECALC-RES-TAX SECTION.
       LH530.
      *                                                                *
      ******************************************************************

           IF LOCALITY OF TXARY(TXARY-IDX) NOT =
               RES-LOCALITY OF TXARY(TXARY-IDX LCL-RCP-IDX)
               AND RES-LOCALITY OF TXARY(TXARY-IDX LCL-RCP-IDX)
               NOT = SPACE
               AND RECIPROCITY-RULE-REDRES OF LOCAL-LOCAL
               (TXARY-IDX LCL-RCP-IDX)

               IF ACTUAL-TAX-CUR OF TXARY(TXARY-IDX)  <  ZERO
                       AND CALCED-TAX-CUR OF TXARY(TXARY-IDX)  <  ZERO

                   IF ACTUAL-TAX-CUR OF TXARY(TXARY-IDX)
                                <  CALCED-TAX-CUR OF TXARY(TXARY-IDX)
                       AND TAX OF LOCAL-RCP OF GRSWK(LRCWK-IDX)  <  ZERO
                       AND NOT TAX-METHOD-SUPPLEMENTAL
                                OF TAX-SET-DATA OF TXARY(TXARY-IDX)

                      COMPUTE TAX OF LOCAL-RCP OF GRSWK(LRCWK-IDX)
                            = TAX OF LOCAL-RCP OF GRSWK(LRCWK-IDX)
                            + CALCED-TAX-CUR OF TXARY(TXARY-IDX)
                            - ACTUAL-TAX-CUR OF TXARY(TXARY-IDX)
                   END-IF

                   SET NEG-LOCAL-RCP-YES  TO  TRUE
               ELSE
                   COMPUTE TAX OF LOCAL-RCP OF GRSWK(LRCWK-IDX)
                         = TAX OF LOCAL-RCP OF GRSWK(LRCWK-IDX)
                         - ACTUAL-TAX-CUR OF TXARY(TXARY-IDX)
                         + CALCED-TAX-CUR OF TXARY(TXARY-IDX)

               END-IF
           ELSE
           IF LOCALITY OF TXARY(TXARY-IDX) NOT =
              RES-LOCALITY OF TXARY(TXARY-IDX LCL-RCP-IDX)
              AND RES-LOCALITY OF TXARY(TXARY-IDX LCL-RCP-IDX)
              NOT = SPACE
              AND RECIPROCITY-RULE-REDRTE OF LOCAL-LOCAL
              (TXARY-IDX LCL-RCP-IDX)
              AND (EARNS-TTL OF GRSWK NOT = ZERO)

                   PERFORM LH540-GET-LOCAL-RATE
                   PERFORM LH520-FIND-RES-TARRY
                   SET SAV-TARRY-IDX TO TARRY-IDX

                   COMPUTE W-ADJ OF W-WK
                         = EARNS-TTL OF GRSWK
                         - TXGRS-CUR OF TARRY(SAV-TARRY-IDX)
                   COMPUTE TAX OF LOCAL-RCP OF GRSWK(LRCWK-IDX)
                           ROUNDED
                         = (TAX OF LOCAL-RCP OF GRSWK(LRCWK-IDX)
                         - ACTUAL-TAX-CUR OF TXARY(TXARY-IDX)
                         + ((TOT-GRS-CUR OF TXARY(TXARY-IDX)
                         - ((TOT-GRS-CUR OF TXARY(TXARY-IDX)
                         / EARNS-TTL OF GRSWK)
                         * W-ADJ)) * WK-LCLPCT))
                         + HIGH-PREC-0

           END-IF

           .

       RECALC-RES-TAX-EXIT.

      /*****************************************************************
      *                                                                *
       LH532-FIND-PA-RES-TARRY SECTION.
       LH532.
      *                                                                *
      ******************************************************************



            SET TARRY-FOR-RES-NO OF W-SW TO TRUE

            SET TARRY-IDX TO 1

            SEARCH TAXCALC-DATA OF TARRY

                WHEN TARRY-IDX > TAXCALC-COUNT OF TARRY

                    CONTINUE

                WHEN STATE OF TARRY(TARRY-IDX)
                   = STATE OF TXARY(TXARY-IDX)
                   AND LOCALITY OF TARRY(TARRY-IDX) NOT = SPACE
                   AND RESIDENT-YES OF TARRY(TARRY-IDX)
                   AND TAX-CLASS-WITHHOLDING OF TARRY(TARRY-IDX)

                       SET TARRY-FOR-RES-YES OF W-SW TO TRUE

                       IF NOT MANUAL-CHECK-YES OF CHECK

                           MOVE SAV-RESIDENCE OF W-WK TO
                               RES-PSD-CD OF TAXCALC-DATA
                                    OF TARRY(TARRY-IDX)
                       END-IF


            END-SEARCH

            .

        FIND-PA-RES-TARRY-EXIT.

      /*****************************************************************
      *                                                                *
       LH533-FIND-PA-RES-TARRY SECTION.
       LH533.
      *                                                                *
      ******************************************************************


            SET TARRY-FOR-RES-NO OF W-SW TO TRUE

            SET TARRY-IDX TO 1

            SEARCH TAXCALC-DATA OF TARRY

                WHEN TARRY-IDX > TAXCALC-COUNT OF TARRY

                    CONTINUE

                WHEN STATE OF TARRY(TARRY-IDX)
                   = RES-STATE OF LOCALITY-DATA
                               OF TXARY(TXARY-IDX LCL-RCP-IDX)
                   AND LOCALITY OF TARRY(TARRY-IDX) NOT = SPACE
                   AND RESIDENT-YES OF TARRY(TARRY-IDX)
                   AND TAX-CLASS-WITHHOLDING OF TARRY(TARRY-IDX)

                       SET TARRY-FOR-RES-YES OF W-SW TO TRUE


            END-SEARCH

            .

        FIND-PA-RES-TARRY-EXIT.


      /*****************************************************************
      *                                                                *
       LH534-FIND-PA-RES-TXARY SECTION.
       LH534.
      *                                                                *
      ******************************************************************

           SET TXARY-FOR-RES-NO OF W-SW TO TRUE

           MOVE TAX-METHOD OF TXARY(TXARY-IDX) TO
                    TAX-METHOD OF W-WK
           IF TAX-METHOD OF TXARY(TXARY-IDX) = 'X'
               MOVE 'S' TO TAX-METHOD OF W-WK
           END-IF

           PERFORM VARYING OTH-TXARY-IDX FROM 1  BY  1
                   UNTIL OTH-TXARY-IDX > TAX-SET-CNT OF TXARY

               MOVE TAX-METHOD OF TXARY(OTH-TXARY-IDX) TO
                    TAX-METHOD1 OF W-WK
               IF TAX-METHOD OF TXARY(OTH-TXARY-IDX) = 'X'
                  MOVE 'S' TO TAX-METHOD1 OF W-WK
               END-IF

               IF PAY-FREQ-TYPE OF TXARY (TXARY-IDX)
                       = PAY-FREQ-TYPE OF TXARY (OTH-TXARY-IDX)
                       AND  TAX-PERIODS OF TXARY (TXARY-IDX)
                       =    TAX-PERIODS OF TXARY (OTH-TXARY-IDX)
                       AND  BENEFIT-RCD-NO OF TXARY (TXARY-IDX)
                       =    BENEFIT-RCD-NO OF TXARY (OTH-TXARY-IDX)
                       AND  (PAID-PRDS-PER-YEAR OF TXARY (TXARY-IDX)    HP00001
                       =    PAID-PRDS-PER-YEAR OF TXARY (OTH-TXARY-IDX) HP99999
                       AND  FICA-STATUS-EE OF TXARY (TXARY-IDX)         HP99999
                       =    FICA-STATUS-EE OF TXARY (OTH-TXARY-IDX))    HP99999
                       AND  TAX-CLASS OF TXARY (TXARY-IDX)
                       =    TAX-CLASS OF TXARY (OTH-TXARY-IDX)
                       AND  RES-STATE OF LOCALITY-DATA
                            OF TXARY (TXARY-IDX LCL-RCP-IDX)
                       =    STATE OF TXARY (OTH-TXARY-IDX)
                       AND (RES-LOCALITY OF LOCALITY-DATA
                            OF TXARY (TXARY-IDX LCL-RCP-IDX)
                       =    LOCALITY OF TXARY (OTH-TXARY-IDX)
                       AND  TAX-METHOD OF W-WK
                       =    TAX-METHOD1 OF W-WK
                       AND  TAX-CLASS-WITHHOLDING
                            OF TXARY(OTH-TXARY-IDX)

      *  MD LOCALITY (BUT NOT RES-LOCALITY) IS SPACE AFTER 12/31/99

                       OR  (RES-STATE OF LOCALITY-DATA
                            OF TXARY (TXARY-IDX LCL-RCP-IDX)
                       =   'MD'
                       AND  LOCALITY OF TXARY(OTH-TXARY-IDX)
                       =    SPACE))

                   IF (TAX-METHOD OF TXARY (TXARY-IDX) =
                       TAX-METHOD OF TXARY (OTH-TXARY-IDX)) OR
                      (TAX-METHOD-SUPPLEMENTAL OF TXARY (TXARY-IDX) AND
                       TAX-METHOD-SUPPLEMENTAL OF TXARY (OTH-TXARY-IDX))

                      MOVE ACTUAL-TAX-CUR OF TXARY (OTH-TXARY-IDX)
                           TO RES-TAX OF W-WK
                      MOVE TXGRS-CUR OF TXARY(OTH-TXARY-IDX)
                           TO RES-TXBL OF W-WK
                      MOVE LOCALITY OF TXARY (OTH-TXARY-IDX)
                           TO SAV-RESIDENCE OF W-WK

                      SET RES-TXARY-IDX OF W-WK TO OTH-TXARY-IDX
                      SET TXARY-FOR-RES-YES OF W-SW TO TRUE

                   END-IF
               END-IF
           END-PERFORM

           IF REDUCE-PCT OF W-WK > ZERO

           COMPUTE RES-TAX OF W-WK = ( RES-TAX OF W-WK  *
                                      REDUCE-PCT OF W-WK ) / 100
           END-IF


            .
       FIND-PA-RES-TXARY-EXIT.

      /*****************************************************************
      *                                                                *
       LH536-FIND-WORK-PA-TARRY SECTION.
       LH536.
      *                                                                *
      ******************************************************************

           SET TARRY-FOR-WORK-NO OF W-SW TO TRUE

           SET TARRY-IDX  TO  1

           SEARCH TAXCALC-DATA OF TARRY

               WHEN TARRY-IDX  >  TAXCALC-COUNT OF TARRY

                   CONTINUE

               WHEN  STATE OF TARRY (TARRY-IDX)
                   = STATE OF TXARY (TXARY-IDX)
                   AND  LOCALITY OF TXARY (TXARY-IDX)
                     = WORK-PSD-CD OF LOCALITY-DATA
                              OF TXARY (TXARY-IDX LCL-RCP-IDX)
                   AND   TAX-CLASS OF TARRY (TARRY-IDX)
                     = TAX-CLASS OF TXARY (TXARY-IDX)
                   AND  TAX-CLASS-WITHHOLDING OF TARRY(TARRY-IDX)
                   AND LOCALITY OF TARRY(TARRY-IDX)
                       NOT = SPACE
                   AND WORK-PSD-CD OF TAXCALC-DATA OF TARRY(TARRY-IDX)
                       NOT = SPACE
                   AND WORK-PSD-CD OF TAXCALC-DATA
                                OF TARRY(TARRY-IDX)
                     = WORK-PSD-CD OF LOCALITY-DATA
                              OF TXARY (TXARY-IDX LCL-RCP-IDX)
                   AND RES-PSD-CD OF TAXCALC-DATA
                                OF TARRY(TARRY-IDX)
                       = RES-PSD-CD OF LOCALITY-DATA
                              OF TXARY (TXARY-IDX LCL-RCP-IDX)
                   AND RESIDENT-NA OF TARRY(TARRY-IDX)

                       IF TAX-METHOD-SUPPLEMENTAL OF TXARY (TXARY-IDX)

                           COMPUTE RES-TAX-SUPPL OF W-WK
                                 =  TAX-CUR OF TARRY(TARRY-IDX)
                       END-IF

                       SET TARRY-FOR-WORK-YES OF W-SW TO TRUE

               WHEN  STATE OF TARRY (TARRY-IDX)
                   = STATE OF TXARY (TXARY-IDX)
                   AND   LOCALITY OF TARRY (TARRY-IDX)
                       = LOCALITY OF TXARY (TXARY-IDX)
                       AND   TAX-CLASS OF TARRY (TARRY-IDX)
                           = TAX-CLASS OF TXARY (TXARY-IDX)
                       AND  TAX-CLASS-WITHHOLDING OF TARRY(TARRY-IDX)
                       AND LOCALITY OF TARRY(TARRY-IDX)
                           NOT = SPACE
                       AND RESIDENT-NA OF TARRY(TARRY-IDX)
                       AND WORK-PSD-CD OF TAXCALC-DATA
                                OF TARRY(TARRY-IDX)
                           = WORK-PSD-CD OF LOCALITY-DATA
                              OF TXARY (TXARY-IDX LCL-RCP-IDX)

                       SET TARRY-FOR-WORK-YES OF W-SW TO TRUE

           END-SEARCH

            .

       FIND-WORK-PA-TARRY-EXIT.

      /*****************************************************************
      *                                                                *
       LH538-FIND-PA-RES-TXARY SECTION.
       LH538.
      *                                                                *
      ******************************************************************


           SET TXARY-FOR-RES-NO OF W-SW TO TRUE

           MOVE TAX-METHOD OF TXARY(TXARY-IDX) TO
                    TAX-METHOD OF W-WK
           IF TAX-METHOD OF TXARY(TXARY-IDX) = 'X'
               MOVE 'S' TO TAX-METHOD OF W-WK
           END-IF

           PERFORM VARYING OTH-TXARY-IDX FROM 1  BY  1
                   UNTIL OTH-TXARY-IDX > TAX-SET-CNT OF TXARY

               MOVE TAX-METHOD OF TXARY(OTH-TXARY-IDX) TO
                    TAX-METHOD1 OF W-WK
               IF TAX-METHOD OF TXARY(OTH-TXARY-IDX) = 'X'
                  MOVE 'S' TO TAX-METHOD1 OF W-WK
               END-IF

               IF PAY-FREQ-TYPE OF TXARY (TXARY-IDX)
                       = PAY-FREQ-TYPE OF TXARY (OTH-TXARY-IDX)
                       AND  TAX-PERIODS OF TXARY (TXARY-IDX)
                       =    TAX-PERIODS OF TXARY (OTH-TXARY-IDX)
                       AND  BENEFIT-RCD-NO OF TXARY (TXARY-IDX)
                       =    BENEFIT-RCD-NO OF TXARY (OTH-TXARY-IDX)
                       AND  (PAID-PRDS-PER-YEAR OF TXARY (TXARY-IDX)    HP00001
                       =    PAID-PRDS-PER-YEAR OF TXARY (OTH-TXARY-IDX) HP99999
                       AND  FICA-STATUS-EE OF TXARY (TXARY-IDX)         HP99999
                       =    FICA-STATUS-EE OF TXARY (OTH-TXARY-IDX))    HP99999
                       AND  TAX-CLASS OF TXARY (TXARY-IDX)
                       =    TAX-CLASS OF TXARY (OTH-TXARY-IDX)
                       AND  RES-STATE OF LOCALITY-DATA
                            OF TXARY (TXARY-IDX LCL-RCP-IDX)
                       =    STATE OF TXARY (OTH-TXARY-IDX)
                       AND (RES-LOCALITY OF LOCALITY-DATA
                            OF TXARY (TXARY-IDX LCL-RCP-IDX)
                       =    LOCALITY OF TXARY (OTH-TXARY-IDX)
                       AND  TAX-METHOD OF W-WK
                       =    TAX-METHOD1 OF W-WK
                       AND  TAX-CLASS-WITHHOLDING
                            OF TXARY(OTH-TXARY-IDX)

      *  MD LOCALITY (BUT NOT RES-LOCALITY) IS SPACE AFTER 12/31/99

                       OR  (RES-STATE OF LOCALITY-DATA
                            OF TXARY (TXARY-IDX LCL-RCP-IDX)
                       =   'MD'
                       AND  LOCALITY OF TXARY(OTH-TXARY-IDX)
                       =    SPACE))


                   IF (TAX-METHOD OF TXARY (TXARY-IDX) =
                       TAX-METHOD OF TXARY (OTH-TXARY-IDX)) OR
                      (TAX-METHOD-SUPPLEMENTAL OF TXARY (TXARY-IDX) AND
                       TAX-METHOD-SUPPLEMENTAL OF TXARY (OTH-TXARY-IDX))

                      MOVE ACTUAL-TAX-CUR OF TXARY (OTH-TXARY-IDX)
                           TO RES-TAX OF W-WK
                      MOVE TXGRS-CUR OF TXARY(OTH-TXARY-IDX)
                           TO RES-TXBL OF W-WK
                      MOVE LOCALITY OF TXARY (OTH-TXARY-IDX)
                           TO SAV-RESIDENCE OF W-WK

                      SET TXARY-FOR-RES-YES OF W-SW TO TRUE

                   END-IF
               END-IF
           END-PERFORM

           IF REDUCE-PCT OF W-WK > ZERO

           COMPUTE RES-TAX OF W-WK = ( RES-TAX OF W-WK  *
                                      REDUCE-PCT OF W-WK ) / 100
           END-IF


            .
       FIND-PA-RES-TXARY-EXIT.

      /*****************************************************************
      *                                                                *
       LH540-GET-LOCAL-RATE SECTION.
       LH540.
      *                                                                *
      ******************************************************************

           MOVE ZERO TO WK-LCLPCT OF W-WK

           SET LCLRT-IDX TO 1

           SEARCH LOCAL-TAX-RT-TABLE OF LCLRT

               AT END

                  CONTINUE

               WHEN STATE OF TXARY(TXARY-IDX)
                  = STATE OF LCLRT(LCLRT-IDX)
                  AND LOCALITY OF TXARY(TXARY-IDX)
                    = LOCALITY OF LCLRT(LCLRT-IDX)

                  IF LOC-TAX-TYPE-ERROR OF LCLRT(LCLRT-IDX)
                     MOVE ZERO TO WK-LCLPCT OF W-WK
                  ELSE
                     MOVE NONRESIDENT-TAX-RT OF LCLRT(LCLRT-IDX)
                          TO WK-LCLPCT OF W-WK
                     MOVE RESIDENT-TAX-RT OF LCLRT(LCLRT-IDX)
                          TO WK-LCLPCT-RES OF W-WK

                  END-IF
           END-SEARCH

            .
       GET-LOCAL-RATE-EXIT.

      /*****************************************************************
      *                                                                *
       LH545-GET-LOCAL-RATE-RES SECTION.
       LH545.
      *                                                                *
      ******************************************************************


           MOVE ZERO TO WK-LCLPCT OF W-WK

           SET LCLRT-IDX TO 1

           SEARCH LOCAL-TAX-RT-TABLE OF LCLRT

               AT END

                  CONTINUE

               WHEN RES-STATE OF LOCALITY-DATA
                            OF TXARY (TXARY-IDX LCL-RCP-IDX)
                  = STATE OF LCLRT(LCLRT-IDX)
                  AND RES-LOCALITY OF LOCALITY-DATA
                            OF TXARY (TXARY-IDX LCL-RCP-IDX)
                    = LOCALITY OF LCLRT(LCLRT-IDX)

                  IF LOC-TAX-TYPE-ERROR OF LCLRT(LCLRT-IDX)
                     MOVE ZERO TO WK-LCLPCT OF W-WK
                  ELSE
                     MOVE NONRESIDENT-TAX-RT OF LCLRT(LCLRT-IDX)
                          TO WK-LCLPCT OF W-WK
                     MOVE RESIDENT-TAX-RT OF LCLRT(LCLRT-IDX)
                          TO WK-LCLPCT-RES OF W-WK

                  END-IF
           END-SEARCH

            .
       GET-LOCAL-RATE-RES-EXIT.

      /*****************************************************************
      *                                                                *
       LH550-FIND-TARRY SECTION.
       LH550.
      *                                                                *
      ******************************************************************

           SET TARRY-IDX  TO  1

           SEARCH TAXCALC-DATA OF TARRY

               WHEN TARRY-IDX  >  TAXCALC-COUNT OF TARRY

                   CONTINUE

               WHEN  STATE OF TARRY (TARRY-IDX)
                   = STATE OF TXARY (TXARY-IDX)
                   AND   LOCALITY OF TARRY (TARRY-IDX)
                       = LOCALITY OF TXARY (TXARY-IDX)
                       AND   TAX-CLASS OF TARRY (TARRY-IDX)
                           = TAX-CLASS OF TXARY (TXARY-IDX)

                   CONTINUE
           END-SEARCH

            .
       FIND-TARRY-EXIT.

      /*****************************************************************
      *                                                                *
       LH555-GET-LOCAL-RES-RATE SECTION.
       LH555.
      *                                                                *
      ******************************************************************

           MOVE ZERO TO WK-LCLPCT-RES OF W-WK

           SET LCLRT-IDX TO 1

           SEARCH LOCAL-TAX-RT-TABLE OF LCLRT

               AT END

                  CONTINUE

               WHEN STATE OF RESIDENCE OF TAXWK
                  = STATE OF LCLRT(LCLRT-IDX)
                  AND LOCALITY OF RESIDENCE OF TAXWK(1)
                       = LOCALITY OF LCLRT(LCLRT-IDX)

                  IF LOC-TAX-TYPE-ERROR OF LCLRT(LCLRT-IDX)
                     MOVE ZERO TO WK-LCLPCT-RES OF W-WK
                  ELSE
                     MOVE RESIDENT-TAX-RT OF LCLRT(LCLRT-IDX)
                          TO WK-LCLPCT-RES OF W-WK

                  END-IF
           END-SEARCH

            .
       GET-LOCAL-RATE-EXIT.

      /*****************************************************************
      *                                                                *
       LH555-ACCUM-TXGRS SECTION.
       LH555.
      *                                                                *
      ******************************************************************

           MOVE ZERO TO PA-PHL-LOC-GRS-ADJ OF W-WK

           PERFORM VARYING WTARRY-IDX FROM 1 BY 1
                               UNTIL WTARRY-IDX
                                        >  TAXCALC-COUNT OF TARRY

               IF STATE OF TARRY(WTARRY-IDX)  = 'PA'
                  AND LOCALITY OF TARRY(WTARRY-IDX) NOT = SPACE
                  AND TAX-CLASS-WITHHOLDING OF TARRY(WTARRY-IDX)
                  AND WORK-PSD-CD OF TARRY(WTARRY-IDX) NOT = SPACE
                  AND WORK-PSD-CD OF TARRY(WTARRY-IDX)
                                    NOT = '880000'
                  AND RES-PSD-CD OF TARRY(WTARRY-IDX) NOT = SPACE

                  IF LOCALITY OF TARRY(WTARRY-IDX) NOT = '510101'
                     IF LOCALITY OF TARRY(WTARRY-IDX) =
                         PA-PREV-LOCALITY OF W-WK
                     OR  TXGRS-CUR OF TARRY(WTARRY-IDX) = ZERO
                         CONTINUE
                     ELSE
                         PERFORM LH572-FIND-TXARY
                         MOVE LOCALITY OF TARRY(WTARRY-IDX) TO
                          PA-PREV-LOCALITY OF W-WK
                     END-IF
                  ELSE
                     COMPUTE PA-PHL-LOC-GRS-ADJ OF W-WK
                                     = PA-PHL-LOC-GRS-ADJ OF W-WK
                                     + TXGRS-CUR OF TARRY(WTARRY-IDX)
                  END-IF
               END-IF
           END-PERFORM

            .
       ACCUM-TXGRS-EXIT.

      /*****************************************************************
      *                                                                *
       LH560-FIND-RES-88000-TARRY SECTION.
       LH560.
      *                                                                *
      ******************************************************************

           SET TARRY-IDX  TO  1

           SEARCH TAXCALC-DATA OF TARRY

               WHEN TARRY-IDX  >  TAXCALC-COUNT OF TARRY


                   SET TAXCALC-COUNT OF TARRY  TO  TARRY-IDX
                   INITIALIZE TAXCALC-DATA OF TARRY(TARRY-IDX)
                   MOVE 'PA'TO  STATE OF TARRY(TARRY-IDX)
                   MOVE LOCALITY OF RESIDENCE OF TAXWK(1)
                               TO  LOCALITY OF TARRY(TARRY-IDX)
                   MOVE 'H' TO  TAX-CLASS OF TARRY(TARRY-IDX)
                   MOVE '880000'
                               TO  WORK-PSD-CD OF TARRY(TARRY-IDX)

                   MOVE LOCALITY OF RESIDENCE OF TAXWK(1)
                               TO  RES-PSD-CD OF TARRY(TARRY-IDX)
                   SET RESIDENT-YES OF TARRY(TARRY-IDX) TO  TRUE
                   PERFORM LH575-FIND-RES-880000-TXARY

                   IF  TXGRS-CUR OF TXARY (TXARY-IDX) > ZERO

                     IF PA-BLANK-FOUND-YES OF W-SW
                        AND PHIL-LOCAL-FOUND-YES OF W-SW
                        AND NON-PA-ERN-FOUND-YES OF W-SW
                        AND PA-RES-ERN-FOUND-NO OF W-SW

                        MOVE ZERO TO TXGRS-CUR OF TARRY(TARRY-IDX)
                        MOVE ZERO TO TAX-CUR OF TARRY(TARRY-IDX)

                     ELSE

                       COMPUTE TXGRS-CUR OF TARRY(TARRY-IDX)
                       =  EARNS-TTL OF GRSWK
                          - PA-LOC-RES-TXGRS-ADJ OF W-WK
                          - PA-PHL-LOC-GRS-ADJ OF W-WK

                       COMPUTE TAX-CUR OF TARRY(TARRY-IDX)ROUNDED
                              = TXGRS-CUR OF TARRY(TARRY-IDX) *
                                WK-LCLPCT-RES OF W-WK
                     END-IF
                   END-IF


               WHEN  STATE OF TARRY (TARRY-IDX) = 'PA'
                   AND LOCALITY OF TARRY (TARRY-IDX)
                       = LOCALITY OF RESIDENCE OF TAXWK(1)
                   AND WORK-PSD-CD OF TARRY(TARRY-IDX)
                       = '880000'
                   AND RES-PSD-CD OF TARRY(TARRY-IDX)
                       = LOCALITY OF TARRY (TARRY-IDX)
                   AND TAX-CLASS-WITHHOLDING OF TARRY(TARRY-IDX)
                   AND TXGRS-CUR OF TARRY(TARRY-IDX) > ZERO

                       COMPUTE TXGRS-CUR OF TARRY(TARRY-IDX)
                       =  EARNS-TTL OF GRSWK
                          - PA-LOC-RES-TXGRS-ADJ OF W-WK
                          - PA-PHL-LOC-GRS-ADJ OF W-WK

                       COMPUTE TAX-CUR OF TARRY(TARRY-IDX)ROUNDED
                              = TXGRS-CUR OF TARRY(TARRY-IDX) *
                                WK-LCLPCT-RES OF W-WK
           END-SEARCH

            .
       FIND-RES-88000-TARRY-EXIT.

      /*****************************************************************
      *                                                                *
       LH565-FIND-RES-TARRY SECTION.
       LH565.
      *                                                                *
      ******************************************************************

           SET TARRY-IDX  TO  1
           SET PA-RES-LOCAL-FOUND-NO OF W-SW TO TRUE

           PERFORM VARYING TARRY-IDX FROM 1 BY 1
                               UNTIL TARRY-IDX
                                        >  TAXCALC-COUNT OF TARRY

               IF  STATE OF TARRY (TARRY-IDX) = 'PA'
                   AND LOCALITY OF TARRY (TARRY-IDX)
                       = LOCALITY OF RESIDENCE OF TAXWK(1)
                   AND WORK-PSD-CD OF TARRY(TARRY-IDX)
                       = LOCALITY OF RESIDENCE OF TAXWK(1)
                   AND RES-PSD-CD OF TARRY(TARRY-IDX)
                       = LOCALITY OF TARRY (TARRY-IDX)
                   AND TAX-CLASS-WITHHOLDING OF TARRY(TARRY-IDX)
                   AND TXGRS-CUR OF TARRY(TARRY-IDX) > ZERO

                      PERFORM LH570-FIND-RES-TXARY

                      IF PA-BLANK-FOUND-YES OF W-SW
                         AND PA-RES-ERN-FOUND-NO OF W-SW
                         AND PHIL-LOCAL-FOUND-YES OF W-SW
                         AND NON-PA-ERN-FOUND-YES OF W-SW

                            CONTINUE
                      ELSE

                      MOVE PA-LOC-RES-TXGRS OF W-WK TO
                           TXGRS-CUR OF TARRY(TARRY-IDX)
                      END-IF
                      SET PA-RES-LOCAL-FOUND-YES OF W-SW TO TRUE

               END-IF
           END-PERFORM

           IF PA-RES-LOCAL-FOUND-NO OF W-SW
              SET TAXCALC-COUNT OF TARRY  TO  TARRY-IDX
              INITIALIZE TAXCALC-DATA OF TARRY(TARRY-IDX)
              MOVE 'PA'TO  STATE OF TARRY(TARRY-IDX)
              MOVE LOCALITY OF RESIDENCE OF TAXWK(1)
                            TO  LOCALITY OF TARRY(TARRY-IDX)
              MOVE 'H' TO  TAX-CLASS OF TARRY(TARRY-IDX)
              MOVE LOCALITY OF RESIDENCE OF TAXWK(1)
                            TO  WORK-PSD-CD OF TARRY(TARRY-IDX)
              MOVE LOCALITY OF RESIDENCE OF TAXWK(1)
                            TO  RES-PSD-CD OF TARRY(TARRY-IDX)
              SET RESIDENT-YES OF TARRY(TARRY-IDX) TO  TRUE
              PERFORM LH570-FIND-RES-TXARY

              IF PA-BLANK-FOUND-YES OF W-SW
                 AND PA-RES-ERN-FOUND-NO OF W-SW
                 AND PHIL-LOCAL-FOUND-YES OF W-SW
                 AND NON-PA-ERN-FOUND-YES OF W-SW

                    CONTINUE
              ELSE

              MOVE PA-LOC-RES-TXGRS OF W-WK TO
                           TXGRS-CUR OF TARRY(TARRY-IDX)
              END-IF
           END-IF
            .
       FIND-RES-TARRY-EXIT.

      /*****************************************************************
      *                                                                *
       LH570-FIND-RES-TXARY SECTION.
       LH570.
      *                                                                *
      ******************************************************************

           SET TXARY-IDX  TO  1

           MOVE ZERO  TO PA-LOC-RES-TXGRS OF W-WK
           MOVE SPACE TO PA-PREV-TAX-METHOD OF W-WK

           PERFORM VARYING TXARY-IDX FROM  1  BY  1
                   UNTIL TXARY-IDX  >  TAX-SET-CNT OF TXARY

               IF  STATE OF TXARY(TXARY-IDX)  = 'PA'
                     AND LOCALITY OF TXARY(TXARY-IDX) =
                         LOCALITY OF RESIDENCE OF TAXWK(1)
                     AND WORK-PSD-CD OF LOCALITY-DATA
                               OF TXARY(TXARY-IDX 1) =
                         WORK-PSD-CD OF TARRY(TARRY-IDX)
                     AND RES-PSD-CD OF LOCALITY-DATA
                               OF TXARY(TXARY-IDX 1) =
                         RES-PSD-CD OF TARRY(TARRY-IDX)
                     AND TXGRS-CUR OF TXARY (TXARY-IDX) > ZERO
                     AND TOT-GRS-CUR OF TXARY (TXARY-IDX) > ZERO
                     AND TAX-METHOD OF TXARY (TXARY-IDX)
                                    NOT = PA-PREV-TAX-METHOD OF W-WK

                     COMPUTE PA-LOC-RES-TXGRS OF W-WK =
                             PA-LOC-RES-TXGRS OF W-WK +
                             TXGRS-CUR OF TXARY (TXARY-IDX)

                     MOVE TAX-METHOD OF TXARY (TXARY-IDX)
                                     TO PA-PREV-TAX-METHOD OF W-WK
               END-IF

           END-PERFORM

            .
       FIND-RES-TXARY-EXIT.

      /*****************************************************************
      *                                                                *
       LH572-FIND-TXARY SECTION.
       LH572.
      *                                                                *
      ******************************************************************

           SET TXARY-IDX  TO  1

           PERFORM VARYING TXARY-IDX FROM  1  BY  1
                   UNTIL TXARY-IDX  >  TAX-SET-CNT OF TXARY

                   IF STATE OF TXARY(TXARY-IDX)  = 'PA'
                     AND LOCALITY OF TXARY(TXARY-IDX) =
                         LOCALITY OF TARRY(WTARRY-IDX)
                     AND WORK-PSD-CD OF LOCALITY-DATA
                               OF TXARY(TXARY-IDX 1) =
                         WORK-PSD-CD OF TARRY(WTARRY-IDX)
                     AND RES-PSD-CD OF LOCALITY-DATA
                               OF TXARY(TXARY-IDX 1) =
                         RES-PSD-CD OF TARRY(WTARRY-IDX)
                     AND TAX-CLASS-WITHHOLDING OF TXARY (TXARY-IDX)
                     AND  TOT-GRS-CUR OF TXARY (TXARY-IDX) > ZERO


                     COMPUTE PA-PHL-LOC-GRS-ADJ OF W-WK
                             = PA-PHL-LOC-GRS-ADJ OF W-WK
                               + TOT-GRS-CUR OF TXARY(TXARY-IDX)

                   END-IF

           END-PERFORM

            .
       FIND-TXARY-EXIT.

      /*****************************************************************
      *                                                                *
       LH575-FIND-RES-880000-TXARY SECTION.
       LH575.
      *                                                                *
      ******************************************************************

           SET TXARY-IDX  TO  1
           SEARCH TAX-SET-DATA OF TXARY

               WHEN TXARY-IDX  >  TAX-SET-CNT OF TXARY

                  CONTINUE

               WHEN  STATE OF TXARY(TXARY-IDX)  = 'PA'
                     AND LOCALITY OF TXARY(TXARY-IDX) =
                         '880000'
                     AND WORK-PSD-CD OF LOCALITY-DATA
                               OF TXARY(TXARY-IDX 1) =
                         '880000'
                     AND RES-PSD-CD OF LOCALITY-DATA
                               OF TXARY(TXARY-IDX 1) =
                         RES-PSD-CD OF TARRY(TARRY-IDX)
                     AND  TXGRS-CUR OF TXARY (TXARY-IDX) > ZERO

                     MOVE TXGRS-CUR OF TXARY (TXARY-IDX) TO
                          TXGRS-CUR OF TARRY (TARRY-IDX)

           END-SEARCH

            .
       FIND-RES-880000-TXARY-EXIT.

      /*****************************************************************
      *                                                                *
       LH580-ADD-RES-88000-TARRY SECTION.
       LH580.
      *                                                                *
      ******************************************************************

           SET TARRY-IDX  TO  1

           SEARCH TAXCALC-DATA OF TARRY

               WHEN TARRY-IDX  >  TAXCALC-COUNT OF TARRY


                   SET TAXCALC-COUNT OF TARRY  TO  TARRY-IDX
                   INITIALIZE TAXCALC-DATA OF TARRY(TARRY-IDX)
                   MOVE 'PA'TO  STATE OF TARRY(TARRY-IDX)
                   MOVE LOCALITY OF RESIDENCE OF TAXWK(1)
                               TO  LOCALITY OF TARRY(TARRY-IDX)
                   MOVE 'H' TO  TAX-CLASS OF TARRY(TARRY-IDX)
                   MOVE '880000'
                               TO  WORK-PSD-CD OF TARRY(TARRY-IDX)

                   MOVE LOCALITY OF RESIDENCE OF TAXWK(1)
                               TO  RES-PSD-CD OF TARRY(TARRY-IDX)
                   SET RESIDENT-YES OF TARRY(TARRY-IDX) TO  TRUE
                   PERFORM LH575-FIND-RES-880000-TXARY

                   COMPUTE TXGRS-CUR OF TARRY(TARRY-IDX)
                       =  PA-DED-NON-PA-ERN OF W-WK
                          - PA-LOC-RES-TXGRS-ADJ OF W-WK

                   COMPUTE TAX-CUR OF TARRY(TARRY-IDX)ROUNDED
                              = TXGRS-CUR OF TARRY(TARRY-IDX) *
                                WK-LCLPCT-RES OF W-WK


               WHEN  STATE OF TARRY (TARRY-IDX) = 'PA'
                   AND LOCALITY OF TARRY (TARRY-IDX)
                       = LOCALITY OF RESIDENCE OF TAXWK(1)
                   AND WORK-PSD-CD OF TARRY(TARRY-IDX)
                       = '880000'
                   AND RES-PSD-CD OF TARRY(TARRY-IDX)
                       = LOCALITY OF TARRY (TARRY-IDX)
                   AND TAX-CLASS-WITHHOLDING OF TARRY(TARRY-IDX)
                   AND TXGRS-CUR OF TARRY(TARRY-IDX) > ZERO

                   COMPUTE TXGRS-CUR OF TARRY(TARRY-IDX)
                       =  PA-DED-NON-PA-ERN OF W-WK
                          - PA-LOC-RES-TXGRS-ADJ OF W-WK

                   COMPUTE TAX-CUR OF TARRY(TARRY-IDX)ROUNDED
                              = TXGRS-CUR OF TARRY(TARRY-IDX) *
                                WK-LCLPCT-RES OF W-WK
           END-SEARCH

            .
       ADD-RES-88000-TARRY-EXIT.


      /*****************************************************************
      *                                                                *
       LH600-ADJUST-TARRY SECTION.
       LH600.
      *                                                                *
      ******************************************************************

           IF  TARRY-IDX  NOT >  TAXCALC-COUNT OF TARRY

               IF REDWRK-ADJ-TARRY OF W-SW

                 IF RES-TAX-TOTAL OF W-WK > ZERO
                   COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                           =  TAX-CUR OF TARRY(TARRY-IDX)
                           -  RES-TAX-TOTAL OF W-WK
                 ELSE
                   COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                           =  TAX-CUR OF TARRY(TARRY-IDX)
                           +  RES-TAX-TOTAL OF W-WK
                 END-IF
               END-IF

               IF NOWRK-ADJ-TARRY OF W-SW

                   MOVE ZEROS  TO TAX-CUR OF TARRY(TARRY-IDX)
                   MOVE ZEROS  TO TXGRS-CUR OF TARRY(TARRY-IDX)
               END-IF

               IF WK-WK-ADJ-TARRY OF W-SW
                 IF STATE OF TXARY(TXARY-IDX) NOT = 'KY'

                   COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                       =   (TAX-CUR OF TARRY(TARRY-IDX)
                       - ((ACTUAL-TAX-CUR OF TAX-SET-DATA
                                      OF TXARY (OTH-TXARY-IDX)
                       *   REDUCE-PCT OF WK-WK-RCP-DATA
                                      OF TXARY (TXARY-IDX WKWKRCP-IDX))
                       /   100))
                       + HIGH-PREC-0
                 ELSE

                   SET LWKRC-IDX  TO  1
                   SEARCH LCL-WK-RECIP-TABLE OF LWKRC

                     AT END
                         CONTINUE

                     WHEN STATE-WORK OF LWKRC(LWKRC-IDX)
                        = STATE OF TARRY(TARRY-IDX)
                      AND OTHER-STATE OF LWKRC(LWKRC-IDX)
                        = STATE OF TXARY(OTH-TXARY-IDX)
                      AND LOCALITY-WORK OF LWKRC(LWKRC-IDX)
                        = LOCALITY OF TARRY(TARRY-IDX)
                      AND OTHER-LOCALITY OF LWKRC(LWKRC-IDX)
                        = LOCALITY OF TXARY(OTH-TXARY-IDX)
                      AND RECIPROCITY-RULE-REDWRK OF LWKRC(LWKRC-IDX)
                      AND ACTUAL-TAX-CUR OF TAX-SET-DATA
                          OF TXARY(OTH-TXARY-IDX) > ZERO
                      AND LOCALITY OF TXARY(OTH-TXARY-IDX)
                        NOT EQUAL WORK-LOCALITY OF W-WK

                     COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                         =   (TAX-CUR OF TARRY(TARRY-IDX)
                         - ((ACTUAL-TAX-CUR OF TAX-SET-DATA
                                      OF TXARY (OTH-TXARY-IDX)
                         *   REDUCE-PCT OF WK-WK-RCP-DATA
                                      OF TXARY (TXARY-IDX WKWKRCP-IDX))
                         /   100))
                         + HIGH-PREC-0

                     MOVE LOCALITY OF TXARY(OTH-TXARY-IDX)
                                TO WORK-LOCALITY OF W-WK
                   END-SEARCH
                 END-IF
               END-IF

               IF REDRES-ADJ-TARRY OF W-SW

                   IF LOCAL-RCP-SAVE OF W-WK  >  ZERO
                       AND TAX-CUR OF TARRY(TARRY-IDX)
                           > LOCAL-RCP-SAVE OF W-WK

                      IF SUPPL-LOCAL-RCP-YES OF W-SW
                          COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                                  =  TAX-CUR OF TARRY (TARRY-IDX)
                                  -  LOCAL-RCP-SAVE OF W-WK
                      END-IF
                   ELSE
                   COMPUTE TAX-CUR OF TARRY (TARRY-IDX)
                           = TAX-CUR OF TARRY (TARRY-IDX)
                           - TAX OF LOCAL-RCP OF GRSWK(LRCWK-IDX)

                       IF TAX-CUR OF TARRY (TARRY-IDX)  <  ZERO

                           MOVE ZERO  TO  TAX-CUR OF TARRY(TARRY-IDX)
                       END-IF
                   END-IF
               END-IF

               IF HIGHER-ADJ-TARRY OF W-SW

                   IF TAX-CLASS-WITHHOLDING OF TARRY(TARRY-IDX)

                       IF  RES-TAX-RECALC OF W-WK > ZERO

                           PERFORM LH620-PA-RES-POSITIVE
                       ELSE

                           IF  RES-TAX-RECALC OF W-WK < ZERO

                               PERFORM LH630-PA-RES-NEGATIVE
                           ELSE

                               PERFORM LH640-PA-RES-ZERO
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF

            .
       ADJUST-TARRY-EXIT.


      /*****************************************************************
      *                                                                *
       LH610-PA-ADJUST-TXGRS SECTION.
       LH610.
      *                                                                *
      ******************************************************************

           MOVE ZERO  TO  LAST-PA-LOC OF W-WK
           MOVE ZERO  TO  PA-LOC-ADJUST OF W-WK
           PERFORM VARYING WTARRY-IDX FROM 1 BY 1
                   UNTIL WTARRY-IDX  >  TAXCALC-COUNT OF TARRY

               IF STATE OF TARRY(WTARRY-IDX)
                        =  STATE OF TXARY(TXARY-IDX)
                   AND LOCALITY OF TARRY(WTARRY-IDX)  NOT =  SPACE
                   AND TAX-CLASS OF TARRY(WTARRY-IDX)
                        =  TAX-CLASS OF TXARY(TXARY-IDX)

                    IF LOCALITY OF TARRY(WTARRY-IDX)
                             NOT = LOCALITY OF TXARY(TXARY-IDX)
                        AND TXGRS-CUR OF TARRY(WTARRY-IDX)
                             NOT =  ZERO

                        MOVE WTARRY-IDX  TO  LAST-PA-LOC OF W-WK
                    END-IF

                    IF LOCALITY OF TARRY(WTARRY-IDX)
                              =  LOCALITY OF TXARY(TXARY-IDX)
                        AND (TXGRS-CUR OF TARRY(WTARRY-IDX)  =  0.01
                            OR TXGRS-CUR OF TARRY(WTARRY-IDX)  =  -0.01)

                        MOVE WTARRY-IDX  TO  PA-LOC-ADJUST OF W-WK
                    END-IF
               END-IF

           END-PERFORM

           IF PA-LOC-ADJUST OF W-WK  NOT =  ZERO

               IF LAST-PA-LOC OF W-WK  NOT =  ZERO

                   COMPUTE TXGRS-CUR OF TARRY(LAST-PA-LOC OF W-WK)
                       =  TXGRS-CUR OF TARRY(LAST-PA-LOC OF W-WK)
                       +  TXGRS-CUR OF TARRY(PA-LOC-ADJUST OF W-WK)
               END-IF

               MOVE ZERO  TO TXGRS-CUR OF TARRY(PA-LOC-ADJUST OF W-WK)
           END-IF

            .
       ADJUST-PA-TXGRS-EXIT.


      /*****************************************************************
      *                                                                *
       LH620-PA-RES-POSITIVE  SECTION.
       LH620.
      *                                                                *
      ******************************************************************


      *    TARRY IS WORK TARRY

           IF  LOCALITY OF TAXCALC-DATA
                        OF TARRY(TARRY-IDX) NOT EQUAL '510101'
               AND SAV-RESIDENCE OF W-WK = '510101'

               COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                    =  TAX-CUR OF TARRY(TARRY-IDX)
                    -  TAX-CUR OF TARRY(TARRY-IDX)

               COMPUTE TXGRS-CUR OF TARRY(TARRY-IDX) ROUNDED
                    =  TXGRS-CUR OF TARRY(TARRY-IDX)
                    -  TXGRS-CUR OF TAX-SET-DATA OF TXARY(TXARY-IDX)

               IF TXGRS-CUR OF TARRY(TARRY-IDX) = ZERO
                  AND TAX-CUR OF TARRY(TARRY-IDX) = ZERO

                  MOVE ZERO TO NON-TIPS-TXGRS-CUR OF TARRY (TARRY-IDX)
               END-IF

           ELSE

               IF  ONE-TIME-NA OF TARRY(TARRY-IDX)

                   IF NOT SPECIAL-LWT-STATUS-MAINTAIN
                                                 OF TARRY(TARRY-IDX)

                   COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                        =  TAX-CUR OF TARRY(TARRY-IDX)
                        -  RES-TAX-PRIOR OF W-WK
                        +  RES-TAX-RECALC OF W-WK
                   END-IF

               ELSE

                   IF  ONE-TIME-OVERRIDE OF TARRY(TARRY-IDX)

                       COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                            =  TAX-CUR OF TARRY(TARRY-IDX)
                            -  TAX-CUR OF TARRY(TARRY-IDX)
                            +  TAX-ONE-TIME OF TARRY(TARRY-IDX)

                   END-IF

                   IF  ONE-TIME-REFUND OF TARRY(TARRY-IDX)
                       AND NOT MANUAL-CHECK-YES OF CHECK

                     IF LOCALITY OF TARRY(TARRY-IDX) NOT = SPACE
                        AND LOCALITY OF TARRY(TARRY-IDX)
                            = WORK-PSD-CD OF TARRY(TARRY-IDX)
                        AND NOT RESIDENT-YES OF TARRY(TARRY-IDX)
                        AND PA-WORK-PSD-CD-SAV OF W-WK
                            = WORK-PSD-CD OF TARRY(TARRY-IDX)

                       COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                            =  TAX-CUR OF TARRY(TARRY-IDX)
                            +  RES-TAX-RECALC OF W-WK
                     ELSE

                       COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                            =  TAX-CUR OF TARRY(TARRY-IDX)
                            -  TAX-CUR OF TARRY(TARRY-IDX)
                            +  RES-TAX-RECALC OF W-WK

                       MOVE WORK-PSD-CD OF TARRY(TARRY-IDX)
                            TO PA-WORK-PSD-CD-SAV OF W-WK
                     END-IF

      *           TAX-ONE-TIME REFUND APPLIED AFTER ALL
      *                 PROCESSING COMPLETED

                   END-IF
               END-IF

               IF LOCALITY OF TAXCALC-DATA OF TARRY(TARRY-IDX)
                  = '880000'
                   MOVE SAV-RESIDENCE OF W-WK
                              TO RES-PSD-CD OF  TAXCALC-DATA
                                        OF TARRY(TARRY-IDX)

                   MOVE SAV-RESIDENCE OF W-WK
                              TO LOCALITY OF TAXCALC-DATA
                                              OF TARRY(TARRY-IDX)
               END-IF
           END-IF

           IF  ONE-TIME-NA OF TARRY(TARRY-IDX)

               MOVE SAV-WORK OF W-WK TO WORK-PSD-CD OF TAXCALC-DATA
                                 OF TARRY(TARRY-IDX)

               MOVE SAV-RESIDENCE OF W-WK TO RES-PSD-CD OF TAXCALC-DATA
                              OF TARRY(TARRY-IDX)
           END-IF

           SET SAV-TARRY-IDX TO TARRY-IDX

           PERFORM LH532-FIND-PA-RES-TARRY

           IF  TARRY-FOR-RES-YES OF W-SW

               IF  LOCALITY OF TAXCALC-DATA
                            OF TARRY(TARRY-IDX) = '510101'
                   AND SAV-RESIDENCE OF W-WK = '510101'

                       MOVE SAV-WORK OF W-WK
                            TO WORK-PSD-CD OF TAXCALC-DATA
                                     OF TARRY(TARRY-IDX)

               ELSE

                   PERFORM LH625-SEARCH-FOR-PHILLY

                   IF PA-RES-LWT OF GRSWK  NOT EQUAL ZERO
                      AND PHIL-LOCAL-FOUND-YES OF W-SW
                      AND PA-BLANK-FOUND-NO OF W-SW
                      AND PA-RES-ERN-FOUND-YES OF W-SW

                      IF STATE OF TARRY(TARRY-IDX) = 'PA'
                         AND LOCALITY OF TARRY(TARRY-IDX) NOT = '510101'
                         AND LOCALITY OF TARRY(TARRY-IDX)
                             NOT = LOCALITY OF TXARY(TXARY-IDX)
                         AND LOCALITY OF TXARY(TXARY-IDX) NOT =  SPACE
                         AND RESIDENT-TAX-NO OF TAX-SET-DATA
                                OF TXARY(TXARY-IDX)
                         AND TAX-CLASS OF TARRY(TARRY-IDX)
                             = TAX-CLASS OF TXARY(TXARY-IDX)
                         AND TAX-METHOD-SUPPLEMENTAL OF TXARY(TXARY-IDX)
                         AND RES-TXARY-IDX OF W-WK  NOT EQUAL ZERO
                         AND CAL-TXARY-IDX OF W-WK  NOT EQUAL ZERO
                         AND RES-TXARY-IDX OF W-WK
                             = CAL-TXARY-IDX OF W-WK

                         COMPUTE TAX-CUR OF TARRY(TARRY-IDX) ROUNDED
                              =  TAX-CUR OF TARRY(TARRY-IDX)
                              -  RES-TAX-RECALC OF W-WK

                         COMPUTE TXGRS-CUR OF TARRY(TARRY-IDX) ROUNDED
                              =  TXGRS-CUR OF TARRY(TARRY-IDX)
                              -  TXGRS-CUR OF TAX-SET-DATA
                                           OF TXARY(TXARY-IDX)

                         MOVE ZERO TO RES-TXARY-IDX OF W-WK
                         MOVE ZERO TO CAL-TXARY-IDX OF W-WK

                      ELSE

                        CONTINUE
                      END-IF

                   ELSE

                     IF MULTI-PA-LOC-AND-OTHER-YES OF W-SW
                        AND  PA-RES-LWT OF GRSWK  NOT EQUAL ZERO

                        CONTINUE

                     ELSE

                        IF LOCALITY OF TARRY(TARRY-IDX) = '510101'
                           AND PHIL-LOCAL-FOUND-NO OF W-SW

                               CONTINUE
                        ELSE
                         IF STATE OF TARRY(TARRY-IDX) = 'PA'
                            AND LOCALITY OF TARRY(TARRY-IDX) NOT = SPACE
                            AND RESIDENT-YES OF TARRY(TARRY-IDX)
                            AND RES-PSD-CD OF TARRY(TARRY-IDX)
                                EQUAL LOCALITY OF TARRY(TARRY-IDX)
                            AND WORK-PSD-CD OF TARRY(TARRY-IDX)
                                EQUAL '880000'
                            AND ONE-TIME-OVERRIDE OF TARRY(TARRY-IDX)
                            AND TAX-CUR OF TARRY(TARRY-IDX)
                                = TAX-ONE-TIME OF TARRY(TARRY-IDX)

                                CONTINUE
                         ELSE
                           COMPUTE TAX-CUR OF TARRY(TARRY-IDX) ROUNDED
                               =  TAX-CUR OF TARRY(TARRY-IDX)
                               -  RES-TAX-RECALC OF W-WK
                         END-IF
                           COMPUTE TXGRS-CUR OF TARRY(TARRY-IDX) ROUNDED
                               =  TXGRS-CUR OF TARRY(TARRY-IDX)
                               -  TXGRS-CUR OF TAX-SET-DATA
                                           OF TXARY(TXARY-IDX)
                        END-IF

                     COMPUTE  SAV-TAX-ADJ OF W-WK
                        =   .01
                        *   LOCALITY-COUNT OF WORK

                      IF  (TAX-CUR OF TARRY(TARRY-IDX) > ZERO
                       AND TAX-CUR OF TARRY(TARRY-IDX)
                       <=  SAV-TAX-ADJ OF W-WK)

                       OR (TAX-CUR OF TARRY(TARRY-IDX) < ZERO
                            AND (TAX-CUR OF TARRY(TARRY-IDX) * -1)
                            <= SAV-TAX-ADJ OF W-WK)

                       IF (TXGRS-CUR OF TARRY(TARRY-IDX) =  0.01
                           OR TXGRS-CUR OF TARRY(TARRY-IDX) =  -0.01)

                            MOVE ZERO TO TXGRS-CUR OF TARRY(TARRY-IDX)
                       END-IF

                       MOVE ZERO TO TAX-CUR OF TARRY(TARRY-IDX)

                      END-IF

                      IF STATE OF TARRY(TARRY-IDX) = 'PA'
                         AND LOCALITY OF TARRY(TARRY-IDX) NOT = SPACE
                         AND LOCALITY OF TARRY(TARRY-IDX) NOT = '510101'
                         AND WORK-PSD-CD OF TARRY(TARRY-IDX) = '880000'
                         AND NON-PA-ERN-FOUND-NO OF W-SW

                         IF (TXGRS-CUR OF TARRY(TARRY-IDX) =  0.01
                             OR TXGRS-CUR OF TARRY(TARRY-IDX) =  -0.01)

                            CONTINUE
                         ELSE

                            MOVE ZERO TO TXGRS-CUR OF TARRY(TARRY-IDX)
                            MOVE ZERO TO TAX-CUR OF TARRY(TARRY-IDX)
                         END-IF
                      END-IF
                     END-IF
                   END-IF
               END-IF

               IF  ONE-TIME-NA OF TARRY(TARRY-IDX)

                   MOVE SAV-RESIDENCE OF W-WK
                       TO RES-PSD-CD OF TAXCALC-DATA
                                     OF TARRY(TARRY-IDX)
               END-IF

               SET TARRY-IDX TO SAV-TARRY-IDX
           END-IF


           .

       PA-RES-POSITIVE-EXIT.

      /*****************************************************************
      *                                                                *
       LH625-SEARCH-FOR-PHILLY SECTION.
       LH625.
      *                                                                *
      ******************************************************************

           SET PHIL-LOCAL-FOUND-NO OF W-SW TO TRUE

           SET TARRY-OTH-IDX  TO  1

           SEARCH TAXCALC-DATA OF TARRY VARYING TARRY-OTH-IDX

               WHEN TARRY-OTH-IDX  >  TAXCALC-COUNT OF TARRY

                   CONTINUE

               WHEN  STATE OF TARRY(TARRY-OTH-IDX)  = 'PA'
                     AND LOCALITY OF TARRY(TARRY-OTH-IDX) = '510101'
                     AND  TXGRS-CUR OF TARRY (TARRY-OTH-IDX) > ZERO

                     SET PHIL-LOCAL-FOUND-YES OF W-SW TO TRUE

           END-SEARCH

           .
       SEARCH-FOR-PHILLY-EXIT.

      /*****************************************************************
      *                                                                *
       LH630-PA-RES-NEGATIVE SECTION.
       LH630.
      *                                                                *
      ******************************************************************


      *    TARRY IS WORK TARRY

           SET NEGATIVE-TAX-REQ-YES OF W-SW TO TRUE

           IF  LOCALITY OF TAXCALC-DATA OF TARRY(TARRY-IDX)
                        NOT EQUAL '510101'
               AND SAV-RESIDENCE OF W-WK = '510101'

                   COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                        =  TAX-CUR OF TARRY(TARRY-IDX)
                        -  TAX-CUR OF TARRY(TARRY-IDX)
                        +  RES-TAX-RECALC OF W-WK

                   MOVE  SAV-WORK OF W-WK
                         TO WORK-PSD-CD OF TAXCALC-DATA
                                        OF TARRY(TARRY-IDX)

                   MOVE SAV-RESIDENCE OF W-WK
                              TO RES-PSD-CD OF  TAXCALC-DATA
                                        OF TARRY(TARRY-IDX)

                   MOVE '510101' TO LOCALITY OF TAXCALC-DATA
                                              OF TARRY(TARRY-IDX)

           ELSE

               IF  ONE-TIME-NA OF TARRY(TARRY-IDX)

                   COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                        =  TAX-CUR OF TARRY(TARRY-IDX)
                        -  RES-TAX-PRIOR OF W-WK
                        +  RES-TAX-RECALC OF W-WK

               ELSE

                   IF  ONE-TIME-OVERRIDE OF TARRY(TARRY-IDX)

                       COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                            =  TAX-CUR OF TARRY(TARRY-IDX)
                            -  TAX-CUR OF TARRY(TARRY-IDX)
                            +  TAX-ONE-TIME OF TARRY(TARRY-IDX)
                   END-IF

                   IF  ONE-TIME-REFUND OF TARRY(TARRY-IDX)
                       AND NOT MANUAL-CHECK-YES OF CHECK

                       COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                            =  TAX-CUR OF TARRY(TARRY-IDX)
                            -  TAX-CUR OF TARRY(TARRY-IDX)
                            +  RES-TAX-RECALC OF W-WK

      *           TAX-ONE-TIME REFUND APPLIED AFTER ALL
      *                 PROCESSING COMPLETED

                   END-IF
               END-IF

               IF LOCALITY OF TAXCALC-DATA OF TARRY(TARRY-IDX)
                  = '880000'
                   MOVE SAV-RESIDENCE OF W-WK
                              TO RES-PSD-CD OF  TAXCALC-DATA
                                        OF TARRY(TARRY-IDX)

                   MOVE SAV-RESIDENCE OF W-WK
                              TO LOCALITY OF TAXCALC-DATA
                                              OF TARRY(TARRY-IDX)
               END-IF
           END-IF

           SET SAV-TARRY-IDX TO TARRY-IDX

           PERFORM LH532-FIND-PA-RES-TARRY

           IF  TARRY-FOR-RES-YES OF W-SW

               IF  LOCALITY OF TAXCALC-DATA
                            OF TARRY(TARRY-IDX) = '510101'
                   AND SAV-RESIDENCE OF W-WK = '510101'

                   COMPUTE TAX-CUR OF TARRY(TARRY-IDX) ROUNDED
                        =  TAX-CUR OF TARRY(TARRY-IDX)
                        -  RES-TAX-RECALC OF W-WK

                   COMPUTE TXGRS-CUR OF TARRY(TARRY-IDX) ROUNDED
                        =  TXGRS-CUR OF TARRY(TARRY-IDX)
                        -  TXGRS-CUR OF TAX-SET-DATA
                                     OF TXARY(TXARY-IDX)

               ELSE

                   COMPUTE TAX-CUR OF TARRY(TARRY-IDX) ROUNDED
                        =  TAX-CUR OF TARRY(TARRY-IDX)
                        -  RES-TAX-RECALC OF W-WK

                   COMPUTE TXGRS-CUR OF TARRY(TARRY-IDX) ROUNDED
                        =  TXGRS-CUR OF TARRY(TARRY-IDX)
                        -  TXGRS-CUR OF TAX-SET-DATA
                                     OF TXARY(TXARY-IDX)

                   COMPUTE  SAV-TAX-ADJ OF W-WK
                        =   .01
                        *   LOCALITY-COUNT OF WORK

                   IF  (TAX-CUR OF TARRY(TARRY-IDX) > ZERO
                       AND TAX-CUR OF TARRY(TARRY-IDX)
                       <=  SAV-TAX-ADJ OF W-WK)

                       OR (TAX-CUR OF TARRY(TARRY-IDX) < ZERO
                            AND (TAX-CUR OF TARRY(TARRY-IDX) * -1)
                            <= SAV-TAX-ADJ OF W-WK)

                       IF (TXGRS-CUR OF TARRY(TARRY-IDX) =  0.01
                           OR TXGRS-CUR OF TARRY(TARRY-IDX) =  -0.01)

                            MOVE ZERO TO TXGRS-CUR OF TARRY(TARRY-IDX)
                       END-IF

                       MOVE ZERO TO TAX-CUR OF TARRY(TARRY-IDX)

                   END-IF
               END-IF
           END-IF

           IF  NOT MANUAL-CHECK-YES OF CHECK

               MOVE SAV-RESIDENCE OF W-WK TO RES-PSD-CD OF TAXCALC-DATA
                                  OF TARRY(TARRY-IDX)
           END-IF
           SET TARRY-IDX TO SAV-TARRY-IDX


            .

       PA-RES-NEGATIVE-EXIT.


      /*****************************************************************
      *                                                                *
       LH640-PA-RES-ZERO SECTION.
       LH640.
      *                                                                *
      ******************************************************************


           IF  LOCALITY OF TAXCALC-DATA OF TARRY(TARRY-IDX)
                        NOT EQUAL '510101'
               AND SAV-RESIDENCE OF W-WK = '510101'

             IF TXGRS-CUR OF TXARY(TXARY-IDX) = ZERO
                AND TAX-METHOD-SUPPLEMENTAL OF TXARY(TXARY-IDX)

                   CONTINUE
             ELSE
                   MOVE '510101' TO LOCALITY OF TAXCALC-DATA
                                             OF TARRY(TARRY-IDX)
             END-IF
           ELSE
               IF LOCALITY OF TAXCALC-DATA
                       OF TARRY(TARRY-IDX) = '880000'
                       AND WORK-PSD-CD OF TAXCALC-DATA
                           OF TARRY(TARRY-IDX) =  '880000'
                       AND STATE OF RESIDENCE OF TAXWK
                           =  STATE OF WORK OF TAXWK
                       AND LOCALITY OF RESIDENCE OF TAXWK(1)
                           =  LOCALITY OF WORK OF TAXWK(1)
                       AND LOCALITY-COUNT OF WORK OF TAXWK  =  1

                   MOVE ZERO  TO  STATE-COUNT OF W-WK
                   PERFORM VARYING WTARRY-IDX FROM 1 BY 1
                               UNTIL WTARRY-IDX
                                        >  TAXCALC-COUNT OF TARRY

                       IF NOT STATE-FEDERAL OF TARRY(WTARRY-IDX)
                               AND STATE OF TARRY(WTARRY-IDX)
                                        NOT =  STATE OF TARRY(TARRY-IDX)
                                AND TAX-CLASS-WITHHOLDING
                                        OF TARRY(TARRY-IDX)

                           ADD 1  TO  STATE-COUNT OF W-WK
                       END-IF

                       IF STATE-COUNT OF W-WK  =  ZERO

                           MOVE SAV-RESIDENCE OF W-WK
                                 TO LOCALITY OF TAXCALC-DATA
                                    OF TARRY(TARRY-IDX)
                           MOVE SAV-RESIDENCE OF W-WK
                                 TO SAV-WORK OF W-WK
                       END-IF
                   END-PERFORM
               END-IF
           END-IF

           IF STATE OF TARRY(TARRY-IDX) = 'PA'
              AND LOCALITY OF TARRY(TARRY-IDX) = '880000'
              AND WORK-PSD-CD OF TARRY(TARRY-IDX) = '880000'
              AND RES-PSD-CD OF TARRY(TARRY-IDX) = '880000'

      *     SUBTRACT FROM WORK, AS RESIDENT SHOULD COVER TAXES.

                     COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                          =  TAX-CUR OF TARRY(TARRY-IDX)
                          -  TAX-CUR OF TARRY(TARRY-IDX)

                     COMPUTE TXGRS-CUR OF TARRY(TARRY-IDX)
                          =  TXGRS-CUR OF TARRY(TARRY-IDX)
                          -  TXGRS-CUR OF TARRY(TARRY-IDX)
           END-IF

           IF  ONE-TIME-NA OF TARRY(TARRY-IDX)

               MOVE SAV-WORK OF W-WK TO WORK-PSD-CD OF TAXCALC-DATA
                             OF TARRY(TARRY-IDX)

               MOVE SAV-RESIDENCE OF W-WK TO RES-PSD-CD OF TAXCALC-DATA
                             OF TARRY(TARRY-IDX)

               PERFORM VARYING WRKWK-IDX FROM 1  BY  1
                       UNTIL WRKWK-IDX  >
                             LOCALITY-COUNT OF WORK OF TAXWK

                  SET WRKDT-IDX  TO  WRKWK-IDX

                  IF LOCALITY OF WORK OF TAXWK(WRKWK-IDX)
                     = WORK-PSD-CD OF TARRY(TARRY-IDX)
                     AND LWT-AMT OF WORK OF TAXDT(WRKDT-IDX) NOT = ZERO

                    IF RES-PSD-CD OF TARRY(TARRY-IDX)
                          = WORK-PSD-CD OF TARRY(TARRY-IDX)

                      IF SEPCHK OF CHECK = 0

                        CONTINUE
                      ELSE

                        COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                              = TAX-CUR OF TARRY(TARRY-IDX)
                              - LWT-AMT OF WORK OF TAXDT(WRKDT-IDX)
                      END-IF

                    ELSE

                     COMPUTE RES-TAX-PRIOR-CALC OF W-WK
                           = ACTUAL-TAX-CUR-SAV OF W-WK
                           - LWT-AMT OF WORK OF TAXDT(WRKDT-IDX)

                     IF RES-TAX-RECALC-SAV OF W-WK
                        > RES-TAX-PRIOR-CALC OF W-WK

                        COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                              = LWT-AMT OF WORK OF TAXDT(WRKDT-IDX)
                              + RES-TAX-RECALC-SAV OF W-WK
                     END-IF

                     MOVE ZERO TO ACTUAL-TAX-CUR-SAV OF W-WK
                     MOVE ZERO TO RES-TAX-RECALC-SAV OF W-WK
                     MOVE ZERO TO RES-TAX-PRIOR-CALC OF W-WK

                    END-IF
                  END-IF
               END-PERFORM
           END-IF

           SET SAV-TARRY-IDX TO TARRY-IDX

           PERFORM LH532-FIND-PA-RES-TARRY

      *    TARRY RESIDENCE

           IF TARRY-FOR-RES-YES OF W-SW

               IF  ONE-TIME-NA OF TARRY(TARRY-IDX)

                   IF WORK-PSD-CD OF TARRY(TARRY-IDX) NOT EQUAL '880000'
                      AND PA-RES-LWT OF GRSWK  NOT EQUAL ZERO
                      AND PHIL-LOCAL-FOUND-YES OF W-SW
                      AND NON-PHIL-LOCAL-FOUND-YES OF W-SW
                      AND NON-PA-ERN-FOUND-NO OF W-SW

                      CONTINUE

                   ELSE

                       IF  PA-WRK-TXGRS-FOUND-YES OF W-SW

                           COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                                =  TAX-CUR OF TARRY(TARRY-IDX)
                                -  RES-TAX-LESS OF W-WK

                           COMPUTE TXGRS-CUR OF TARRY(TARRY-IDX)
                                =  TXGRS-CUR OF TARRY(TARRY-IDX)
                                -  RES-TXGRS-RECALC OF W-WK
                       END-IF

                       COMPUTE  SAV-TAX-ADJ OF W-WK
                             =   .01
                             *   LOCALITY-COUNT OF WORK

                       IF  (TAX-CUR OF TARRY(TARRY-IDX) > ZERO
                           AND TAX-CUR OF TARRY(TARRY-IDX)
                           <=  SAV-TAX-ADJ OF W-WK)

                           OR (TAX-CUR OF TARRY(TARRY-IDX) < ZERO
                                AND (TAX-CUR OF TARRY(TARRY-IDX) * -1)
                                <= SAV-TAX-ADJ OF W-WK)

                           IF (TXGRS-CUR OF TARRY(TARRY-IDX) =  0.01
                               OR TXGRS-CUR OF TARRY(TARRY-IDX)
                                             =  -0.01)

                               MOVE ZERO TO TXGRS-CUR
                                         OF TARRY(TARRY-IDX)
                           END-IF

                           MOVE ZERO TO TAX-CUR OF TARRY(TARRY-IDX)

                       END-IF
                   END-IF

               ELSE

                   IF  ONE-TIME-OVERRIDE OF TARRY(TARRY-IDX)

                       COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                            =  TAX-CUR OF TARRY(TARRY-IDX)
                            -  TAX-CUR OF TARRY(TARRY-IDX)
                            +  TAX-ONE-TIME OF TARRY(TARRY-IDX)

                       IF  RES-TXGRS-RECALC OF W-WK > ZERO

                           COMPUTE TXGRS-CUR OF TARRY(TARRY-IDX)
                                =  TXGRS-CUR OF TARRY(TARRY-IDX)
                                - RES-TXGRS-RECALC OF W-WK
                       END-IF
                   END-IF

                   IF  ONE-TIME-REFUND OF TARRY(TARRY-IDX)
                       IF NOT MANUAL-CHECK-YES OF CHECK

                           COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                                =  TAX-CUR OF TARRY(TARRY-IDX)
                                +  RES-TAX-RECALC OF W-WK

                       END-IF

                       IF  MANUAL-CHECK-YES OF CHECK

                           COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                                 = TAX-CUR OF TARRY(TARRY-IDX)
                                 - TAX-CUR OF TARRY(TARRY-IDX)

      *                    TAX-ONE-TIME REFUND APPLIED AFTER ALL
      *                    PROCESSING COMPLETED

                       END-IF
                   END-IF
               END-IF

               IF LOCALITY OF TAXCALC-DATA OF TARRY(TARRY-IDX)
                  = '880000'
                   MOVE SAV-RESIDENCE OF W-WK
                              TO RES-PSD-CD OF  TAXCALC-DATA
                                        OF TARRY(TARRY-IDX)

                   MOVE SAV-RESIDENCE OF W-WK
                              TO LOCALITY OF TAXCALC-DATA
                                              OF TARRY(TARRY-IDX)
               END-IF

               IF  ONE-TIME-NA OF TARRY(TARRY-IDX)

                   MOVE SAV-RESIDENCE OF W-WK TO RES-PSD-CD
                        OF TAXCALC-DATA OF TARRY(TARRY-IDX)
               END-IF
           END-IF

           SET TARRY-IDX TO SAV-TARRY-IDX

           .

       PA-RES-ZERO-EXIT.

      /*****************************************************************
      *                                                                *
       LH700-MATCH-TARRY-TGCID SECTION.
       LH700.
      *                                                                *
      ******************************************************************

           MOVE SPACE TO SUT-STATE OF W-WK

           PERFORM VARYING TARRY-IDX FROM 1 BY 1
                   UNTIL TARRY-IDX  >  TAXCALC-COUNT

      *Find matching values for the current state and locality
      *being processed.
               IF  STATE OF TARRY (TARRY-IDX)
                   = STATE-TG OF TGCTB-PASS
                  AND LOCALITY OF TARRY (TARRY-IDX)
                  =  LOCALITY-TG OF TGCTB-PASS

                   PERFORM LH710-LOAD-TARRY-TGCID

                   IF STATE OF TARRY (TARRY-IDX) NOT =
                      SUT-JURISDICTION OF TARRY (TARRY-IDX)

                      MOVE SUT-JURISDICTION OF TARRY (TARRY-IDX)
                          TO SUT-STATE OF W-WK
                   END-IF

               END-IF

               IF  STATE OF TARRY (TARRY-IDX)
                   = STATE-TG OF TGCTB-PASS
                  AND LOCALITY OF TARRY (TARRY-IDX)
                  NOT =  LOCALITY-TG OF TGCTB-PASS
                  AND LOCALITY-TG OF TGCTB-PASS =
                      LOCALITY OF WORK OF TAXWK(1)
                  AND TAX-CLASS OF TARRY (TARRY-IDX)
                  NOT =  'U'

                   MOVE ZERO TO TAXGRS-CMP-COUNT OF TARRY(TARRY-IDX)
                   PERFORM LH710-LOAD-TARRY-TGCID

               END-IF


           END-PERFORM


      *Find matching values for the SUT state if that's not the current
      *state
           IF SUT-STATE OF W-WK NOT = SPACE
             PERFORM VARYING TARRY-IDX FROM 1 BY 1
                   UNTIL TARRY-IDX  >  TAXCALC-COUNT


               IF  STATE OF TARRY (TARRY-IDX)
                   = SUT-STATE OF W-WK
                   AND TAX-CLASS OF TARRY (TARRY-IDX)
                   =  'U'
                   AND LOCALITY-TG OF TGCTB-PASS = SPACE

                   PERFORM LH720-LOAD-TARRY-SUT-STATE

               END-IF


             END-PERFORM
           END-IF

            .
       MATCH-TARRY-TGCID-EXIT.

      /*****************************************************************
      *                                                                *
       LH710-LOAD-TARRY-TGCID SECTION.
       LH710.
      *                                                                *
      ******************************************************************

           PERFORM VARYING L-TAXGRS-IDX FROM 1  BY  1
                       UNTIL L-TAXGRS-IDX
                               >  TAXGRS-CMP-COUNT OF GRSWK

                   IF TAX-CLASS OF TARRY (TARRY-IDX)
                       = TAX-CLASS-TG OF TAXGRS-CMP-DATA OF
                                                GRSWK(L-TAXGRS-IDX)
                   AND STATE OF TARRY (TARRY-IDX)
                       = STATE-TG OF TAXGRS-CMP-DATA OF
                                                GRSWK(L-TAXGRS-IDX)
                   AND LOCALITY OF TARRY(TARRY-IDX)
                       = LOCALITY-TG OF TAXGRS-CMP-DATA OF
                                                GRSWK(L-TAXGRS-IDX)
                   AND TARRY-MATCHED OF GRSWK(L-TAXGRS-IDX)
                       NOT = 'Y'

                        ADD 1 TO TAXGRS-CMP-COUNT OF TARRY(TARRY-IDX)

                        SET TAXGRS-IDX TO
                                  TAXGRS-CMP-COUNT OF TARRY(TARRY-IDX)

                        MOVE 'Y' TO TARRY-MATCHED OF GRSWK(L-TAXGRS-IDX)
                        MOVE ERNCD-TG OF TAXGRS-CMP-DATA OF
                                                GRSWK(L-TAXGRS-IDX)
                          TO ERNCD OF TARRY(TARRY-IDX TAXGRS-IDX)

                        MOVE DEDCD-TG  OF TAXGRS-CMP-DATA OF
                                                GRSWK(L-TAXGRS-IDX)
                          TO DEDCD OF  TAXGRS-CMP-DATA
                                 OF TARRY(TARRY-IDX TAXGRS-IDX)
                        MOVE DED-CLASS-TG OF TAXGRS-CMP-DATA OF
                                                GRSWK(L-TAXGRS-IDX)
                          TO DED-CLASS OF  TAXGRS-CMP-DATA
                                   OF TARRY(TARRY-IDX TAXGRS-IDX)
                        MOVE TAX-GRS-COMPNT-TG OF
                                          TAXGRS-CMP-DATA OF
                                                GRSWK(L-TAXGRS-IDX)
                          TO TAX-GRS-COMPNT OF  TAXGRS-CMP-DATA
                                 OF TARRY(TARRY-IDX TAXGRS-IDX)
                        MOVE OTHER-RATE-ID OF TAXGRS-CMP-DATA OF
                                                GRSWK(L-TAXGRS-IDX)
                          TO OTHER-RATE-ID OF  TAXGRS-CMP-DATA
                                 OF TARRY(TARRY-IDX TAXGRS-IDX)
                        MOVE TAX-GROSS-EFFECT OF TAXGRS-CMP-DATA OF
                                                GRSWK(L-TAXGRS-IDX)
                          TO TAX-GROSS-EFFECT  OF  TAXGRS-CMP-DATA
                                 OF TARRY(TARRY-IDX TAXGRS-IDX)
                        MOVE TAX-GROSS-ADJ OF TAXGRS-CMP-DATA OF
                                                GRSWK(L-TAXGRS-IDX)
                          TO TAX-GROSS-ADJ OF  TAXGRS-CMP-DATA
                                 OF TARRY(TARRY-IDX TAXGRS-IDX)
                        MOVE TAX-GROSS-ADJAPL OF TAXGRS-CMP-DATA OF
                                                GRSWK(L-TAXGRS-IDX)
                          TO TAX-GROSS-ADJAPL OF  TAXGRS-CMP-DATA
                                 OF TARRY(TARRY-IDX TAXGRS-IDX)

                   END-IF

           END-PERFORM



            .
       LOAD-TARRY-TGCID-EXIT.

      /*****************************************************************
      *                                                                *
       LH720-LOAD-TARRY-SUT-STATE SECTION.
       LH720.
      *                                                                *
      ******************************************************************

           SET TGR-SUT-MATCH-FOUND-NO OF W-SW TO TRUE

           PERFORM VARYING L-TAXGRS-IDX FROM 1  BY  1
                       UNTIL L-TAXGRS-IDX
                               >  TAXGRS-CMP-COUNT OF GRSWK

                   IF TAX-CLASS OF TARRY (TARRY-IDX)
                       = TAX-CLASS-TG OF TAXGRS-CMP-DATA OF
                                                GRSWK(L-TAXGRS-IDX)
                   AND STATE OF TARRY (TARRY-IDX)
                       = STATE-TG OF TAXGRS-CMP-DATA OF
                                                GRSWK(L-TAXGRS-IDX)
                   AND LOCALITY OF TARRY(TARRY-IDX)
                       = LOCALITY-TG OF TAXGRS-CMP-DATA OF
                                                GRSWK(L-TAXGRS-IDX)

                       IF TARRY-MATCHED OF TAXGRS-CMP-DATA OF
                                                GRSWK(L-TAXGRS-IDX)
                          = 'Y'

                          SET TGR-SUT-MATCH-FOUND-YES OF W-SW TO TRUE
                       ELSE
                          SET TGR-SUT-MATCH-FOUND-NO OF W-SW TO TRUE
                       END-IF

                       IF TGR-SUT-MATCH-FOUND-NO OF W-SW
                          PERFORM LH730-FIND-MATCH-TARRY-TGCID
                       END-IF

                   END-IF

           END-PERFORM

            .
       LOAD-TARRY-SUT-STATE-EXIT.

      /*****************************************************************
      *                                                                *
       LH730-FIND-MATCH-TARRY-TGCID SECTION.
       LH730.
      *                                                                *
      ******************************************************************

           PERFORM VARYING TAXGRS-IDX FROM 1  BY  1
                       UNTIL TAXGRS-IDX >  TAXGRS-CMP-COUNT
                                           OF TARRY (TARRY-IDX)

                   IF TAX-GRS-COMPNT-TG OF TAXGRS-CMP-DATA OF
                         GRSWK(L-TAXGRS-IDX)
                       = TAX-GRS-COMPNT OF  TAXGRS-CMP-DATA
                                 OF TARRY(TARRY-IDX TAXGRS-IDX)
                      AND ERNCD-TG OF TAXGRS-CMP-DATA OF
                         GRSWK(L-TAXGRS-IDX)
                       = ERNCD OF  TAXGRS-CMP-DATA
                                 OF TARRY(TARRY-IDX TAXGRS-IDX)
                      AND DEDCD-TG OF TAXGRS-CMP-DATA OF
                         GRSWK(L-TAXGRS-IDX)
                       = DEDCD OF  TAXGRS-CMP-DATA
                                 OF TARRY(TARRY-IDX TAXGRS-IDX)
                      AND DED-CLASS-TG OF TAXGRS-CMP-DATA OF
                         GRSWK(L-TAXGRS-IDX)
                       = DED-CLASS OF  TAXGRS-CMP-DATA
                                 OF TARRY(TARRY-IDX TAXGRS-IDX)
                      AND OTHER-RATE-ID OF TAXGRS-CMP-DATA OF
                         GRSWK(L-TAXGRS-IDX)
                       = OTHER-RATE-ID OF  TAXGRS-CMP-DATA
                                 OF TARRY(TARRY-IDX TAXGRS-IDX)

                    EVALUATE TRUE

                       WHEN TAX-GROSS-EFFECT OF TAXGRS-CMP-DATA
                            OF GRSWK(L-TAXGRS-IDX) = 'A'

                            COMPUTE TAX-GROSS-ADJ
                                 OF TAXGRS-CMP-DATA
                                 OF TARRY(TARRY-IDX TAXGRS-IDX)
                            = TAX-GROSS-ADJ OF TAXGRS-CMP-DATA
                                 OF TARRY(TARRY-IDX TAXGRS-IDX)
                            + TAX-GROSS-ADJ OF TAXGRS-CMP-DATA
                                 OF GRSWK(L-TAXGRS-IDX)

                            COMPUTE TAX-GROSS-ADJAPL
                                 OF TAXGRS-CMP-DATA
                                 OF TARRY(TARRY-IDX TAXGRS-IDX)
                            = TAX-GROSS-ADJAPL OF TAXGRS-CMP-DATA
                                 OF TARRY(TARRY-IDX TAXGRS-IDX)
                            + TAX-GROSS-ADJAPL OF TAXGRS-CMP-DATA
                                 OF GRSWK(L-TAXGRS-IDX)

                       WHEN TAX-GROSS-EFFECT OF TAXGRS-CMP-DATA
                            OF GRSWK(L-TAXGRS-IDX) = 'S'

                            COMPUTE TAX-GROSS-ADJ
                                 OF TAXGRS-CMP-DATA
                                 OF TARRY(TARRY-IDX TAXGRS-IDX)
                            = TAX-GROSS-ADJ OF TAXGRS-CMP-DATA
                                 OF TARRY(TARRY-IDX TAXGRS-IDX)
                            - TAX-GROSS-ADJ OF TAXGRS-CMP-DATA
                                 OF GRSWK(L-TAXGRS-IDX)

                            COMPUTE TAX-GROSS-ADJAPL
                                 OF TAXGRS-CMP-DATA
                                 OF TARRY(TARRY-IDX TAXGRS-IDX)
                            = TAX-GROSS-ADJAPL OF TAXGRS-CMP-DATA
                                 OF TARRY(TARRY-IDX TAXGRS-IDX)
                            - TAX-GROSS-ADJAPL OF TAXGRS-CMP-DATA
                                 OF GRSWK(L-TAXGRS-IDX)

                            IF TAX-GROSS-ADJ OF TAXGRS-CMP-DATA
                                 OF TARRY(TARRY-IDX TAXGRS-IDX)
                                    < ZERO
                               MOVE ZERO TO
                               TAX-GROSS-ADJ OF TAXGRS-CMP-DATA
                                 OF TARRY(TARRY-IDX TAXGRS-IDX)
                            END-IF

                            IF TAX-GROSS-ADJAPL OF TAXGRS-CMP-DATA
                                 OF TARRY(TARRY-IDX TAXGRS-IDX)
                                    < ZERO
                               MOVE ZERO TO
                               TAX-GROSS-ADJAPL OF TAXGRS-CMP-DATA
                                 OF TARRY(TARRY-IDX TAXGRS-IDX)
                            END-IF

                    END-EVALUATE

                    MOVE 'Y' TO TARRY-MATCHED OF GRSWK(L-TAXGRS-IDX)

                   END-IF

           END-PERFORM

            .
       FIND-MATCH-TARRY-TGCID-EXIT.

      /*****************************************************************
      *                                                                *
       LH740-MATCH-PA-LOC-TGCID SECTION.
       LH740.
      *                                                                *
      ******************************************************************

           PERFORM VARYING TARRY-IDX FROM 1 BY 1
                   UNTIL TARRY-IDX  >  TAXCALC-COUNT

               IF  STATE OF TARRY (TARRY-IDX) = 'PA'
                   AND LOCALITY OF TARRY (TARRY-IDX) NOT = SPACE

                   PERFORM LH710-LOAD-TARRY-TGCID

               END-IF


           END-PERFORM

            .
       MATCH-PA-LOC-TGCID-EXIT.

      /*****************************************************************
      *                                                                *
       LH750-MATCH-LAST-TGCID SECTION.
       LH750.
      *                                                                *
      ******************************************************************

           PERFORM VARYING TARRY-IDX FROM 1 BY 1
                   UNTIL TARRY-IDX  >  TAXCALC-COUNT

               IF  STATE OF TARRY (TARRY-IDX) =
                   STATE OF TAX-SET OF TAXWK
                   AND LOCALITY OF TARRY (TARRY-IDX) = SPACE
                   AND TAX-CLASS OF TARRY (TARRY-IDX) = 'H'

                   PERFORM LH720-LOAD-TARRY-SUT-STATE

               END-IF


           END-PERFORM

            .
       MATCH-LAST-TGCID-EXIT.


      /*****************************************************************
      *                                                                *
       LH760-PA-LOC-RES-ROW SECTION.
       LH760.
      *                                                                *
      ******************************************************************

           MOVE ZERO TO LOC-RES-COUNT OF W-WK
           SET PA-880000-ROW-FOUND-NO OF W-SW   TO  TRUE

           PERFORM VARYING TARRY-IDX FROM 1 BY 1
                   UNTIL TARRY-IDX  >  TAXCALC-COUNT OF TARRY

                  IF STATE OF TARRY (TARRY-IDX) = 'PA'
                       AND LOCALITY OF TARRY (TARRY-IDX) NOT = SPACE
                       AND LOCALITY OF TARRY (TARRY-IDX) NOT = '510101'
                       AND LOCALITY OF TARRY (TARRY-IDX)
                           = LOCALITY OF RESIDENCE OF TAXWK(1)
                       AND RESIDENT-YES OF TARRY(TARRY-IDX)
                       AND TAX-CLASS-WITHHOLDING OF TARRY (TARRY-IDX)

                           COMPUTE LOC-RES-COUNT OF W-WK
                                 = LOC-RES-COUNT OF W-WK
                                 + 1

                     IF WORK-PSD-CD OF TARRY(TARRY-IDX)
                        = '880000'

                        SET PA-880000-ROW-FOUND-YES OF W-SW TO TRUE
                     END-IF
                  END-IF
           END-PERFORM

            .
       PA-LOC-RES-ROW-EXIT.


      /*****************************************************************
      *                                                                *
       LH765-PA-LOC-RES-WRK-PSD SECTION.
       LH765.
      *                                                                *
      ******************************************************************

           SET PA-RES-WRK-PSD-FOUND-NO OF W-SW   TO  TRUE
           SET TARRY-IDX TO 1

           SEARCH TAXCALC-DATA OF TARRY

                WHEN TARRY-IDX > TAXCALC-COUNT OF TARRY

                    CONTINUE

                WHEN STATE OF TARRY(TARRY-IDX) = 'PA'
                   AND LOCALITY OF TARRY (TARRY-IDX) NOT = SPACE
                   AND LOCALITY OF TARRY (TARRY-IDX) NOT = '510101'
                   AND LOCALITY OF TARRY(TARRY-IDX)
                       = LOCALITY OF RESIDENCE OF TAXWK(1)
                   AND WORK-PSD-CD OF TARRY(TARRY-IDX)
                       = LOCALITY OF RESIDENCE OF TAXWK(1)
                   AND RESIDENT-YES OF TARRY(TARRY-IDX)
                   AND TAX-CLASS-WITHHOLDING OF TARRY(TARRY-IDX)

                   SET PA-RES-WRK-PSD-FOUND-YES OF W-SW   TO  TRUE
           END-SEARCH

            .
       PA-LOC-RES-WRK-PSD-EXIT.


      /*****************************************************************
      *                                                                *
       LH767-PHILLY-LOC-RES-ROW SECTION.
       LH767.
      *                                                                *
      ******************************************************************

           SET PA-PHILLY-ROW-FOUND-NO OF W-SW  TO  TRUE
           SET TARRY-IDX TO 1

           SEARCH TAXCALC-DATA OF TARRY

                WHEN TARRY-IDX > TAXCALC-COUNT OF TARRY

                    CONTINUE

                WHEN STATE OF TARRY(TARRY-IDX) = 'PA'
                   AND LOCALITY OF TARRY (TARRY-IDX) = '510101'
                   AND WORK-PSD-CD OF TARRY(TARRY-IDX)
                       = LOCALITY OF TARRY (TARRY-IDX)
                   AND RES-PSD-CD OF TARRY(TARRY-IDX)
                       = LOCALITY OF TARRY (TARRY-IDX)
                   AND RESIDENT-YES OF TARRY(TARRY-IDX)
                   AND TAX-CUR OF TARRY(TARRY-IDX) > ZERO
                   AND TXGRS-CUR OF TARRY(TARRY-IDX) > ZERO

                   SET PA-PHILLY-ROW-FOUND-YES OF W-SW  TO  TRUE
           END-SEARCH

            .
       PHILLY-LOC-RES-ROW-EXIT.


      /*****************************************************************
      *                                                                *
       LH770-CREATE-WRK-PSD-880000 SECTION.
       LH770.
      *                                                                *
      ******************************************************************

           IF PA-EARNS-TTL OF GRSWK NOT EQUAL ZERO

              COMPUTE PA-PERC-DED OF W-WK =
               PA-DED-NON-PA-ERN OF W-WK /
               PA-EARNS-TTL OF GRSWK
           ELSE

              COMPUTE PA-PERC-DED OF W-WK =
               PA-DED-NON-PA-ERN OF W-WK /
               EARNS-TTL OF GRSWK
           END-IF

              COMPUTE PA-LOC-RES-TXGRS-ADJ OF W-WK =
               PA-TOT-125-DED OF W-WK *
               PA-PERC-DED OF W-WK

              PERFORM LH555-GET-LOCAL-RES-RATE
              PERFORM LH580-ADD-RES-88000-TARRY

            .
       CREATE-WRK-PSD-880000-EXIT.


      /*****************************************************************
      *                                                                *
       MA000-ADJ-NEG-TAXES SECTION.
       MA000.
      *                                                                *
      ******************************************************************

           PERFORM VARYING TARRY-IDX  FROM  1  BY  1
                   UNTIL TARRY-IDX  >  TAXCALC-COUNT OF TARRY

      *  ADJUST RES-STATE TAX IF LESS THAN TAX ON RES-STATE EARNINGS
      *  OR LESS THAN RES-STATE ADDITIONAL TAX.  (NOTE EXCEPTION FOR
      *  KY DUE TO KREDA TAX CREDIT.)

               IF NOT TAX-CLASS-EICREDIT OF TARRY(TARRY-IDX)
                   AND MANUAL-CHECK-NO OF CHECK
                       AND TAX-CLASS-WITHHOLDING OF TARRY(TARRY-IDX)
                           AND NOT STATE-FEDERAL OF TARRY(TARRY-IDX)
                               AND LOCALITY OF TARRY(TARRY-IDX) = SPACE
                                   AND RESIDENT-YES OF TARRY(TARRY-IDX)
                                       AND STATE-RCP-TAKEN-YES OF W-SW

                   IF TAX-CUR OF TARRY(TARRY-IDX) > ZERO
                       IF TAX-CUR OF TARRY(TARRY-IDX)  + 1
                           <  (TAX-ON-RES-EARNS OF W-WK
                           -  TAX-SUPP-PRIOR-RES OF GRSWK)
                           AND NBR-OF-STATES OF GRSWK < 2
                           AND STATE OF TARRY(TARRY-IDX) NOT = 'KY'

                          IF PA-BLANK-FOUND-YES OF W-SW
                             AND PA-RES-ERN-FOUND-NO OF W-SW
                             AND TXGRS-CUR OF TARRY(TARRY-IDX) = ZERO

                                CONTINUE
                          ELSE

                              MOVE TAX-ON-RES-EARNS OF W-WK
                                 TO  TAX-CUR OF TARRY(TARRY-IDX)
                          END-IF
                       END-IF
                   END-IF

                   IF  (TAX-CUR OF TARRY(TARRY-IDX) > ZERO
                       OR STATE OF TARRY(TARRY-IDX)  = 'CT')
                       AND TAX-CUR OF TARRY(TARRY-IDX)
                               <  TAX-CUR-ADDL-AMT OF TARRY(TARRY-IDX)

                       MOVE TAX-CUR-ADDL-AMT OF TARRY(TARRY-IDX)
                               TO  TAX-CUR OF TARRY(TARRY-IDX)
                   END-IF
               END-IF

      *  ADJUST RES-STATE TAX IF LESS THAN TAX ON RES-STATE EARNINGS
      *  ON MANUAL CHECK WITH RECIPROCITY RULE #2.
      *  (NOTE EXCEPTION FOR KY DUE TO KREDA TAX CREDIT.)

               IF NOT TAX-CLASS-EICREDIT OF TARRY(TARRY-IDX)
                   AND MANUAL-CHECK-YES OF CHECK
                       AND TAX-CLASS-WITHHOLDING OF TARRY(TARRY-IDX)
                           AND NOT STATE-FEDERAL OF TARRY(TARRY-IDX)
                             AND WK-RECIPROCITY-RULE-REDRES OF W-WK
                               AND LOCALITY OF TARRY(TARRY-IDX) = SPACE
                                   AND RESIDENT-YES OF TARRY(TARRY-IDX)
                                       AND STATE-RCP-TAKEN-YES OF W-SW

                   IF TAX-CUR OF TARRY(TARRY-IDX)
                               <  TAX-ON-RES-EARNS OF W-WK
                          AND STATE OF TARRY(TARRY-IDX) NOT = 'KY'

                       MOVE TAX-ON-RES-EARNS OF W-WK
                               TO  TAX-CUR OF TARRY(TARRY-IDX)
                   END-IF
               END-IF

      *  APPLY TAX REFUND

               IF (TAX-CLASS-OASD-ER-EXEMPT OF TARRY(TARRY-IDX)
                  OR TAX-CLASS-OASD-TIPS-EXMPT OF TARRY(TARRY-IDX)
                  OR TAX-CLASS-OASD-EE-EXEMPT OF TARRY(TARRY-IDX)
                  OR TAX-CLASS-OASD-TIP-EE-EX OF TARRY(TARRY-IDX))
                  IF ONE-TIME-CD OF TARRY(TARRY-IDX) = 'R'

                       COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                           =  TAX-CUR OF TARRY(TARRY-IDX)
                           -  TAX-ONE-TIME OF TARRY(TARRY-IDX)
                   END-IF
                   IF ONE-TIME-CD OF TARRY(TARRY-IDX)  =  'A'
                       COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                           =  TAX-CUR OF TARRY(TARRY-IDX)
                           +  TAX-ONE-TIME OF TARRY(TARRY-IDX)
                   END-IF

               ELSE

                   IF NOT ONE-TIME-OVERRIDE OF TARRY(TARRY-IDX)
                           AND TAX-ONE-TIME OF TARRY(TARRY-IDX) < ZERO

                      IF FRMWK-STATE-TAX-YES OF TARRY (TARRY-IDX) AND
                         MANUAL-CHECK-YES OF CHECK

                         CONTINUE

                      ELSE

                         COMPUTE TAX-CUR OF TARRY(TARRY-IDX)
                               =  TAX-CUR OF TARRY(TARRY-IDX)
                               +  TAX-ONE-TIME OF TARRY(TARRY-IDX)
                      END-IF
                   END-IF
               END-IF

      *  PREVENT NEGATIVE YTD TXBL/TAX BY ADJUSTING CURRENT TXBL/TAX

               MOVE TAX-CLASS OF TARRY(TARRY-IDX)
                        TO  PFML-MA-SW OF W-SW
                            PFML-CT-SW OF W-SW
               IF NOT TAX-CLASS-EICREDIT OF TARRY(TARRY-IDX)
                  AND NOT TAX-CLASS-OASD-ER-EXEMPT OF TARRY(TARRY-IDX)
                  AND NOT TAX-CLASS-OASD-TIPS-EXMPT OF TARRY(TARRY-IDX)
                  AND NOT TAX-CLASS-OASD-EE-EXEMPT OF TARRY(TARRY-IDX)
                  AND NOT TAX-CLASS-OASD-TIP-EE-EX OF TARRY(TARRY-IDX)
                  AND NOT (STATE OF TARRY(TARRY-IDX) = 'MA'
                                AND PFML-MA-YES OF W-SW)
                  AND NOT (STATE OF TARRY(TARRY-IDX) = 'CT'
                         AND PFML-CT-YES OF W-SW)
                  AND NOT (STATE OF TARRY(TARRY-IDX) = 'WA'
                                AND PFML-WA-SUBJECT OF W-SW)

                   COMPUTE TXGRS-ADJ OF W-WK  ROUNDED
                           =  TXGRS-CUR OF TARRY(TARRY-IDX)
                           +  TXGRS-YTD OF TARRY(TARRY-IDX)
                   COMPUTE TAX-ADJ OF W-WK ROUNDED
                           =  TAX-CUR OF TARRY(TARRY-IDX)
                           +  TAX-YTD OF TARRY(TARRY-IDX)

                   IF  NEGATIVE-TAX-REQ-NO OF W-SW

                       IF TXGRS-ADJ OF W-WK > ZERO

                           MOVE ZERO TO TXGRS-ADJ OF W-WK
                       END-IF

                       IF TAX-ADJ OF W-WK > ZERO

                           MOVE ZERO TO TAX-ADJ OF W-WK
                       END-IF

                       IF TAX-GRS-BASE-COUNT OF TGBTB > ZERO
                          AND TXGRS-ADJ OF W-WK = ZERO
                          AND TAX-ADJ OF W-WK  = ZERO
                          AND LOCALITY OF TARRY(TARRY-IDX)
                                         NOT = SPACE
                          AND TAX-CLASS-WITHHOLDING OF TARRY(TARRY-IDX)

                          PERFORM MA100-ADJ-NEG-LWT
                       END-IF

                       IF TXGRS-ADJ OF W-WK NOT = ZERO
                               OR TAX-ADJ OF W-WK NOT = ZERO

                           COMPUTE TXGRS-CUR OF TARRY(TARRY-IDX) ROUNDED
                                   =  TXGRS-CUR OF TARRY(TARRY-IDX)
                                   -  TXGRS-ADJ OF W-WK

                           COMPUTE TAX-CUR OF TARRY(TARRY-IDX) ROUNDED
                                   =  TAX-CUR OF TARRY(TARRY-IDX)
                                   -  TAX-ADJ OF W-WK

                           IF NLGRS-CUR OF TARRY(TARRY-IDX) NOT = ZERO

                               COMPUTE NLGRS-CUR OF TARRY(TARRY-IDX)
                                       =  NLGRS-CUR OF TARRY(TARRY-IDX)
                                       -  TXGRS-ADJ OF W-WK
                           END-IF

                           IF TXGRS-AGG OF TARRY(TARRY-IDX) NOT = ZERO
                                   OR TAX-AGG OF TARRY(TARRY-IDX)
                                                            NOT = ZERO

                             IF FRMWK-STATE-TAX-YES OF TARRY (TARRY-IDX)
                                AND MANUAL-CHECK-YES OF CHECK

                                CONTINUE

                             ELSE

                               COMPUTE TXGRS-AGG OF TARRY(TARRY-IDX)
                                                               ROUNDED
                                       =  TXGRS-AGG OF TARRY(TARRY-IDX)
                                       -  TXGRS-ADJ OF W-WK
                               COMPUTE TAX-AGG OF TARRY(TARRY-IDX)
                                                               ROUNDED
                                       =  TAX-AGG OF TARRY(TARRY-IDX)
                                       -  TAX-ADJ OF W-WK

                               IF STATE OF TARRY(TARRY-IDX) = 'PA'
                                  AND LOCALITY OF TARRY(TARRY-IDX)
                                                         NOT = SPACE
                                  AND TAX OF STATE-RCP OF GRSWK
                                      = TAX-CUR OF TARRY(TARRY-IDX)
                                      + TAX-ADJ OF W-WK
                                  AND STATE-NEG-RCP-YES OF W-SW

                                 SET STATE-NEG-RCP-NO OF W-SW TO TRUE
                                 COMPUTE TAX-CUR OF TARRY(RES-TARRY-IDX)
                                       = TAX-CUR OF TARRY(RES-TARRY-IDX)
                                       + TAX-ADJ OF W-WK
                               END-IF
                             END-IF
                           END-IF
                       ELSE

                           IF PA-EARNS-TTL OF GRSWK > ZERO
                              AND STATE OF TARRY(TARRY-IDX) = 'PA'
                              AND LOCALITY OF TARRY(TARRY-IDX) = SPACE
                              AND TAX-CUR OF TARRY(TARRY-IDX) < ZERO
                              AND TXGRS-CUR OF TARRY(TARRY-IDX) < ZERO

                              MOVE ZERO TO TAX-CUR OF TARRY(TARRY-IDX)
                              MOVE ZERO TO TXGRS-CUR OF TARRY(TARRY-IDX)
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-PERFORM

           .
       ADJ-NEG-TAXES-EXIT.


      /*****************************************************************
      *                                                                *
       MA100-ADJ-NEG-LWT SECTION.
       MA100.
      *     IF CHECK HAS EARNINGS THAT ARE SET UP WITH A TAXABLE GROSS *
      *     COMPONENT THAT SUBTRACTS FROM GROSS, THEN MAKE SURE THE    *
      *     LOCAL WITHOLDING TAX IS NOT NEGATIVE                       *
      ******************************************************************

           IF TAXGRS-CMP-COUNT OF TARRY (TARRY-IDX) > ZERO

              PERFORM VARYING TAXGRS-IDX FROM 1  BY  1
                       UNTIL TAXGRS-IDX >  TAXGRS-CMP-COUNT
                                           OF TARRY (TARRY-IDX)

                IF ERNCD
                   OF TAXGRS-CMP-DATA OF TARRY(TARRY-IDX,TAXGRS-IDX)
                   NOT = SPACE
                   AND TAX-GROSS-EFFECT OF
                       TAXGRS-CMP-DATA OF TARRY(TARRY-IDX,TAXGRS-IDX)
                       = 'S'

                  IF STATE OF TARRY(TARRY-IDX) = 'PA'
                     AND LOCALITY OF TARRY(TARRY-IDX) NOT = SPACE
                     AND ((TAX OF STATE-RCP OF GRSWK
                         = TAX-CUR OF TARRY(TARRY-IDX)
                          AND STATE-NEG-RCP-YES OF W-SW)
                       OR (LOCALITY OF TARRY(TARRY-IDX)
                          = WORK-PSD-CD OF TARRY(TARRY-IDX)
                          AND RES-PSD-CD OF TARRY(TARRY-IDX)
                             NOT = LOCALITY OF TARRY(TARRY-IDX)
                          AND NOT RESIDENT-YES OF TARRY(TARRY-IDX)))

                        SET STATE-NEG-RCP-NO OF W-SW TO TRUE
                  ELSE

                   IF TXGRS-CUR OF TARRY(TARRY-IDX) < ZERO
                      MOVE ZERO TO TXGRS-CUR OF TARRY(TARRY-IDX)
                   END-IF

                   IF TAX-CUR OF TARRY(TARRY-IDX) < ZERO
                      MOVE ZERO TO TAX-CUR OF TARRY(TARRY-IDX)
                   END-IF

                  END-IF

                END-IF
              END-PERFORM

           END-IF

            .
       ADJ-NEG-LWT-EXIT.

      /*****************************************************************
      *                                                                *
       NA000-DEDUCT-TAXES SECTION.
       NA000.
      *                                                                *
      ******************************************************************

           IF NOT PAY-LINE-STATUS-ERROR OF CHECK

               COMPUTE DEDNET OF GRSWK
                   =   DEDNET OF GRSWK
                   +   DED-REFUND-TTL OF W-TTL

               IF PAYCHECK-ADJUST-YES OF CHECK

                   COMPUTE DEDNET OF GRSWK
                           =  DEDNET OF GRSWK
                           -  ADJ-EARNINGS OF W-WK
               END-IF

               CALL 'PSPDEDTX' USING   SQLRT
                                       TARRY
                                       TAXWK
                                       TAXDT
                                       EARRY
                                       DARRY
                                       DEDT1
                                       DEDT2
                                       DEDT3
                                       DEDT4
                                       DEDT5
                                       DEDT6
                                       GRSWK
                                       PYGRP
                                       PSLCT
                                       CHECK
                                       PCTST
                                       ATGSW
                                       YARRY                            HP90001
                                       RFNTL
               IF RTNCD-ERROR OF SQLRT

                   MOVE 'DEDUCT-TAXES(PSPDEDTX)'
                           TO  ERR-SECTION OF SQLRT
                   PERFORM ZZ000-SQL-ERROR
               END-IF

               COMPUTE DEDNET OF GRSWK
                   =   DEDNET OF GRSWK
                   -   DED-REFUND-TTL OF W-TTL

               IF PAYCHECK-ADJUST-YES OF CHECK

                   COMPUTE DEDNET OF GRSWK
                           =  DEDNET OF GRSWK
                           +  ADJ-EARNINGS OF W-WK
               END-IF

           END-IF

           .
       DEDUCT-TAXES-EXIT.


      /*****************************************************************
      *                                                                *
       PA000-GROSSUP-LOOP SECTION.
       PA000.
      *                                                                *
      * STOP GROSSUP LOOP ON ERROR                                     *
      *                                                                *
      ******************************************************************

           IF PAY-LINE-STATUS-ERROR OF CHECK

               SET GROSSUP-WORK-NO OF GRSWK  TO  TRUE
           END-IF

           IF GROSSUP-WORK-YES OF GRSWK

               IF DEDNET OF GRSWK  =  EXPNET OF GRSWK

                   SET GROSSUP-WORK-NO OF GRSWK  TO  TRUE
               ELSE

                   IF ITER-COUNT OF GRSWK  >  19

                       SET MSGID-NO-GROSS-UP OF PYMSG  TO  TRUE
                       MOVE ITER-COUNT OF GRSWK
                               TO  MSGDATA1-INT OF PYMSG
                       MOVE DEDNET OF GRSWK
                               TO  MSGDATA2-DOL OF PYMSG
                       PERFORM ZM000-MESSAGE
                       SET PAY-LINE-STATUS-ERROR OF CHECK  TO  TRUE
                       SET GROSSUP-WORK-NO OF GRSWK  TO  TRUE
                   ELSE
                       MOVE TAXWK-SAVE  TO  TAXWK
                       MOVE TAXDT-SAVE  TO  TAXDT
                       ADD  1  TO  ITER-COUNT OF GRSWK
                       SET EARRY-IDX  TO  EARRY-SAVE OF GRSWK

                       IF DEDNET OF GRSWK  -  EXPNET OF GRSWK
                               >  .25
                               OR DEDNET OF GRSWK
                                       -  EXPNET OF GRSWK
                                       <  -.25

                           COMPUTE EARNS OF EARRY(EARRY-IDX)
                                   =  EARNS OF EARRY(EARRY-IDX)
                                   -  ((DEDNET OF GRSWK
                                   -  EXPNET OF GRSWK)
                                   *  1.3)
                       ELSE
                           COMPUTE EARNS OF EARRY(EARRY-IDX)
                                   =  EARNS OF EARRY(EARRY-IDX)
                                   -  (DEDNET OF GRSWK
                                   -  EXPNET OF GRSWK)
                       END-IF

                       MOVE ZERO TO SAV-MEDCR-WAGES OF TARRY
                   END-IF
               END-IF
           END-IF

           .
       GROSSUP-LOOP-EXIT.


      /*****************************************************************
      *                                                                *
       PD000-CLEAR-TAX-CALC SECTION.
       PD000.
      *                                                                *
      ******************************************************************

           MOVE ZEROS TO TAX-SET-CNT OF TXARY
           MOVE ZEROS TO SAV-MEDCR-WAGES OF TARRY

           PERFORM VARYING TARRY-IDX  FROM  1  BY  1
                   UNTIL TARRY-IDX  >  TAXCALC-COUNT OF TARRY

      *
      *  Switch gets set in PSPTCALC DG000 Once the switch is set
      *  it should not be initalized again.  A reclaculation of taxes
      *  will need to know that there is an
      *  additional flat amount that has already been processed
      *  Initialize tax-curr-addl-amt so that it is not
      *  re-added everytime there is a reclaculation of taxes.
      *
      *        SET FLAT-TAKEN-NO OF TARRY(TARRY-IDX)  TO  TRUE
               MOVE ZERO  TO  TAX-CUR-ADDL-AMT OF TARRY(TARRY-IDX)
      *
               SET DED-TAX-NO OF TARRY(TARRY-IDX)  TO  TRUE
               MOVE TAX-ONE-TIME OF TARRY(TARRY-IDX)
                       TO  TAX-CUR OF TARRY(TARRY-IDX)

      * Tax refund will be re-applied during tax re-calc
               IF NOT ONE-TIME-OVERRIDE OF TARRY(TARRY-IDX)
               AND TAX-ONE-TIME OF TARRY(TARRY-IDX) < ZERO
               AND CALC-TAX-YES OF GRSWK
               AND GROSSUP-NO OF CHECK
                  MOVE ZERO TO TAX-CUR OF TARRY(TARRY-IDX)
               END-IF

               MOVE WK-PA-OPT-YTD OF W-WK TO PA-OPT-YTD OF TARRY
               MOVE ZERO  TO  TXGRS-CUR OF TARRY(TARRY-IDX)
               MOVE ZERO  TO  NLGRS-CUR OF TARRY(TARRY-IDX)
               MOVE ZERO  TO  TAX-NOT-TAKEN OF TARRY(TARRY-IDX)
               MOVE ZERO  TO  TIPS-TAX-CUR  OF TARRY(TARRY-IDX)
               MOVE ZERO  TO  NON-TIPS-TAX-CUR OF TARRY(TARRY-IDX)
               SET PTX-MTD-TIPS-NOT-ADDED OF TARRY(TARRY-IDX) TO TRUE
               MOVE ZERO  TO  PTX-CUR-TIPS  OF TARRY(TARRY-IDX)
               MOVE ZERO  TO  PTX-CUR-TIPS-DELAY  OF TARRY(TARRY-IDX)
               MOVE ZERO  TO  NON-TIPS-TXGRS-CUR  OF TARRY(TARRY-IDX)
               MOVE ZERO  TO  NON-TIPS-NLGRS-CUR  OF TARRY(TARRY-IDX)
               MOVE ZERO  TO  NON-TIPS-TAX-NT-TAK OF TARRY(TARRY-IDX)
               SET TAX-CUR-ADDL-AMT-NOTADDED OF TARRY(TARRY-IDX)
                          TO TRUE
               SET TXGRS-WORK-STATE-RQD-NO OF TARRY(TARRY-IDX)
                                           TO TRUE
           END-PERFORM

           PERFORM VARYING YARRY-IDX  FROM  1  BY  1                    HP99999
                   UNTIL YARRY-IDX  >  TAXCALC-1042-COUNT OF YARRY      HP99999
                                                                        HP99999
               MOVE ZERO  TO  TXGRS-1042-CUR OF YARRY(YARRY-IDX)        HP99999
               MOVE ZERO  TO  TXGRS-1042-OVER-LMT OF YARRY(YARRY-IDX)   HP99997
               MOVE ZERO  TO  TAX-1042-CUR   OF YARRY(YARRY-IDX)        HP99999
               MOVE ZERO  TO  WH-ALLOW-CUR   OF YARRY(YARRY-IDX)        HP99999
           END-PERFORM                                                  HP99999

           .
       CLEAR-TAX-CALC-EXIT.


      /*****************************************************************
      *                                                                *
       PD100-CLEAR-TAX-EXCEPT-TIPS SECTION.
       PD100.
      *                                                                *
      ******************************************************************

           PERFORM VARYING TARRY-IDX  FROM  1  BY  1
                   UNTIL TARRY-IDX  >  TAXCALC-COUNT OF TARRY

      *
      *  Switch gets set in PSPTCALC DG000 Once the switch is set
      *  it should not be initalized again.  A reclaculation of taxes
      *  will need to know that there is an
      *  additional flat amount that has already been processed
      *  Initialize tax-curr-addl-amt so that it is not
      *  re-added everytime there is a reclaculation of taxes.
      *
      *        SET FLAT-TAKEN-NO OF TARRY(TARRY-IDX)  TO  TRUE
               MOVE ZERO  TO  TAX-CUR-ADDL-AMT OF TARRY(TARRY-IDX)
      *
               SET DED-TAX-NO OF TARRY(TARRY-IDX)  TO  TRUE
               IF NOT ONE-TIME-OVERRIDE OF TARRY(TARRY-IDX)
                  AND TAX-ONE-TIME OF TARRY(TARRY-IDX) < ZERO
                   MOVE ZERO TO TAX-CUR OF TARRY(TARRY-IDX)
               ELSE
                   MOVE TAX-ONE-TIME OF TARRY(TARRY-IDX)
                       TO  TAX-CUR OF TARRY(TARRY-IDX)
               END-IF
               MOVE ZERO  TO  TXGRS-CUR OF TARRY(TARRY-IDX)
               MOVE ZERO  TO  NLGRS-CUR OF TARRY(TARRY-IDX)
               MOVE ZERO  TO  TAX-NOT-TAKEN OF TARRY(TARRY-IDX)
               MOVE WK-PA-OPT-YTD OF W-WK TO PA-OPT-YTD OF TARRY
               SET TAX-CUR-ADDL-AMT-NOTADDED OF TARRY(TARRY-IDX)
                          TO TRUE
               SET TXGRS-WORK-STATE-RQD-NO OF TARRY(TARRY-IDX)
                          TO TRUE
           END-PERFORM

           MOVE ZERO TO SAV-MEDCR-WAGES OF TARRY

           SET TAX-SET-MAX OF TXARY  TO  TRUE
           PERFORM VARYING TXARY-IDX  FROM  1  BY  1
                   UNTIL TXARY-IDX  >  TAX-SET-CNT OF TXARY

               MOVE ZERO  TO  FWT OF TXARY(TXARY-IDX)
               MOVE ZERO  TO  FICA OF TXARY(TXARY-IDX)
               MOVE ZERO  TO  FUT OF TXARY(TXARY-IDX)
               MOVE ZERO  TO  SWT OF TXARY(TXARY-IDX)
               MOVE ZERO  TO  SUT OF TXARY(TXARY-IDX)
               MOVE ZERO  TO  SDI OF TXARY(TXARY-IDX)
               MOVE ZERO  TO  TOT-GRS-CUR OF TXARY(TXARY-IDX)
               MOVE ZERO  TO  TXGRS-CUR OF TXARY(TXARY-IDX)
               MOVE ZERO  TO  CALCED-TAX-CUR OF TXARY(TXARY-IDX)
               MOVE ZERO  TO  ACTUAL-TAX-CUR OF TXARY(TXARY-IDX)
               MOVE ZERO  TO  WRK-TGC-ADJ OF TXARY(TXARY-IDX)
               MOVE ZERO  TO  RES-TGC-ADJ-ST OF TXARY(TXARY-IDX)
               MOVE ZERO  TO  WRK-TGC-WH-ADJ OF TXARY(TXARY-IDX)
               MOVE ZERO  TO  RES-TGC-WH-ADJ-ST OF TXARY(TXARY-IDX)
               MOVE ZERO  TO  TXGRS-SW-THIS-PERIOD OF TXARY(TXARY-IDX)
           END-PERFORM

           PERFORM VARYING YARRY-IDX  FROM  1  BY  1
                   UNTIL YARRY-IDX  >  TAXCALC-1042-COUNT OF YARRY

               MOVE ZERO  TO  TXGRS-1042-CUR OF YARRY(YARRY-IDX)
               MOVE ZERO  TO  TXGRS-1042-OVER-LMT OF YARRY(YARRY-IDX)
               MOVE ZERO  TO  TAX-1042-CUR   OF YARRY(YARRY-IDX)
               MOVE ZERO  TO  WH-ALLOW-CUR   OF YARRY(YARRY-IDX)
           END-PERFORM

           MOVE ZERO  TO  TAX-SET-CNT
           .
       CLEAR-TAX-CALC-NTIPS-EXIT.


      /*****************************************************************
      *                                                                *
       PD200-CLEAR-DED-CALC SECTION.
       PD200.
      *                                                                *
      *                                                                *
      ******************************************************************

           PERFORM VARYING DARRY-IDX  FROM  1  BY  1
                   UNTIL DARRY-IDX  >  DEDUCTION-COUNT OF DARRY
                           OR PAY-LINE-STATUS-ERROR OF CHECK

               PERFORM VARYING DCLAS-IDX  FROM  1  BY  1
                       UNTIL DCLAS-IDX
                               >  DEDUCTION-CLASS-MAX OF DARRY
                               OR DED-CLASS
                                       OF DARRY(DARRY-IDX DCLAS-IDX)
                                       =  SPACE
                               OR PAY-LINE-STATUS-ERROR OF CHECK

                   MOVE ZERO  TO  DED-CUR OF DARRY(DARRY-IDX DCLAS-IDX)

                   MOVE ZERO  TO  DED-CUR-PAYBK OF DARRY(DARRY-IDX
                                                         DCLAS-IDX)
                   MOVE ZERO  TO  DED-NOT-TAKEN OF DARRY(DARRY-IDX
                                                         DCLAS-IDX)
                   MOVE ZERO  TO  ROLLOVER-AMT OF DARRY(DARRY-IDX
                                                         DCLAS-IDX)
                   SET REASON-NOT-TAKEN-NONE   OF DARRY(DARRY-IDX
                                                    DCLAS-IDX) TO TRUE
                   SET ROLLOVER-TAKEN-NO       OF DARRY(DARRY-IDX
                                                    DCLAS-IDX) TO TRUE
                   SET CALC-YES OF DARRY(DARRY-IDX DCLAS-IDX)  TO  TRUE

                   PERFORM VARYING DCALC-IDX  FROM  1  BY  1
                           UNTIL DCALC-IDX
                                   >  DEDUCTION-CALC-MAX OF DARRY

                       SET FLAT-TAKEN-NO OF DARRY(DARRY-IDX DCLAS-IDX
                                                            DCALC-IDX)
                               TO  TRUE
                   END-PERFORM
               END-PERFORM
           END-PERFORM

           .
       CLEAR-DED-CALC-EXIT.


      /*****************************************************************
      *                                                                *
       PD300-SET-DED-FOR-RECALC SECTION.
       PD300.
      *                                                                *
      *                                                                *
      ******************************************************************

           PERFORM VARYING DARRY-IDX  FROM  1  BY  1
                   UNTIL DARRY-IDX  >  DEDUCTION-COUNT OF DARRY
                           OR PAY-LINE-STATUS-ERROR OF CHECK

               PERFORM VARYING DCLAS-IDX  FROM  1  BY  1
                       UNTIL DCLAS-IDX
                               >  DEDUCTION-CLASS-MAX OF DARRY
                               OR DED-CLASS
                                       OF DARRY(DARRY-IDX DCLAS-IDX)
                                       =  SPACE
                               OR PAY-LINE-STATUS-ERROR OF CHECK

                   IF DED-CLASS-BEFORE-TAX OF DARRY(DARRY-IDX
                                                    DCLAS-IDX)
                       OR DED-CLASS-AFTER-TAX OF DARRY(DARRY-IDX
                                                       DCLAS-IDX)

                       COMPUTE DED-CUR OF DARRY(DARRY-IDX DCLAS-IDX)
                               =  DED-CUR OF DARRY(DARRY-IDX
                                                   DCLAS-IDX)
                               -  DED-CUR-PAYBK OF DARRY(DARRY-IDX
                                                         DCLAS-IDX)
                   END-IF
               END-PERFORM
           END-PERFORM

           .
       SET-DED-FOR-RECALC-EXIT.


      /*****************************************************************
      *                                                                *
       RA000-TAKE-DEDUCTIONS SECTION.
       RA000.
      *                                                                *
      * SUBTRACT DEDUCTIONS IN PRIORITY ORDER                          *
      *                                                                *
      ******************************************************************


           IF PAY-FREQ-TYPE-FOURWEEK OF PYGRP

               SET PAY-FREQ-TYPE-WEEKLY OF TAXWK  TO  TRUE
               MOVE 4  TO  TAX-PERIODS OF TAXWK
           ELSE
               MOVE PAY-FREQ-TYPE OF PYGRP  TO  PAY-FREQ-TYPE OF TAXWK
               MOVE 1  TO  TAX-PERIODS OF TAXWK
           END-IF

           IF DEDNET OF GRSWK  >=  MIN-CHECK-ADJUST OF GRSWK

               COMPUTE DEDNET OF GRSWK
                       =  DEDNET OF GRSWK
                       -  MIN-CHECK-ADJUST OF GRSWK
           ELSE

               IF DEDNET OF GRSWK  >  ZERO

                   MOVE DEDNET OF GRSWK  TO  MIN-CHECK-ADJUST OF GRSWK
                   MOVE ZERO  TO  DEDNET OF GRSWK
               ELSE

                   MOVE ZERO  TO  MIN-CHECK-ADJUST OF GRSWK
               END-IF
           END-IF

           IF PAYCHECK-ADJUST-YES OF CHECK
               AND ADJ-EARNINGS OF W-WK NOT= ZERO

               MOVE DEDNET OF GRSWK TO PREV-DEDNET OF W-WK

               COMPUTE DEDNET OF GRSWK
                       =  DEDNET OF GRSWK
                       -  ADJ-EARNINGS OF W-WK

               IF DEDNET OF GRSWK  <  ZERO
                   AND PREV-DEDNET OF W-WK = ZERO

                   MOVE ZERO  TO  DEDNET OF GRSWK
               END-IF
           END-IF

           PERFORM VARYING DARRY-IDX  FROM  1  BY  1
                   UNTIL DARRY-IDX  >  DEDUCTION-COUNT OF DARRY
                           OR PAY-LINE-STATUS-ERROR OF CHECK

               SET DEDT1-IDX  TO  DEDTB-PTR OF DARRY(DARRY-IDX)
               SET DEDT2-IDX  TO  DEDTB-PTR OF DARRY(DARRY-IDX)
               SET DEDT3-IDX  TO  DEDTB-PTR OF DARRY(DARRY-IDX)
               SET DEDT4-IDX  TO  DEDTB-PTR OF DARRY(DARRY-IDX)
               SET DEDT5-IDX  TO  DEDTB-PTR OF DARRY(DARRY-IDX)
               SET BEFORE-TAX-NO OF W-SW  TO  TRUE
               SET AFTER-TAX-NO  OF W-SW  TO  TRUE

               PERFORM VARYING DCLAS-IDX  FROM  1  BY  1
                       UNTIL DCLAS-IDX
                               >  DEDUCTION-CLASS-MAX OF DARRY
                               OR DED-CLASS
                                       OF DARRY(DARRY-IDX DCLAS-IDX)
                                       =  SPACE
                               OR PAY-LINE-STATUS-ERROR OF CHECK

      /*       SAVE OFF ORIGINAL DED-CUR TO PREV-DED-CUR IN FIRST DED
      /*       DCLASS LOOP BEFORE ANY NET PROCESSING OCCURS TO BE USED
      /*       LATER IN ADJUSTMENT OF EMPLOYER MATCH IN CASE EMPLOYEE'S
      /*       DEDUCTION IS LESS DUE TO INSUFFICIENT NET PAY
                   IF DED-CUR-PREV OF DARRY(DARRY-IDX DCLAS-IDX) = 0

                       MOVE DED-CUR OF DARRY(DARRY-IDX DCLAS-IDX)  TO
                            DED-CUR-PREV OF DARRY(DARRY-IDX DCLAS-IDX)
                   END-IF

                   IF DED-CLASS-BEFORE-TAX OF DARRY(DARRY-IDX
                                                    DCLAS-IDX)
                       AND (DED-CUR OF DARRY(DARRY-IDX DCLAS-IDX)
                                               <  ZERO
                         AND DED-CUR-PAYBK OF DARRY(DARRY-IDX DCLAS-IDX)
                                               =  ZERO)

                       IF NOT MANUAL-CHECK-NO OF CHECK

                           PERFORM RB100-ADJUST-NET-AMOUNT

                           IF DCLAS-IDX  >  1

                               SET DCLAS-IDX  TO  1
                               PERFORM RA200-DEDUCT-AFTER-TAX
                               SET DCLAS-IDX  TO  2
                           END-IF
                       ELSE

                           PERFORM RA100-DEDUCT-BEFORE-TAX
                       END-IF
                   END-IF
               END-PERFORM

           END-PERFORM

           PERFORM VARYING DARRY-IDX  FROM  1  BY  1
                   UNTIL DARRY-IDX  >  DEDUCTION-COUNT OF DARRY
                           OR PAY-LINE-STATUS-ERROR OF CHECK

               SET DEDT1-IDX  TO  DEDTB-PTR OF DARRY(DARRY-IDX)
               SET DEDT2-IDX  TO  DEDTB-PTR OF DARRY(DARRY-IDX)
               SET DEDT3-IDX  TO  DEDTB-PTR OF DARRY(DARRY-IDX)
               SET DEDT4-IDX  TO  DEDTB-PTR OF DARRY(DARRY-IDX)
               SET DEDT5-IDX  TO  DEDTB-PTR OF DARRY(DARRY-IDX)
               SET BEFORE-TAX-NO OF W-SW  TO  TRUE
               SET AFTER-TAX-NO  OF W-SW  TO  TRUE

               PERFORM VARYING DCLAS-IDX  FROM  1  BY  1
                       UNTIL DCLAS-IDX
                               >  DEDUCTION-CLASS-MAX OF DARRY
                               OR DED-CLASS
                                       OF DARRY(DARRY-IDX DCLAS-IDX)
                                       =  SPACE
                               OR PAY-LINE-STATUS-ERROR OF CHECK

                   IF DED-CLASS-BEFORE-TAX OF DARRY(DARRY-IDX
                                                    DCLAS-IDX)
                       AND (DED-CUR OF DARRY(DARRY-IDX DCLAS-IDX)
                                               >  ZERO
                         OR (DED-CUR OF DARRY(DARRY-IDX DCLAS-IDX)
                                               =  ZERO
                        AND ARREARS-BAL OF DARRY(DARRY-IDX DCLAS-IDX)
                                           NOT =  ZERO)
                         OR (DED-CUR OF DARRY(DARRY-IDX DCLAS-IDX)
                                               =  ZERO
                        AND DED-NOT-TAKEN OF DARRY(DARRY-IDX DCLAS-IDX)
                                           NOT =  ZERO)
                         OR ((DED-CUR OF DARRY(DARRY-IDX DCLAS-IDX)
                                               <  ZERO
                        AND DED-CUR-PAYBK OF DARRY(DARRY-IDX DCLAS-IDX)
                                           NOT =  ZERO)))

                       IF NOT MANUAL-CHECK-NO OF CHECK

                           PERFORM RB100-ADJUST-NET-AMOUNT

                           IF DCLAS-IDX  >  1

                               SET DCLAS-IDX  TO  1
                               PERFORM RA200-DEDUCT-AFTER-TAX
                               SET DCLAS-IDX  TO  2
                           END-IF
                       ELSE

                           PERFORM RA100-DEDUCT-BEFORE-TAX
                       END-IF

                       SET BEFORE-TAX-YES OF W-SW  TO  TRUE
                   ELSE

                       IF DED-CLASS-BEFORE-TAX OF DARRY(DARRY-IDX
                                                        DCLAS-IDX)
                          AND (DED-CUR OF DARRY(DARRY-IDX DCLAS-IDX)
                                           NOT =  ZERO
                              OR
                               ARREARS-BAL OF DARRY(DARRY-IDX DCLAS-IDX)
                                           NOT =  ZERO)

                              SET BEFORE-TAX-YES OF W-SW  TO  TRUE
                       END-IF
                   END-IF
               END-PERFORM

               IF BEFORE-TAX-NO OF W-SW

                   PERFORM VARYING DCLAS-IDX  FROM  1  BY  1
                           UNTIL DCLAS-IDX
                               >  DEDUCTION-CLASS-MAX OF DARRY
                               OR DED-CLASS
                                       OF DARRY(DARRY-IDX DCLAS-IDX)
                                       =  SPACE
                               OR PAY-LINE-STATUS-ERROR OF CHECK

                       IF DED-CLASS-AFTER-TAX OF DARRY(DARRY-IDX
                                                       DCLAS-IDX)

                           IF SPCL-PROCESS-GARN OF DEDT1(DEDT1-IDX)

                               IF DED-CUR OF DARRY
                                             (DARRY-IDX DCLAS-IDX)
                                       >  DEDNET OF GRSWK

                                   IF DED-CUR OF DARRY(DARRY-IDX
                                                       DCLAS-IDX)
                                           >  DEDNET OF GRSWK
                                           +  MIN-CHECK-ADJUST
                                                   OF GRSWK
                                       COMPUTE DEDNET OF GRSWK
                                               =  DEDNET OF GRSWK
                                               +  MIN-CHECK-ADJUST
                                                       OF GRSWK

                                       MOVE ZERO  TO  MIN-CHECK-ADJUST
                                                         OF GRSWK

                                       PERFORM
                                           RA500-PRORATE-GARNISHMENTS
                                   ELSE

                                       COMPUTE MIN-CHECK-ADJUST
                                                          OF GRSWK
                                               =  MIN-CHECK-ADJUST
                                                          OF GRSWK
                                               -  (DED-CUR OF DARRY
                                                  (DARRY-IDX DCLAS-IDX)
                                               -  DEDNET OF GRSWK)

                                       MOVE DED-CUR OF DARRY
                                                  (DARRY-IDX DCLAS-IDX)
                                               TO  DEDNET OF GRSWK
                                       PERFORM RB100-ADJUST-NET-AMOUNT
                                   END-IF
                               ELSE

                                   IF DED-CUR OF DARRY
                                             (DARRY-IDX DCLAS-IDX)
                                           NOT =  ZERO

                                       PERFORM RB100-ADJUST-NET-AMOUNT
                                   END-IF
                               END-IF
                           ELSE

                               IF NOT MANUAL-CHECK-NO OF CHECK

                                   PERFORM RB100-ADJUST-NET-AMOUNT
                               ELSE

                                   PERFORM RA200-DEDUCT-AFTER-TAX
                               END-IF
                           END-IF

                           SET AFTER-TAX-YES OF W-SW  TO  TRUE
                       END-IF
                   END-PERFORM
               END-IF

               IF BEFORE-TAX-NO OF W-SW
                       AND AFTER-TAX-NO OF W-SW

                   PERFORM VARYING DCLAS-IDX  FROM  1  BY  1
                           UNTIL DCLAS-IDX
                               >  DEDUCTION-CLASS-MAX OF DARRY
                               OR DED-CLASS
                                       OF DARRY(DARRY-IDX DCLAS-IDX)
                                       =  SPACE
                               OR PAY-LINE-STATUS-ERROR OF CHECK

                       SET CALC-NO OF DARRY(DARRY-IDX DCLAS-IDX) TO TRUE
                   END-PERFORM
               END-IF
           END-PERFORM

      /*   ADJUST EMPLOYER MATCH IF EMPLOYEE MATCH WAS REDUCED
           CALL 'PSPADJEM' USING    DARRY
                                    DEDT2
                                    DEDT3
                                    DEDT5

           IF PAYCHECK-ADJUST-YES OF CHECK

               COMPUTE DEDNET OF GRSWK
                       =  DEDNET OF GRSWK
                       +  ADJ-EARNINGS OF W-WK
           END-IF

           IF CALC-TAX-YES OF GRSWK
               AND GROSSUP-NO OF CHECK

               PERFORM PD000-CLEAR-TAX-CALC
               PERFORM PD300-SET-DED-FOR-RECALC

           END-IF

           .
       TAKE-DEDUCTIONS-EXIT.


      /*****************************************************************
      *                                                                *
       RA100-DEDUCT-BEFORE-TAX SECTION.
       RA100.
      *                                                                *
      ******************************************************************

           SET GENERATE-378-NO OF W-SW TO TRUE
           COMPUTE DED-CUR OF DARRY(DARRY-IDX DCLAS-IDX)
                   =  DED-CUR OF DARRY(DARRY-IDX DCLAS-IDX)
                   +  DED-CUR-PAYBK OF DARRY(DARRY-IDX DCLAS-IDX)

           COMPUTE WK-DED-CUR OF W-WK
                   =  DED-CUR OF DARRY(DARRY-IDX DCLAS-IDX)
                   -  DED-CUR-PAYBK OF DARRY(DARRY-IDX DCLAS-IDX)

                  MOVE DED-CUR-PAYBK OF DARRY(DARRY-IDX DCLAS-IDX)
                    TO WK-DED-CUR-PAYBK-HOLD OF W-WK

           SET PCT-NET-NO OF W-SW  TO  TRUE

           IF DED-CUR OF DARRY(DARRY-IDX DCLAS-IDX)
                   >  ZERO
               AND PAYROLL-STEP-CALC OF PSLCT
               AND DEDNET OF GRSWK
                       <  DED-CUR OF DARRY(DARRY-IDX DCLAS-IDX)
               AND MANUAL-CHECK-NO OF CHECK

               SET DEDC1-IDX  TO  DCLAS-IDX

               COMPUTE DED-CUR OF DARRY(DARRY-IDX DCLAS-IDX)
                       =  DEDNET OF GRSWK

               IF DED-CUR OF DARRY(DARRY-IDX DCLAS-IDX)
                       >  WK-DED-CUR OF W-WK

                  COMPUTE DED-CUR-PAYBK OF DARRY(DARRY-IDX DCLAS-IDX)
                          =  DED-CUR OF DARRY(DARRY-IDX DCLAS-IDX)
                          -  WK-DED-CUR OF W-WK
                  SET DED-CUR-PAYBK-YES OF W-SW  TO TRUE
                  SET DED-NOT-TAKEN-NO  OF W-SW TO TRUE
                  PERFORM RC000-ADJUST-SPCL-ACCUM

               ELSE
                   PERFORM VARYING DCALC-IDX FROM  1  BY  1
                           UNTIL DCALC-IDX  >  DEDUCTION-CALC-MAX
                                                  OF DARRY

                       IF DED-CALC-PCT-NET OF DARRY(DARRY-IDX
                                               DCLAS-IDX DCALC-IDX)

                           SET PCT-NET-YES OF W-SW  TO  TRUE
                       END-IF
                   END-PERFORM

                   IF PCT-NET-YES OF W-SW

                       COMPUTE WK-DED-CUR-ADJUST OF W-WK
                           =  WK-DED-CUR OF W-WK
                           -  DEDNET OF GRSWK
                       MOVE DEDNET OF GRSWK
                               TO DED-CUR OF DARRY(DARRY-IDX DCLAS-IDX)
                       MOVE DEDNET OF GRSWK  TO  WK-DED-CUR OF W-WK
                       ADD MIN-CHECK-ADJUST OF GRSWK
                               TO DED-CUR OF DARRY(DARRY-IDX DCLAS-IDX)
                       ADD MIN-CHECK-ADJUST OF GRSWK
                               TO  WK-DED-CUR OF W-WK

                       MOVE ZERO  TO  DED-CUR-PAYBK OF DARRY
                                          (DARRY-IDX DCLAS-IDX)
                       IF WK-DED-CUR-ADJUST OF W-WK > ZERO
                          MOVE  DED-NOT-TAKEN OF DARRY
                                                 (DARRY-IDX DCLAS-IDX)
                            TO  HOLD-DED-NOT-TAKEN OF W-WK
                          MOVE  WK-DED-CUR-ADJUST OF W-WK
                            TO  DED-NOT-TAKEN OF DARRY
                                                 (DARRY-IDX DCLAS-IDX)
                          SET DED-NOT-TAKEN-YES OF W-SW TO TRUE
                          SET DED-CUR-PAYBK-NO  OF W-SW TO TRUE
                          PERFORM RC000-ADJUST-SPCL-ACCUM
                          MOVE  HOLD-DED-NOT-TAKEN OF W-WK
                            TO  DED-NOT-TAKEN OF DARRY
                                                 (DARRY-IDX DCLAS-IDX)
                       END-IF

                   ELSE

                       IF PARTIAL-DED-ALLOW-YES OF DEDT5(DEDT5-IDX
                                                         DEDC1-IDX)
                           AND DEDNET OF GRSWK > ZERO

                           IF REASON-NOT-TAKEN-ANN OF DARRY(DARRY-IDX
                                                            DCLAS-IDX)

                              MOVE ZERO  TO  DED-NOT-TAKEN OF DARRY
                                                (DARRY-IDX DCLAS-IDX)
                              SET REASON-NOT-TAKEN-NONE OF DARRY
                                                (DARRY-IDX DCLAS-IDX)
                                      TO  TRUE
                           ELSE
                              SET GENERATE-378-YES OF W-SW TO TRUE
                              SET REASON-NOT-TAKEN-NET OF
                                     DARRY(DARRY-IDX DCLAS-IDX)
                               TO TRUE
                           END-IF

                           COMPUTE WK-DED-NOT-TAKEN OF W-WK
                                   =  WK-DED-CUR OF W-WK
                                   -  DEDNET OF GRSWK
                           MOVE  DED-NOT-TAKEN OF DARRY(DARRY-IDX
                                                        DCLAS-IDX)
                             TO  HOLD-DED-NOT-TAKEN OF W-WK
                           MOVE  WK-DED-NOT-TAKEN OF W-WK
                             TO  DED-NOT-TAKEN OF DARRY(DARRY-IDX
                                                        DCLAS-IDX)
                           SET DED-NOT-TAKEN-YES OF W-SW TO TRUE
                           SET DED-CUR-PAYBK-NO  OF W-SW TO TRUE
                           PERFORM RC000-ADJUST-SPCL-ACCUM
                           MOVE  HOLD-DED-NOT-TAKEN OF W-WK
                             TO  DED-NOT-TAKEN OF DARRY(DARRY-IDX
                                                        DCLAS-IDX)

                           COMPUTE DED-NOT-TAKEN OF DARRY(DARRY-IDX
                                                          DCLAS-IDX)
                                   =  DED-NOT-TAKEN OF DARRY(DARRY-IDX
                                                             DCLAS-IDX)
                                   +  (WK-DED-CUR OF W-WK
                                   -  DEDNET OF GRSWK)

                           MOVE DEDNET OF GRSWK
                                   TO DED-CUR OF DARRY(DARRY-IDX
                                                       DCLAS-IDX)
                           MOVE DEDNET OF GRSWK  TO  WK-DED-CUR OF W-WK

                           MOVE ZERO  TO  DED-CUR-PAYBK OF DARRY
                                              (DARRY-IDX DCLAS-IDX)
                           IF WK-DED-CUR-PAYBK-HOLD OF W-WK  NOT = ZERO
                              SET DED-CUR-PAYBK-YES OF W-SW TO TRUE
                              SET DED-NOT-TAKEN-NO  OF W-SW TO TRUE
                              PERFORM RC000-ADJUST-SPCL-ACCUM
                           END-IF
                       ELSE

                           IF REASON-NOT-TAKEN-ANN OF DARRY(DARRY-IDX
                                                            DCLAS-IDX)

                               MOVE ZERO  TO  DED-NOT-TAKEN OF DARRY
                                                 (DARRY-IDX DCLAS-IDX)
                               SET REASON-NOT-TAKEN-NONE OF DARRY
                                                 (DARRY-IDX DCLAS-IDX)
                                       TO  TRUE
                           END-IF

                           MOVE DED-NOT-TAKEN OF DARRY
                                                 (DARRY-IDX DCLAS-IDX)
                                   TO  HOLD-DED-NOT-TAKEN OF W-WK
                           MOVE WK-DED-CUR OF W-WK
                                   TO  WK-DED-NOT-TAKEN OF W-WK

                           COMPUTE DED-NOT-TAKEN OF DARRY(DARRY-IDX
                                                          DCLAS-IDX)
                                   =  DED-NOT-TAKEN OF DARRY(DARRY-IDX
                                                            DCLAS-IDX)
                                   +  WK-DED-CUR OF W-WK

                           MOVE ZERO  TO  WK-DED-CUR OF W-WK

                           IF DED-CUR-PAYBK OF DARRY(DARRY-IDX
                                                     DCLAS-IDX)
                                   >  ZERO
                               AND DED-CUR-PAYBK OF DARRY(DARRY-IDX
                                                            DCLAS-IDX)
                                       >  DEDNET OF GRSWK

                               MOVE DEDNET OF GRSWK
                                       TO  DED-CUR-PAYBK OF DARRY
                                                (DARRY-IDX DCLAS-IDX)
                           END-IF

                           IF DEDNET OF GRSWK  <  ZERO

                               MOVE ZERO TO  DED-CUR-PAYBK OF DARRY
                                                (DARRY-IDX DCLAS-IDX)
                           END-IF

                           MOVE DED-CUR-PAYBK OF DARRY(DARRY-IDX
                                                           DCLAS-IDX)
                                   TO  DED-CUR OF DARRY
                                                (DARRY-IDX DCLAS-IDX)
                           SET DED-CUR-PAYBK-YES OF W-SW TO TRUE
                           SET DED-NOT-TAKEN-NO  OF W-SW TO TRUE

                           IF HOLD-DED-NOT-TAKEN OF W-WK  >  ZERO

                               SET DED-NOT-TAKEN-YES OF W-SW  TO  TRUE
                               MOVE WK-DED-NOT-TAKEN OF W-WK
                                    TO  DED-NOT-TAKEN
                                           OF DARRY(DARRY-IDX DCLAS-IDX)
                           END-IF

                           PERFORM RC000-ADJUST-SPCL-ACCUM

                           IF HOLD-DED-NOT-TAKEN OF W-WK  >  ZERO

                               COMPUTE DED-NOT-TAKEN
                                           OF DARRY(DARRY-IDX DCLAS-IDX)
                                       =  HOLD-DED-NOT-TAKEN OF W-WK
                                       +  WK-DED-NOT-TAKEN OF W-WK
                           END-IF
                       END-IF
                   END-IF
               END-IF

               PERFORM RB100-ADJUST-NET-AMOUNT

               IF PCT-NET-NO OF W-SW

                   SET CALC-NO OF DARRY(DARRY-IDX DCLAS-IDX)  TO  TRUE
                   SET CALC-TAX-YES OF GRSWK  TO  TRUE
                   MOVE WK-DED-CUR OF W-WK  TO  DED-CUR-ACC OF DARRY
                                                (DARRY-IDX DCLAS-IDX)
               END-IF

               IF DED-NOT-TAKEN OF DARRY(DARRY-IDX DCLAS-IDX)  >  ZERO
                   AND REASON-NOT-TAKEN-NONE OF DARRY(DARRY-IDX
                                                      DCLAS-IDX)

                   SET GENERATE-378-YES OF W-SW TO TRUE
                   SET REASON-NOT-TAKEN-NET OF DARRY(DARRY-IDX
                                                     DCLAS-IDX)
                           TO  TRUE
                   SET DED-NOT-TAKEN-YES OF W-SW TO TRUE
                   SET DED-CUR-PAYBK-NO  OF W-SW TO TRUE
                   PERFORM RC000-ADJUST-SPCL-ACCUM
               END-IF

               IF PCT-NET-NO OF W-SW

                   SET START-VALUE OF W-WK  TO  DCLAS-IDX
                   IF START-VALUE OF W-WK  =  2
                       AND (DED-CUR OF DARRY(DARRY-IDX 1)  >  ZERO
                       OR  DED-CUR-PAYBK OF DARRY(DARRY-IDX 1)
                               >  ZERO)

                       IF REASON-NOT-TAKEN-ANN OF DARRY(DARRY-IDX 1)

                           MOVE ZERO  TO  DED-NOT-TAKEN OF DARRY
                                                       (DARRY-IDX 1)
                           SET REASON-NOT-TAKEN-NONE OF DARRY
                                                       (DARRY-IDX 1)
                                   TO  TRUE
                       END-IF

                       MOVE DED-NOT-TAKEN OF DARRY(DARRY-IDX 1)
                            TO  HOLD-DED-NOT-TAKEN OF W-WK
                       MOVE DED-CUR OF DARRY(DARRY-IDX 1)
                            TO  DED-NOT-TAKEN OF DARRY(DARRY-IDX 1)
                       SET DCLAS-HOLD OF W-WK  TO  DCLAS-IDX
                       SET DCLAS-IDX           TO  1
                       SET HOLD-DEDC1 OF W-WK  TO  DEDC1-IDX
                       SET DEDC1-IDX           TO  1
                       PERFORM RC000-ADJUST-SPCL-ACCUM
                       SET DCLAS-IDX           TO  DCLAS-HOLD OF W-WK
                       SET DEDC1-IDX           TO  HOLD-DEDC1 OF W-WK
                       MOVE HOLD-DED-NOT-TAKEN OF W-WK
                            TO DED-NOT-TAKEN OF DARRY(DARRY-IDX 1)
                       COMPUTE DED-NOT-TAKEN OF DARRY(DARRY-IDX 1)
                               =  DED-NOT-TAKEN OF DARRY(DARRY-IDX 1)
                               +  DED-CUR OF DARRY(DARRY-IDX 1)

                       MOVE ZERO  TO DED-CUR OF DARRY(DARRY-IDX 1)
                       MOVE ZERO  TO DED-CUR-PAYBK OF DARRY
                                                     (DARRY-IDX 1)
                       SET CALC-NO OF DARRY(DARRY-IDX 1)  TO  TRUE

                       IF DED-NOT-TAKEN OF DARRY(DARRY-IDX 1)  >  ZERO
                           AND REASON-NOT-TAKEN-NONE OF DARRY
                                                       (DARRY-IDX 1)

                           SET GENERATE-378-YES OF W-SW TO TRUE
                           SET REASON-NOT-TAKEN-NET OF DARRY
                                                      (DARRY-IDX 1)
                                   TO  TRUE
                       END-IF
                   END-IF

                   PERFORM RA150-RECALC-OTHERS
               END-IF
           ELSE

               IF PAYROLL-STEP-CALC OF PSLCT
                   AND ARREARS-BAL OF DARRY(DARRY-IDX DCLAS-IDX)
                           NOT =  ZERO
                   AND REASON-NOT-TAKEN-NONE OF DARRY(DARRY-IDX
                                                      DCLAS-IDX)
                   AND MANUAL-CHECK-NO OF CHECK

                   IF CALC-TAX-YES OF GRSWK

                       SET CALC-NO OF DARRY(DARRY-IDX DCLAS-IDX)
                               TO  TRUE
                   END-IF
               END-IF

               IF DED-NOT-TAKEN OF DARRY(DARRY-IDX DCLAS-IDX)
                       =  ZERO

                   SET REASON-NOT-TAKEN-NONE OF DARRY(DARRY-IDX
                                                          DCLAS-IDX)
                           TO  TRUE
               END-IF

               PERFORM RB100-ADJUST-NET-AMOUNT

               COMPUTE DED-CUR-ACC OF DARRY(DARRY-IDX DCLAS-IDX)
                       =  DED-CUR OF DARRY(DARRY-IDX DCLAS-IDX)
                       -  DED-CUR-PAYBK OF DARRY(DARRY-IDX DCLAS-IDX)

               IF DCLAS-IDX  >  1

                   SET DCLAS-IDX  TO  1
                   PERFORM RA200-DEDUCT-AFTER-TAX
                   SET DCLAS-IDX  TO  2
               ELSE

                   PERFORM VARYING DCLAS-IDX FROM  2  BY  1
                           UNTIL DCLAS-IDX  >  DEDUCTION-CLASS-MAX
                                                      OF DARRY

                       SET CALC-NO OF DARRY(DARRY-IDX DCLAS-IDX)
                               TO  TRUE
                   END-PERFORM

                   SET DCLAS-IDX  TO  1
               END-IF
           END-IF

           .
       DEDUCT-BEFORE-TAX-EXIT.


      /*****************************************************************
      *                                                                *
       RA150-RECALC-OTHERS SECTION.
       RA150.
      *                                                                *
      ******************************************************************


           SET START-VALUE OF W-WK  TO  DCLAS-IDX
           ADD 1  TO  START-VALUE OF W-WK

           PERFORM VARYING DCLAS-IDX  FROM  START-VALUE OF W-WK
                                       BY  1
                   UNTIL DCLAS-IDX  >  DEDUCTION-CLASS-MAX
                                            OF DARRY
                       OR DED-CLASS OF DARRY(DARRY-IDX DCLAS-IDX)
                               =  SPACE

               IF DED-CLASS-TAXABLE OF DARRY(DARRY-IDX DCLAS-IDX)
                   AND PLAN-TYPE-LIFE-ADD OF DARRY(DARRY-IDX)

                   CONTINUE
               ELSE

                   IF NOT DED-CLASS-BEFORE-TAX OF DARRY(DARRY-IDX
                                                        DCLAS-IDX)

                       SET CALC-YES OF DARRY(DARRY-IDX DCLAS-IDX)
                               TO  TRUE
                       SET CALC-TAX-YES OF GRSWK  TO  TRUE
                       SET HOLD-DEDC1 OF W-WK TO DEDC1-IDX
                       MOVE DED-NOT-TAKEN OF DARRY(DARRY-IDX
                                                   DCLAS-IDX)
                               TO  HOLD-DED-NOT-TAKEN OF W-WK
                       MOVE DED-CUR OF DARRY(DARRY-IDX DCLAS-IDX)
                               TO  DED-NOT-TAKEN OF DARRY(DARRY-IDX
                                                   DCLAS-IDX)
                       PERFORM VARYING DEDC1-IDX FROM 1 BY 1 UNTIL
                               DEDC1-IDX > DED-CLASS-TABLE-MAX
                                                           OF DEDT1
                               OR DED-CLASS OF DEDT5(DEDT5-IDX
                                                     DEDC1-IDX)
                                       = SPACE
                           IF DED-CLASS OF DEDT5(DEDT5-IDX DEDC1-IDX)
                               = DED-CLASS OF DARRY(DARRY-IDX
                                                    DCLAS-IDX)

                               SET DED-NOT-TAKEN-YES OF W-SW TO TRUE
                               SET DED-CUR-PAYBK-NO  OF W-SW TO TRUE
                               PERFORM RC000-ADJUST-SPCL-ACCUM
                           END-IF
                       END-PERFORM
                       MOVE HOLD-DED-NOT-TAKEN OF W-WK
                               TO  DED-NOT-TAKEN OF DARRY(DARRY-IDX
                                                          DCLAS-IDX)
                       SET DEDC1-IDX TO HOLD-DEDC1 OF W-WK
                       MOVE ZERO  TO  DED-CUR OF DARRY(DARRY-IDX
                                                       DCLAS-IDX)
                       PERFORM VARYING DCALC-IDX  FROM  1  BY  1
                               UNTIL DCALC-IDX
                                       >  DEDUCTION-CALC-MAX OF DARRY

                           SET FLAT-TAKEN-NO OF DARRY(DARRY-IDX
                                                   DCLAS-IDX DCALC-IDX)
                                   TO  TRUE
                       END-PERFORM
                   END-IF
               END-IF
           END-PERFORM

           .
       RECALC-OTHERS-EXIT.


      /*****************************************************************
      *                                                                *
       RA200-DEDUCT-AFTER-TAX SECTION.
       RA200.
      *                                                                *
      ******************************************************************

           SET GENERATE-378-NO OF W-SW TO TRUE
           IF MANUAL-CHECK-NO OF CHECK

               SET CALC-DED-NO OF W-SW  TO  TRUE
               MOVE ZERO  TO  SAVE-PCT-NET OF W-WK
               MOVE ZERO  TO  TOT-ADDL-AMT OF W-WK
               PERFORM VARYING DCALC-IDX FROM 1 BY 1
                       UNTIL DCALC-IDX  >  DEDUCTION-CALC-MAX
                                                         OF DARRY

                   IF DED-CALC-PCT-NET OF DARRY(DARRY-IDX
                                              DCLAS-IDX DCALC-IDX)

                       SET CALC-DED-YES OF W-SW  TO  TRUE
                       SET DEDT1-IDX  TO  DEDTB-PTR OF DARRY(DARRY-IDX)
                       SET DEDT5-IDX  TO  DEDTB-PTR OF DARRY(DARRY-IDX)
                       SET DEDC1-IDX  TO  DCLAS-IDX

                       IF SEPCHK OF CHECK  NOT =  ZERO
                           AND SEPCHK-DED-NO OF DEDT5(DEDT5-IDX
                                                      DEDC1-IDX)

                           SET CALC-DED-NO OF W-SW  TO  TRUE
                       END-IF

                       IF  PLAN-TYPE-SAVINGS  OF DARRY(DARRY-IDX) AND
                           (NOT REASON-NOT-TAKEN-NONE
                              OF DARRY(DARRY-IDX DCLAS-IDX)) AND
                           DED-NOT-TAKEN
                                OF DARRY(DARRY-IDX DCLAS-IDX) NOT = 0


                           SET CALC-DED-NO OF W-SW  TO  TRUE
                       END-IF


                       IF CALC-NO OF DARRY(DARRY-IDX DCLAS-IDX)

                           SET CALC-DED-NO OF W-SW  TO  TRUE

                       END-IF

                       IF PLAN-TYPE-DEDUCTION OF DARRY(DARRY-IDX)
                           IF (DED-SUBSET-NO OF DEDT1(DEDT1-IDX)
                               AND GNL-DED-TAKEN-SUBSET OF CHECK)
                               OR (GNL-DED-TAKEN-NONE OF CHECK)
                               OR (EMPL-STATUS-TERMINATED OF CHECK
                                  AND STOP-DED-TERM-YES
                                      OF DEDT5(DEDT5-IDX DEDC1-IDX))

                               SET CALC-DED-NO OF W-SW  TO  TRUE
                           END-IF
                       ELSE
                           SET BENEFIT-RCD-IDX
                               TO  CHECK-PTR OF DARRY(DARRY-IDX)
                           IF (DED-SUBSET-NO OF DEDT1(DEDT1-IDX)
                              AND BEN-DED-TAKEN-SUBSET
                                      OF CHECK(BENEFIT-RCD-IDX))
                               OR (BEN-DED-TAKEN-NONE
                                      OF CHECK(BENEFIT-RCD-IDX))
                               OR (EMPL-STATUS-TERMINATED OF CHECK
                                  AND STOP-DED-TERM-YES
                                      OF DEDT5(DEDT5-IDX DEDC1-IDX))

                               SET CALC-DED-NO OF W-SW  TO  TRUE
                           END-IF
                       END-IF

                       IF ONE-TIME-OVERRIDE OF DARRY
                          (DARRY-IDX DCLAS-IDX DCALC-IDX)

                          SET CALC-DED-YES OF W-SW TO TRUE
                       END-IF

                       IF CALC-DED-YES OF W-SW

                           MOVE DED-RATE-PCT OF DARRY(DARRY-IDX
                                            DCLAS-IDX DCALC-IDX)
                                   TO  SAVE-PCT-NET OF W-WK
                       END-IF
                   ELSE

                       COMPUTE TOT-ADDL-AMT OF W-WK
                              =  TOT-ADDL-AMT OF W-WK
                              +  DED-ADDL-AMT OF DARRY(DARRY-IDX
                                            DCLAS-IDX DCALC-IDX)
                   END-IF
               END-PERFORM

               IF SAVE-PCT-NET OF W-WK  NOT =  ZERO

                   COMPUTE DED-CUR OF DARRY(DARRY-IDX DCLAS-IDX)
                                                         ROUNDED
                           =  (TOT-ADDL-AMT OF W-WK
                           +  ((DEDNET OF GRSWK
                           +  MIN-CHECK-ADJUST OF GRSWK)
                           *  SAVE-PCT-NET OF W-WK
                           /  100))
                           + HIGH-PREC-0

                   IF DED-CUR OF DARRY(DARRY-IDX DCLAS-IDX)  >  ZERO
                       AND PLAN-TYPE-SAVINGS OF DARRY(DARRY-IDX)
                       AND LIMIT-TEST-YES OF CHECK

                       SET DARRY-PASS OF W-PASSX  TO  DARRY-IDX
                       SET DCLAS-PASS OF W-PASSX  TO  DCLAS-IDX
                       SET DCALC-PASS OF W-PASSX  TO  DCALC-IDX
                       IF  COUNTRY-CANADA OF PYGRP
                           MOVE SAV-ATAX-CALC-SW OF W-SW
                                TO  SAV-ATAX-CALC-PASS OF W-PASSX
                           MOVE DARRY-SAV OF W-WK
                                TO  DARRY-SAV-PASS OF W-PASSX
                       ELSE
                           SET SAV-ATAX-CALC-NO OF W-PASSX  TO  TRUE
                       END-IF
                       MOVE DED-CUR OF DARRY(DARRY-IDX DCLAS-IDX) TO
                                             WK-AMT OF W-WK
                       MOVE WK-AMT OF W-WK  TO  FLAT-AMT OF GRSWK

                       CALL 'PSPDCLIM' USING   DARRY
                                               SARRY
                                               DEDT1
                                               DEDT2
                                               DEDT3
                                               DEDT4
                                               DEDT5
                                               DEDT6
                                               LMTTB
                                               GRSWK
                                               CHECK
                                               PSLCT
                                               PYGRP                    HP99999
                                               W-PASSX
                                               SQLRT

                       MOVE FLAT-AMT OF GRSWK  TO
                                   DED-CUR OF DARRY(DARRY-IDX DCLAS-IDX)
                   END-IF

                   IF GOAL-AMT OF DARRY(DARRY-IDX)  NOT =  ZERO
                       AND GOAL-BAL OF DARRY(DARRY-IDX)
                               +  DED-CUR OF DARRY(DARRY-IDX
                                                   DCLAS-IDX)
                               >  GOAL-AMT OF DARRY(DARRY-IDX)

                       COMPUTE DED-CUR OF DARRY(DARRY-IDX
                                                DCLAS-IDX)
                               =  GOAL-AMT OF DARRY(DARRY-IDX)
                               -  GOAL-BAL OF DARRY(DARRY-IDX)
                   END-IF
               END-IF
           END-IF

           COMPUTE DED-CUR OF DARRY(DARRY-IDX DCLAS-IDX)
                   =  DED-CUR OF DARRY(DARRY-IDX DCLAS-IDX)
                   +  DED-CUR-PAYBK OF DARRY(DARRY-IDX DCLAS-IDX)

           COMPUTE WK-DED-CUR OF W-WK
                   =  DED-CUR OF DARRY(DARRY-IDX DCLAS-IDX)
                   -  DED-CUR-PAYBK OF DARRY(DARRY-IDX DCLAS-IDX)

                  MOVE DED-CUR-PAYBK OF DARRY(DARRY-IDX DCLAS-IDX)
                    TO WK-DED-CUR-PAYBK-HOLD OF W-WK

           IF DED-CUR OF DARRY(DARRY-IDX DCLAS-IDX)
                   >  ZERO

               IF GROSSUP-YES OF CHECK
                   AND SEPCHK OF CHECK NOT = ZERO
                   AND GROSSUP-ADD-GROSS-NO OF GRSWK > ZERO

                   MOVE DEDNET OF GRSWK
                                TO  GROSSUP-DEDNET OF W-WK
                   COMPUTE DEDNET OF GRSWK
                           =  DEDNET OF GRSWK
                           -  GROSSUP-ADD-GROSS-NO OF GRSWK
               END-IF

               IF WK-DED-CUR OF W-WK
                       >  ZERO
                   AND PAYROLL-STEP-CALC OF PSLCT
                   AND WK-DED-CUR OF W-WK
                           >  DEDNET OF GRSWK
                   AND MANUAL-CHECK-NO OF CHECK

                   PERFORM RA300-CALC-PARTIAL

                   IF WK-DED-CUR OF W-WK  >  ZERO

                       MOVE ZERO  TO  DED-CUR-PAYBK OF DARRY(DARRY-IDX
                                                            DCLAS-IDX)
                       IF WK-DED-CUR-PAYBK-HOLD OF W-WK > ZERO
                          SET DEDC1-IDX  TO  DCLAS-IDX
                          SET DED-CUR-PAYBK-YES OF W-SW TO TRUE
                          SET DED-NOT-TAKEN-NO  OF W-SW TO TRUE
                          PERFORM RC000-ADJUST-SPCL-ACCUM
                       END-IF
                   ELSE

                       IF DEDNET OF GRSWK
                               <  DED-CUR-PAYBK OF DARRY(DARRY-IDX
                                                         DCLAS-IDX)
                           IF DEDNET OF GRSWK > ZERO

                               MOVE DEDNET OF GRSWK
                                       TO  DED-CUR-PAYBK OF DARRY
                                               (DARRY-IDX DCLAS-IDX)
                           ELSE

                               MOVE ZERO
                                       TO  DED-CUR-PAYBK OF DARRY
                                               (DARRY-IDX DCLAS-IDX)
                           END-IF
                           SET DEDC1-IDX  TO  DCLAS-IDX
                           SET DED-CUR-PAYBK-YES OF W-SW TO TRUE
                           SET DED-NOT-TAKEN-NO  OF W-SW TO TRUE
                           PERFORM RC000-ADJUST-SPCL-ACCUM

                       END-IF
                   END-IF

                   COMPUTE DED-CUR OF DARRY(DARRY-IDX DCLAS-IDX)
                           =  WK-DED-CUR OF W-WK
                           +  DED-CUR-PAYBK OF DARRY(DARRY-IDX
                                                     DCLAS-IDX)

                   MOVE WK-DED-CUR OF W-WK  TO  DED-CUR-ACC OF DARRY
                                               (DARRY-IDX DCLAS-IDX)

                   SET CALC-NO OF DARRY(DARRY-IDX DCLAS-IDX)  TO  TRUE

                   PERFORM RB100-ADJUST-NET-AMOUNT

                   IF DED-NOT-TAKEN OF DARRY(DARRY-IDX DCLAS-IDX)
                           >  ZERO

                       PERFORM RA150-RECALC-OTHERS
                   ELSE

                       PERFORM VARYING DCLAS-IDX FROM  2  BY  1
                               UNTIL DCLAS-IDX  >  DEDUCTION-CLASS-MAX
                                                          OF DARRY

                           SET CALC-NO OF DARRY(DARRY-IDX DCLAS-IDX)
                                   TO  TRUE
                       END-PERFORM

                       SET DCLAS-IDX  TO  1
                   END-IF
               ELSE

                   IF PAYROLL-STEP-CALC OF PSLCT
                       AND DED-CUR OF DARRY(DARRY-IDX DCLAS-IDX)
                               >  DEDNET OF GRSWK
                       AND MANUAL-CHECK-NO OF CHECK

                       COMPUTE DED-CUR-PAYBK OF DARRY(DARRY-IDX
                                                      DCLAS-IDX)
                               =  DEDNET OF GRSWK
                               -  WK-DED-CUR OF W-WK

                       SET DEDC1-IDX  TO  DCLAS-IDX
                       SET DED-CUR-PAYBK-YES OF W-SW TO TRUE
                       SET DED-NOT-TAKEN-NO  OF W-SW TO TRUE
                       PERFORM RC000-ADJUST-SPCL-ACCUM

                       MOVE DEDNET OF GRSWK  TO  DED-CUR OF DARRY
                                               (DARRY-IDX DCLAS-IDX)
                       PERFORM RB100-ADJUST-NET-AMOUNT

                       PERFORM VARYING DCLAS-IDX FROM  2  BY  1
                               UNTIL DCLAS-IDX  >  DEDUCTION-CLASS-MAX
                                                          OF DARRY

                           SET CALC-NO OF DARRY(DARRY-IDX DCLAS-IDX)
                                   TO  TRUE
                       END-PERFORM

                       SET DCLAS-IDX  TO  1
                   ELSE

                       IF PAYROLL-STEP-CALC OF PSLCT

                           PERFORM RB100-ADJUST-NET-AMOUNT

                           PERFORM VARYING DCLAS-IDX FROM  2  BY  1
                                   UNTIL DCLAS-IDX
                                           >  DEDUCTION-CLASS-MAX
                                                          OF DARRY

                               SET CALC-NO OF DARRY(DARRY-IDX
                                                    DCLAS-IDX)
                                       TO  TRUE
                           END-PERFORM

                           SET DCLAS-IDX  TO  1
                       END-IF
                   END-IF
               END-IF

               IF GROSSUP-YES OF CHECK
                   AND SEPCHK OF CHECK NOT = ZERO
                   AND GROSSUP-ADD-GROSS-NO OF GRSWK > ZERO

                   MOVE GROSSUP-DEDNET OF W-WK
                                TO  DEDNET OF GRSWK
               END-IF
           ELSE

               IF PAYROLL-STEP-CALC OF PSLCT

                   IF DED-NOT-TAKEN OF DARRY(DARRY-IDX DCLAS-IDX)
                                >  ZERO
                       AND REASON-NOT-TAKEN-NET
                                OF DARRY(DARRY-IDX DCLAS-IDX)
                       AND NOT RECALC-YES OF DARRY(DARRY-IDX DCLAS-IDX)

                       SET HOLD-DEDC1 OF W-WK TO DEDC1-IDX
                       SET DEDC1-IDX  TO  DCLAS-IDX
                       PERFORM RC000-ADJUST-SPCL-ACCUM
                       SET DEDC1-IDX TO HOLD-DEDC1 OF W-WK
                   END-IF

                   PERFORM RB100-ADJUST-NET-AMOUNT

                   PERFORM VARYING DCLAS-IDX FROM  2  BY  1
                           UNTIL DCLAS-IDX  >  DEDUCTION-CLASS-MAX
                                                      OF DARRY

                       SET CALC-NO OF DARRY(DARRY-IDX DCLAS-IDX)
                               TO  TRUE
                   END-PERFORM

                   SET DCLAS-IDX  TO  1
               END-IF
           END-IF

           .
       DEDUCT-AFTER-TAX-EXIT.


      /*****************************************************************
      *                                                                *
       RA300-CALC-PARTIAL SECTION.
       RA300.
      *                                                                *
      ******************************************************************

           SET GENERATE-378-NO OF W-SW TO TRUE
           SET DEDC1-IDX  TO  DCLAS-IDX

           SET DEDCD-FOUND-NO TO TRUE
           PERFORM VARYING DEARY-IDX FROM 1 BY 1
                     UNTIL DEARY-IDX > 20
                       OR  DEDCD-FOUND-YES
                       OR GARNID OF DEARY(DEARY-IDX) = SPACE

               PERFORM VARYING DEARY-RL-IDX FROM 1 BY 1
                         UNTIL DEARY-RL-IDX > 3
                           OR  DEDCD-FOUND-YES
                           OR GARN-RULE-ID OF
                                DEARY(DEARY-IDX DEARY-RL-IDX) = SPACE

                   PERFORM VARYING DEARY-DED-IDX FROM 1 BY 1
                             UNTIL DEARY-DED-IDX > 40
                               OR  DEDCD-FOUND-YES
                               OR DEDCD OF DEARY(DEARY-IDX
                                    DEARY-RL-IDX DEARY-DED-IDX) = SPACE
                       IF DEDCD OF DEARY(DEARY-IDX
                                         DEARY-RL-IDX
                                         DEARY-DED-IDX) =
                          DEDCD OF DARRY(DARRY-IDX)
                          SET DEDCD-FOUND-YES TO TRUE
                          SET CALC-TAX-YES OF GRSWK TO TRUE
                           IF GARN-DED-CALC-MAX OF DEARY(DEARY-IDX) AND
                              PARTIAL-DED-ALLOW-YES OF DEDT5
                                              (DEDT5-IDX DEDC1-IDX)

                              MOVE GARNID OF DEARY(DEARY-IDX) TO
                                   GARNID OF MSGDATA1-A
                              MOVE DEDCD OF DEARY(DEARY-IDX,
                                                  DEARY-RL-IDX
                                                  DEARY-DED-IDX) TO
                                   DEDCD OF MSGDATA2-A
                              MOVE MSGDATA1-A TO MSGDATA1 OF PYMSG
                              MOVE MSGDATA2-A TO MSGDATA2 OF PYMSG
                              SET MSGID-DE-DED-CONFLICT OF
                                   PYMSG TO TRUE
                              PERFORM ZM000-MESSAGE
                              SET PAY-LINE-STATUS-ERROR OF
                                   CHECK TO TRUE
                           END-IF
                       END-IF
                   END-PERFORM
               END-PERFORM
           END-PERFORM

           IF REASON-NOT-TAKEN-ANN OF DARRY(DARRY-IDX
                                            DCLAS-IDX)

               MOVE ZERO  TO  DED-NOT-TAKEN OF DARRY
                                 (DARRY-IDX DCLAS-IDX)
               SET REASON-NOT-TAKEN-NONE OF DARRY
                                 (DARRY-IDX DCLAS-IDX)
                       TO  TRUE
           END-IF

           MOVE DED-NOT-TAKEN OF DARRY(DARRY-IDX DCLAS-IDX)
                         TO  HOLD-DED-NOT-TAKEN OF W-WK
           IF PARTIAL-DED-ALLOW-YES OF DEDT5(DEDT5-IDX DEDC1-IDX)
                   AND DEDNET OF GRSWK  >  ZERO
                   AND CHECK-NET OF GRSWK  >  ZERO

               IF CHECK-NET OF GRSWK  >  DEDNET OF GRSWK

                   MOVE DEDNET OF GRSWK  TO  PARTIAL-DED OF W-WK
               ELSE
                   MOVE CHECK-NET OF GRSWK  TO  PARTIAL-DED OF W-WK
               END-IF

               COMPUTE WK-DED-NOT-TAKEN OF W-WK
                       =  WK-DED-CUR OF W-WK
                       -  PARTIAL-DED OF W-WK

               COMPUTE DED-NOT-TAKEN OF DARRY(DARRY-IDX DCLAS-IDX)
                       =  DED-NOT-TAKEN OF DARRY(DARRY-IDX DCLAS-IDX)
                       +  WK-DED-CUR OF W-WK
                       -  PARTIAL-DED OF W-WK
               MOVE PARTIAL-DED OF W-WK  TO  WK-DED-CUR OF W-WK
           ELSE
               COMPUTE DED-NOT-TAKEN
                       OF DARRY(DARRY-IDX DCLAS-IDX)
                       =  DED-NOT-TAKEN
                               OF DARRY(DARRY-IDX DCLAS-IDX)
                       +  WK-DED-CUR OF W-WK
               MOVE WK-DED-CUR OF W-WK  TO  WK-DED-NOT-TAKEN OF W-WK
               MOVE ZERO  TO  WK-DED-CUR OF W-WK
           END-IF

           IF DED-NOT-TAKEN OF DARRY(DARRY-IDX DCLAS-IDX)  >  ZERO

               SET GENERATE-378-YES OF W-SW TO TRUE
               SET REASON-NOT-TAKEN-NET OF DARRY(DARRY-IDX DCLAS-IDX)
                       TO  TRUE

               IF PAYCHECK-ADJUST-YES OF CHECK

                   COMPUTE TTL-DED-NOT-TAKEN OF W-WK
                           =  TTL-DED-NOT-TAKEN OF W-WK
                           +  DED-NOT-TAKEN OF DARRY(DARRY-IDX
                                                     DCLAS-IDX)
               END-IF
               SET DED-NOT-TAKEN-YES OF W-SW TO TRUE
               SET DED-CUR-PAYBK-NO  OF W-SW TO TRUE

               IF HOLD-DED-NOT-TAKEN OF W-WK  >  ZERO

                   MOVE WK-DED-NOT-TAKEN OF W-WK
                        TO  DED-NOT-TAKEN OF DARRY(DARRY-IDX DCLAS-IDX)
               END-IF

               PERFORM RC000-ADJUST-SPCL-ACCUM

               IF HOLD-DED-NOT-TAKEN OF W-WK  >  ZERO

                   COMPUTE DED-NOT-TAKEN OF DARRY(DARRY-IDX DCLAS-IDX)
                           =  HOLD-DED-NOT-TAKEN OF W-WK
                           +  WK-DED-NOT-TAKEN OF W-WK
               END-IF
           END-IF

           SET CALC-NO OF DARRY(DARRY-IDX DCLAS-IDX)  TO  TRUE

           .
       CALC-PARTIAL-EXIT.


      /*****************************************************************
      *                                                                *
       RA500-PRORATE-GARNISHMENTS SECTION.
       RA500.
      *                                                                *
      *                                                                *
      ******************************************************************

           MOVE DEDNET OF GRSWK  TO  AVAIL-AMT OF W-PRORATE
           MOVE ZERO  TO  CUR-MAX-AMT OF W-PRORATE
           MOVE ZERO  TO  ARR-MAX-AMT OF W-PRORATE
           MOVE SPACE  TO  GARN-TYPE OF W-PRORATE
           SET INSUFF-PAY OF W-PRORATE  TO  TRUE

           CALL 'PSPGPROR' USING   SQLRT
                                   GARRY
                                   PSLCT
                                   CHECK
                                   W-PRORATE

           IF RTNCD-ERROR OF SQLRT

               MOVE 'PRORATE-GARNS(PSPPYNET)'
                       TO  ERR-SECTION OF SQLRT
               PERFORM ZZ000-SQL-ERROR
           ELSE

               MOVE AVAIL-AMT OF W-PRORATE
                       TO  DED-CUR OF DARRY(DARRY-IDX DCLAS-IDX)
               PERFORM RB100-ADJUST-NET-AMOUNT
           END-IF

           .
       PRORATE-GARNISHMENTS-EXIT.


      /*****************************************************************
      *                                                                *
       RB100-ADJUST-NET-AMOUNT SECTION.
       RB100.
      *                                                                *
      * SUBTRACTS DEDUCTIONS FROM EARNINGS.                            *
      *                                                                *
      ******************************************************************

           COMPUTE CHECK-NET OF GRSWK
                   =  CHECK-NET OF GRSWK
                   -  DED-CUR OF DARRY(DARRY-IDX DCLAS-IDX)

           COMPUTE DEDNET OF GRSWK
                   =  DEDNET OF GRSWK
                   -  DED-CUR OF DARRY(DARRY-IDX DCLAS-IDX)

           COMPUTE TTLDED OF GRSWK
                   =  TTLDED OF GRSWK
                   +  DED-CUR OF DARRY(DARRY-IDX DCLAS-IDX)

           SET CALC-NO OF DARRY(DARRY-IDX DCLAS-IDX)  TO  TRUE

           .
       ADJUST-NET-AMOUNT-EXIT.


      /*****************************************************************
      *                                                                *
       RC000-ADJUST-SPCL-ACCUM SECTION.
       RC000.
      *                                                                *
      ******************************************************************

           IF  SPCL-EARNS-DEDN-COUNT OF DEDT5(DEDT5-IDX DEDC1-IDX) > 0


               SET SINGLE-JOB-FOUND-YES OF W-SW TO TRUE

               PERFORM VARYING SARRY-IDX FROM 1 BY 1 UNTIL
                       SARRY-IDX > SPCL-EARNS-COUNT OF SARRY

                   IF  SARRY-IDX = 1

                       MOVE EMPL-RCD-NO OF SARRY(SARRY-IDX)
                               TO EMPL-RCD-NO-SPCL OF W-WK
                       MOVE EMPL-RCD-NO OF SARRY(SARRY-IDX)
                         TO EMPL-RCD-NO-SPCL-HOLD OF W-WK
                   ELSE
                       IF EMPL-RCD-NO OF SARRY(SARRY-IDX)
                               NOT= EMPL-RCD-NO-SPCL OF W-WK

                           IF EMPL-RCD-NO OF SARRY(SARRY-IDX)
                              < EMPL-RCD-NO-SPCL-HOLD OF W-WK
                              MOVE EMPL-RCD-NO OF SARRY(SARRY-IDX)
                                TO EMPL-RCD-NO-SPCL-HOLD OF W-WK
                           END-IF

                           SET SINGLE-JOB-FOUND-NO OF W-SW TO TRUE
                       END-IF
                   END-IF
               END-PERFORM

               IF SINGLE-JOB-FOUND-NO OF W-SW

                   PERFORM VARYING DARRY-JOBS-IDX FROM 1 BY 1 UNTIL
                           DARRY-JOBS-IDX > INCLUDE-JOBS-COUNT OF DARRY
                           OR SINGLE-JOB-FOUND-YES OF W-SW

                       IF  BENEFIT-RCD-NO OF INCLUDE-JOBS
                               OF DARRY (DARRY-JOBS-IDX)
                           =  BENEFIT-RCD-NO OF DEDUCTION-DATA
                               OF DARRY(DARRY-IDX)

                           SET SINGLE-JOB-FOUND-YES OF W-SW TO TRUE
                           MOVE EMPL-RCD-NO OF INCLUDE-JOBS
                                   OF DARRY (DARRY-JOBS-IDX)
                                   TO EMPL-RCD-NO-SPCL OF W-WK
                       END-IF
                   END-PERFORM
               END-IF

               IF SINGLE-JOB-FOUND-NO OF W-SW
                  MOVE EMPL-RCD-NO-SPCL-HOLD OF W-WK
                    TO EMPL-RCD-NO-SPCL OF W-WK
                  SET SINGLE-JOB-FOUND-YES OF W-SW TO TRUE
               END-IF

               IF SINGLE-JOB-FOUND-YES OF W-SW

                   PERFORM RC100-ADJUST-SPCL-AMTS
               END-IF

           END-IF

           .
       ADJUST-SPCL-ACCUM-EXIT.


      /*****************************************************************
      *                                                                *
       RC100-ADJUST-SPCL-AMTS SECTION.
       RC100.
      *                                                                *
      ******************************************************************

           SET RECALC-YES OF DARRY(DARRY-IDX DCLAS-IDX)  TO  TRUE
           PERFORM VARYING DEDSP-IDX  FROM  1  BY  1
                   UNTIL PAY-LINE-STATUS-ERROR OF CHECK
                       OR DEDSP-IDX  >  SPCL-EARNS-DEDN-MAX OF DEDT5
                       OR ERNCD-SPCL
                               OF DEDT5(DEDT5-IDX DEDC1-IDX DEDSP-IDX)
                               =  SPACE

               SET CREATE-YES OF W-SW TO TRUE

               CALL 'PSPFNDSE' USING   SARRY
                                       SARRY-PASS OF W-WK
                                       ERNCD-SPCL OF DEDT5
                                               (DEDT5-IDX DEDC1-IDX
                                                            DEDSP-IDX)
                                       EMPL-RCD-NO-SPCL OF W-WK
                                       CREATE-SW OF W-SW

               SET SARRY-IDX  TO  SPCL-EARNS-COUNT OF SARRY

               IF SARRY-PASS OF W-WK  >  SARRY-IDX

                   SET MSGID-SARRY-MAX OF PYMSG  TO  TRUE
                   MOVE SPCL-EARNS-COUNT OF SARRY
                           TO  MSGDATA1-INT OF PYMSG
                   SET PAY-LINE-STATUS-ERROR OF CHECK  TO  TRUE
                   PERFORM ZM000-MESSAGE
               ELSE
                   SET SARRY-IDX  TO  SARRY-PASS OF W-WK

                   IF NOT COUNTRY-CANADA OF PYGRP

                       IF SPCL-EFFECT-ADD OF DEDT5 (DEDT5-IDX
                                                 DEDC1-IDX DEDSP-IDX)

                          IF DED-NOT-TAKEN-YES OF W-SW
                             COMPUTE SPCL-EARNS OF SARRY(SARRY-IDX)
                                   =  SPCL-EARNS OF SARRY(SARRY-IDX)
                                   -  DED-NOT-TAKEN
                                        OF  DARRY(DARRY-IDX DCLAS-IDX)
                          ELSE
                             IF DED-CUR-PAYBK-YES OF W-SW
                                IF DED-CUR-PAYBK
                                   OF DARRY(DARRY-IDX DCLAS-IDX)
                                   NOT = WK-DED-CUR-PAYBK-HOLD OF W-WK
                                    COMPUTE
                                         WK-CUR-PAYBK-ADJUST OF W-WK
                                      =  DED-CUR-PAYBK
                                         OF DARRY(DARRY-IDX DCLAS-IDX)
                                      -  WK-DED-CUR-PAYBK-HOLD OF W-WK
                                    COMPUTE
                                         SPCL-EARNS OF SARRY(SARRY-IDX)
                                      =  SPCL-EARNS OF SARRY(SARRY-IDX)
                                      +  WK-CUR-PAYBK-ADJUST OF W-WK
                                ELSE
                                    COMPUTE
                                         SPCL-EARNS OF SARRY(SARRY-IDX)
                                      =  SPCL-EARNS OF SARRY(SARRY-IDX)
                                      +  DED-CUR-PAYBK
                                         OF  DARRY(DARRY-IDX DCLAS-IDX)
                                END-IF
                             END-IF
                          END-IF
                       ELSE
                          IF DED-NOT-TAKEN-YES OF W-SW
                             COMPUTE SPCL-EARNS OF SARRY(SARRY-IDX)
                                   =  SPCL-EARNS OF SARRY(SARRY-IDX)
                                   +  DED-NOT-TAKEN
                                        OF  DARRY(DARRY-IDX DCLAS-IDX)
                          ELSE
                             IF DED-CUR-PAYBK-YES OF W-SW
                                IF DED-CUR-PAYBK
                                   OF DARRY(DARRY-IDX DCLAS-IDX)
                                   NOT = WK-DED-CUR-PAYBK-HOLD OF W-WK
                                    COMPUTE
                                         WK-CUR-PAYBK-ADJUST OF W-WK
                                      =  DED-CUR-PAYBK
                                         OF DARRY(DARRY-IDX DCLAS-IDX)
                                      -  WK-DED-CUR-PAYBK-HOLD OF W-WK
                                    COMPUTE
                                         SPCL-EARNS OF SARRY(SARRY-IDX)
                                      =  SPCL-EARNS OF SARRY(SARRY-IDX)
                                      -  WK-CUR-PAYBK-ADJUST OF W-WK
                                ELSE
                                    COMPUTE
                                         SPCL-EARNS OF SARRY(SARRY-IDX)
                                      =  SPCL-EARNS OF SARRY(SARRY-IDX)
                                      -  DED-CUR-PAYBK
                                         OF  DARRY(DARRY-IDX DCLAS-IDX)
                                END-IF
                             END-IF
                          END-IF
                       END-IF

                       IF SPCL-EFFECT-ADD OF DEDT5 (DEDT5-IDX
                                                 DEDC1-IDX DEDSP-IDX)

                          IF DED-NOT-TAKEN-YES OF W-SW
                             COMPUTE SPCL-EARNS-ACCUM
                                                   OF SARRY(SARRY-IDX)
                                   =  SPCL-EARNS-ACCUM
                                                   OF SARRY(SARRY-IDX)
                                   -  DED-NOT-TAKEN
                                        OF  DARRY(DARRY-IDX DCLAS-IDX)
                          ELSE
                             IF DED-CUR-PAYBK-YES OF W-SW
                                IF DED-CUR-PAYBK
                                   OF DARRY(DARRY-IDX DCLAS-IDX)
                                   NOT = WK-DED-CUR-PAYBK-HOLD OF W-WK
                                    COMPUTE
                                         WK-CUR-PAYBK-ADJUST OF W-WK
                                      =  DED-CUR-PAYBK
                                         OF DARRY(DARRY-IDX DCLAS-IDX)
                                      -  WK-DED-CUR-PAYBK-HOLD OF W-WK
                                    COMPUTE
                                         SPCL-EARNS-ACCUM
                                         OF SARRY(SARRY-IDX)
                                      =  SPCL-EARNS-ACCUM
                                         OF SARRY(SARRY-IDX)
                                      +  WK-CUR-PAYBK-ADJUST OF W-WK
                                ELSE
                                    COMPUTE
                                         SPCL-EARNS-ACCUM
                                         OF SARRY(SARRY-IDX)
                                      =  SPCL-EARNS-ACCUM
                                         OF SARRY(SARRY-IDX)
                                      +  DED-CUR-PAYBK
                                         OF  DARRY(DARRY-IDX DCLAS-IDX)
                                END-IF
                             END-IF
                          END-IF
                       ELSE
                          IF DED-NOT-TAKEN-YES OF W-SW
                             COMPUTE SPCL-EARNS-ACCUM
                                                   OF SARRY(SARRY-IDX)
                                   =  SPCL-EARNS-ACCUM
                                                   OF SARRY(SARRY-IDX)
                                   +  DED-NOT-TAKEN
                                        OF  DARRY(DARRY-IDX DCLAS-IDX)
                          ELSE
                             IF DED-CUR-PAYBK-YES OF W-SW
                                IF DED-CUR-PAYBK
                                   OF DARRY(DARRY-IDX DCLAS-IDX)
                                   NOT = WK-DED-CUR-PAYBK-HOLD OF W-WK
                                    COMPUTE
                                         WK-CUR-PAYBK-ADJUST OF W-WK
                                      =  DED-CUR-PAYBK
                                         OF DARRY(DARRY-IDX DCLAS-IDX)
                                      -  WK-DED-CUR-PAYBK-HOLD OF W-WK
                                    COMPUTE
                                         SPCL-EARNS-ACCUM
                                         OF SARRY(SARRY-IDX)
                                      =  SPCL-EARNS-ACCUM
                                         OF SARRY(SARRY-IDX)
                                      -  WK-CUR-PAYBK-ADJUST OF W-WK
                                ELSE
                                    COMPUTE
                                         SPCL-EARNS-ACCUM
                                         OF SARRY(SARRY-IDX)
                                      =  SPCL-EARNS-ACCUM
                                         OF SARRY(SARRY-IDX)
                                      -  DED-CUR-PAYBK
                                         OF  DARRY(DARRY-IDX DCLAS-IDX)
                                END-IF
                             END-IF
                          END-IF
                       END-IF
                   ELSE

                       IF NOT (DED-CLASS-BEFORE-TAX
                                        OF DARRY(DARRY-IDX DCLAS-IDX)
                             OR DED-CLASS-TAXABLE
                                        OF DARRY(DARRY-IDX DCLAS-IDX))
                           IF SPCL-EFFECT-ADD OF DEDT5 (DEDT5-IDX
                                                  DEDC1-IDX DEDSP-IDX)

                              IF DED-NOT-TAKEN-YES OF W-SW
                                 COMPUTE SPCL-EARNS OF SARRY(SARRY-IDX)
                                       =  SPCL-EARNS
                                                  OF SARRY(SARRY-IDX)
                                       -  DED-NOT-TAKEN
                                        OF  DARRY(DARRY-IDX DCLAS-IDX)
                              ELSE
                                 IF DED-CUR-PAYBK-YES OF W-SW
                                    IF DED-CUR-PAYBK
                                       OF DARRY(DARRY-IDX DCLAS-IDX)
                                       NOT = WK-DED-CUR-PAYBK-HOLD
                                             OF W-WK
                                      COMPUTE
                                         WK-CUR-PAYBK-ADJUST OF W-WK
                                      =  DED-CUR-PAYBK
                                         OF DARRY(DARRY-IDX DCLAS-IDX)
                                      -  WK-DED-CUR-PAYBK-HOLD OF W-WK
                                      COMPUTE
                                         SPCL-EARNS OF SARRY(SARRY-IDX)
                                      =  SPCL-EARNS OF SARRY(SARRY-IDX)
                                      +  WK-CUR-PAYBK-ADJUST OF W-WK
                                    ELSE
                                      COMPUTE
                                         SPCL-EARNS OF SARRY(SARRY-IDX)
                                      =  SPCL-EARNS OF SARRY(SARRY-IDX)
                                      +  DED-CUR-PAYBK
                                         OF  DARRY(DARRY-IDX DCLAS-IDX)
                                    END-IF
                                 END-IF
                              END-IF
                           ELSE
                              IF DED-NOT-TAKEN-YES OF W-SW
                                 COMPUTE SPCL-EARNS OF SARRY(SARRY-IDX)
                                       =  SPCL-EARNS
                                                  OF SARRY(SARRY-IDX)
                                       +  DED-NOT-TAKEN
                                        OF  DARRY(DARRY-IDX DCLAS-IDX)
                              ELSE
                                 IF DED-CUR-PAYBK-YES OF W-SW
                                    IF DED-CUR-PAYBK
                                       OF DARRY(DARRY-IDX DCLAS-IDX)
                                       NOT = WK-DED-CUR-PAYBK-HOLD
                                             OF W-WK
                                      COMPUTE
                                         WK-CUR-PAYBK-ADJUST OF W-WK
                                      =  DED-CUR-PAYBK
                                         OF DARRY(DARRY-IDX DCLAS-IDX)
                                      -  WK-DED-CUR-PAYBK-HOLD OF W-WK
                                      COMPUTE
                                         SPCL-EARNS OF SARRY(SARRY-IDX)
                                      =  SPCL-EARNS OF SARRY(SARRY-IDX)
                                      -  WK-CUR-PAYBK-ADJUST OF W-WK
                                    ELSE
                                      COMPUTE
                                         SPCL-EARNS OF SARRY(SARRY-IDX)
                                      =  SPCL-EARNS OF SARRY(SARRY-IDX)
                                      -  DED-CUR-PAYBK
                                         OF  DARRY(DARRY-IDX DCLAS-IDX)
                                    END-IF
                                 END-IF
                              END-IF
                           END-IF
                       END-IF

                       IF SPCL-EFFECT-ADD OF DEDT5 (DEDT5-IDX
                                                   DEDC1-IDX DEDSP-IDX)

                          IF DED-NOT-TAKEN-YES OF W-SW
                             COMPUTE SPCL-EARNS-ACCUM
                                                    OF SARRY(SARRY-IDX)
                                   =  SPCL-EARNS-ACCUM
                                                    OF SARRY(SARRY-IDX)
                                   -  DED-NOT-TAKEN
                                        OF  DARRY(DARRY-IDX DCLAS-IDX)
                          ELSE
                             IF DED-CUR-PAYBK-YES OF W-SW
                                IF DED-CUR-PAYBK
                                   OF DARRY(DARRY-IDX DCLAS-IDX)
                                   NOT = WK-DED-CUR-PAYBK-HOLD OF W-WK
                                    COMPUTE
                                         WK-CUR-PAYBK-ADJUST OF W-WK
                                      =  DED-CUR-PAYBK
                                         OF DARRY(DARRY-IDX DCLAS-IDX)
                                      -  WK-DED-CUR-PAYBK-HOLD OF W-WK
                                    COMPUTE
                                         SPCL-EARNS-ACCUM
                                         OF SARRY(SARRY-IDX)
                                      =  SPCL-EARNS-ACCUM
                                         OF SARRY(SARRY-IDX)
                                      +  WK-CUR-PAYBK-ADJUST OF W-WK
                                ELSE
                                    COMPUTE
                                         SPCL-EARNS-ACCUM
                                         OF SARRY(SARRY-IDX)
                                      =  SPCL-EARNS-ACCUM
                                         OF SARRY(SARRY-IDX)
                                      +  DED-CUR-PAYBK
                                         OF  DARRY(DARRY-IDX DCLAS-IDX)
                                END-IF
                             END-IF
                          END-IF
                       ELSE
                          IF DED-NOT-TAKEN-YES OF W-SW
                             COMPUTE SPCL-EARNS-ACCUM
                                                    OF SARRY(SARRY-IDX)
                                   =  SPCL-EARNS-ACCUM
                                                    OF SARRY(SARRY-IDX)
                                   +  DED-NOT-TAKEN
                                        OF  DARRY(DARRY-IDX DCLAS-IDX)
                          ELSE
                             IF DED-CUR-PAYBK-YES OF W-SW
                                IF DED-CUR-PAYBK
                                   OF DARRY(DARRY-IDX DCLAS-IDX)
                                   NOT = WK-DED-CUR-PAYBK-HOLD OF W-WK
                                    COMPUTE
                                         WK-CUR-PAYBK-ADJUST OF W-WK
                                      =  DED-CUR-PAYBK
                                         OF DARRY(DARRY-IDX DCLAS-IDX)
                                      -  WK-DED-CUR-PAYBK-HOLD OF W-WK
                                    COMPUTE
                                         SPCL-EARNS-ACCUM
                                         OF SARRY(SARRY-IDX)
                                      =  SPCL-EARNS-ACCUM
                                         OF SARRY(SARRY-IDX)
                                      -  WK-CUR-PAYBK-ADJUST OF W-WK
                                ELSE
                                    COMPUTE
                                         SPCL-EARNS-ACCUM
                                         OF SARRY(SARRY-IDX)
                                      =  SPCL-EARNS-ACCUM
                                         OF SARRY(SARRY-IDX)
                                      -  DED-CUR-PAYBK
                                         OF  DARRY(DARRY-IDX DCLAS-IDX)
                                END-IF
                             END-IF
                          END-IF
                       END-IF
                   END-IF
               END-IF
               PERFORM RC200-CHECK-IF-ACCUM-USED
           END-PERFORM

           .
       ADJUST-SPCL-AMTS-EXIT.


      /*****************************************************************
      *                                                                *
       RC200-CHECK-IF-ACCUM-USED SECTION.
       RC200.
      *                                                                *
      ******************************************************************

           PERFORM VARYING DARRY-NEXT-IDX  FROM  1  BY  1
                   UNTIL PAY-LINE-STATUS-ERROR OF CHECK
                       OR DARRY-NEXT-IDX  >  DEDUCTION-COUNT OF DARRY


               SET DEDT1-NEXT-IDX TO DEDTB-PTR OF DARRY(DARRY-NEXT-IDX)
               IF  (ERNCD-SPCL OF DEDT1(DEDT1-NEXT-IDX) =
                     ERNCD-SPCL
                        OF DEDT5(DEDT5-IDX DEDC1-IDX DEDSP-IDX)) AND
                   (((DED-PRIORITY OF DEDT1(DEDT1-IDX) >=
                    DED-PRIORITY OF DEDT1(DEDT1-NEXT-IDX)) AND
                   NOT (DED-CLASS-BEFORE-TAX
                          OF DEDT5(DEDT5-IDX DEDC1-IDX) OR
                        DED-CLASS-TAXABLE
                          OF DEDT5(DEDT5-IDX DEDC1-IDX))) OR
                    GENERATE-378-YES OF W-SW)

                   SET MSGID-SPCL-DED-NET-PAY OF PYMSG  TO  TRUE
                   MOVE ERNCD-SPCL OF DEDT1(DEDT1-NEXT-IDX)
                           TO  MSGDATA1 OF PYMSG
                   MOVE DEDCD OF DARRY(DARRY-IDX)
                           TO  MSGDATA2 OF PYMSG
                   SET PAY-LINE-STATUS-ERROR OF CHECK  TO  TRUE
                   PERFORM ZM000-MESSAGE

               END-IF
           END-PERFORM

           .
       CHECK-IF-ACCUM-USED-EXIT.


      /*****************************************************************
      *                                                                *
       SA000-GARNISHMENT-PROCESS SECTION.
       SA000.
      *                                                                *
      * CALCULATE GARNISHMENT AMOUNTS                                  *
      *                                                                *
      ******************************************************************

           MOVE TXBL-TTL OF GRSWK  TO  HOLD2-TXBL-TTL OF W-TTL
           MOVE HOLD-TXBL-TTL OF W-TTL  TO  TXBL-TTL OF GRSWK

           CALL 'PSPPYGRN' USING   SQLRT
                                   PSLCT
                                   EARRY
                                   DARRY
                                   DEDT1
                                   DEDT2
                                   DEDT3
                                   DEDT4
                                   DEDT5
                                   DEDT6
                                   PYGRP
                                   CHECK
                                   ERNTB
                                   LMTTB
                                   PENRT
                                   SVCST
                                   GARRY
                                   DEARY
                                   GRULE
                                   GDERN
                                   SARRY
                                   TARRY
                                   TAXWK
                                   TAXWK-SAVE
                                   TAXDT
                                   TAXDT-SAVE
                                   GRSWK
                                   STTRT
                                   LCLRT
                                   SUTRT
                                   SDIRT
                                   VDIRT
                                   FLIRT
                                   SUTTB
                                   SRCTB
                                   LRCTB
                                   LWKRC
                                   TGBTB
                                   TGCTB
                                   FMLRT
           IF RTNCD-ERROR OF SQLRT

               MOVE 'CALC-GARN-DEDUCTIONS(PSPPYNET)'
                       TO  ERR-SECTION OF SQLRT
               PERFORM ZZ000-SQL-ERROR
           END-IF

           MOVE HOLD2-TXBL-TTL OF W-TTL  TO  TXBL-TTL OF GRSWK

           .
       GARNISHMENT-PROCESS-EXIT.


      /*****************************************************************
      *                                                                *
       SA100-INITIALIZE-DEARY SECTION.
       SA100.
      *                                                                *
      *                                                                *
      ******************************************************************

           PERFORM VARYING DEARY-IDX
                   FROM 1 BY 1 UNTIL DEARY-IDX > 20
                   INITIALIZE GARN-DE-DATA OF DEARY(DEARY-IDX)

                   PERFORM VARYING DEARY-RL-IDX
                           FROM 1 BY 1 UNTIL DEARY-RL-IDX > 3
                           INITIALIZE GARN-RULE-DATA OF DEARY
                              (DEARY-IDX,DEARY-RL-IDX)

                           PERFORM VARYING DEARY-DED-IDX
                                   FROM 1 BY 1 UNTIL DEARY-DED-IDX > 40
                                   INITIALIZE GARN-DEDUCT-DATA OF
                                      DEARY(DEARY-IDX,DEARY-RL-IDX,
                                            DEARY-DED-IDX)

                           END-PERFORM
                   END-PERFORM
           END-PERFORM

           .
       INIT-DEARY-EXIT.


      /*****************************************************************
      *                                                                *
       SB000-SAVE-PREV-DED-AMT SECTION.
       SB000.
      *                                                                *
      * SAVE DEDUCTION CUR AND PAYBACK AMOUNT FOR NEXT LOOP OF         *
      * PYNET/GRNDE CALCULATION.   THIS EFFECTES THE GARN DE AMT CALC  *
      * WHEN THERE ARE INSUFFICIANT PAY TO COVER THE DEDUCTIONS.       *
      *
      ******************************************************************

           PERFORM VARYING WK-DED-IDX1 FROM 1 BY 1
             UNTIL WK-DED-IDX1 > DEDUCTION-COUNT OF DARRY

             PERFORM VARYING WK-DED-IDX2 FROM 1 BY 1
               UNTIL WK-DED-IDX2 > DEDUCTION-CLASS-MAX OF DARRY

      *
      *          SAVE DED PAYBACK AMT
      *
                 MOVE DED-CUR-PAYBK OF DARRY (WK-DED-IDX1 WK-DED-IDX2)
                   TO DED-CUR-PAYBK-PREV OF
                      DARRY (WK-DED-IDX1 WK-DED-IDX2)

      *
      *          SAVE DED-CUR TO DED-CUR-PREV-LAST
      *          DED-CUR-PREV IS THE ORIGINAL DED-CUR AMT USE
      *          TO ADJUST ER MATCH FOR INSUFFICENT PAY
      *
                 MOVE DED-CUR OF DARRY (WK-DED-IDX1 WK-DED-IDX2)
                   TO DED-CUR-PREV-LAST OF
                      DARRY (WK-DED-IDX1 WK-DED-IDX2)
             END-PERFORM
           END-PERFORM
      *


           .
       SAVE-PREV-DED-AMT.

      /*****************************************************************
      *                                                                *
       TA000-SAVE-NON-TIPS-CALC SECTION.
       TA000.
      *                                                                *
      ******************************************************************

           PERFORM VARYING TARRY-IDX FROM 1 BY 1
              UNTIL TARRY-IDX GREATER THAN TAXCALC-COUNT OF TARRY
               MOVE TAX-CUR OF TARRY (TARRY-IDX)
                  TO NON-TIPS-TAX-CUR OF TARRY (TARRY-IDX)
               MOVE TXGRS-CUR OF TARRY (TARRY-IDX)
                  TO NON-TIPS-TXGRS-CUR OF TARRY (TARRY-IDX)
               MOVE NLGRS-CUR OF TARRY (TARRY-IDX)
                  TO NON-TIPS-NLGRS-CUR OF TARRY (TARRY-IDX)
               MOVE TAX-NOT-TAKEN OF TARRY(TARRY-IDX)
                  TO NON-TIPS-TAX-NT-TAK (TARRY-IDX)
           END-PERFORM

           .
       SAVE-NON-TIPS-CALC-EXIT.


      /*****************************************************************
      *                                                                *
       TB000-SAVE-TIPS-CALC SECTION.
       TB000.
      *                                                                *
      ******************************************************************


           CALL 'PSPPYTIP' USING   SQLRT
                                   PSLCT
                                   PYGRP
                                   CHECK
                                   TARRY
                                   STTRT

           IF RTNCD-ERROR OF SQLRT

               MOVE 'CALL PSPPYTIP'  TO  ERR-SECTION OF SQLRT
               PERFORM ZZ000-SQL-ERROR
           END-IF

           .
       SAVE-TIPS-CALC-EXIT.

      /*****************************************************************
      *                                                                *
       XA000-CALC-DEDUCTION SECTION.
       XA000.
      *                                                                *
      ******************************************************************

           IF CALC-YES OF DARRY(DARRY-IDX DCLAS-IDX)

               SET DEDT1-IDX  TO  DEDTB-PTR OF DARRY(DARRY-IDX)
               SET DEDT2-IDX  TO  DEDTB-PTR OF DARRY(DARRY-IDX)
               SET DEDT3-IDX  TO  DEDTB-PTR OF DARRY(DARRY-IDX)
               SET DEDT4-IDX  TO  DEDTB-PTR OF DARRY(DARRY-IDX)

               IF GOVERNMENT OF PAYGROUP-DATA                           FEDMERG
                   AND US-FEDERAL-GOVT OF PAYGROUP-DATA                 FEDMERG
                   IF GVT-SUBJECT-OASDI-YES OF DEDT3(DEDT3-IDX)         FED0001
                       IF NOT FICA-STATUS-EE-SUBJECT OF TAXWK           FED0001
                        MOVE EMPLID OF CHECK  TO  MSGDATA1 OF PYMSG     FED0001
                        MOVE BENEFIT-PLAN OF DARRY(DARRY-IDX)           FED0001
                          TO  MSGDATA2 OF PYMSG                         FED0001
                        MOVE DEDCD OF DARRY(DARRY-IDX)                  FED0001
                          TO  MSGDATA3 OF PYMSG                         FED0001
                        SET MSGID-GVT-FED-OASDI-WARN OF PYMSG           FED0001
                         TO  TRUE                                       FED0001
                        SET PAY-LINE-STATUS-ERROR OF CHECK              FED0001
                         TO  TRUE                                       FED0001
                        PERFORM ZM000-MESSAGE                           FED0001
                       END-IF                                           FED0001
                   END-IF                                               FED0001
               END-IF                                                   FEDMERG

               SET DARRY-PASS OF W-PASS  TO  DARRY-IDX
               SET DCLAS-PASS OF W-PASS  TO  DCLAS-IDX

               CALL 'PSPDEDTN' USING   DARRY
                                       DEDT1
                                       DEDT2
                                       DEDT3
                                       DEDT4
                                       DEDT5
                                       DEDT6
                                       LMTTB
                                       PENRT
                                       SVCST
                                       PYGRP
                                       GRSWK
                                       SARRY
                                       TAXWK
                                       CHECK
                                       PSLCT
                                       W-PASS
                                       SQLRT

               IF RTNCD-ERROR OF SQLRT

                   MOVE 'CALC-DED(PSPPYNET)'  TO  ERR-SECTION
                                                    OF SQLRT
                   PERFORM ZZ000-SQL-ERROR
               END-IF
           END-IF

           IF (PUBLIC-SECTOR-YES OF PSLCT AND
               ADD-TO-FICA-CR-YES OF DEDT5(DEDT5-IDX DEDC1-IDX))        FED0999
            OR (GOVERNMENT OF PSLCT AND                                 FED0999
                US-FEDERAL-GOVT OF PSLCT AND                            HP90003
                ADD-TO-FICA-CR-YES OF DEDT5(DEDT5-IDX DEDC1-IDX))       FED0999
               SET DED-FOUND-YES OF W-SW TO TRUE                        HP90003
               PERFORM XE000-SEL-FICA-CR-DED-YTD                        HP90003
               PERFORM XF000-FICA-CREDIT-DED-AMT                        HP90003
           END-IF                                                       HP90003

           MOVE ZERO  TO  WK-AMT OF W-WK

           IF REASON-NOT-TAKEN-NONE OF DARRY(DARRY-IDX DCLAS-IDX)
               AND MANUAL-CHECK-NO OF CHECK
               AND ARREARS-BAL OF DARRY(DARRY-IDX DCLAS-IDX)
                       > ZERO
               AND (CALC-YES OF DARRY(DARRY-IDX DCLAS-IDX)  OR
                CAN-CALC-YES OF DARRY(DARRY-IDX DCLAS-IDX))
               AND (DED-CLASS-BEFORE-TAX OF DARRY(DARRY-IDX DCLAS-IDX)
                 OR DED-CLASS-AFTER-TAX OF DARRY(DARRY-IDX DCLAS-IDX))

               PERFORM XA100-CALC-ARR-PAYBK
               SET CAN-CALC-NO OF DARRY(DARRY-IDX DCLAS-IDX) TO TRUE
           END-IF

           IF CALC-NO OF DARRY(DARRY-IDX DCLAS-IDX)

               MOVE DED-CUR-PAYBK OF DARRY(DARRY-IDX DCLAS-IDX)
                       TO  WK-AMT OF W-WK
               COMPUTE WK-AMT OF GRSWK
                       =  DED-CUR OF DARRY(DARRY-IDX DCLAS-IDX)
                       -  DED-CUR-PAYBK OF DARRY(DARRY-IDX DCLAS-IDX)
           END-IF

           IF WK-AMT OF GRSWK  NOT =  ZERO
               OR WK-AMT OF W-WK  NOT = ZERO

               PERFORM XD000-ADJUST-GROSSES
           END-IF


           .
       CALC-DEDUCTION-EXIT.



      /*****************************************************************
      *                                                                *
       XA100-CALC-ARR-PAYBK SECTION.
       XA100.
      *                                                                *
      ******************************************************************

           IF PAYBACK-YES OF DARRY(DARRY-IDX DCLAS-IDX)

               IF DED-PAYBACK OF DARRY(DARRY-IDX DCLAS-IDX)
                       <=  ARREARS-BAL OF DARRY(DARRY-IDX
                                                DCLAS-IDX)

                   MOVE DED-PAYBACK OF DARRY(DARRY-IDX
                                             DCLAS-IDX)
                           TO  WK-AMT OF W-WK
               ELSE
                   MOVE ARREARS-BAL OF DARRY(DARRY-IDX
                                             DCLAS-IDX)
                           TO  WK-AMT OF W-WK
               END-IF
           ELSE
               IF MAX-PAYBACK-YES OF DARRY(DARRY-IDX)
                      AND MAX-ARREARS-PAYBK OF DARRY(DARRY-IDX)
                               <=  ARREARS-BAL
                                       OF DARRY(DARRY-IDX
                                                DCLAS-IDX)

                   MOVE MAX-ARREARS-PAYBK OF DARRY(DARRY-IDX)
                           TO  WK-AMT OF W-WK
               ELSE

                   IF MAX-PAYBACK-FACTOR OF DARRY(DARRY-IDX)

                       IF DED-CUR OF DARRY(DARRY-IDX DCLAS-IDX)
                              <= ZERO

                           PERFORM XA200-FIND-CUR-DED
                       ELSE

                           COMPUTE WK-ARREARS OF W-WK
                                   =  DED-CUR OF DARRY(DARRY-IDX
                                                   DCLAS-IDX)
                                   *  MAX-ARREARS-FACTOR OF DARRY
                                                   (DARRY-IDX)
                       END-IF

                       IF WK-ARREARS OF W-WK
                               <=  ARREARS-BAL
                                       OF DARRY(DARRY-IDX
                                                DCLAS-IDX)

                           MOVE WK-ARREARS OF W-WK
                                   TO  WK-AMT OF W-WK
                       ELSE
                           MOVE ARREARS-BAL
                                      OF DARRY(DARRY-IDX
                                               DCLAS-IDX)
                                   TO  WK-AMT OF W-WK
                       END-IF
                   ELSE
                       MOVE ARREARS-BAL OF DARRY(DARRY-IDX
                                                 DCLAS-IDX)
                                   TO  WK-AMT OF W-WK
                   END-IF
               END-IF
           END-IF

           MOVE WK-AMT OF W-WK  TO  DED-CUR-PAYBK OF DARRY
                                               (DARRY-IDX DCLAS-IDX)
           MOVE WK-AMT OF W-WK  TO  WK-DED-CUR-PAYBK-HOLD OF W-WK

           IF ARREARS-UPD-NO OF DARRY(DARRY-IDX DCLAS-IDX)
               IF (WK-AMT OF W-WK - ARREARS-BAL OF DARRY(DARRY-IDX
                                                     DCLAS-IDX) = ZERO)

                   IF NOT PAYCHECK-MODELING-YES OF PSLCT

                       PERFORM XA110-SELECT-CALCED-CHK
                   END-IF

                   IF (CHK-YES OF S-CHKCNT = 'X')

                       MOVE ZERO TO DED-CUR-PAYBK
                       OF DARRY(DARRY-IDX DCLAS-IDX)

                   END-IF

               END-IF
           END-IF
           SET DED-CUR-PAYBK-YES OF W-SW TO TRUE
           SET DED-NOT-TAKEN-NO  OF W-SW TO TRUE
           PERFORM RC000-ADJUST-SPCL-ACCUM

           .
       CALC-ARR-PAYBK-EXIT.

      /*****************************************************************
      *
       XA110-SELECT-CALCED-CHK SECTION.
      *
      ******************************************************************

           MOVE COMPANY OF PSLCT TO COMPANY OF S-CHKCNT
           MOVE PAY-END-DT OF PSLCT TO PAY-END-DT OF S-CHKCNT
           MOVE EMPLID OF CHECK TO EMPLID OF S-CHKCNT
           MOVE OFF-CYCLE OF PSLCT TO OFF-CYCLE OF S-CHKCNT
           MOVE PLAN-TYPE OF DARRY(DARRY-IDX) TO PLAN-TYPE OF S-CHKCNT
           MOVE BENEFIT-PLAN OF DARRY(DARRY-IDX)
           TO BENEFIT-PLAN OF S-CHKCNT
           MOVE DEDCD OF DARRY(DARRY-IDX) TO DEDCD OF S-CHKCNT
           MOVE DED-CLASS OF DARRY(DARRY-IDX DCLAS-IDX)
           TO DED-CLASS OF S-CHKCNT

           CALL 'PTPSQLRT' USING ACTION-SELECT OF SQLRT
                                     SQLRT
                                     SQL-CURSOR-COMMON OF SQLRT
                                     SQL-STMT OF S-CHKCNT
                                     BIND-SETUP OF S-CHKCNT
                                     BIND-DATA OF S-CHKCNT
                                     SELECT-SETUP OF S-CHKCNT
                                     SELECT-DATA OF S-CHKCNT

               IF RTNCD-ERROR OF SQLRT

                   MOVE 'SELECT-CALCED-CHK'  TO  ERR-SECTION OF SQLRT
                   PERFORM ZZ000-SQL-ERROR
               END-IF

               INITIALIZE SELECT-DATA OF S-CHKCNT

               CALL 'PTPSQLRT' USING   ACTION-FETCH OF SQLRT
                                       SQLRT
                                       SQL-CURSOR-COMMON OF SQLRT

               IF (RTNCD-ERROR OF SQLRT) AND NOT (RTNCD-END OF SQLRT)

                   MOVE 'FETCH-CALCED-CHK' TO ERR-SECTION OF SQLRT
                   PERFORM ZZ000-SQL-ERROR

               END-IF
               SET RTNCD-OK OF SQLRT TO TRUE
           .
       SELECT-CALCED-CHK.

      /*****************************************************************
      *                                                                *
       XA200-FIND-CUR-DED SECTION.
       XA200.
      *                                                                *
      ******************************************************************

           MOVE CORR CHECK  TO  BIND-DATA OF S-CUR
           MOVE PLAN-TYPE OF DARRY(DARRY-IDX)
                   TO  PLAN-TYPE OF S-CUR
           MOVE BENEFIT-PLAN OF DARRY(DARRY-IDX)
                   TO  BENEFIT-PLAN OF S-CUR
           MOVE DEDCD OF DARRY(DARRY-IDX)
                   TO  DEDCD OF S-CUR
           MOVE DED-CLASS OF DARRY(DARRY-IDX DCLAS-IDX)
                   TO  DED-CLASS OF S-CUR

           IF PAYCHECK-MODELING-YES OF PSLCT
               MOVE SQL-STMT-MOD OF S-CUR TO SQL-STMT OF S-CUR
           END-IF

           CALL 'PTPSQLRT' USING   ACTION-SELECT OF SQLRT
                                   SQLRT
                                   SQL-CURSOR-COMMON OF SQLRT
                                   SQL-STMT OF S-CUR
                                   BIND-SETUP OF S-CUR
                                   BIND-DATA OF S-CUR
                                   SELECT-SETUP OF S-CUR
                                   SELECT-DATA OF S-CUR
           IF RTNCD-ERROR OF SQLRT

               MOVE 'GET-CUR-DED'  TO  ERR-SECTION OF SQLRT
               PERFORM ZZ000-SQL-ERROR
           END-IF

           SET REG-DED-FOUND-NO OF W-SW  TO  TRUE
           MOVE ZERO  TO  WK-ARREARS OF W-WK

           PERFORM UNTIL REG-DED-FOUND-YES OF W-SW
                    OR RTNCD-END OF SQLRT

               INITIALIZE SELECT-DATA OF S-CUR

               CALL 'PTPSQLRT' USING   ACTION-FETCH OF SQLRT
                                       SQLRT
                                       SQL-CURSOR-COMMON OF SQLRT

               IF NOT RTNCD-END OF SQLRT

                   IF RTNCD-ERROR OF SQLRT

                       MOVE 'GET-CUR-DED(FETCH)'
                               TO  ERR-SECTION OF SQLRT
                       PERFORM ZZ000-SQL-ERROR
                   ELSE
                       COMPUTE WK-REG-DED OF W-WK
                               =  DED-CUR OF S-CUR
                               -  DED-CUR-REFUND OF S-CUR
                               -  DED-CUR-PAYBK OF S-CUR
                               +  DED-NOT-TAKEN OF S-CUR

                       IF WK-REG-DED OF W-WK  >  ZERO

                           SET REG-DED-FOUND-YES OF W-SW  TO  TRUE

                           COMPUTE WK-ARREARS OF W-WK
                                   =  WK-REG-DED OF W-WK
                                   *  MAX-ARREARS-FACTOR OF DARRY
                                                        (DARRY-IDX)
                       END-IF
                   END-IF
               END-IF
           END-PERFORM

           IF RTNCD-END OF SQLRT

               SET RTNCD-OK OF SQLRT  TO  TRUE
           END-IF

            .
       FIND-CUR-DED-EXIT.


      /*****************************************************************
      *                                                                *
       XD000-ADJUST-GROSSES SECTION.
       XD000.
      *                                                                *
      ******************************************************************

           IF DED-CLASS-TAXABLE OF DARRY(DARRY-IDX DCLAS-IDX)

               COMPUTE TXBL-TTL OF GRSWK
                       =  TXBL-TTL OF GRSWK
                       +  WK-AMT OF GRSWK
                       +  WK-AMT OF W-WK
           ELSE

               IF DED-CLASS-BEFORE-TAX OF DARRY(DARRY-IDX DCLAS-IDX)

                   COMPUTE TXBL-TTL OF GRSWK
                           =  TXBL-TTL OF GRSWK
                           -  WK-AMT OF GRSWK
                           -  WK-AMT OF W-WK
               END-IF
           END-IF

      *  ACCUMULATE PRE-TAX DED REFUNDS THAT'LL LATER INCREASE NET CHECK
      *  THE REFUNDS WILL BE USED TO COVER TAXES

           IF DED-CLASS-BEFORE-TAX OF DARRY(DARRY-IDX DCLAS-IDX)

               IF WK-AMT OF GRSWK < ZERO

                   COMPUTE DED-REFUND-TTL OF W-TTL
                       =   DED-REFUND-TTL OF W-TTL
                       -   WK-AMT OF GRSWK
               END-IF

               IF WK-AMT OF W-WK < ZERO

                   COMPUTE DED-REFUND-TTL OF W-TTL
                       =   DED-REFUND-TTL OF W-TTL
                       -   WK-AMT OF W-WK
               END-IF
           END-IF

      *  ACCUMULATE AFT-TAX DED REFUNDS THAT'LL LATER INCREASE NET CHECK
      *  THE REFUNDS WILL BE USED TO COVER ONE-TIME TAX OVERRIDES

           IF DED-CLASS-AFTER-TAX OF DARRY(DARRY-IDX DCLAS-IDX)

               IF WK-AMT OF GRSWK < ZERO

                   COMPUTE AFT-TAX-DED-REFUND-TTL OF RFNTL
                       =   AFT-TAX-DED-REFUND-TTL OF RFNTL
                       -   WK-AMT OF GRSWK
               END-IF

               IF WK-AMT OF W-WK < ZERO

                   COMPUTE AFT-TAX-DED-REFUND-TTL OF RFNTL
                       =   AFT-TAX-DED-REFUND-TTL OF RFNTL
                       -   WK-AMT OF W-WK
               END-IF
           END-IF

           .
       ADJUST-GROSSES-EXIT.


      /*****************************************************************
      *
       XE000-SEL-FICA-CR-DED-YTD SECTION.
      *
      ******************************************************************

           SET BALID-IDX               TO BALID-IDX-FOR-CAL-YR
           MOVE EMPLID OF CHECK        TO EMPLID OF S-FICADED
           MOVE BALANCE-YEAR OF BALID(BALID-IDX)
                                       TO BAL-YEAR OF S-FICADED
           MOVE BAL-ID-FOR-CAL-YR OF PSLCT
                                       TO BAL-ID     OF S-FICADED

           CALL 'PTPSQLRT' USING ACTION-SELECT OF SQLRT
                                     SQLRT
                                     SQL-CURSOR-COMMON OF SQLRT
                                     SQL-STMT OF S-FICADED
                                     BIND-SETUP OF S-FICADED
                                     BIND-DATA OF S-FICADED
                                     SELECT-SETUP OF S-FICADED
                                     SELECT-DATA OF S-FICADED

               IF RTNCD-ERROR OF SQLRT

                   MOVE 'SEL-FICA-CR-DED-YTD'  TO  ERR-SECTION OF SQLRT
                   PERFORM ZZ000-SQL-ERROR
               END-IF

               INITIALIZE SELECT-DATA OF S-FICADED

               CALL 'PTPSQLRT' USING   ACTION-FETCH OF SQLRT
                                       SQLRT
                                       SQL-CURSOR-COMMON OF SQLRT

               IF (RTNCD-ERROR OF SQLRT) AND NOT (RTNCD-END OF SQLRT)

                   MOVE 'SEL-FICA-CR-DED-YTD' TO ERR-SECTION OF SQLRT
                   PERFORM ZZ000-SQL-ERROR

               END-IF
               SET RTNCD-OK OF SQLRT TO TRUE
               MOVE DED-YTD OF SELECT-DATA OF S-FICADED TO
                    WK-DED-YTD OF W-WK


           .
       SEL-FICA-CR-DED-YTD-EXIT.


      /*****************************************************************HP90003
      *                                                                *HP90003
       XF000-FICA-CREDIT-DED-AMT SECTION.                               HP90003
       XF000.                                                           HP90003
      *                                                                *HP90003
      ******************************************************************HP90003
                                                                        HP90003
           IF DED-FOUND-YES OF W-SW
              COMPUTE PENSION-DED-CUR OF TAXWK                          HP90003
                 =   PENSION-DED-CUR OF TAXWK                           HP90003
                 +   DED-CUR OF DARRY(DARRY-IDX DCLAS-IDX)              HP90003
                                                                        HP90003
              PERFORM XF050-ESTITMATE-DED                               HP90003
                                                                        HP90003
              COMPUTE PENSION-DED-EST OF TAXWK                          HP90003
                 =   PENSION-DED-EST OF TAXWK                           HP90003
                 +   DED-EST OF W-WK                                    HP90003

           END-IF
           .                                                            HP90003
           IF PENSION-DED-YTD OF TAXWK = 0                              HP90003
              COMPUTE PENSION-DED-YTD OF TAXWK                          HP90003
                  =   PENSION-DED-YTD OF TAXWK                          HP90003
                  +   WK-DED-YTD OF W-WK                                HP90003
           END-IF                                                       HP90003

           .
                                                                        HP90003
       FICA-CREDIT-DED-AMT-EXIT.                                        HP90003
                                                                        HP90003
                                                                        HP90003
      /*****************************************************************HP90003
      *                                                                *HP90003
       XF050-ESTITMATE-DED SECTION.                                     HP90003
       XF050.                                                           HP90003
      *                                                                *HP90003
      ******************************************************************HP90003
                                                                        HP90003
           MOVE ZERO TO DED-EST OF W-WK                                 HP90003
           MOVE ZERO TO FUTURE-DED-PERIOD OF W-WK                       HP90003
           MOVE ZERO TO CONF-DED-PERIOD   OF W-WK                       HP90003
           MOVE ZERO TO CUM-DED-PERIOD    OF W-WK                       HP90003
           MOVE ZERO TO TOT-PAY-PERIOD    OF W-WK                       HP90003
                                                                        HP90003
           IF PAY-PERIOD-YES OF DARRY(DARRY-IDX 1)                      HP90003
               COMPUTE FUTURE-DED-PERIOD OF W-WK                        HP90003
                   =   FUTURE-DED-PERIOD OF W-WK                        HP90003
                   +   FUTURE-DED-PRD-CNT OF PSLCT(1)                   HP90003
                                                                        HP90003
               COMPUTE CONF-DED-PERIOD OF W-WK                          HP90003
                   =   CONF-DED-PERIOD OF W-WK                          HP90003
                   +   CONF-DED-PRD-CNT OF PSLCT(1)                     HP90003
           END-IF                                                       HP90003
                                                                        HP90003
           IF PAY-PERIOD-YES OF DARRY(DARRY-IDX 2)                      HP90003
               COMPUTE FUTURE-DED-PERIOD OF W-WK                        HP90003
                   =   FUTURE-DED-PERIOD OF W-WK                        HP90003
                   +   FUTURE-DED-PRD-CNT OF PSLCT(2)                   HP90003
                                                                        HP90003
               COMPUTE CONF-DED-PERIOD OF W-WK                          HP90003
                   =   CONF-DED-PERIOD OF W-WK                          HP90003
                   +   CONF-DED-PRD-CNT OF PSLCT(2)                     HP90003
           END-IF                                                       HP90003
                                                                        HP90003
           IF PAY-PERIOD-YES OF DARRY(DARRY-IDX 3)                      HP90003
               COMPUTE FUTURE-DED-PERIOD OF W-WK                        HP90003
                   =   FUTURE-DED-PERIOD OF W-WK                        HP90003
                   +   FUTURE-DED-PRD-CNT OF PSLCT(3)                   HP90003
                                                                        HP90003
               COMPUTE CONF-DED-PERIOD OF W-WK                          HP90003
                   =   CONF-DED-PERIOD OF W-WK                          HP90003
                   +   CONF-DED-PRD-CNT OF PSLCT(3)                     HP90003
           END-IF                                                       HP90003
                                                                        HP90003
           IF PAY-PERIOD-YES OF DARRY(DARRY-IDX 4)                      HP90003
               COMPUTE FUTURE-DED-PERIOD OF W-WK                        HP90003
                   =   FUTURE-DED-PERIOD OF W-WK                        HP90003
                   +   FUTURE-DED-PRD-CNT OF PSLCT(4)                   HP90003
                                                                        HP90003
               COMPUTE CONF-DED-PERIOD OF W-WK                          HP90003
                   =   CONF-DED-PERIOD OF W-WK                          HP90003
                   +   CONF-DED-PRD-CNT OF PSLCT(4)                     HP90003
           END-IF                                                       HP90003
                                                                        HP90003
           IF PAY-PERIOD-YES OF DARRY(DARRY-IDX 5)                      HP90003
               COMPUTE FUTURE-DED-PERIOD OF W-WK                        HP90003
                   =   FUTURE-DED-PERIOD OF W-WK                        HP90003
                   +   FUTURE-DED-PRD-CNT OF PSLCT(5)                   HP90003
                                                                        HP90003
               COMPUTE CONF-DED-PERIOD OF W-WK                          HP90003
                   =   CONF-DED-PERIOD OF W-WK                          HP90003
                   +   CONF-DED-PRD-CNT OF PSLCT(5)                     HP90003
           END-IF                                                       HP90003
                                                                        HP90003
           COMPUTE CUM-DED-PERIOD OF W-WK                               HP90003
               =   FUTURE-DED-PERIOD OF W-WK                            HP90003
               +   CONF-DED-PERIOD   OF W-WK                            HP90003
                                                                        HP90003
           COMPUTE TOT-PAY-PERIOD OF W-WK                               HP90003
               =   TOT-CONF-DED-PRD-CNT OF PSLCT                        HP90003
               +   TOT-FUTURE-DED-PRD-CNT OF PSLCT                      HP90003
                                                                        HP90003
           IF TOT-PAY-PERIOD OF W-WK NOT=ZERO                           HP90003
                                                                        HP90003
               IF DED-CUR OF DARRY(DARRY-IDX DCLAS-IDX) NOT=ZERO        HP90003
                                                                        HP90003
                   COMPUTE DED-EST OF W-WK ROUNDED                      HP90003
                       = (DED-CUR OF DARRY(DARRY-IDX DCLAS-IDX)         HP90003
                       *   CUM-DED-PERIOD OF W-WK)                      HP90003
                       / TOT-PAY-PERIOD OF W-WK                         HP90003
               ELSE                                                     HP90003
                   IF CONF-DED-PERIOD OF W-WK NOT= ZERO                 HP90003
                                                                        HP90003
                       COMPUTE DED-EST OF W-WK ROUNDED                  HP90003
                           =   DED-YTD OF DARRY(DARRY-IDX DCLAS-IDX)    HP90003
                           /    CONF-DED-PERIOD OF W-WK                 HP90003
                                                                        HP90003
                       COMPUTE DED-EST OF W-WK ROUNDED                  HP90003
                           = (DED-EST OF W-WK                           HP90003
                           *   CUM-DED-PERIOD OF W-WK)                  HP90003
                           / TOT-PAY-PERIOD OF W-WK                     HP90003
                   END-IF                                               HP90003
               END-IF                                                   HP90003
           END-IF                                                       HP90003
           .                                                            HP90003
       ESTITMATE-DED-EXIT.                                              HP90003

      /*****************************************************************
      *                                                                *
       XG000-GET-BN-RATES SECTION.
       XG000.
      *                                                                *
      * Get Benefit Rates Stored in Darry or Call PSPBENRT             *
      *                                                                *
      ******************************************************************

           MOVE 20 TO BN-RATE-OTHER-MAX OF DARRY(DARRY-IDX DCLAS-IDX)

           PERFORM VARYING RTOTHD-IDX  FROM  1  BY  1
                    UNTIL RTOTHD-IDX  > BN-RATE-OTHER-MAX OF
                                          DARRY(DARRY-IDX DCLAS-IDX)

              INITIALIZE RATE-OTH-DATA OF BENRT-PASS(RTOTHD-IDX)
           END-PERFORM

           MOVE ZERO TO RATE-OTH-DATA-CNT OF BENRT-PASS
           MOVE PAY-END-DT OF PSLCT  TO  EFFDT OF BENRT-PASS
           SET DARRY-PASS OF BENRT-PASS  TO  DARRY-IDX
           MOVE DEDTB-PTR OF DARRY(DARRY-IDX)
                TO  DEDTB-PASS OF BENRT-PASS
           MOVE SPACES  TO  DEPENDENT-BENEF OF BENRT-PASS
           SET DCLAS-PASS OF BENRT-PASS  TO  DCLAS-IDX
           SET OPTION-GET-RATE OF BENRT-PASS  TO  TRUE

           IF BN-RATE-IN-DARRY-YES OF DARRY(DARRY-IDX DCLAS-IDX)

               MOVE RATE-TBL-ID OF DARRY(DARRY-IDX DCLAS-IDX)
                    TO  RATE-TBL-ID OF BENRT-PASS
               MOVE BN-CURRENT-RATE OF DARRY(DARRY-IDX DCLAS-IDX)
                    TO  BN-CURRENT-RATE OF BENRT-PASS

               IF BN-RATE-OTHER-CNT OF DARRY(DARRY-IDX DCLAS-IDX)  >  0

                   SET OTH-RATES-EXIST-YES OF BENRT-PASS  TO  TRUE
               ELSE
                   SET OTH-RATES-EXIST-NO OF BENRT-PASS  TO  TRUE
               END-IF

               PERFORM VARYING BNRTOTH-IDX  FROM  1  BY  1
                   UNTIL BNRTOTH-IDX  >
                       BN-RATE-OTHER-CNT OF DARRY(DARRY-IDX DCLAS-IDX)

                   ADD 1  TO RATE-OTH-DATA-CNT OF BENRT-PASS
                   SET RTOTHD-IDX  TO  RATE-OTH-DATA-CNT OF BENRT-PASS
                   MOVE DED-CLASS OF DARRY(DARRY-IDX DCLAS-IDX)
                        TO  DED-CLASS OF RATE-OTH-DATA(RTOTHD-IDX)
                   MOVE OTHER-RATE-ID
                        OF DARRY(DARRY-IDX DCLAS-IDX BNRTOTH-IDX)
                        TO  OTHER-RATE-ID OF RATE-OTH-DATA(RTOTHD-IDX)
                   MOVE OTHER-TAX-RATE
                        OF DARRY(DARRY-IDX DCLAS-IDX BNRTOTH-IDX)
                        TO OTHER-TAX-RATE OF RATE-OTH-DATA(RTOTHD-IDX)
               END-PERFORM

           ELSE
               MOVE RATE-TBL-ID OF DEDT1(DEDT1-IDX)
                    TO RATE-TBL-ID OF BENRT-PASS

               CALL 'PSPBENRT' USING   SQLRT
                                       CHECK
                                       PYGRP
                                       DARRY
                                       DEDT2
                                       PSLCT
                                       BENRT-PASS
               IF RTNCD-ERROR OF SQLRT

                   MOVE 'APPLY DEDUCTIONS(PSPPYNET)'  TO
                         ERR-SECTION OF SQLRT
                   PERFORM ZZ000-SQL-ERROR
               END-IF
           END-IF

           .
       GET-BN-RATES-EXIT.


      /*****************************************************************
      *                                                                *
       ZM000-MESSAGE SECTION.
       ZM000.
      *                                                                *
      * WRITE PAY MESSAGE                                              *
      *                                                                *
      ******************************************************************

           MOVE CORR PSLCT  TO  PYMSG
           MOVE CORR CHECK  TO  PYMSG
           MOVE MSGTB OF PSLCT TO MSGTB OF PYMSG

           CALL 'PSPPYMSG' USING   SQLRT
                                   PYMSG
           IF RTNCD-ERROR OF SQLRT

               MOVE 'PSPPYNET-MESSAGE'  TO  ERR-SECTION OF SQLRT
               PERFORM ZZ000-SQL-ERROR
           END-IF

           MOVE MSGTB OF PYMSG TO MSGTB OF PSLCT

           .
       MESSAGE-EXIT.


      /*****************************************************************
      *                                                                *
       ZZ000-SQL-ERROR SECTION.
       ZZ000.
      *                                                                *
      * SQL ERROR PROCESSING                                           *
      *                                                                *
      ******************************************************************

           CALL 'PTPSQLRT' USING   ACTION-ERROR OF SQLRT
                                   SQLRT

           .
       SQL-ERROR-EXIT.
           EXIT PROGRAM.
