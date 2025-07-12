1000 REM STRANGE ATTRACTOR PROGRAM  BASIC Ver 2.0 (c) 1993 by J. C. Sprott
1010 DEFDBL A-Z                 'Use double precision
1020 DIM XS(499), YS(499), ZS(499), WS(499), A(504), V(99), XY(4), XN(4), COLR%(15)
1030 SM% = 12                   'Assume VGA graphics
1040 PREV% = 5                  'Plot versus fifth previous iterate
1050 NMAX = 11000               'Maximum number of iterations
1060 OMAX% = 5                  'Maximum order of polynomial
1070 D% = 2                     'Dimension of system
1080 EPS = .1                   'Step size for ODE
1090 ODE% = 0                   'System is map
1100 SND% = 0                   'Turn sound off
1110 PJT% = 0                   'Projection is planar
1120 TRD% = 1                   'Display third dimension as shadow
1130 FTH% = 2                   'Display fourth dimension as colors
1140 SAV% = 0                   'Don't save any data
1150 TWOPI = 6.28318530717959#  'A useful constant (2 pi)
1160 RANDOMIZE TIMER            'Reseed random number generator
1170 GOSUB 4200                 'Display menu screen
1180 IF Q$ = "X" THEN GOTO 1250 'Exit immediately on command
1190 GOSUB 1300                 'Initialize
1200 GOSUB 1500                 'Set parameters
1210 GOSUB 1700                 'Iterate equations
1220 GOSUB 2100                 'Display results
1230 GOSUB 2400                 'Test results
1240 ON T% GOTO 1190, 1200, 1210
1250 CLS
1260 END

1300 REM Initialize
1310 ON ERROR GOTO 6600         'Find legal graphics mode
1320 SCREEN SM%                 'Set graphics mode
1330 ON ERROR GOTO 0            'Resume default error trapping
1340 DEF SEG = 64: WID% = PEEK(74)  'Number of text columns
1350 WINDOW (-.1, -.1)-(1.1, 1.1)
1360 CLS : LOCATE 13, WID% / 2 - 6: PRINT "Searching..."
1370 GOSUB 5600                 'Set colors
1380 IF QM% <> 2 THEN GOTO 1420
1390    NE = 0: CLOSE
1400    OPEN "SA.DIC" FOR APPEND AS #1: CLOSE
1410    OPEN "SA.DIC" FOR INPUT AS #1
1420 RETURN

1500 REM Set parameters
1510 X = .05                    'Initial condition
1520 Y = .05
1530 Z = .05
1540 W = .05
1550 XE = X + .000001: YE = Y: ZE = Z: WE = W
1560 GOSUB 2600                 'Get coefficients
1570 T% = 3
1580 P% = 0: LSUM = 0: N = 0: NL = 0: N1 = 0: N2 = 0
1590 XMIN = 1000000!: XMAX = -XMIN: YMIN = XMIN: YMAX = XMAX
1600 ZMIN = XMIN: ZMAX = XMAX
1610 WMIN = XMIN: WMAX = XMAX
1620 TWOD% = 2 ^ D%
1630 RETURN

1700 REM Iterate equations
1710 IF ODE% > 1 THEN GOSUB 6200: GOTO 2020     'Special function
1720 M% = 1: XY(1) = X: XY(2) = Y: XY(3) = Z: XY(4) = W
1730 FOR I% = 1 TO D%
1740 XN(I%) = A(M%)
1750 M% = M% + 1
1760 FOR I1% = 1 TO D%
1770 XN(I%) = XN(I%) + A(M%) * XY(I1%)
1780 M% = M% + 1
1790 FOR I2% = I1% TO D%
1800 XN(I%) = XN(I%) + A(M%) * XY(I1%) * XY(I2%)
1810 M% = M% + 1
1820 IF O% = 2 THEN GOTO 1970
1830 FOR I3% = I2% TO D%
1840 XN(I%) = XN(I%) + A(M%) * XY(I1%) * XY(I2%) * XY(I3%)
1850 M% = M% + 1
1860 IF O% = 3 THEN GOTO 1960
1870 FOR I4% = I3% TO D%
1880 XN(I%) = XN(I%) + A(M%) * XY(I1%) * XY(I2%) * XY(I3%) * XY(I4%)
1890 M% = M% + 1
1900 IF O% = 4 THEN GOTO 1950
1910 FOR I5% = I4% TO D%
1920 XN(I%) = XN(I%) + A(M%) * XY(I1%) * XY(I2%) * XY(I3%) * XY(I4%) * XY(I5%)
1930 M% = M% + 1
1940 NEXT I5%
1950 NEXT I4%
1960 NEXT I3%
1970 NEXT I2%
1980 NEXT I1%
1990 IF ODE% = 1 THEN XN(I%) = XY(I%) + EPS * XN(I%)
2000 NEXT I%
2010 M% = M% - 1: XNEW = XN(1): YNEW = XN(2): ZNEW = XN(3): WNEW = XN(4)
2020 N = N + 1
2030 RETURN

2100 REM Display results
2110 IF N < 100 OR N > 1000 THEN GOTO 2200
2120    IF X < XMIN THEN XMIN = X
2130    IF X > XMAX THEN XMAX = X
2140    IF Y < YMIN THEN YMIN = Y
2150    IF Y > YMAX THEN YMAX = Y
2160    IF Z < ZMIN THEN ZMIN = Z
2170    IF Z > ZMAX THEN ZMAX = Z
2180    IF W < WMIN THEN WMIN = W
2190    IF W > WMAX THEN WMAX = W
2200 IF N = 1000 THEN GOSUB 3100        'Resize the screen
2210 XS(P%) = X: YS(P%) = Y: ZS(P%) = Z: WS(P%) = W
2220 P% = (P% + 1) MOD 500
2230 I% = (P% + 500 - PREV%) MOD 500
2240 IF D% = 1 THEN XP = XS(I%): YP = XNEW ELSE XP = X: YP = Y
2250 IF N < 1000 OR XP <= XL OR XP >= XH OR YP <= YL OR YP >= YH THEN GOTO 2320
2260    IF PJT% = 1 THEN GOSUB 4100     'Project onto a sphere
2270    IF PJT% = 2 THEN GOSUB 6700     'Project onto a horizontal cylinder
2280    IF PJT% = 3 THEN GOSUB 6800     'Project onto a vertical cylinder
2290    IF PJT% = 4 THEN GOSUB 6900     'Project onto a torus
2300    GOSUB 5000                      'Plot point on screen
2310    IF SND% = 1 THEN GOSUB 3500     'Produce sound
2320 RETURN

2400 REM Test results
2410 IF ABS(XNEW) + ABS(YNEW) + ABS(ZNEW) + ABS(WNEW) > 1000000! THEN T% = 2
2420 IF QM% = 2 THEN GOTO 2490  'Speed up evaluation mode
2430 GOSUB 2900                 'Calculate Lyapunov exponent
2440 GOSUB 3900                 'Calculate fractal dimension
2450 IF QM% > 0 THEN GOTO 2490  'Skip tests when not in search mode
2460 IF N >= NMAX THEN T% = 2: GOSUB 4900   'Strange attractor found
2470 IF ABS(XNEW - X) + ABS(YNEW - Y) + ABS(ZNEW - Z) + ABS(WNEW - W) < .000001 THEN T% = 2
2480 IF N > 100 AND L < .005 THEN T% = 2    'Limit cycle
2490 Q$ = INKEY$: IF LEN(Q$) THEN GOSUB 3600        'Respond to user command
2500 IF SAV% > 0 THEN IF N > 1000 AND N < 17001 THEN GOSUB 7000 'Save data
2510 X = XNEW                   'Update value of X
2520 Y = YNEW
2530 Z = ZNEW
2540 W = WNEW
2550 RETURN

2600 REM Get coefficients
2610 IF QM% <> 2 THEN GOTO 2640 'Not in evaluate mode
2620    IF EOF(1) THEN QM% = 0: GOSUB 6000: GOTO 2640
2630    IF EOF(1) = 0 THEN LINE INPUT #1, CODE$: GOSUB 4700: GOSUB 5600
2640 IF QM% > 0 THEN GOTO 2730  'Not in search mode
2650    O% = 2 + INT((OMAX% - 1) * RND)
2660    CODE$ = CHR$(59 + 4 * D% + O% + 8 * ODE%)
2670    IF ODE% > 1 THEN CODE$ = CHR$(87 + ODE%)
2680    GOSUB 4700              'Get value of M%
2690    FOR I% = 1 TO M%        'Construct CODE$
2700        GOSUB 2800          'Shuffle random numbers
2710        CODE$ = CODE$ + CHR$(65 + INT(25 * RAN))
2720    NEXT I%
2730 FOR I% = 1 TO M%           'Convert CODE$ to coefficient values
2740    A(I%) = (ASC(MID$(CODE$, I% + 1, 1)) - 77) / 10
2750 NEXT I%
2760 RETURN

2800 REM Shuffle random numbers
2810 IF V(0) = 0 THEN FOR J% = 0 TO 99: V(J%) = RND: NEXT J%
2820 J% = INT(100 * RAN)
2830 RAN = V(J%)
2840 V(J%) = RND
2850 RETURN

2900 REM Calculate Lyapunov exponent
2910 XSAVE = XNEW: YSAVE = YNEW: ZSAVE = ZNEW: WSAVE = WNEW
2920 X = XE: Y = YE: Z = ZE: W = WE: N = N - 1
2930 GOSUB 1700                 'Reiterate equations
2940 DLX = XNEW - XSAVE: DLY = YNEW - YSAVE
2950 DLZ = ZNEW - ZSAVE: DLW = WNEW - WSAVE
2960 DL2 = DLX * DLX + DLY * DLY + DLZ * DLZ + DLW * DLW
2970 IF CSNG(DL2) <= 0 THEN GOTO 3070       'Don't divide by zero
2980    DF = 1000000000000# * DL2
2990    RS = 1 / SQR(DF)
3000    XE = XSAVE + RS * (XNEW - XSAVE): YE = YSAVE + RS * (YNEW - YSAVE)
3010    ZE = ZSAVE + RS * (ZNEW - ZSAVE): WE = WSAVE + RS * (WNEW - WSAVE)
3020    XNEW = XSAVE: YNEW = YSAVE: ZNEW = ZSAVE: WNEW = WSAVE
3030    LSUM = LSUM + LOG(DF): NL = NL + 1
3040    L = .721347 * LSUM / NL
3050    IF ODE% = 1 OR ODE% = 7 THEN L = L / EPS
3060    IF N > 1000 AND N MOD 10 = 0 THEN LOCATE 1, WID% - 4: PRINT USING "##.##"; L;
3070 RETURN

3100 REM Resize the screen
3110 IF D% = 1 THEN YMIN = XMIN: YMAX = XMAX
3120 IF XMAX - XMIN < .000001 THEN XMIN = XMIN - .0000005: XMAX = XMAX + .0000005
3130 IF YMAX - YMIN < .000001 THEN YMIN = YMIN - .0000005: YMAX = YMAX + .0000005
3140 IF ZMAX - ZMIN < .000001 THEN ZMIN = ZMIN - .0000005: ZMAX = ZMAX + .0000005
3150 IF WMAX - WMIN < .000001 THEN WMIN = WMIN - .0000005: WMAX = WMAX + .0000005
3160 MX = .1 * (XMAX - XMIN): MY = .1 * (YMAX - YMIN)
3170 XL = XMIN - MX: XH = XMAX + MX: YL = YMIN - MY: YH = YMAX + 1.5 * MY
3180 WINDOW (XL, YL)-(XH, YH): CLS
3190 YH = YH - .5 * MY
3200 XA = (XL + XH) / 2: YA = (YL + YH) / 2
3210 IF D% < 3 THEN GOTO 3310
3220    ZA = (ZMAX + ZMIN) / 2
3230    IF TRD% = 1 THEN LINE (XL, YL)-(XH, YH), COLR%(1), BF: GOSUB 5400
3240    IF TRD% = 4 THEN LINE (XL, YL)-(XH, YH), WH%, BF
3250    IF TRD% = 5 THEN LINE (XA, YL)-(XA, YH)
3260    IF TRD% <> 6 THEN GOTO 3310
3270        FOR I% = 1 TO 3
3280            XP = XL + I% * (XH - XL) / 4: LINE (XP, YL)-(XP, YH)
3290            YP = YL + I% * (YH - YL) / 4: LINE (XL, YP)-(XH, YP)
3300        NEXT I%
3310 IF PJT% <> 1 THEN LINE (XL, YL)-(XH, YH), , B
3320 IF PJT% = 1 AND TRD% < 5 THEN CIRCLE (XA, YA), .36 * (XH - XL)
3330 TT = 3.1416 / (XMAX - XMIN): PT = 3.1416 / (YMAX - YMIN)
3340 IF QM% <> 2 THEN GOTO 3400         'Not in evaluate mode
3350    LOCATE 1, 1: PRINT "<Space Bar>: Discard       <Enter>: Save";
3360    IF WID% < 80 THEN GOTO 3390
3370    LOCATE 1, 49: PRINT "<Esc>: Exit";
3380    LOCATE 1, 69: PRINT CINT((LOF(1) - 128 * LOC(1)) / 1024); "K left";
3390    GOTO 3430
3400 LOCATE 1, 1: IF LEN(CODE$) < WID% - 18 THEN PRINT CODE$
3410 IF LEN(CODE$) >= WID% - 18 THEN PRINT LEFT$(CODE$, WID% - 23) + "..."
3420 LOCATE 1, WID% - 17: PRINT "F =": LOCATE 1, WID% - 7: PRINT "L ="
3430 TIA = .05               'Tangent of illumination angle
3440 XZ = -TIA * (XMAX - XMIN) / (ZMAX - ZMIN)
3450 YZ = TIA * (YMAX - YMIN) / (ZMAX - ZMIN)
3460 RETURN

3500 REM Produce sound
3510 FREQ% = 220 * 2 ^ (CINT(36 * (XNEW - XL) / (XH - XL)) / 12)
3520 DUR = 1
3530 IF D% > 1 THEN DUR = 2 ^ INT(.5 * (YH - YL) / (YNEW - 9 * YL / 8 + YH / 8))
3540 SOUND FREQ%, DUR: IF PLAY(0) THEN PLAY "MF"
3550 RETURN

3600 REM Respond to user command
3610 IF ASC(Q$) > 96 THEN Q$ = CHR$(ASC(Q$) - 32)   'Convert to upper case
3620 IF QM% = 2 THEN GOSUB 5800                 'Process evaluation command
3630 IF INSTR("ACDEHINPRSVX", Q$) = 0 THEN GOSUB 4200   'Display menu screen
3640 IF Q$ = "A" THEN T% = 1: QM% = 0
3650 IF ODE% > 1 THEN D% = ODE% + 5
3660 IF ODE% = 1 THEN D% = D% + 2
3670 IF Q$ = "C" THEN IF N > 999 THEN N = 999
3680 IF Q$ = "D" THEN D% = 1 + (D% MOD 12): T% = 1
3690 IF D% > 6 THEN ODE% = D% - 5: D% = 4: GOTO 3710
3700 IF D% > 4 THEN ODE% = 1: D% = D% - 2 ELSE ODE% = 0
3710 IF Q$ = "E" THEN T% = 1: QM% = 2
3720 IF Q$ = "H" THEN FTH% = (FTH% + 1) MOD 3: T% = 3: IF N > 999 THEN N = 999: GOSUB 5600
3730 IF Q$ = "I" THEN IF T% <> 1 THEN SCREEN 0: WIDTH 80: COLOR 15, 1: CLS : LINE INPUT "Code? "; CODE$: IF CODE$ = "" THEN Q$ = " ": CLS :  ELSE T% = 1: QM% = 1: GOSUB 4700
3740 IF Q$ = "N" THEN NMAX = 10 * (NMAX - 1000) + 1000: IF NMAX > 10 ^ 10 THEN NMAX = 2000
3750 IF Q$ = "P" THEN PJT% = (PJT% + 1) MOD 5: T% = 3: IF N > 999 THEN N = 999
3760 IF Q$ = "R" THEN TRD% = (TRD% + 1) MOD 7: T% = 3: IF N > 999 THEN N = 999: GOSUB 5600
3770 IF Q$ = "S" THEN SND% = (SND% + 1) MOD 2: T% = 3
3780 IF Q$ = "V" THEN SAV% = (SAV% + 1) MOD 5: FAV$ = CHR$(87 + SAV% MOD 4): T% = 3: IF N > 999 THEN N = 999
3790 IF Q$ = "X" THEN T% = 0
3800 RETURN

3900 REM Calculate fractal dimension
3910 IF N < 1000 THEN GOTO 4010         'Wait for transient to settle
3920 IF N = 1000 THEN D2MAX = (XMAX - XMIN) ^ 2 + (YMAX - YMIN) ^ 2 + (ZMAX - ZMIN) ^ 2 + (WMAX - WMIN) ^ 2
3930 J% = (P% + 1 + INT(480 * RND)) MOD 500
3940 DX = XNEW - XS(J%): DY = YNEW - YS(J%): DZ = ZNEW - ZS(J%): DW = WNEW - WS(J%)
3950 D2 = DX * DX + DY * DY + DZ * DZ + DW * DW
3960 IF D2 < .001 * TWOD% * D2MAX THEN N2 = N2 + 1
3970 IF D2 > .00001 * TWOD% * D2MAX THEN GOTO 4010
3980    N1 = N1 + 1
3990    F = .434294 * LOG(N2 / (N1 - .5))
4000    LOCATE 1, WID% - 14: PRINT USING "##.##"; F;
4010 RETURN

4100 REM Project onto a sphere
4110 TH = TT * (XMAX - XP)
4120 PH = PT * (YMAX - YP)
4130 XP = XA + .36 * (XH - XL) * COS(TH) * SIN(PH)
4140 YP = YA + .5 * (YH - YL) * COS(PH)
4150 RETURN

4200 REM Display menu screen
4210 SCREEN 0: WIDTH 80: COLOR 15, 1: CLS
4220 WHILE Q$ = "" OR INSTR("AEIX", Q$) = 0
4230    LOCATE 1, 27: PRINT "STRANGE ATTRACTOR PROGRAM"
4240    PRINT TAB(27); "IBM PC BASIC Version 2.0"
4250    PRINT TAB(27); "(c) 1993 by J. C. Sprott"
4260    PRINT : PRINT
4270    PRINT TAB(27); "A: Search for attractors"
4280    PRINT TAB(27); "C: Clear screen and restart"
4290    IF ODE% > 1 THEN PRINT TAB(27); "D: System is 4-D special map "; CHR$(87 + ODE%); " ": GOTO 4320
4300    PRINT TAB(27); "D: System is"; STR$(D%); "-D polynomial ";
4310    IF ODE% = 1 THEN PRINT "ODE" ELSE PRINT "map"
4320    PRINT TAB(27); "E: Evaluate attractors"
4330    PRINT TAB(27); "H: Fourth dimension is ";
4340        IF FTH% = 0 THEN PRINT "projection"
4350        IF FTH% = 1 THEN PRINT "bands     "
4360        IF FTH% = 2 THEN PRINT "colors    "
4370    PRINT TAB(27); "I: Input code from keyboard"
4380    PRINT TAB(27); "N: Number of iterations is 10^";
4390    PRINT USING "#"; CINT(LOG(NMAX - 1000) / LOG(10))
4400    PRINT TAB(27); "P: Projection is ";
4410        IF PJT% = 0 THEN PRINT "planar   "
4420        IF PJT% = 1 THEN PRINT "spherical"
4430        IF PJT% = 2 THEN PRINT "horiz cyl"
4440        IF PJT% = 3 THEN PRINT "vert cyl "
4450        IF PJT% = 4 THEN PRINT "toroidal "
4460    PRINT TAB(27); "R: Third dimension is ";
4470        IF TRD% = 0 THEN PRINT "projection"
4480        IF TRD% = 1 THEN PRINT "shadow    "
4490        IF TRD% = 2 THEN PRINT "bands     "
4500        IF TRD% = 3 THEN PRINT "colors    "
4510        IF TRD% = 4 THEN PRINT "anaglyph  "
4520        IF TRD% = 5 THEN PRINT "stereogram"
4530        IF TRD% = 6 THEN PRINT "slices    "
4540    PRINT TAB(27); "S: Sound is ";
4550        IF SND% = 0 THEN PRINT "off"
4560        IF SND% = 1 THEN PRINT "on "
4570    PRINT TAB(27); "V: ";
4580        IF SAV% = 0 THEN PRINT "No data will be saved       "
4590        IF SAV% > 0 THEN PRINT FAV$; " will be saved in "; FAV$; "DATA.DAT"
4600    PRINT TAB(27); "X: Exit program"
4610    Q$ = INKEY$
4620    IF Q$ <> "" THEN GOSUB 3600     'Respond to user command
4630 WEND
4640 RETURN

4700 REM Get dimension and order
4710 D% = 1 + INT((ASC(LEFT$(CODE$, 1)) - 65) / 4)
4720 IF D% > 6 THEN ODE% = ASC(LEFT$(CODE$, 1)) - 87: D% = 4: GOSUB 6200: GOTO 4770
4730 IF D% > 4 THEN D% = D% - 2: ODE% = 1 ELSE ODE% = 0
4740 O% = 2 + (ASC(LEFT$(CODE$, 1)) - 65) MOD 4
4750 M% = 1: FOR I% = 1 TO D%: M% = M% * (O% + I%): NEXT I%
4760 IF D% > 2 THEN FOR I% = 3 TO D%: M% = M% / (I% - 1): NEXT I%
4770 IF LEN(CODE$) = M% + 1 OR QM% <> 1 THEN GOTO 4810
4780    BEEP        'Illegal code warning
4790    WHILE LEN(CODE$) < M% + 1: CODE$ = CODE$ + "M": WEND
4800    IF LEN(CODE$) > M% + 1 THEN CODE$ = LEFT$(CODE$, M% + 1)
4810 RETURN

4900 REM Save attractor to disk file SA.DIC
4910 OPEN "SA.DIC" FOR APPEND AS #1
4920 PRINT #1, CODE$; : PRINT #1, USING "##.##"; F; L
4930 CLOSE #1
4940 RETURN

5000 REM Plot point on screen
5010 C4% = WH%
5020 IF D% < 4 THEN GOTO 5050
5030    IF FTH% = 1 THEN IF INT(30 * (W - WMIN) / (WMAX - WMIN)) MOD 2 THEN GOTO 5330
5040    IF FTH% = 2 THEN C4% = 1 + INT(NC% * (W - WMIN) / (WMAX - WMIN) + NC%) MOD NC%
5050 IF D% < 3 THEN PSET (XP, YP): GOTO 5330    'Skip 3-D stuff
5060 IF TRD% = 0 THEN PSET (XP, YP), C4%
5070 IF TRD% <> 1 THEN GOTO 5130
5080    IF D% > 3 AND FTH% = 2 THEN PSET (XP, YP), C4%: GOTO 5110
5090    C% = POINT(XP, YP)
5100    IF C% = COLR%(2) THEN PSET (XP, YP), COLR%(3) ELSE IF C% <> COLR%(3) THEN PSET (XP, YP), COLR%(2)
5110    XP = XP - XZ * (Z - ZMIN): YP = YP - YZ * (Z - ZMIN)
5120    IF POINT(XP, YP) = COLR%(1) THEN PSET (XP, YP), 0
5130 IF TRD% <> 2 THEN GOTO 5160
5140    IF D% > 3 AND FTH% = 2 AND (INT(15 * (Z - ZMIN) / (ZMAX - ZMIN) + 2) MOD 2) = 1 THEN PSET (XP, YP), C4%
5150    IF D% < 4 OR FTH% <> 2 THEN C% = COLR%(INT(60 * (Z - ZMIN) / (ZMAX - ZMIN) + 4) MOD 4): PSET (XP, YP), C%
5160 IF TRD% = 3 THEN PSET (XP, YP), COLR%(INT(NC% * (Z - ZMIN) / (ZMAX - ZMIN) + NC%) MOD NC%)
5170 IF TRD% <> 4 THEN GOTO 5240
5180    XRT = XP + XZ * (Z - ZA): C% = POINT(XRT, YP)
5190    IF C% = WH% THEN PSET (XRT, YP), RD%
5200    IF C% = CY% THEN PSET (XRT, YP), BK%
5210    XLT = XP - XZ * (Z - ZA): C% = POINT(XLT, YP)
5220    IF C% = WH% THEN PSET (XLT, YP), CY%
5230    IF C% = RD% THEN PSET (XLT, YP), BK%
5240 IF TRD% <> 5 THEN GOTO 5280
5250    HSF = 2                 'Horizontal scale factor
5260    XRT = XA + (XP + XZ * (Z - ZA) - XL) / HSF: PSET (XRT, YP), C4%
5270    XLT = XA + (XP - XZ * (Z - ZA) - XH) / HSF: PSET (XLT, YP), C4%
5280 IF TRD% <> 6 THEN GOTO 5330
5290    DZ = (15 * (Z - ZMIN) / (ZMAX - ZMIN) + .5) / 16
5300    XP = (XP - XL + (INT(16 * DZ) MOD 4) * (XH - XL)) / 4 + XL
5310    YP = (YP - YL + (3 - INT(4 * DZ) MOD 4) * (YH - YL)) / 4 + YL
5320    PSET (XP, YP), C4%
5330 RETURN

5400 REM Plot background grid
5410 FOR I% = 0 TO 15           'Draw 15 vertical grid lines
5420    XP = XMIN + I% * (XMAX - XMIN) / 15
5430    LINE (XP, YMIN)-(XP, YMAX), 0
5440 NEXT I%
5450 FOR I% = 0 TO 10           'Draw 10 horizontal grid lines
5460    YP = YMIN + I% * (YMAX - YMIN) / 10
5470    LINE (XMIN, YP)-(XMAX, YP), 0
5480 NEXT I%
5490 RETURN

5600 REM Set colors
5610 NC% = 15                   'Number of colors
5620 COLR%(0) = 0: COLR%(1) = 8: COLR%(2) = 7: COLR%(3) = 15
5630 IF TRD% = 3 OR (D% > 3 AND FTH% = 2 AND TRD% <> 1) THEN FOR I% = 0 TO NC%: COLR%(I%) = I% + 1: NEXT I%
5640 WH% = 15: BK% = 8: RD% = 12: CY% = 11
5650 IF SM% > 2 THEN GOTO 5720  'Not in CGA mode
5660    WID% = 80: IF D% < 3 THEN SCREEN 2: GOTO 5720
5670    IF (TRD% = 0 OR TRD% > 4) AND (D% = 3 OR FTH% <> 2) THEN SCREEN 2: GOTO 5720
5680    WID% = 40: SCREEN 1
5690    COLR%(0) = 0: COLR%(1) = 2: COLR%(2) = 1: COLR%(3) = 3
5700    WH% = 3: BK% = 0: RD% = 2: CY% = 1
5710    FOR I% = 4 TO NC%: COLR%(I%) = COLR%(I% MOD 4 + 1): NEXT I%
5720 RETURN

5800 REM Process evaluation command
5810 IF Q$ = " " THEN T% = 2: NE = NE + 1: CLS
5820 IF Q$ = CHR$(13) THEN T% = 2: NE = NE + 1: CLS : GOSUB 5900
5830 IF Q$ = CHR$(27) THEN CLS : GOSUB 6000: Q$ = " ": QM% = 0: GOTO 5850
5840 IF Q$ <> CHR$(27) AND INSTR("CHNPRVS", Q$) = 0 THEN Q$ = ""
5850 RETURN

5900 REM Save favorite attractors to disk file FAVORITE.DIC
5910 OPEN "FAVORITE.DIC" FOR APPEND AS #2
5920 PRINT #2, CODE$
5930 CLOSE #2
5940 RETURN

6000 REM Update SA.DIC file
6010 LOCATE 11, 9: PRINT "Evaluation complete"
6020 LOCATE 12, 8: PRINT NE; "cases evaluated"
6030 OPEN "SATEMP.DIC" FOR OUTPUT AS #2
6040 IF QM% = 2 THEN PRINT #2, CODE$
6050 WHILE NOT EOF(1): LINE INPUT #1, CODE$: PRINT #2, CODE$: WEND
6060 CLOSE
6070 KILL "SA.DIC"
6080 NAME "SATEMP.DIC" AS "SA.DIC"
6090 RETURN

6200 REM Special function definitions
6210 ZNEW = X * X + Y * Y       'Default 3rd and 4th dimension
6220 WNEW = (N - 100) / 900: IF N > 1000 THEN WNEW = (N - 1000) / (NMAX - 1000)
6230 IF ODE% <> 2 THEN GOTO 6270
6240    M% = 10
6250    XNEW = A(1) + A(2) * X + A(3) * Y + A(4) * ABS(X) + A(5) * ABS(Y)
6260    YNEW = A(6) + A(7) * X + A(8) * Y + A(9) * ABS(X) + A(10) * ABS(Y)
6270 IF ODE% <> 3 THEN GOTO 6310
6280    M% = 14
6290    XNEW = A(1) + A(2) * X + A(3) * Y + (CINT(A(4) * X) AND CINT(A(5) * Y)) + (CINT(A(6) * X) OR CINT(A(7) * Y))
6300    YNEW = A(8) + A(9) * X + A(10) * Y + (CINT(A(11) * X) AND CINT(A(12) * Y)) + (CINT(A(13) * X) OR CINT(A(14) * Y))
6310 IF ODE% <> 4 THEN GOTO 6350
6320    M% = 14
6330    XNEW = A(1) + A(2) * X + A(3) * Y + A(4) * ABS(X) ^ A(5) + A(6) * ABS(Y) ^ A(7)
6340    YNEW = A(8) + A(9) * X + A(10) * Y + A(11) * ABS(X) ^ A(12) + A(13) * ABS(Y) ^ A(14)
6350 IF ODE% <> 5 THEN GOTO 6390
6360    M% = 18
6370    XNEW = A(1) + A(2) * X + A(3) * Y + A(4) * SIN(A(5) * X + A(6)) + A(7) * SIN(A(8) * Y + A(9))
6380    YNEW = A(10) + A(11) * X + A(12) * Y + A(13) * SIN(A(14) * X + A(15)) + A(16) * SIN(A(17) * Y + A(18))
6390 IF ODE% <> 6 THEN GOTO 6450
6400    M% = 6
6410    IF N < 2 THEN AL = TWOPI / (13 + 10 * A(6)): SINAL = SIN(AL): COSAL = COS(AL)
6420    DUM = X + A(2) * SIN(A(3) * Y + A(4))
6430    XNEW = 10 * A(1) + DUM * COSAL + Y * SINAL
6440    YNEW = 10 * A(5) - DUM * SINAL + Y * COSAL
6450 IF ODE% <> 7 THEN GOTO 6500
6460    M% = 9
6470    XNEW = X + EPS * A(1) * Y
6480    YNEW = Y + EPS * (A(2) * X + A(3) * X * X * X + A(4) * X * X * Y + A(5) * X * Y * Y + A(6) * Y + A(7) * Y * Y * Y + A(8) * SIN(Z))
6490    ZNEW = Z + EPS * (A(9) + 1.3): IF ZNEW > TWOPI THEN ZNEW = ZNEW - TWOPI
6500 RETURN

6600 REM Find legal graphics mode
6610 SM% = SM% - 1
6620 IF SM% = 0 THEN PRINT "This program requires a graphics monitor": STOP
6630 RESUME

6700 REM Project onto a horizontal cylinder
6710 PH = PT * (YMAX - YP)
6720 YP = YA + .5 * (YH - YL) * COS(PH)
6730 RETURN

6800 REM Project onto a vertical cylinder
6810 TH = TT * (XMAX - XP)
6820 XP = XA + .5 * (XH - XL) * COS(TH)
6830 RETURN

6900 REM Project onto a torus (unity aspect ratio)
6910 TH = TT * (XMAX - XP)
6920 PH = 2 * PT * (YMAX - YP)
6930 XP = XA + .18 * (XH - XL) * (1 + COS(TH)) * SIN(PH)
6940 YP = YA + .25 * (YH - YL) * (1 + COS(TH)) * COS(PH)
6950 RETURN

7000 REM Save data
7010 IF N = 1001 THEN CLOSE #3: OPEN FAV$ + "DATA.DAT" FOR OUTPUT AS #3
7020 IF SAV% = 1 THEN DUM = XNEW
7030 IF SAV% = 2 THEN DUM = YNEW
7040 IF SAV% = 3 THEN DUM = ZNEW
7050 IF SAV% = 4 THEN DUM = WNEW
7060 PRINT #3, CSNG(DUM)
7070 RETURN

