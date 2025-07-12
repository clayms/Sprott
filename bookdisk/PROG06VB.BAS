VERSION 2.00
Begin Form PROG06VB 
   Caption         =   "Strange Attractors"
   Height          =   4620
   Left            =   828
   LinkTopic       =   "Form1"
   ScaleHeight     =   4200
   ScaleWidth      =   6420
   Top             =   1128
   Width           =   6516
End
DefDbl A-Z

Sub Form_Activate ()

1000 Rem TWO-D MAP SEARCH  VisualBASIC Ver 1.0 (c) 1993 by J. C. Sprott
1020 ReDim XS(499), A(504), V(99)
1040 PREV% = 5                 'Plot versus fifth previous iterate
1050 NMAX = 11000              'Maximum number of iterations
1060 OMAX% = 2                 'Maximum order of polynomial
1070 D% = 2                    'Dimension of system
1160 Randomize Timer           'Reseed random number generator
1190 GoSub 1300                'Initialize
1200 GoSub 1500                'Set parameters
1210 GoSub 1700                'Iterate equations
1220 GoSub 2100                'Display results
1230 GoSub 2400                'Test results
1240 On T% GoTo 1190, 1200, 1210
1250 Cls
1260 End

1300 Rem Initialize
1320 Cls: Msg$ = "Searching..."
1350 CurrentX = (ScaleWidth - TextWidth(Msg$)) / 2
1360 CurrentY = (ScaleHeight - TextHeight(Msg$)) / 2
1370 Print Msg$
1420 Return

1500 Rem Set parameters
1510 X = .05                   'Initial condition
1520 Y = .05
1550 XE = X + .000001: YE = Y
1560 GoSub 2600                'Get coefficients
1570 T% = 3
1580 P% = 0: LSUM = 0:  N = 0:  NL = 0
1590 XMIN = 1000000!: XMAX = -XMIN:  YMIN = XMIN:  YMAX = XMAX
1630 Return

1700 Rem Iterate equations
1720 XNEW = A(1) + X * (A(2) + A(3) * X + A(4) * Y)
1730 XNEW = XNEW + Y * (A(5) + A(6) * Y)
1830 YNEW = A(7) + X * (A(8) + A(9) * X + A(10) * Y)
1930 YNEW = YNEW + Y * (A(11) + A(12) * Y)
2020 N = N + 1
2030 Return

2100 Rem Display results
2110 If N < 100 Or N > 1000 Then GoTo 2200
2120      If X < XMIN Then XMIN = X
2130      If X > XMAX Then XMAX = X
2140      If Y < YMIN Then YMIN = Y
2150      If Y > YMAX Then YMAX = Y
2200 If N = 1000 Then GoSub 3100       'Resize the screen
2210 XS(P%) = X
2220 P% = (P% + 1) Mod 500
2230 I% = (P% + 500 - PREV%) Mod 500
2240 If D% = 1 Then XP = XS(I%): YP = XNEW Else XP = X:  YP = Y
2250 If N < 1000 Or XP <= XL Or XP >= XH Or YP <= YL Or YP >= YH Then GoTo 2320
2300      PSet (XP, YP)                   'Plot point on screen
2320 Return

2400 Rem Test results
2410 If Abs(XNEW) + Abs(YNEW) > 1000000! Then T% = 2   'Unbounded
2430 GoSub 2900                'Calculate Lyapunov exponent
2460 If N >= NMAX Then T% = 2  'Strange attractor found
2470 If Abs(XNEW - X) + Abs(YNEW - Y) < .000001 Then T% = 2
2480 If N > 100 And L < .005 Then T% = 2   'Limit cycle
2490 DoEvents                  'Respond to user command
2510 X = XNEW                  'Update value of X
2520 Y = YNEW
2550 Return

2600 Rem Get coefficients
2650 O% = 2 + Int((OMAX% - 1) * Rnd)
2660 CODE$ = Chr$(59 + 4 * D% + O%)
2680 M% = 1: For I% = 1 To D%:  M% = M% * (O% + I%):  Next I%
2690      For I% = 1 To M%        'Construct CODE$
2700              GoSub 2800          'Shuffle random numbers
2710              CODE$ = CODE$ + Chr$(65 + Int(25 * RAN))
2720      Next I%
2730 For I% = 1 To M%          'Convert CODE$ to coefficient values
2740      A(I%) = (Asc(Mid$(CODE$, I% + 1, 1)) - 77) / 10
2750 Next I%
2760 Return

2800 Rem Shuffle random numbers
2810 If V(0) = 0 Then For J% = 0 To 99: V(J%) = Rnd:  Next J%
2820 J% = Int(100 * RAN)
2830 RAN = V(J%)
2840 V(J%) = Rnd
2850 Return

2900 Rem Calculate Lyapunov exponent
2910 XSAVE = XNEW: YSAVE = YNEW:  X = XE:  Y = YE:  N = N - 1
2930 GoSub 1700                'Reiterate equations
2940 DLX = XNEW - XSAVE: DLY = YNEW - YSAVE
2960 DL2 = DLX * DLX + DLY * DLY
2970 If CSng(DL2) <= 0 Then GoTo 3070      'Don't divide by zero
2980      DF = 1000000000000# * DL2
2990      RS = 1 / Sqr(DF)
3000      XE = XSAVE + RS * (XNEW - XSAVE):  YE = YSAVE + RS * (YNEW - YSAVE)
3020      XNEW = XSAVE:  YNEW = YSAVE
3030      If DF > 0 Then LSUM = LSUM + Log(DF):  NL = NL + 1
3040      L = .721347 * LSUM / NL
3070 Return

3100 Rem Resize the screen
3110 If D% = 1 Then YMIN = XMIN: YMAX = XMAX
3120 If XMAX - XMIN < .000001 Then XMIN = XMIN - .0000005: XMAX = XMAX + .0000005
3130 If YMAX - YMIN < .000001 Then YMIN = YMIN - .0000005: YMAX = YMAX + .0000005
3160 MX = .1 * (XMAX - XMIN): MY = .1 * (YMAX - YMIN)
3170 XL = XMIN - MX: XH = XMAX + MX:  YL = YMIN - MY:  YH = YMAX + MY
3180 Scale (XL, YL)-(XH, YH): Cls
3460 Return

End Sub

