#lang racket

(provide column-labels
         ids-header-1
         ids-header-2)

(define column-labels (list "Command" "Needle" "Dispense" "X1" "Y1" "Z1" "X2" "Y2" "Z2" "X3" "Y3" "Z3" "Speed" "NeedleGap" "Duration" "Travel Delay" "Retract Delay" "Approach Height" "Retract V" "Retract Heigh" "Clearance Heigh" "Detail Distance" "Arc Radius" "Pitch" "Fill" "Run Time" "Spiral" "RtAngle" "Edge" "IO"))
(define ids-header-1 "#<<#File name: ")
(define ids-header-2 "#<<#

#Objective: Data format defination for Pattern data. ptp file is used to store pattern defination in txt files.

#

#

#Note: 1, Empty line is allowed

#      2, Items are seperated by commas



#Pattern starts

Unit=mm

PatternID =ABCDE_GHIJ_KLMN



#  0  1       2         3   4   5   6   7   8   9   10  11  12    13        14       15       16       17   18    19    20     21        22     23    24    25      26     27      28   29

#Cmd, Needle, Dispense, x1, y1, z1, x2, y2, z2, x3, y3, z3, Speed,NeedleGap,Duration,TrvDelay,RetDelay,AppH,RetV, RetH, ClearH,DTailDist,ArcRad,Pitch,FillH,RunTime,Sprial,RtAngle,Edge,IO

#")