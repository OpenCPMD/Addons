! hello emacs this is -*- f90 -*-
!-----------------------------------------------------------------------------!
! Copyright by Stefan Goedecker, Lausanne, Switzerland, August 1, 1991
! modified by Stefan Goedecker, Cornell, Ithaca, USA, March 25, 1994
! modified by Stefan Goedecker, Stuttgart, Germany, October 6, 1995
! Commercial use is prohibited
! without the explicit permission of the author.
!-----------------------------------------------------------------------------!

SUBROUTINE ctrig ( n, trig, after, before, now, isign, ic )

  IMPLICIT NONE

  INTEGER, PARAMETER :: dbl = SELECTED_REAL_KIND ( 14, 200 )

  INTEGER, INTENT ( IN ) :: n
  INTEGER, INTENT ( IN ) :: isign
  INTEGER, INTENT ( OUT ) :: ic
  INTEGER, DIMENSION ( 7 ), INTENT ( OUT ) :: after, before, now
  REAL ( dbl ) , DIMENSION ( 2, 1024 ), INTENT ( OUT ) :: trig

  INTEGER :: i, j, itt
  REAL ( dbl ) :: twopi, angle
  INTEGER, PARAMETER :: nt = 100
  INTEGER, DIMENSION ( 7, nt ), PARAMETER :: idata = RESHAPE ((/ &
    3,  3, 1, 1, 1, 1, 1,    4,  4, 1, 1, 1, 1, 1,    5,  5, 1, 1, 1, 1, 1,    6,  6, 1, 1, 1, 1, 1, &
    8,  8, 1, 1, 1, 1, 1,    9,  3, 3, 1, 1, 1, 1,   12,  4, 3, 1, 1, 1, 1,   15,  5, 3, 1, 1, 1, 1, &
   16,  4, 4, 1, 1, 1, 1,   18,  6, 3, 1, 1, 1, 1,   20,  5, 4, 1, 1, 1, 1,   24,  8, 3, 1, 1, 1, 1, &
   25,  5, 5, 1, 1, 1, 1,   27,  3, 3, 3, 1, 1, 1,   30,  6, 5, 1, 1, 1, 1,   32,  8, 4, 1, 1, 1, 1, &
   36,  4, 3, 3, 1, 1, 1,   40,  8, 5, 1, 1, 1, 1,   45,  5, 3, 3, 1, 1, 1,   48,  4, 4, 3, 1, 1, 1, &
   54,  6, 3, 3, 1, 1, 1,   60,  5, 4, 3, 1, 1, 1,   64,  4, 4, 4, 1, 1, 1,   72,  8, 3, 3, 1, 1, 1, &
   75,  5, 5, 3, 1, 1, 1,   80,  5, 4, 4, 1, 1, 1,   81,  3, 3, 3, 3, 1, 1,   90,  6, 5, 3, 1, 1, 1, &
   96,  8, 4, 3, 1, 1, 1,  100,  5, 5, 4, 1, 1, 1,  108,  4, 3, 3, 3, 1, 1,  120,  8, 5, 3, 1, 1, 1, &
  125,  5, 5, 5, 1, 1, 1,  128,  8, 4, 4, 1, 1, 1,  135,  5, 3, 3, 3, 1, 1,  144,  4, 4, 3, 3, 1, 1, &
  150,  6, 5, 5, 1, 1, 1,  160,  8, 5, 4, 1, 1, 1,  162,  6, 3, 3, 3, 1, 1,  180,  5, 4, 3, 3, 1, 1, &
  192,  4, 4, 4, 3, 1, 1,  200,  8, 5, 5, 1, 1, 1,  216,  8, 3, 3, 3, 1, 1,  225,  5, 5, 3, 3, 1, 1, &
  240,  5, 4, 4, 3, 1, 1,  243,  3, 3, 3, 3, 3, 1,  256,  4, 4, 4, 4, 1, 1,  270,  6, 5, 3, 3, 1, 1, &
  288,  8, 4, 3, 3, 1, 1,  300,  5, 5, 4, 3, 1, 1,  320,  5, 4, 4, 4, 1, 1,  324,  4, 3, 3, 3, 3, 1, &
  360,  8, 5, 3, 3, 1, 1,  375,  5, 5, 5, 3, 1, 1,  384,  8, 4, 4, 3, 1, 1,  400,  5, 5, 4, 4, 1, 1, &
  405,  5, 3, 3, 3, 3, 1,  432,  4, 4, 3, 3, 3, 1,  450,  6, 5, 5, 3, 1, 1,  480,  8, 5, 4, 3, 1, 1, &
  486,  6, 3, 3, 3, 3, 1,  500,  5, 5, 5, 4, 1, 1,  512,  8, 4, 4, 4, 1, 1,  540,  5, 4, 3, 3, 3, 1, &
  576,  4, 4, 4, 3, 3, 1,  600,  8, 5, 5, 3, 1, 1,  625,  5, 5, 5, 5, 1, 1,  640,  8, 5, 4, 4, 1, 1, &
  648,  8, 3, 3, 3, 3, 1,  675,  5, 5, 3, 3, 3, 1,  720,  5, 4, 4, 3, 3, 1,  729,  3, 3, 3, 3, 3, 3, &
  750,  6, 5, 5, 5, 1, 1,  768,  4, 4, 4, 4, 3, 1,  800,  8, 5, 5, 4, 1, 1,  810,  6, 5, 3, 3, 3, 1, &
  864,  8, 4, 3, 3, 3, 1,  900,  5, 5, 4, 3, 3, 1,  960,  5, 4, 4, 4, 3, 1,  972,  4, 3, 3, 3, 3, 3, &
 1000,  8, 5, 5, 5, 1, 1, 1024,  4, 4, 4, 4, 4, 1, 1080,  8, 5, 3, 3, 3, 1, 1152,  8, 4, 4, 3, 3, 1, &
 1200,  5, 5, 4, 4, 3, 1, 1280,  5, 4, 4, 4, 4, 1, 1296,  4, 4, 3, 3, 3, 3, 1350,  6, 5, 5, 3, 3, 1, &
 1440,  6, 5, 4, 4, 3, 1, 1458,  6, 3, 3, 3, 3, 3, 1500,  5, 5, 5, 4, 3, 1, 1536,  8, 4, 4, 4, 3, 1, &
 1600,  8, 8, 5, 5, 1, 1, 1620,  5, 4, 3, 3, 3, 3, 1728,  8, 8, 3, 3, 3, 1, 1800,  8, 5, 5, 3, 3, 1, &
 1920,  8, 5, 4, 4, 3, 1, 1944,  8, 3, 3, 3, 3, 3, 2000,  5, 5, 5, 4, 4, 1, 2048,  8, 4, 4, 4, 4, 1 /), &
         (/7,nt/))

!-----------------------------------------------------------------------------!

  mloop: DO i = 1, nt
    IF ( n == idata ( 1, i ) ) THEN
      ic=0
      DO j = 1, 6
        itt = idata ( 1 + j, i )
        IF ( itt > 1 ) THEN
          ic = ic + 1
          now ( j ) = idata ( 1 + j, i )
        ELSE
          EXIT mloop
        END IF
      END DO
      EXIT mloop
    END IF
    IF ( i == nt ) THEN
      WRITE ( *, '(A,i5,A)' ) " Value of ",n, &
            " not allowed for fft, allowed values are:"
      WRITE ( *, '(15i5)' ) ( idata ( 1, j ), j = 1, nt )
      stop 'ctrig'
    END IF
  END DO mloop

  after ( 1 ) = 1
  before ( ic ) = 1
  DO i = 2, ic
    after ( i ) = after ( i - 1 ) * now ( i - 1 )
    before ( ic - i + 1 ) = before ( ic - i + 2 ) * now ( ic - i + 2 )
  END DO

  twopi = 8._dbl * ATAN ( 1._dbl )
  angle = isign * twopi / DBLE(n)
  trig ( 1, 1 ) = 1._dbl
  trig ( 2, 1 ) = 0._dbl
!$OMP parallel do private(i)
!OCL NOALIAS
  DO i = 1, n - 1
    trig ( 1, i + 1 ) = cos ( DBLE(i) * angle )
    trig ( 2, i + 1 ) = sin ( DBLE(i) * angle )
  END DO

!-----------------------------------------------------------------------------!

  END SUBROUTINE ctrig

!-----------------------------------------------------------------------------!
!-----------------------------------------------------------------------------!
!Copyright by Stefan Goedecker, Cornell, Ithaca, USA, March 25, 1994
!modified by Stefan Goedecker, Stuttgart, Germany, October 15, 1995
!Commercial use is prohibited without the explicit permission of the author.
!-----------------------------------------------------------------------------!

SUBROUTINE fftpre ( mm, nfft, m, nn, n, zin, zout, &
                    trig, now, after, before, isign )

  IMPLICIT NONE

  INTEGER, PARAMETER :: dbl = SELECTED_REAL_KIND ( 14, 200 )

! Arguments
  INTEGER, INTENT ( IN ) :: mm, nfft, m, nn, n, now, after, before, isign
  REAL ( dbl ), DIMENSION ( 2, 1024 ), INTENT ( IN ) :: trig
  REAL ( dbl ), DIMENSION ( 2, m, mm ), INTENT ( IN ) :: zin
  REAL ( dbl ), DIMENSION ( 2, nn, n ), INTENT ( OUT ) :: zout

  INTEGER :: atn, atb, ia, ib, nin1, nin2, nin3, nin4, nin5, nin6, nin7, nin8
  INTEGER :: nout1, nout2, nout3, nout4, nout5, nout6, nout7, nout8, &
             i, j, ias, itt, itrig
  REAL ( dbl ) :: s, s1, s2, s3, s4, s5, s6, s7, s8, &
                  r, r1, r2, r3, r4, r5, r6, r7, r8, cr2, cr3, cr4, cr5, &
                  ci2, ci3, ci4, ci5, ur1, ur2, ur3, ui1, ui2, ui3, &
                  vr1, vr2, vr3, vi1, vi2, vi3, cm, cp, dm, dp, &
                  am, ap, bm, bp,s25, s34, r34, r25, sin2, sin4
  REAL ( dbl ), PARAMETER :: rt2i = 0.7071067811865475_dbl  ! sqrt(0.5)
  REAL ( dbl ), PARAMETER :: bb = 0.8660254037844387_dbl  ! sqrt(3)/2
  REAL ( dbl ), PARAMETER :: cos2 = 0.3090169943749474_dbl ! cos(2*pi/5)
  REAL ( dbl ), PARAMETER :: cos4 = - 0.8090169943749474_dbl !  cos(4*pi/5)
  REAL ( dbl ), PARAMETER :: sin2p = 0.9510565162951536_dbl ! sin(2*pi/5)
  REAL ( dbl ), PARAMETER :: sin4p = 0.5877852522924731_dbl ! sin(4*pi/5)

!-----------------------------------------------------------------------------!

  atn = after * now
  atb = after * before

  IF ( now == 4 ) THEN
    IF ( isign == 1 ) THEN
      ia = 1
      nin1 = ia - after
      nout1 = ia - atn
      DO ib = 1, before
        nin1 = nin1 + after
        nin2 = nin1 + atb
        nin3 = nin2 + atb
        nin4 = nin3 + atb
        nout1 = nout1 + atn
        nout2 = nout1 + after
        nout3 = nout2 + after
        nout4 = nout3 + after
        DO j = 1, nfft
          r1 = zin ( 1, nin1, j )
          s1 = zin ( 2, nin1, j )
          r2 = zin ( 1, nin2, j )
          s2 = zin ( 2, nin2, j )
          r3 = zin ( 1, nin3, j )
          s3 = zin ( 2, nin3, j )
          r4 = zin ( 1, nin4, j )
          s4 = zin ( 2, nin4, j )
          r = r1 + r3
          s = r2 + r4
          zout ( 1, j, nout1 ) = r + s
          zout ( 1, j, nout3 ) = r - s
          r = r1 - r3
          s = s2 - s4
          zout ( 1, j, nout2 ) = r - s
          zout ( 1, j, nout4 ) = r + s
          r = s1 + s3
          s = s2 + s4
          zout ( 2, j, nout1 ) = r + s
          zout ( 2, j, nout3 ) = r - s
          r = s1 - s3
          s = r2 - r4
          zout ( 2, j, nout2 ) = r + s
          zout ( 2, j, nout4 ) = r - s
        END DO
      END DO
      DO ia = 2, after
        ias = ia - 1
        IF ( 2*ias == after ) THEN
          nin1 = ia - after
          nout1 = ia - atn
          DO ib = 1, before
            nin1 = nin1 + after
            nin2 = nin1 + atb
            nin3 = nin2 + atb
            nin4 = nin3 + atb
            nout1 = nout1 + atn
            nout2 = nout1 + after
            nout3 = nout2 + after
            nout4 = nout3 + after
            DO j = 1, nfft
              r1 = zin ( 1, nin1, j )
              s1 = zin ( 2, nin1, j )
              r = zin ( 1, nin2, j )
              s = zin ( 2, nin2, j )
              r2 = ( r - s ) * rt2i
              s2 = ( r + s ) * rt2i
              r3 = - zin ( 2, nin3, j )
              s3 = zin ( 1, nin3, j )
              r = zin ( 1, nin4, j )
              s = zin ( 2, nin4, j )
              r4 = - ( r + s ) * rt2i
              s4 = ( r -  s ) * rt2i
              r = r1 + r3
              s = r2 + r4
              zout ( 1, j, nout1 ) = r + s
              zout ( 1, j, nout3 ) = r - s
              r = r1 - r3
              s = s2 - s4
              zout ( 1, j, nout2 ) = r - s
              zout ( 1, j, nout4 ) = r + s
              r = s1 + s3
              s = s2 + s4
              zout ( 2, j, nout1 ) = r + s
              zout ( 2, j, nout3 ) = r - s
              r = s1 - s3
              s = r2 - r4
              zout ( 2, j, nout2 ) = r + s
              zout ( 2, j, nout4 ) = r - s
            END DO
          END DO
        ELSE
          itt = ias * before
          itrig = itt + 1
          cr2 = trig ( 1, itrig )
          ci2 = trig ( 2, itrig )
          itrig = itrig + itt
          cr3 = trig ( 1, itrig )
          ci3 = trig ( 2, itrig )
          itrig = itrig + itt
          cr4 = trig ( 1, itrig )
          ci4 = trig ( 2, itrig )
          nin1 = ia - after
          nout1 = ia - atn
          DO ib = 1, before
            nin1 = nin1 + after
            nin2 = nin1 + atb
            nin3 = nin2 + atb
            nin4 = nin3 + atb
            nout1 = nout1 + atn
            nout2 = nout1 + after
            nout3 = nout2 + after
            nout4 = nout3 + after
            DO j = 1, nfft
              r1 = zin ( 1, nin1, j )
              s1 = zin ( 2, nin1, j )
              r = zin ( 1, nin2, j )
              s = zin ( 2, nin2, j )
              r2 = r * cr2 - s * ci2
              s2 = r * ci2 + s * cr2
              r = zin ( 1, nin3, j )
              s = zin ( 2, nin3, j )
              r3 = r * cr3 - s * ci3
              s3 = r * ci3 + s * cr3
              r = zin ( 1, nin4, j )
              s = zin ( 2, nin4, j )
              r4 = r * cr4 - s * ci4
              s4 = r * ci4 + s * cr4
              r = r1 + r3
              s = r2 + r4
              zout ( 1, j, nout1 ) = r + s
              zout ( 1, j, nout3 ) = r - s
              r = r1 - r3
              s = s2 - s4
              zout ( 1, j, nout2 ) = r - s
              zout ( 1, j, nout4 ) = r + s
              r = s1 + s3
              s = s2 + s4
              zout ( 2, j, nout1 ) = r + s
              zout ( 2, j, nout3 ) = r - s
              r = s1 - s3
              s = r2 - r4
              zout ( 2, j, nout2 ) = r + s
              zout ( 2, j, nout4 ) = r - s
            END DO
          END DO
        END IF
      END DO
    ELSE
      ia = 1
      nin1 = ia - after
      nout1 = ia - atn
      DO ib = 1, before
        nin1 = nin1 + after
        nin2 = nin1 + atb
        nin3 = nin2 + atb
        nin4 = nin3 + atb
        nout1 = nout1 + atn
        nout2 = nout1 + after
        nout3 = nout2 + after
        nout4 = nout3 + after
        DO j = 1, nfft
          r1 = zin ( 1, nin1, j )
          s1 = zin ( 2, nin1, j )
          r2 = zin ( 1, nin2, j )
          s2 = zin ( 2, nin2, j )
          r3 = zin ( 1, nin3, j )
          s3 = zin ( 2, nin3, j )
          r4 = zin ( 1, nin4, j )
          s4 = zin ( 2, nin4, j )
          r = r1 + r3
          s = r2 + r4
          zout ( 1, j, nout1 ) = r + s
          zout ( 1, j, nout3 ) = r - s
          r = r1 - r3
          s = s2 - s4
          zout ( 1, j, nout2 ) = r + s
          zout ( 1, j, nout4 ) = r - s
          r = s1 + s3
          s = s2 + s4
          zout ( 2, j, nout1 ) = r + s
          zout ( 2, j, nout3 ) = r - s
          r = s1 - s3
          s = r2 - r4
          zout ( 2, j, nout2 ) = r - s
          zout ( 2, j, nout4 ) = r + s
        END DO
      END DO
      DO ia = 2, after
        ias = ia - 1
        IF ( 2 * ias == after ) THEN
          nin1 = ia - after
          nout1 = ia - atn
          DO ib = 1, before
            nin1 = nin1 + after
            nin2 = nin1 + atb
            nin3 = nin2 + atb
            nin4 = nin3 + atb
            nout1 = nout1 + atn
            nout2 = nout1 + after
            nout3 = nout2 + after
            nout4 = nout3 + after
            DO j = 1, nfft
              r1 = zin ( 1, nin1, j )
              s1 = zin ( 2, nin1, j )
              r = zin ( 1, nin2, j )
              s = zin ( 2, nin2, j )
              r2 = ( r + s ) * rt2i
              s2 = ( s - r ) * rt2i
              r3 = zin ( 2, nin3, j )
              s3 = - zin ( 1, nin3, j )
              r = zin ( 1, nin4, j )
              s = zin ( 2, nin4, j )
              r4 = ( s - r ) * rt2i
              s4 = - ( r + s ) * rt2i
              r = r1 + r3
              s = r2 + r4
              zout ( 1, j, nout1 ) = r + s
              zout ( 1, j, nout3 ) = r - s
              r = r1 - r3
              s = s2 - s4
              zout ( 1, j, nout2 ) = r + s
              zout ( 1, j, nout4 ) = r - s
              r =s1 + s3
              s =s2 + s4
              zout ( 2, j, nout1 ) = r + s
              zout ( 2, j, nout3 ) = r - s
              r = s1 - s3
              s = r2 - r4
              zout ( 2, j, nout2 ) = r - s
              zout ( 2, j, nout4 ) = r + s
            END DO
          END DO
        ELSE
          itt = ias * before
          itrig = itt + 1
          cr2 = trig ( 1, itrig )
          ci2 = trig ( 2, itrig )
          itrig = itrig + itt
          cr3 = trig ( 1, itrig )
          ci3 = trig ( 2, itrig )
          itrig = itrig + itt
          cr4 = trig ( 1, itrig )
          ci4 = trig ( 2, itrig )
          nin1 = ia - after
          nout1 = ia - atn
          DO ib = 1, before
            nin1 = nin1 + after
            nin2 = nin1 + atb
            nin3 = nin2 + atb
            nin4 = nin3 + atb
            nout1 = nout1 + atn
            nout2 = nout1 + after
            nout3 = nout2 + after
            nout4 = nout3 + after
            DO j = 1, nfft
              r1 = zin ( 1, nin1, j )
              s1 = zin ( 2, nin1, j )
              r = zin ( 1, nin2, j )
              s = zin ( 2, nin2, j )
              r2 = r * cr2 - s * ci2
              s2 = r * ci2 + s * cr2
              r = zin ( 1, nin3, j )
              s = zin ( 2, nin3, j )
              r3 = r * cr3 - s * ci3
              s3 = r * ci3 + s * cr3
              r = zin ( 1, nin4, j )
              s = zin ( 2, nin4, j )
              r4 = r * cr4 - s * ci4
              s4 = r * ci4 + s * cr4
              r = r1 + r3
              s = r2 + r4
              zout ( 1, j, nout1 ) = r + s
              zout ( 1, j, nout3 ) = r - s
              r = r1 - r3
              s = s2 - s4
              zout ( 1, j, nout2 ) = r + s
              zout ( 1, j, nout4 ) = r - s
              r = s1 + s3
              s = s2 + s4
              zout ( 2, j, nout1 ) = r + s
              zout ( 2, j, nout3 ) = r - s
              r = s1 - s3
              s = r2 - r4
              zout ( 2, j, nout2 ) = r - s
              zout ( 2, j, nout4 ) = r + s
            END DO
          END DO
        END IF
      END DO
    END IF
  ELSE IF ( now == 8 ) THEN
    IF ( isign == -1 ) THEN
      ia = 1
      nin1 = ia - after
      nout1 = ia - atn
      DO ib = 1, before
        nin1 = nin1 + after
        nin2 = nin1 + atb
        nin3 = nin2 + atb
        nin4 = nin3 + atb
        nin5 = nin4 + atb
        nin6 = nin5 + atb
        nin7 = nin6 + atb
        nin8 = nin7 + atb
        nout1 = nout1 + atn
        nout2 = nout1 + after
        nout3 = nout2 + after
        nout4 = nout3 + after
        nout5 = nout4 + after
        nout6 = nout5 + after
        nout7 = nout6 + after
        nout8 = nout7 + after
        DO j = 1, nfft
          r1 = zin ( 1, nin1, j )
          s1 = zin ( 2, nin1, j )
          r2 = zin ( 1, nin2, j )
          s2 = zin ( 2, nin2, j )
          r3 = zin ( 1, nin3, j )
          s3 = zin ( 2, nin3, j )
          r4 = zin ( 1, nin4, j )
          s4 = zin ( 2, nin4, j )
          r5 = zin ( 1, nin5, j )
          s5 = zin ( 2, nin5, j )
          r6 = zin ( 1, nin6, j )
          s6 = zin ( 2, nin6, j )
          r7 = zin ( 1, nin7, j )
          s7 = zin ( 2, nin7, j )
          r8 = zin ( 1, nin8, j )
          s8 = zin ( 2, nin8, j )
          r = r1 + r5
          s = r3 + r7
          ap = r + s
          am = r - s
          r = r2 + r6
          s = r4 + r8
          bp = r + s
          bm = r - s
          r = s1 + s5
          s = s3 + s7
          cp = r + s
          cm = r - s
          r = s2 + s6
          s = s4 + s8
          dp = r + s
          dm = r - s
          zout ( 1, j, nout1 ) = ap + bp
          zout ( 2, j, nout1 ) = cp + dp
          zout ( 1, j, nout5 ) = ap - bp
          zout ( 2, j, nout5 ) = cp - dp
          zout ( 1, j, nout3 ) = am + dm
          zout ( 2, j, nout3 ) = cm - bm
          zout ( 1, j, nout7 ) = am - dm
          zout ( 2, j, nout7 ) = cm + bm
          r = r1 - r5
          s = s3 - s7
          ap = r + s
          am = r - s
          r = s1 - s5
          s = r3 - r7
          bp = r + s
          bm = r - s
          r = s4 - s8
          s = r2 - r6
          cp = r + s
          cm = r - s
          r = s2 - s6
          s = r4 - r8
          dp = r + s
          dm = r - s
          r = ( cp + dm ) * rt2i
          s = (-cp + dm ) * rt2i
          cp = ( cm + dp ) * rt2i
          dp = ( cm - dp ) * rt2i
          zout ( 1, j, nout2 ) = ap + r
          zout ( 2, j, nout2 ) = bm + s
          zout ( 1, j, nout6 ) = ap - r
          zout ( 2, j, nout6 ) = bm - s
          zout ( 1, j, nout4 ) = am + cp
          zout ( 2, j, nout4 ) = bp + dp
          zout ( 1, j, nout8 ) = am - cp
          zout ( 2, j, nout8 ) = bp - dp
        END DO
      END DO
    ELSE
      ia = 1
      nin1 = ia - after
      nout1 = ia - atn
      DO ib = 1, before
        nin1 = nin1 + after
        nin2 = nin1 + atb
        nin3 = nin2 + atb
        nin4 = nin3 + atb
        nin5 = nin4 + atb
        nin6 = nin5 + atb
        nin7 = nin6 + atb
        nin8 = nin7 + atb
        nout1 = nout1 + atn
        nout2 = nout1 + after
        nout3 = nout2 + after
        nout4 = nout3 + after
        nout5 = nout4 + after
        nout6 = nout5 + after
        nout7 = nout6 + after
        nout8 = nout7 + after
        DO j = 1, nfft
          r1 = zin ( 1, nin1, j )
          s1 = zin ( 2, nin1, j )
          r2 = zin ( 1, nin2, j )
          s2 = zin ( 2, nin2, j )
          r3 = zin ( 1, nin3, j )
          s3 = zin ( 2, nin3, j )
          r4 = zin ( 1, nin4, j )
          s4 = zin ( 2, nin4, j )
          r5 = zin ( 1, nin5, j )
          s5 = zin ( 2, nin5, j )
          r6 = zin ( 1, nin6, j )
          s6 = zin ( 2, nin6, j )
          r7 = zin ( 1, nin7, j )
          s7 = zin ( 2, nin7, j )
          r8 = zin ( 1, nin8, j )
          s8 = zin ( 2, nin8, j )
          r = r1 + r5
          s = r3 + r7
          ap = r + s
          am = r - s
          r = r2 + r6
          s = r4 + r8
          bp = r + s
          bm = r - s
          r = s1 + s5
          s = s3 + s7
          cp = r + s
          cm = r - s
          r = s2 + s6
          s = s4 + s8
          dp = r + s
          dm = r - s
          zout ( 1, j, nout1 ) = ap + bp
          zout ( 2, j, nout1 ) = cp + dp
          zout ( 1, j, nout5 ) = ap - bp
          zout ( 2, j, nout5 ) = cp - dp
          zout ( 1, j, nout3 ) = am - dm
          zout ( 2, j, nout3 ) = cm + bm
          zout ( 1, j, nout7 ) = am + dm
          zout ( 2, j, nout7 ) = cm - bm
          r = r1 - r5
          s = -s3 + s7
          ap = r + s
          am = r - s
          r = s1 - s5
          s = r7 - r3
          bp = r + s
          bm = r - s
          r = -s4 + s8
          s = r2 - r6
          cp = r + s
          cm = r - s
          r = -s2 + s6
          s = r4 - r8
          dp = r + s
          dm = r - s
          r = ( cp + dm ) * rt2i
          s = ( cp - dm ) * rt2i
          cp = ( cm + dp ) * rt2i
          dp = (-cm + dp ) * rt2i
          zout ( 1, j, nout2 ) = ap + r
          zout ( 2, j, nout2 ) = bm + s
          zout ( 1, j, nout6 ) = ap - r
          zout ( 2, j, nout6 ) = bm - s
          zout ( 1, j, nout4 ) = am + cp
          zout ( 2, j, nout4 ) = bp + dp
          zout ( 1, j, nout8 ) = am - cp
          zout ( 2, j, nout8 ) = bp - dp
        END DO
      END DO
    END IF
  ELSE IF ( now == 3 ) THEN
    ia = 1
    nin1 = ia - after
    nout1 = ia - atn
    DO ib = 1, before
      nin1 = nin1 + after
      nin2 = nin1 + atb
      nin3 = nin2 + atb
      nout1 = nout1 + atn
      nout2 = nout1 + after
      nout3 = nout2 + after
      DO j = 1, nfft
        r1 = zin ( 1, nin1, j )
        s1 = zin ( 2, nin1, j )
        r2 = zin ( 1, nin2, j )
        s2 = zin ( 2, nin2, j )
        r3 = zin ( 1, nin3, j )
        s3 = zin ( 2, nin3, j )
        r = r2 + r3
        s = s2 + s3
        zout ( 1, j, nout1 ) = r + r1
        zout ( 2, j, nout1 ) = s + s1
        r1 = r1 - 0.5_dbl * r
        s1 = s1 - 0.5_dbl * s
        r2 = bb * ( r2 - r3 )
        s2 = bb * ( s2 - s3 )
        zout ( 1, j, nout2 ) = r1 - s2
        zout ( 2, j, nout2 ) = s1 + r2
        zout ( 1, j, nout3 ) = r1 + s2
        zout ( 2, j, nout3 ) = s1 - r2
      END DO
    END DO
    DO ia = 2, after
      ias = ia - 1
      IF ( 4*ias == 3*after ) THEN
        IF ( isign == 1 ) THEN
          nin1 = ia - after
          nout1 = ia - atn
          DO ib = 1, before
            nin1 = nin1 + after
            nin2=nin1+atb
            nin3=nin2+atb
            nout1=nout1+atn
            nout2=nout1+after
            nout3=nout2+after
            DO j = 1, nfft
              r1 = zin ( 1, nin1, j )
              s1 = zin ( 2, nin1, j )
              r2 = -zin ( 2, nin2, j )
              s2 = zin ( 1, nin2, j )
              r3 = -zin ( 1, nin3, j )
              s3 = -zin ( 2, nin3, j )
              r = r2 + r3
              s = s2 + s3
              zout ( 1, j, nout1 ) = r + r1
              zout ( 2, j, nout1 ) = s + s1
              r1 = r1 - 0.5_dbl * r
              s1 = s1 - 0.5_dbl * s
              r2 = bb*(r2-r3)
              s2 = bb*(s2-s3)
              zout ( 1, j, nout2 ) = r1 - s2
              zout ( 2, j, nout2 ) = s1 + r2
              zout ( 1, j, nout3 ) = r1 + s2
              zout ( 2, j, nout3 ) = s1 - r2
            END DO
          END DO
        ELSE
          nin1 = ia - after
          nout1 = ia - atn
          DO ib = 1, before
            nin1 = nin1 + after
            nin2 = nin1 + atb
            nin3 = nin2 + atb
            nout1 = nout1 + atn
            nout2 = nout1 + after
            nout3 = nout2 + after
            DO j = 1, nfft
              r1 = zin ( 1, nin1, j )
              s1 = zin ( 2, nin1, j )
              r2 = zin ( 2, nin2, j )
              s2 = -zin ( 1, nin2, j )
              r3 = -zin ( 1, nin3, j )
              s3 = -zin ( 2, nin3, j )
              r = r2 + r3
              s = s2 + s3
              zout ( 1, j, nout1 ) = r + r1
              zout ( 2, j, nout1 ) = s + s1
              r1 = r1 - 0.5_dbl * r
              s1 = s1 - 0.5_dbl * s
              r2 = bb * ( r2 - r3 )
              s2 = bb * ( s2 - s3 )
              zout ( 1, j, nout2 ) = r1 - s2
              zout ( 2, j, nout2 ) = s1 + r2
              zout ( 1, j, nout3 ) = r1 + s2
              zout ( 2, j, nout3 ) = s1 - r2
            END DO
          END DO
        END IF
      ELSE IF ( 8 * ias == 3 * after ) THEN
        IF ( isign == 1 ) THEN
          nin1 = ia - after
          nout1 = ia - atn
          DO ib = 1, before
            nin1 = nin1 + after
            nin2 = nin1 + atb
            nin3 = nin2 + atb
            nout1 = nout1 + atn
            nout2 = nout1 + after
            nout3 = nout2 + after
            DO j = 1, nfft
              r1 = zin ( 1, nin1, j )
              s1 = zin ( 2, nin1, j )
              r = zin ( 1, nin2, j )
              s = zin ( 2, nin2, j )
              r2 = ( r - s ) * rt2i
              s2 = ( r + s ) * rt2i
              r3 = -zin ( 2, nin3, j )
              s3 = zin ( 1, nin3, j )
              r = r2 + r3
              s = s2 + s3
              zout ( 1, j, nout1 ) = r + r1
              zout ( 2, j, nout1 ) = s + s1
              r1 = r1 - 0.5_dbl * r
              s1 = s1 - 0.5_dbl * s
              r2 = bb * ( r2 - r3 )
              s2 = bb * ( s2 - s3 )
              zout ( 1, j, nout2 ) = r1 - s2
              zout ( 2, j, nout2 ) = s1 + r2
              zout ( 1, j, nout3 ) = r1 + s2
              zout ( 2, j, nout3 ) = s1 - r2
            END DO
          END DO
        ELSE
          nin1 = ia - after
          nout1 = ia - atn
          DO ib = 1, before
            nin1 = nin1 + after
            nin2 = nin1 + atb
            nin3 = nin2 + atb
            nout1 = nout1 + atn
            nout2 = nout1 + after
            nout3 = nout2 + after
            DO j = 1, nfft
              r1 = zin ( 1, nin1, j )
              s1 = zin ( 2, nin1, j )
              r = zin ( 1, nin2, j )
              s = zin ( 2, nin2, j )
              r2 = ( r + s ) * rt2i
              s2 = ( -r + s ) * rt2i
              r3 = zin ( 2, nin3, j )
              s3 = -zin ( 1, nin3, j )
              r = r2 + r3
              s = s2 + s3
              zout ( 1, j, nout1 ) = r + r1
              zout ( 2, j, nout1 ) = s + s1
              r1 = r1 - 0.5_dbl * r
              s1 = s1 - 0.5_dbl * s
              r2 = bb * ( r2 - r3 )
              s2 = bb * ( s2 - s3 )
              zout ( 1, j, nout2 ) = r1 - s2
              zout ( 2, j, nout2 ) = s1 + r2
              zout ( 1, j, nout3 ) = r1 + s2
              zout ( 2, j, nout3 ) = s1 - r2
            END DO
          END DO
        END IF
      ELSE
        itt = ias * before
        itrig = itt + 1
        cr2 = trig ( 1, itrig )
        ci2 = trig ( 2, itrig )
        itrig = itrig + itt
        cr3 = trig ( 1, itrig )
        ci3 = trig ( 2, itrig )
        nin1 = ia - after
        nout1 = ia - atn
        DO ib = 1, before
          nin1 = nin1 + after
          nin2 = nin1 + atb
          nin3 = nin2 + atb
          nout1 = nout1 + atn
          nout2 = nout1 + after
          nout3 = nout2 + after
          DO j = 1, nfft
            r1 = zin ( 1, nin1, j )
            s1 = zin ( 2, nin1, j )
            r = zin ( 1, nin2, j )
            s = zin ( 2, nin2, j )
            r2 = r * cr2 - s * ci2
            s2 = r * ci2 + s * cr2
            r = zin ( 1, nin3, j )
            s = zin ( 2, nin3, j )
            r3 = r * cr3 - s * ci3
            s3 = r * ci3 + s * cr3
            r = r2 + r3
            s = s2 + s3
            zout ( 1, j, nout1 ) = r + r1
            zout ( 2, j, nout1 ) = s + s1
            r1 = r1 - 0.5_dbl * r
            s1 = s1 - 0.5_dbl * s
            r2 = bb * ( r2 - r3 )
            s2 = bb * ( s2 - s3 )
            zout ( 1, j, nout2 ) = r1 - s2
            zout ( 2, j, nout2 ) = s1 + r2
            zout ( 1, j, nout3 ) = r1 + s2
            zout ( 2, j, nout3 ) = s1 - r2
          END DO
        END DO
      END IF
    END DO
  ELSE IF ( now == 5 ) THEN
    sin2 = isign * sin2p
    sin4 = isign * sin4p
    ia = 1
    nin1 = ia - after
    nout1 = ia - atn
    DO ib = 1, before
      nin1 = nin1 + after
      nin2 = nin1 + atb
      nin3 = nin2 + atb
      nin4 = nin3 + atb
      nin5 = nin4 + atb
      nout1 = nout1 + atn
      nout2 = nout1 + after
      nout3 = nout2 + after
      nout4 = nout3 + after
      nout5 = nout4 + after
      DO j = 1, nfft
        r1 = zin ( 1, nin1, j )
        s1 = zin ( 2, nin1, j )
        r2 = zin ( 1, nin2, j )
        s2 = zin ( 2, nin2, j )
        r3 = zin ( 1, nin3, j )
        s3 = zin ( 2, nin3, j )
        r4 = zin ( 1, nin4, j )
        s4 = zin ( 2, nin4, j )
        r5 = zin ( 1, nin5, j )
        s5 = zin ( 2, nin5, j )
        r25 = r2 + r5
        r34 = r3 + r4
        s25 = s2 - s5
        s34 = s3 - s4
        zout ( 1, j, nout1 ) = r1 + r25 + r34
        r = cos2 * r25 + cos4 * r34 + r1
        s = sin2 * s25 + sin4 * s34
        zout ( 1, j, nout2 ) = r - s
        zout ( 1, j, nout5 ) = r + s
        r = cos4 * r25 + cos2 * r34 + r1
        s = sin4 * s25 - sin2 * s34
        zout ( 1, j, nout3 ) = r - s
        zout ( 1, j, nout4 ) = r + s
        r25 = r2 - r5
        r34 = r3 - r4
        s25 = s2 + s5
        s34 = s3 + s4
        zout ( 2, j, nout1 ) = s1 + s25 + s34
        r = cos2 * s25 + cos4 * s34 + s1
        s = sin2 * r25 + sin4 * r34
        zout ( 2, j, nout2 ) = r + s
        zout ( 2, j, nout5 ) = r - s
        r = cos4 * s25 + cos2 * s34 + s1
        s = sin4 * r25 - sin2 * r34
        zout ( 2, j, nout3 ) = r + s
        zout ( 2, j, nout4 ) = r - s
      END DO
    END DO
    DO ia = 2, after
      ias = ia - 1
      IF ( 8 * ias == 5 * after ) THEN
        IF ( isign == 1 ) THEN
          nin1 = ia - after
          nout1 = ia - atn
          DO ib = 1, before
            nin1 = nin1 + after
            nin2 = nin1 + atb
            nin3 = nin2 + atb
            nin4 = nin3 + atb
            nin5 = nin4 + atb
            nout1 = nout1 + atn
            nout2 = nout1 + after
            nout3 = nout2 + after
            nout4 = nout3 + after
            nout5 = nout4 + after
            DO j = 1, nfft
              r1 = zin ( 1, nin1, j )
              s1 = zin ( 2, nin1, j )
              r = zin ( 1, nin2, j )
              s = zin ( 2, nin2, j )
              r2 = ( r - s ) * rt2i
              s2 = ( r + s ) * rt2i
              r3 = -zin ( 2, nin3, j )
              s3 = zin ( 1, nin3, j )
              r = zin ( 1, nin4, j )
              s = zin ( 2, nin4, j )
              r4 = -( r + s ) * rt2i
              s4 = ( r - s ) * rt2i
              r5 = -zin ( 1, nin5, j )
              s5 = -zin ( 2, nin5, j )
              r25 = r2 + r5
              r34 = r3 + r4
              s25 = s2 - s5
              s34 = s3 - s4
              zout ( 1, j, nout1 ) = r1 + r25 + r34
              r = cos2 * r25 + cos4 * r34 + r1
              s = sin2 * s25 + sin4 * s34
              zout ( 1, j, nout2 ) = r - s
              zout ( 1, j, nout5 ) = r + s
              r = cos4 * r25 + cos2 * r34 + r1
              s = sin4 * s25 - sin2 * s34
              zout ( 1, j, nout3 ) = r - s
              zout ( 1, j, nout4 ) = r + s
              r25 = r2 - r5
              r34 = r3 - r4
              s25 = s2 + s5
              s34 = s3 + s4
              zout ( 2, j, nout1 ) = s1 + s25 + s34
              r = cos2 * s25 + cos4 * s34 + s1
              s = sin2 * r25 + sin4 * r34
              zout ( 2, j, nout2 ) = r + s
              zout ( 2, j, nout5 ) = r - s
              r = cos4 * s25 + cos2 * s34 + s1
              s = sin4 * r25 - sin2 * r34
              zout ( 2, j, nout3 ) = r + s
              zout ( 2, j, nout4 ) = r - s
            END DO
          END DO
        ELSE
          nin1 = ia - after
          nout1 = ia - atn
          DO ib = 1, before
            nin1 = nin1 + after
            nin2 = nin1 + atb
            nin3 = nin2 + atb
            nin4 = nin3 + atb
            nin5 = nin4 + atb
            nout1 = nout1 + atn
            nout2 = nout1 + after
            nout3 = nout2 + after
            nout4 = nout3 + after
            nout5 = nout4 + after
            DO j = 1, nfft
              r1 = zin ( 1, nin1, j )
              s1 = zin ( 2, nin1, j )
              r = zin ( 1, nin2, j )
              s = zin ( 2, nin2, j )
              r2 = ( r + s ) * rt2i
              s2 = ( -r + s ) * rt2i
              r3 = zin ( 2, nin3, j )
              s3 = -zin ( 1, nin3, j )
              r = zin ( 1, nin4, j )
              s = zin ( 2, nin4, j )
              r4 = ( s - r ) * rt2i
              s4 = - ( r + s ) * rt2i
              r5 = -zin ( 1, nin5, j )
              s5 = -zin ( 2, nin5, j )
              r25 = r2 + r5
              r34 = r3 + r4
              s25 = s2 - s5
              s34 = s3 - s4
              zout ( 1, j, nout1 ) = r1 + r25 + r34
              r = cos2 * r25 + cos4 * r34 + r1
              s = sin2 * s25 + sin4 * s34
              zout ( 1, j, nout2 ) = r - s
              zout ( 1, j, nout5 ) = r + s
              r = cos4 * r25 + cos2 * r34 + r1
              s = sin4 * s25 - sin2 * s34
              zout ( 1, j, nout3 ) = r - s
              zout ( 1, j, nout4 ) = r + s
              r25 = r2 - r5
              r34 = r3 - r4
              s25 = s2 + s5
              s34 = s3 + s4
              zout ( 2, j, nout1) = s1 + s25 + s34
              r = cos2 * s25 + cos4 * s34 + s1
              s = sin2 * r25 + sin4 * r34
              zout ( 2, j, nout2 ) = r + s
              zout ( 2, j, nout5 ) = r - s
              r = cos4 * s25 + cos2 * s34 + s1
              s = sin4 * r25 - sin2 * r34
              zout ( 2, j, nout3 ) = r + s
              zout ( 2, j, nout4 ) = r - s
            END DO
          END DO
        END IF
      ELSE
        ias = ia - 1
        itt = ias * before
        itrig = itt + 1
        cr2 = trig ( 1, itrig )
        ci2 = trig ( 2, itrig )
        itrig = itrig + itt
        cr3 = trig ( 1, itrig )
        ci3 = trig ( 2, itrig )
        itrig = itrig + itt
        cr4 = trig ( 1, itrig )
        ci4 = trig ( 2, itrig )
        itrig = itrig + itt
        cr5 = trig ( 1, itrig )
        ci5 = trig ( 2, itrig )
        nin1 = ia - after
        nout1 = ia - atn
        DO ib = 1, before
          nin1 = nin1 + after
          nin2 = nin1 + atb
          nin3 = nin2 + atb
          nin4 = nin3 + atb
          nin5 = nin4 + atb
          nout1 = nout1 + atn
          nout2 = nout1 + after
          nout3 = nout2 + after
          nout4 = nout3 + after
          nout5 = nout4 + after
          DO j = 1, nfft
            r1 = zin ( 1, nin1, j )
            s1 = zin ( 2, nin1, j )
            r = zin ( 1, nin2, j )
            s = zin ( 2, nin2, j )
            r2 = r * cr2 - s * ci2
            s2 = r * ci2 + s * cr2
            r = zin ( 1, nin3, j )
            s = zin ( 2, nin3, j )
            r3 = r * cr3 - s * ci3
            s3 = r * ci3 + s * cr3
            r = zin ( 1, nin4, j )
            s = zin ( 2, nin4, j )
            r4 = r * cr4 - s * ci4
            s4 = r * ci4 + s * cr4
            r = zin ( 1, nin5, j )
            s = zin ( 2, nin5, j )
            r5 = r * cr5 - s * ci5
            s5 = r * ci5 + s * cr5
            r25 = r2 + r5
            r34 = r3 + r4
            s25 = s2 - s5
            s34 = s3 - s4
            zout ( 1, j, nout1 ) = r1 + r25 + r34
            r = cos2 * r25 + cos4 * r34 + r1
            s = sin2 * s25 + sin4 * s34
            zout ( 1, j, nout2 ) = r - s
            zout ( 1, j, nout5 ) = r + s
            r = cos4 * r25 + cos2 * r34 + r1
            s = sin4 * s25 - sin2 * s34
            zout ( 1, j, nout3 ) = r - s
            zout ( 1, j, nout4 ) = r + s
            r25 = r2 - r5
            r34 = r3 - r4
            s25 = s2 + s5
            s34 = s3 + s4
            zout ( 2, j, nout1 ) = s1 + s25 + s34
            r = cos2 * s25 + cos4 * s34 + s1
            s = sin2 * r25 + sin4 * r34
            zout ( 2, j, nout2 ) = r + s
            zout ( 2, j, nout5 ) = r - s
            r = cos4 * s25 + cos2 * s34 + s1
            s = sin4 * r25 - sin2 * r34
            zout ( 2, j, nout3 ) = r + s
            zout ( 2, j, nout4 ) = r - s
          END DO
        END DO
      END IF
    END DO
  ELSE IF ( now == 6 ) THEN
    ia = 1
    nin1 = ia - after
    nout1 = ia - atn
    DO ib = 1, before
      nin1 = nin1 + after
      nin2 = nin1 + atb
      nin3 = nin2 + atb
      nin4 = nin3 + atb
      nin5 = nin4 + atb
      nin6 = nin5 + atb
      nout1 = nout1 + atn
      nout2 = nout1 + after
      nout3 = nout2 + after
      nout4 = nout3 + after
      nout5 = nout4 + after
      nout6 = nout5 + after
      DO j = 1, nfft
        r2 = zin ( 1, nin3, j )
        s2 = zin ( 2, nin3, j )
        r3 = zin ( 1, nin5, j )
        s3 = zin ( 2, nin5, j )
        r = r2 + r3
        s = s2 + s3
        r1 = zin ( 1, nin1, j )
        s1 = zin ( 2, nin1, j )
        ur1 = r + r1
        ui1 = s + s1
        r1 = r1 - 0.5_dbl * r
        s1 = s1 - 0.5_dbl * s
        r = r2 - r3
        s = s2 - s3
        ur2 = r1 - s * bb
        ui2 = s1 + r * bb
        ur3 = r1 + s * bb
        ui3 = s1 - r * bb

        r2 = zin ( 1, nin6, j )
        s2 = zin ( 2, nin6, j )
        r3 = zin ( 1, nin2, j )
        s3 = zin ( 2, nin2, j )
        r = r2 + r3
        s = s2 + s3
        r1 = zin ( 1, nin4, j )
        s1 = zin ( 2, nin4, j )
        vr1 = r + r1
        vi1 = s + s1
        r1 = r1 - 0.5_dbl * r
        s1 = s1 - 0.5_dbl * s
        r = r2 - r3
        s = s2 - s3
        vr2 = r1 - s * bb
        vi2 = s1 + r * bb
        vr3 = r1 + s * bb
        vi3 = s1 - r * bb

        zout ( 1, j, nout1 ) = ur1 + vr1
        zout ( 2, j, nout1 ) = ui1 + vi1
        zout ( 1, j, nout5 ) = ur2 + vr2
        zout ( 2, j, nout5 ) = ui2 + vi2
        zout ( 1, j, nout3 ) = ur3 + vr3
        zout ( 2, j, nout3 ) = ui3 + vi3
        zout ( 1, j, nout4 ) = ur1 - vr1
        zout ( 2, j, nout4 ) = ui1 - vi1
        zout ( 1, j, nout2 ) = ur2 - vr2
        zout ( 2, j, nout2 ) = ui2 - vi2
        zout ( 1, j, nout6 ) = ur3 - vr3
        zout ( 2, j, nout6 ) = ui3 - vi3
      END DO
    END DO
  ELSE
    STOP 'Error fftpre'
  END If

!-----------------------------------------------------------------------------!

END SUBROUTINE fftpre

!-----------------------------------------------------------------------------!
!-----------------------------------------------------------------------------!
!Copyright by Stefan Goedecker, Cornell, Ithaca, USA, March 25, 1994
!modified by Stefan Goedecker, Stuttgart, Germany, October 15, 1995
!Commercial use is prohibited without the explicit permission of the author.
!-----------------------------------------------------------------------------!

SUBROUTINE fftrot ( mm, nfft, m, nn, n, zin, zout, &
                    trig, now, after, before, isign )

  IMPLICIT NONE

  INTEGER, PARAMETER :: dbl = SELECTED_REAL_KIND ( 14, 200 )

! Arguments
  INTEGER, INTENT ( IN ) :: mm, nfft, m, nn, n, now, after, before, isign
  REAL ( dbl ), DIMENSION ( 2, 1024 ), INTENT ( IN ) :: trig
  REAL ( dbl ), DIMENSION ( 2, mm, m ), INTENT ( IN ) :: zin
  REAL ( dbl ), DIMENSION ( 2, n, nn ), INTENT ( OUT ) :: zout

  INTEGER :: atn, atb, ia, ib, nin1, nin2, nin3, nin4, nin5, nin6, nin7, nin8
  INTEGER :: nout1, nout2, nout3, nout4, nout5, nout6, nout7, nout8, &
             i, j, ias, itt, itrig
  REAL ( dbl ) :: s, s1, s2, s3, s4, s5, s6, s7, s8, &
                  r, r1, r2, r3, r4, r5, r6, r7, r8, cr2, cr3, cr4, cr5, &
                  ci2, ci3, ci4, ci5, ur1, ur2, ur3, ui1, ui2, ui3, &
                  vr1, vr2, vr3, vi1, vi2, vi3, cm, cp, dm, dp, &
                  am, ap, bm, bp,s25, s34, r34, r25, sin2, sin4
  REAL ( dbl ), PARAMETER :: rt2i = 0.7071067811865475_dbl  ! sqrt(0.5)
  REAL ( dbl ), PARAMETER :: bb = 0.8660254037844387_dbl  ! sqrt(3)/2
  REAL ( dbl ), PARAMETER :: cos2 = 0.3090169943749474_dbl ! cos(2*pi/5)
  REAL ( dbl ), PARAMETER :: cos4 = - 0.8090169943749474_dbl !  cos(4*pi/5)
  REAL ( dbl ), PARAMETER :: sin2p = 0.9510565162951536_dbl ! sin(2*pi/5)
  REAL ( dbl ), PARAMETER :: sin4p = 0.5877852522924731_dbl ! sin(4*pi/5)

!-----------------------------------------------------------------------------!

  atn = after * now
  atb = after * before

  IF ( now == 4 ) THEN
    IF ( isign == 1 ) THEN
      ia = 1
      nin1 = ia - after
      nout1 = ia - atn
      DO ib = 1, before
        nin1 = nin1 + after
        nin2 = nin1 + atb
        nin3 = nin2 + atb
        nin4 = nin3 + atb
        nout1 = nout1 + atn
        nout2 = nout1 + after
        nout3 = nout2 + after
        nout4 = nout3 + after
        DO j = 1, nfft
          r1 = zin ( 1, j, nin1 )
          s1 = zin ( 2, j, nin1 )
          r2 = zin ( 1, j, nin2 )
          s2 = zin ( 2, j, nin2 )
          r3 = zin ( 1, j, nin3 )
          s3 = zin ( 2, j, nin3 )
          r4 = zin ( 1, j, nin4 )
          s4 = zin ( 2, j, nin4 )
          r = r1 + r3
          s = r2 + r4
          zout ( 1, nout1, j ) = r + s
          zout ( 1, nout3, j ) = r - s
          r = r1 - r3
          s = s2 - s4
          zout ( 1, nout2, j ) = r - s
          zout ( 1, nout4, j ) = r + s
          r = s1 + s3
          s = s2 + s4
          zout ( 2, nout1, j ) = r + s
          zout ( 2, nout3, j ) = r - s
          r = s1 - s3
          s = r2 - r4
          zout ( 2, nout2, j ) = r + s
          zout ( 2, nout4, j ) = r - s
        END DO
      END DO
      DO ia = 2, after
        ias = ia - 1
        IF ( 2*ias == after ) THEN
          nin1 = ia - after
          nout1 = ia - atn
          DO ib = 1, before
            nin1 = nin1 + after
            nin2 = nin1 + atb
            nin3 = nin2 + atb
            nin4 = nin3 + atb
            nout1 = nout1 + atn
            nout2 = nout1 + after
            nout3 = nout2 + after
            nout4 = nout3 + after
            DO j = 1, nfft
              r1 = zin ( 1, j, nin1 )
              s1 = zin ( 2, j, nin1 )
              r = zin ( 1, j, nin2 )
              s = zin ( 2, j, nin2 )
              r2 = ( r - s ) * rt2i
              s2 = ( r + s ) * rt2i
              r3 = - zin ( 2, j, nin3 )
              s3 = zin ( 1, j, nin3 )
              r = zin ( 1, j, nin4 )
              s = zin ( 2, j, nin4 )
              r4 = - ( r + s ) * rt2i
              s4 = ( r -  s ) * rt2i
              r = r1 + r3
              s = r2 + r4
              zout ( 1, nout1, j ) = r + s
              zout ( 1, nout3, j ) = r - s
              r = r1 - r3
              s = s2 - s4
              zout ( 1, nout2, j ) = r - s
              zout ( 1, nout4, j ) = r + s
              r = s1 + s3
              s = s2 + s4
              zout ( 2, nout1, j ) = r + s
              zout ( 2, nout3, j ) = r - s
              r = s1 - s3
              s = r2 - r4
              zout ( 2, nout2, j ) = r + s
              zout ( 2, nout4, j ) = r - s
            END DO
          END DO
        ELSE
          itt = ias * before
          itrig = itt + 1
          cr2 = trig ( 1, itrig )
          ci2 = trig ( 2, itrig )
          itrig = itrig + itt
          cr3 = trig ( 1, itrig )
          ci3 = trig ( 2, itrig )
          itrig = itrig + itt
          cr4 = trig ( 1, itrig )
          ci4 = trig ( 2, itrig )
          nin1 = ia - after
          nout1 = ia - atn
          DO ib = 1, before
            nin1 = nin1 + after
            nin2 = nin1 + atb
            nin3 = nin2 + atb
            nin4 = nin3 + atb
            nout1 = nout1 + atn
            nout2 = nout1 + after
            nout3 = nout2 + after
            nout4 = nout3 + after
            DO j = 1, nfft
              r1 = zin ( 1, j, nin1 )
              s1 = zin ( 2, j, nin1 )
              r = zin ( 1, j, nin2 )
              s = zin ( 2, j, nin2 )
              r2 = r * cr2 - s * ci2
              s2 = r * ci2 + s * cr2
              r = zin ( 1, j, nin3 )
              s = zin ( 2, j, nin3 )
              r3 = r * cr3 - s * ci3
              s3 = r * ci3 + s * cr3
              r = zin ( 1, j, nin4 )
              s = zin ( 2, j, nin4 )
              r4 = r * cr4 - s * ci4
              s4 = r * ci4 + s * cr4
              r = r1 + r3
              s = r2 + r4
              zout ( 1, nout1, j ) = r + s
              zout ( 1, nout3, j ) = r - s
              r = r1 - r3
              s = s2 - s4
              zout ( 1, nout2, j ) = r - s
              zout ( 1, nout4, j ) = r + s
              r = s1 + s3
              s = s2 + s4
              zout ( 2, nout1, j ) = r + s
              zout ( 2, nout3, j ) = r - s
              r = s1 - s3
              s = r2 - r4
              zout ( 2, nout2, j ) = r + s
              zout ( 2, nout4, j ) = r - s
            END DO
          END DO
        END IF
      END DO
    ELSE
      ia = 1
      nin1 = ia - after
      nout1 = ia - atn
      DO ib = 1, before
        nin1 = nin1 + after
        nin2 = nin1 + atb
        nin3 = nin2 + atb
        nin4 = nin3 + atb
        nout1 = nout1 + atn
        nout2 = nout1 + after
        nout3 = nout2 + after
        nout4 = nout3 + after
        DO j = 1, nfft
          r1 = zin ( 1, j, nin1 )
          s1 = zin ( 2, j, nin1 )
          r2 = zin ( 1, j, nin2 )
          s2 = zin ( 2, j, nin2 )
          r3 = zin ( 1, j, nin3 )
          s3 = zin ( 2, j, nin3 )
          r4 = zin ( 1, j, nin4 )
          s4 = zin ( 2, j, nin4 )
          r = r1 + r3
          s = r2 + r4
          zout ( 1, nout1, j ) = r + s
          zout ( 1, nout3, j ) = r - s
          r = r1 - r3
          s = s2 - s4
          zout ( 1, nout2, j ) = r + s
          zout ( 1, nout4, j ) = r - s
          r = s1 + s3
          s = s2 + s4
          zout ( 2, nout1, j ) = r + s
          zout ( 2, nout3, j ) = r - s
          r = s1 - s3
          s = r2 - r4
          zout ( 2, nout2, j ) = r - s
          zout ( 2, nout4, j ) = r + s
        END DO
      END DO
      DO ia = 2, after
        ias = ia - 1
        IF ( 2 * ias == after ) THEN
          nin1 = ia - after
          nout1 = ia - atn
          DO ib = 1, before
            nin1 = nin1 + after
            nin2 = nin1 + atb
            nin3 = nin2 + atb
            nin4 = nin3 + atb
            nout1 = nout1 + atn
            nout2 = nout1 + after
            nout3 = nout2 + after
            nout4 = nout3 + after
            DO j = 1, nfft
              r1 = zin ( 1, j, nin1 )
              s1 = zin ( 2, j, nin1 )
              r = zin ( 1, j, nin2 )
              s = zin ( 2, j, nin2 )
              r2 = ( r + s ) * rt2i
              s2 = ( s - r ) * rt2i
              r3 = zin ( 2, j, nin3 )
              s3 = - zin ( 1, j, nin3 )
              r = zin ( 1, j, nin4 )
              s = zin ( 2, j, nin4 )
              r4 = ( s - r ) * rt2i
              s4 = - ( r + s ) * rt2i
              r = r1 + r3
              s = r2 + r4
              zout ( 1, nout1, j ) = r + s
              zout ( 1, nout3, j ) = r - s
              r = r1 - r3
              s = s2 - s4
              zout ( 1, nout2, j ) = r + s
              zout ( 1, nout4, j ) = r - s
              r =s1 + s3
              s =s2 + s4
              zout ( 2, nout1, j ) = r + s
              zout ( 2, nout3, j ) = r - s
              r = s1 - s3
              s = r2 - r4
              zout ( 2, nout2, j ) = r - s
              zout ( 2, nout4, j ) = r + s
            END DO
          END DO
        ELSE
          itt = ias * before
          itrig = itt + 1
          cr2 = trig ( 1, itrig )
          ci2 = trig ( 2, itrig )
          itrig = itrig + itt
          cr3 = trig ( 1, itrig )
          ci3 = trig ( 2, itrig )
          itrig = itrig + itt
          cr4 = trig ( 1, itrig )
          ci4 = trig ( 2, itrig )
          nin1 = ia - after
          nout1 = ia - atn
          DO ib = 1, before
            nin1 = nin1 + after
            nin2 = nin1 + atb
            nin3 = nin2 + atb
            nin4 = nin3 + atb
            nout1 = nout1 + atn
            nout2 = nout1 + after
            nout3 = nout2 + after
            nout4 = nout3 + after
            DO j = 1, nfft
              r1 = zin ( 1, j, nin1 )
              s1 = zin ( 2, j, nin1 )
              r = zin ( 1, j, nin2 )
              s = zin ( 2, j, nin2 )
              r2 = r * cr2 - s * ci2
              s2 = r * ci2 + s * cr2
              r = zin ( 1, j, nin3 )
              s = zin ( 2, j, nin3 )
              r3 = r * cr3 - s * ci3
              s3 = r * ci3 + s * cr3
              r = zin ( 1, j, nin4 )
              s = zin ( 2, j, nin4 )
              r4 = r * cr4 - s * ci4
              s4 = r * ci4 + s * cr4
              r = r1 + r3
              s = r2 + r4
              zout ( 1, nout1, j ) = r + s
              zout ( 1, nout3, j ) = r - s
              r = r1 - r3
              s = s2 - s4
              zout ( 1, nout2, j ) = r + s
              zout ( 1, nout4, j ) = r - s
              r = s1 + s3
              s = s2 + s4
              zout ( 2, nout1, j ) = r + s
              zout ( 2, nout3, j ) = r - s
              r = s1 - s3
              s = r2 - r4
              zout ( 2, nout2, j ) = r - s
              zout ( 2, nout4, j ) = r + s
            END DO
          END DO
        END IF
      END DO
    END IF
  ELSE IF ( now == 8 ) THEN
    IF ( isign == -1 ) THEN
      ia = 1
      nin1 = ia - after
      nout1 = ia - atn
      DO ib = 1, before
        nin1 = nin1 + after
        nin2 = nin1 + atb
        nin3 = nin2 + atb
        nin4 = nin3 + atb
        nin5 = nin4 + atb
        nin6 = nin5 + atb
        nin7 = nin6 + atb
        nin8 = nin7 + atb
        nout1 = nout1 + atn
        nout2 = nout1 + after
        nout3 = nout2 + after
        nout4 = nout3 + after
        nout5 = nout4 + after
        nout6 = nout5 + after
        nout7 = nout6 + after
        nout8 = nout7 + after
        DO j = 1, nfft
          r1 = zin ( 1, j, nin1 )
          s1 = zin ( 2, j, nin1 )
          r2 = zin ( 1, j, nin2 )
          s2 = zin ( 2, j, nin2 )
          r3 = zin ( 1, j, nin3 )
          s3 = zin ( 2, j, nin3 )
          r4 = zin ( 1, j, nin4 )
          s4 = zin ( 2, j, nin4 )
          r5 = zin ( 1, j, nin5 )
          s5 = zin ( 2, j, nin5 )
          r6 = zin ( 1, j, nin6 )
          s6 = zin ( 2, j, nin6 )
          r7 = zin ( 1, j, nin7 )
          s7 = zin ( 2, j, nin7 )
          r8 = zin ( 1, j, nin8 )
          s8 = zin ( 2, j, nin8 )
          r = r1 + r5
          s = r3 + r7
          ap = r + s
          am = r - s
          r = r2 + r6
          s = r4 + r8
          bp = r + s
          bm = r - s
          r = s1 + s5
          s = s3 + s7
          cp = r + s
          cm = r - s
          r = s2 + s6
          s = s4 + s8
          dp = r + s
          dm = r - s
          zout ( 1, nout1, j ) = ap + bp
          zout ( 2, nout1, j ) = cp + dp
          zout ( 1, nout5, j ) = ap - bp
          zout ( 2, nout5, j ) = cp - dp
          zout ( 1, nout3, j ) = am + dm
          zout ( 2, nout3, j ) = cm - bm
          zout ( 1, nout7, j ) = am - dm
          zout ( 2, nout7, j ) = cm + bm
          r = r1 - r5
          s = s3 - s7
          ap = r + s
          am = r - s
          r = s1 - s5
          s = r3 - r7
          bp = r + s
          bm = r - s
          r = s4 - s8
          s = r2 - r6
          cp = r + s
          cm = r - s
          r = s2 - s6
          s = r4 - r8
          dp = r + s
          dm = r - s
          r = ( cp + dm ) * rt2i
          s = (-cp + dm ) * rt2i
          cp = ( cm + dp ) * rt2i
          dp = ( cm - dp ) * rt2i
          zout ( 1, nout2, j ) = ap + r
          zout ( 2, nout2, j ) = bm + s
          zout ( 1, nout6, j ) = ap - r
          zout ( 2, nout6, j ) = bm - s
          zout ( 1, nout4, j ) = am + cp
          zout ( 2, nout4, j ) = bp + dp
          zout ( 1, nout8, j ) = am - cp
          zout ( 2, nout8, j ) = bp - dp
        END DO
      END DO
    ELSE
      ia = 1
      nin1 = ia - after
      nout1 = ia - atn
      DO ib = 1, before
        nin1 = nin1 + after
        nin2 = nin1 + atb
        nin3 = nin2 + atb
        nin4 = nin3 + atb
        nin5 = nin4 + atb
        nin6 = nin5 + atb
        nin7 = nin6 + atb
        nin8 = nin7 + atb
        nout1 = nout1 + atn
        nout2 = nout1 + after
        nout3 = nout2 + after
        nout4 = nout3 + after
        nout5 = nout4 + after
        nout6 = nout5 + after
        nout7 = nout6 + after
        nout8 = nout7 + after
        DO j = 1, nfft
          r1 = zin ( 1, j, nin1 )
          s1 = zin ( 2, j, nin1 )
          r2 = zin ( 1, j, nin2 )
          s2 = zin ( 2, j, nin2 )
          r3 = zin ( 1, j, nin3 )
          s3 = zin ( 2, j, nin3 )
          r4 = zin ( 1, j, nin4 )
          s4 = zin ( 2, j, nin4 )
          r5 = zin ( 1, j, nin5 )
          s5 = zin ( 2, j, nin5 )
          r6 = zin ( 1, j, nin6 )
          s6 = zin ( 2, j, nin6 )
          r7 = zin ( 1, j, nin7 )
          s7 = zin ( 2, j, nin7 )
          r8 = zin ( 1, j, nin8 )
          s8 = zin ( 2, j, nin8 )
          r = r1 + r5
          s = r3 + r7
          ap = r + s
          am = r - s
          r = r2 + r6
          s = r4 + r8
          bp = r + s
          bm = r - s
          r = s1 + s5
          s = s3 + s7
          cp = r + s
          cm = r - s
          r = s2 + s6
          s = s4 + s8
          dp = r + s
          dm = r - s
          zout ( 1, nout1, j ) = ap + bp
          zout ( 2, nout1, j ) = cp + dp
          zout ( 1, nout5, j ) = ap - bp
          zout ( 2, nout5, j ) = cp - dp
          zout ( 1, nout3, j ) = am - dm
          zout ( 2, nout3, j ) = cm + bm
          zout ( 1, nout7, j ) = am + dm
          zout ( 2, nout7, j ) = cm - bm
          r = r1 - r5
          s = -s3 + s7
          ap = r + s
          am = r - s
          r = s1 - s5
          s = r7 - r3
          bp = r + s
          bm = r - s
          r = -s4 + s8
          s = r2 - r6
          cp = r + s
          cm = r - s
          r = -s2 + s6
          s = r4 - r8
          dp = r + s
          dm = r - s
          r = ( cp + dm ) * rt2i
          s = ( cp - dm ) * rt2i
          cp = ( cm + dp ) * rt2i
          dp = (-cm + dp ) * rt2i
          zout ( 1, nout2, j ) = ap + r
          zout ( 2, nout2, j ) = bm + s
          zout ( 1, nout6, j ) = ap - r
          zout ( 2, nout6, j ) = bm - s
          zout ( 1, nout4, j ) = am + cp
          zout ( 2, nout4, j ) = bp + dp
          zout ( 1, nout8, j ) = am - cp
          zout ( 2, nout8, j ) = bp - dp
        END DO
      END DO
    END IF
  ELSE IF ( now == 3 ) THEN
    ia = 1
    nin1 = ia - after
    nout1 = ia - atn
    DO ib = 1, before
      nin1 = nin1 + after
      nin2 = nin1 + atb
      nin3 = nin2 + atb
      nout1 = nout1 + atn
      nout2 = nout1 + after
      nout3 = nout2 + after
      DO j = 1, nfft
        r1 = zin ( 1, j, nin1 )
        s1 = zin ( 2, j, nin1 )
        r2 = zin ( 1, j, nin2 )
        s2 = zin ( 2, j, nin2 )
        r3 = zin ( 1, j, nin3 )
        s3 = zin ( 2, j, nin3 )
        r = r2 + r3
        s = s2 + s3
        zout ( 1, nout1, j ) = r + r1
        zout ( 2, nout1, j ) = s + s1
        r1 = r1 - 0.5_dbl * r
        s1 = s1 - 0.5_dbl * s
        r2 = bb * ( r2 - r3 )
        s2 = bb * ( s2 - s3 )
        zout ( 1, nout2, j ) = r1 - s2
        zout ( 2, nout2, j ) = s1 + r2
        zout ( 1, nout3, j ) = r1 + s2
        zout ( 2, nout3, j ) = s1 - r2
      END DO
    END DO
    DO ia = 2, after
      ias = ia - 1
      IF ( 4*ias == 3*after ) THEN
        IF ( isign == 1 ) THEN
          nin1 = ia - after
          nout1 = ia - atn
          DO ib = 1, before
            nin1 = nin1 + after
            nin2=nin1+atb
            nin3=nin2+atb
            nout1=nout1+atn
            nout2=nout1+after
            nout3=nout2+after
            DO j = 1, nfft
              r1 = zin ( 1, j, nin1 )
              s1 = zin ( 2, j, nin1 )
              r2 = -zin ( 2, j, nin2 )
              s2 = zin ( 1, j, nin2 )
              r3 = -zin ( 1, j, nin3 )
              s3 = -zin ( 2, j, nin3 )
              r = r2 + r3
              s = s2 + s3
              zout ( 1, nout1, j ) = r + r1
              zout ( 2, nout1, j ) = s + s1
              r1 = r1 - 0.5_dbl * r
              s1 = s1 - 0.5_dbl * s
              r2 = bb*(r2-r3)
              s2 = bb*(s2-s3)
              zout ( 1, nout2, j ) = r1 - s2
              zout ( 2, nout2, j ) = s1 + r2
              zout ( 1, nout3, j ) = r1 + s2
              zout ( 2, nout3, j ) = s1 - r2
            END DO
          END DO
        ELSE
          nin1 = ia - after
          nout1 = ia - atn
          DO ib = 1, before
            nin1 = nin1 + after
            nin2 = nin1 + atb
            nin3 = nin2 + atb
            nout1 = nout1 + atn
            nout2 = nout1 + after
            nout3 = nout2 + after
            DO j = 1, nfft
              r1 = zin ( 1, j, nin1 )
              s1 = zin ( 2, j, nin1 )
              r2 = zin ( 2, j, nin2 )
              s2 = -zin ( 1, j, nin2 )
              r3 = -zin ( 1, j, nin3 )
              s3 = -zin ( 2, j, nin3 )
              r = r2 + r3
              s = s2 + s3
              zout ( 1, nout1, j ) = r + r1
              zout ( 2, nout1, j ) = s + s1
              r1 = r1 - 0.5_dbl * r
              s1 = s1 - 0.5_dbl * s
              r2 = bb * ( r2 - r3 )
              s2 = bb * ( s2 - s3 )
              zout ( 1, nout2, j ) = r1 - s2
              zout ( 2, nout2, j ) = s1 + r2
              zout ( 1, nout3, j ) = r1 + s2
              zout ( 2, nout3, j ) = s1 - r2
            END DO
          END DO
        END IF
      ELSE IF ( 8 * ias == 3 * after ) THEN
        IF ( isign == 1 ) THEN
          nin1 = ia - after
          nout1 = ia - atn
          DO ib = 1, before
            nin1 = nin1 + after
            nin2 = nin1 + atb
            nin3 = nin2 + atb
            nout1 = nout1 + atn
            nout2 = nout1 + after
            nout3 = nout2 + after
            DO j = 1, nfft
              r1 = zin ( 1, j, nin1 )
              s1 = zin ( 2, j, nin1 )
              r = zin ( 1, j, nin2 )
              s = zin ( 2, j, nin2 )
              r2 = ( r - s ) * rt2i
              s2 = ( r + s ) * rt2i
              r3 = -zin ( 2, j, nin3 )
              s3 = zin ( 1, j, nin3 )
              r = r2 + r3
              s = s2 + s3
              zout ( 1, nout1, j ) = r + r1
              zout ( 2, nout1, j ) = s + s1
              r1 = r1 - 0.5_dbl * r
              s1 = s1 - 0.5_dbl * s
              r2 = bb * ( r2 - r3 )
              s2 = bb * ( s2 - s3 )
              zout ( 1, nout2, j ) = r1 - s2
              zout ( 2, nout2, j ) = s1 + r2
              zout ( 1, nout3, j ) = r1 + s2
              zout ( 2, nout3, j ) = s1 - r2
            END DO
          END DO
        ELSE
          nin1 = ia - after
          nout1 = ia - atn
          DO ib = 1, before
            nin1 = nin1 + after
            nin2 = nin1 + atb
            nin3 = nin2 + atb
            nout1 = nout1 + atn
            nout2 = nout1 + after
            nout3 = nout2 + after
            DO j = 1, nfft
              r1 = zin ( 1, j, nin1 )
              s1 = zin ( 2, j, nin1 )
              r = zin ( 1, j, nin2 )
              s = zin ( 2, j, nin2 )
              r2 = ( r + s ) * rt2i
              s2 = ( -r + s ) * rt2i
              r3 = zin ( 2, j, nin3 )
              s3 = -zin ( 1, j, nin3 )
              r = r2 + r3
              s = s2 + s3
              zout ( 1, nout1, j ) = r + r1
              zout ( 2, nout1, j ) = s + s1
              r1 = r1 - 0.5_dbl * r
              s1 = s1 - 0.5_dbl * s
              r2 = bb * ( r2 - r3 )
              s2 = bb * ( s2 - s3 )
              zout ( 1, nout2, j ) = r1 - s2
              zout ( 2, nout2, j ) = s1 + r2
              zout ( 1, nout3, j ) = r1 + s2
              zout ( 2, nout3, j ) = s1 - r2
            END DO
          END DO
        END IF
      ELSE
        itt = ias * before
        itrig = itt + 1
        cr2 = trig ( 1, itrig )
        ci2 = trig ( 2, itrig )
        itrig = itrig + itt
        cr3 = trig ( 1, itrig )
        ci3 = trig ( 2, itrig )
        nin1 = ia - after
        nout1 = ia - atn
        DO ib = 1, before
          nin1 = nin1 + after
          nin2 = nin1 + atb
          nin3 = nin2 + atb
          nout1 = nout1 + atn
          nout2 = nout1 + after
          nout3 = nout2 + after
          DO j = 1, nfft
            r1 = zin ( 1, j, nin1 )
            s1 = zin ( 2, j, nin1 )
            r = zin ( 1, j, nin2 )
            s = zin ( 2, j, nin2 )
            r2 = r * cr2 - s * ci2
            s2 = r * ci2 + s * cr2
            r = zin ( 1, j, nin3 )
            s = zin ( 2, j, nin3 )
            r3 = r * cr3 - s * ci3
            s3 = r * ci3 + s * cr3
            r = r2 + r3
            s = s2 + s3
            zout ( 1, nout1, j ) = r + r1
            zout ( 2, nout1, j ) = s + s1
            r1 = r1 - 0.5_dbl * r
            s1 = s1 - 0.5_dbl * s
            r2 = bb * ( r2 - r3 )
            s2 = bb * ( s2 - s3 )
            zout ( 1, nout2, j ) = r1 - s2
            zout ( 2, nout2, j ) = s1 + r2
            zout ( 1, nout3, j ) = r1 + s2
            zout ( 2, nout3, j ) = s1 - r2
          END DO
        END DO
      END IF
    END DO
  ELSE IF ( now == 5 ) THEN
    sin2 = isign * sin2p
    sin4 = isign * sin4p
    ia = 1
    nin1 = ia - after
    nout1 = ia - atn
    DO ib = 1, before
      nin1 = nin1 + after
      nin2 = nin1 + atb
      nin3 = nin2 + atb
      nin4 = nin3 + atb
      nin5 = nin4 + atb
      nout1 = nout1 + atn
      nout2 = nout1 + after
      nout3 = nout2 + after
      nout4 = nout3 + after
      nout5 = nout4 + after
      DO j = 1, nfft
        r1 = zin ( 1, j, nin1 )
        s1 = zin ( 2, j, nin1 )
        r2 = zin ( 1, j, nin2 )
        s2 = zin ( 2, j, nin2 )
        r3 = zin ( 1, j, nin3 )
        s3 = zin ( 2, j, nin3 )
        r4 = zin ( 1, j, nin4 )
        s4 = zin ( 2, j, nin4 )
        r5 = zin ( 1, j, nin5 )
        s5 = zin ( 2, j, nin5 )
        r25 = r2 + r5
        r34 = r3 + r4
        s25 = s2 - s5
        s34 = s3 - s4
        zout ( 1, nout1, j ) = r1 + r25 + r34
        r = cos2 * r25 + cos4 * r34 + r1
        s = sin2 * s25 + sin4 * s34
        zout ( 1, nout2, j ) = r - s
        zout ( 1, nout5, j ) = r + s
        r = cos4 * r25 + cos2 * r34 + r1
        s = sin4 * s25 - sin2 * s34
        zout ( 1, nout3, j ) = r - s
        zout ( 1, nout4, j ) = r + s
        r25 = r2 - r5
        r34 = r3 - r4
        s25 = s2 + s5
        s34 = s3 + s4
        zout ( 2, nout1, j ) = s1 + s25 + s34
        r = cos2 * s25 + cos4 * s34 + s1
        s = sin2 * r25 + sin4 * r34
        zout ( 2, nout2, j ) = r + s
        zout ( 2, nout5, j ) = r - s
        r = cos4 * s25 + cos2 * s34 + s1
        s = sin4 * r25 - sin2 * r34
        zout ( 2, nout3, j ) = r + s
        zout ( 2, nout4, j ) = r - s
      END DO
    END DO
    DO ia = 2, after
      ias = ia - 1
      IF ( 8 * ias == 5 * after ) THEN
        IF ( isign == 1 ) THEN
          nin1 = ia - after
          nout1 = ia - atn
          DO ib = 1, before
            nin1 = nin1 + after
            nin2 = nin1 + atb
            nin3 = nin2 + atb
            nin4 = nin3 + atb
            nin5 = nin4 + atb
            nout1 = nout1 + atn
            nout2 = nout1 + after
            nout3 = nout2 + after
            nout4 = nout3 + after
            nout5 = nout4 + after
            DO j = 1, nfft
              r1 = zin ( 1, j, nin1 )
              s1 = zin ( 2, j, nin1 )
              r = zin ( 1, j, nin2 )
              s = zin ( 2, j, nin2 )
              r2 = ( r - s ) * rt2i
              s2 = ( r + s ) * rt2i
              r3 = -zin ( 2, j, nin3 )
              s3 = zin ( 1, j, nin3 )
              r = zin ( 1, j, nin4 )
              s = zin ( 2, j, nin4 )
              r4 = -( r + s ) * rt2i
              s4 = ( r - s ) * rt2i
              r5 = -zin ( 1, j, nin5 )
              s5 = -zin ( 2, j, nin5 )
              r25 = r2 + r5
              r34 = r3 + r4
              s25 = s2 - s5
              s34 = s3 - s4
              zout ( 1, nout1, j ) = r1 + r25 + r34
              r = cos2 * r25 + cos4 * r34 + r1
              s = sin2 * s25 + sin4 * s34
              zout ( 1, nout2, j ) = r - s
              zout ( 1, nout5, j ) = r + s
              r = cos4 * r25 + cos2 * r34 + r1
              s = sin4 * s25 - sin2 * s34
              zout ( 1, nout3, j ) = r - s
              zout ( 1, nout4, j ) = r + s
              r25 = r2 - r5
              r34 = r3 - r4
              s25 = s2 + s5
              s34 = s3 + s4
              zout ( 2, nout1, j ) = s1 + s25 + s34
              r = cos2 * s25 + cos4 * s34 + s1
              s = sin2 * r25 + sin4 * r34
              zout ( 2, nout2, j ) = r + s
              zout ( 2, nout5, j ) = r - s
              r = cos4 * s25 + cos2 * s34 + s1
              s = sin4 * r25 - sin2 * r34
              zout ( 2, nout3, j ) = r + s
              zout ( 2, nout4, j ) = r - s
            END DO
          END DO
        ELSE
          nin1 = ia - after
          nout1 = ia - atn
          DO ib = 1, before
            nin1 = nin1 + after
            nin2 = nin1 + atb
            nin3 = nin2 + atb
            nin4 = nin3 + atb
            nin5 = nin4 + atb
            nout1 = nout1 + atn
            nout2 = nout1 + after
            nout3 = nout2 + after
            nout4 = nout3 + after
            nout5 = nout4 + after
            DO j = 1, nfft
              r1 = zin ( 1, j, nin1 )
              s1 = zin ( 2, j, nin1 )
              r = zin ( 1, j, nin2 )
              s = zin ( 2, j, nin2 )
              r2 = ( r + s ) * rt2i
              s2 = ( -r + s ) * rt2i
              r3 = zin ( 2, j, nin3 )
              s3 = -zin ( 1, j, nin3 )
              r = zin ( 1, j, nin4 )
              s = zin ( 2, j, nin4 )
              r4 = ( s - r ) * rt2i
              s4 = - ( r + s ) * rt2i
              r5 = -zin ( 1, j, nin5 )
              s5 = -zin ( 2, j, nin5 )
              r25 = r2 + r5
              r34 = r3 + r4
              s25 = s2 - s5
              s34 = s3 - s4
              zout ( 1, nout1, j ) = r1 + r25 + r34
              r = cos2 * r25 + cos4 * r34 + r1
              s = sin2 * s25 + sin4 * s34
              zout ( 1, nout2, j ) = r - s
              zout ( 1, nout5, j ) = r + s
              r = cos4 * r25 + cos2 * r34 + r1
              s = sin4 * s25 - sin2 * s34
              zout ( 1, nout3, j ) = r - s
              zout ( 1, nout4, j ) = r + s
              r25 = r2 - r5
              r34 = r3 - r4
              s25 = s2 + s5
              s34 = s3 + s4
              zout ( 2, nout1, j ) = s1 + s25 + s34
              r = cos2 * s25 + cos4 * s34 + s1
              s = sin2 * r25 + sin4 * r34
              zout ( 2, nout2, j ) = r + s
              zout ( 2, nout5, j ) = r - s
              r = cos4 * s25 + cos2 * s34 + s1
              s = sin4 * r25 - sin2 * r34
              zout ( 2, nout3, j ) = r + s
              zout ( 2, nout4, j ) = r - s
            END DO
          END DO
        END IF
      ELSE
        ias = ia - 1
        itt = ias * before
        itrig = itt + 1
        cr2 = trig ( 1, itrig )
        ci2 = trig ( 2, itrig )
        itrig = itrig + itt
        cr3 = trig ( 1, itrig )
        ci3 = trig ( 2, itrig )
        itrig = itrig + itt
        cr4 = trig ( 1, itrig )
        ci4 = trig ( 2, itrig )
        itrig = itrig + itt
        cr5 = trig ( 1, itrig )
        ci5 = trig ( 2, itrig )
        nin1 = ia - after
        nout1 = ia - atn
        DO ib = 1, before
          nin1 = nin1 + after
          nin2 = nin1 + atb
          nin3 = nin2 + atb
          nin4 = nin3 + atb
          nin5 = nin4 + atb
          nout1 = nout1 + atn
          nout2 = nout1 + after
          nout3 = nout2 + after
          nout4 = nout3 + after
          nout5 = nout4 + after
          DO j = 1, nfft
            r1 = zin ( 1, j, nin1 )
            s1 = zin ( 2, j, nin1 )
            r = zin ( 1, j, nin2 )
            s = zin ( 2, j, nin2 )
            r2 = r * cr2 - s * ci2
            s2 = r * ci2 + s * cr2
            r = zin ( 1, j, nin3 )
            s = zin ( 2, j, nin3 )
            r3 = r * cr3 - s * ci3
            s3 = r * ci3 + s * cr3
            r = zin ( 1, j, nin4 )
            s = zin ( 2, j, nin4 )
            r4 = r * cr4 - s * ci4
            s4 = r * ci4 + s * cr4
            r = zin ( 1, j, nin5 )
            s = zin ( 2, j, nin5 )
            r5 = r * cr5 - s * ci5
            s5 = r * ci5 + s * cr5
            r25 = r2 + r5
            r34 = r3 + r4
            s25 = s2 - s5
            s34 = s3 - s4
            zout ( 1, nout1, j ) = r1 + r25 + r34
            r = cos2 * r25 + cos4 * r34 + r1
            s = sin2 * s25 + sin4 * s34
            zout ( 1, nout2, j ) = r - s
            zout ( 1, nout5, j ) = r + s
            r = cos4 * r25 + cos2 * r34 + r1
            s = sin4 * s25 - sin2 * s34
            zout ( 1, nout3, j ) = r - s
            zout ( 1, nout4, j ) = r + s
            r25 = r2 - r5
            r34 = r3 - r4
            s25 = s2 + s5
            s34 = s3 + s4
            zout ( 2, nout1, j ) = s1 + s25 + s34
            r = cos2 * s25 + cos4 * s34 + s1
            s = sin2 * r25 + sin4 * r34
            zout ( 2, nout2, j ) = r + s
            zout ( 2, nout5, j ) = r - s
            r = cos4 * s25 + cos2 * s34 + s1
            s = sin4 * r25 - sin2 * r34
            zout ( 2, nout3, j ) = r + s
            zout ( 2, nout4, j ) = r - s
          END DO
        END DO
      END IF
    END DO
  ELSE IF ( now == 6 ) THEN
    ia = 1
    nin1 = ia - after
    nout1 = ia - atn
    DO ib = 1, before
      nin1 = nin1 + after
      nin2 = nin1 + atb
      nin3 = nin2 + atb
      nin4 = nin3 + atb
      nin5 = nin4 + atb
      nin6 = nin5 + atb
      nout1 = nout1 + atn
      nout2 = nout1 + after
      nout3 = nout2 + after
      nout4 = nout3 + after
      nout5 = nout4 + after
      nout6 = nout5 + after
      DO j = 1, nfft
        r2 = zin ( 1, j, nin3 )
        s2 = zin ( 2, j, nin3 )
        r3 = zin ( 1, j, nin5 )
        s3 = zin ( 2, j, nin5 )
        r = r2 + r3
        s = s2 + s3
        r1 = zin ( 1, j, nin1 )
        s1 = zin ( 2, j, nin1 )
        ur1 = r + r1
        ui1 = s + s1
        r1 = r1 - 0.5_dbl * r
        s1 = s1 - 0.5_dbl * s
        r = r2 - r3
        s = s2 - s3
        ur2 = r1 - s * bb
        ui2 = s1 + r * bb
        ur3 = r1 + s * bb
        ui3 = s1 - r * bb

        r2 = zin ( 1, j, nin6 )
        s2 = zin ( 2, j, nin6 )
        r3 = zin ( 1, j, nin2 )
        s3 = zin ( 2, j, nin2 )
        r = r2 + r3
        s = s2 + s3
        r1 = zin ( 1, j, nin4 )
        s1 = zin ( 2, j, nin4 )
        vr1 = r + r1
        vi1 = s + s1
        r1 = r1 - 0.5_dbl * r
        s1 = s1 - 0.5_dbl * s
        r = r2 - r3
        s = s2 - s3
        vr2 = r1 - s * bb
        vi2 = s1 + r * bb
        vr3 = r1 + s * bb
        vi3 = s1 - r * bb

        zout ( 1, nout1, j ) = ur1 + vr1
        zout ( 2, nout1, j ) = ui1 + vi1
        zout ( 1, nout5, j ) = ur2 + vr2
        zout ( 2, nout5, j ) = ui2 + vi2
        zout ( 1, nout3, j ) = ur3 + vr3
        zout ( 2, nout3, j ) = ui3 + vi3
        zout ( 1, nout4, j ) = ur1 - vr1
        zout ( 2, nout4, j ) = ui1 - vi1
        zout ( 1, nout2, j ) = ur2 - vr2
        zout ( 2, nout2, j ) = ui2 - vi2
        zout ( 1, nout6, j ) = ur3 - vr3
        zout ( 2, nout6, j ) = ui3 - vi3
      END DO
    END DO
  ELSE
    STOP 'Error fftrot'
  END If

!-----------------------------------------------------------------------------!

END SUBROUTINE fftrot

!-----------------------------------------------------------------------------!
!-----------------------------------------------------------------------------!
!Copyright by Stefan Goedecker, Cornell, Ithaca, USA, March 25, 1994
!modified by Stefan Goedecker, Stuttgart, Germany, October 15, 1995
!Commercial use is prohibited without the explicit permission of the author.
!-----------------------------------------------------------------------------!

SUBROUTINE fftstp ( mm, nfft, m, nn, n, zin, zout, &
                    trig, now, after, before, isign )

  IMPLICIT NONE

  INTEGER, PARAMETER :: dbl = SELECTED_REAL_KIND ( 14, 200 )

! Arguments
  INTEGER, INTENT ( IN ) :: mm, nfft, m, nn, n, now, after, before, isign
  REAL ( dbl ), DIMENSION ( 2, 1024 ), INTENT ( IN ) :: trig
  REAL ( dbl ), DIMENSION ( 2, mm, m ), INTENT ( IN ) :: zin
  REAL ( dbl ), DIMENSION ( 2, nn, n ), INTENT ( OUT ) :: zout

  INTEGER :: atn, atb, ia, ib, nin1, nin2, nin3, nin4, nin5, nin6, nin7, nin8
  INTEGER :: nout1, nout2, nout3, nout4, nout5, nout6, nout7, nout8, &
             i, j, ias, itt, itrig
  REAL ( dbl ) :: s, s1, s2, s3, s4, s5, s6, s7, s8, &
                  r, r1, r2, r3, r4, r5, r6, r7, r8, cr2, cr3, cr4, cr5, &
                  ci2, ci3, ci4, ci5, ur1, ur2, ur3, ui1, ui2, ui3, &
                  vr1, vr2, vr3, vi1, vi2, vi3, cm, cp, dm, dp, &
                  am, ap, bm, bp,s25, s34, r34, r25, sin2, sin4
  REAL ( dbl ), PARAMETER :: rt2i = 0.7071067811865475_dbl  ! sqrt(0.5)
  REAL ( dbl ), PARAMETER :: bb = 0.8660254037844387_dbl  ! sqrt(3)/2
  REAL ( dbl ), PARAMETER :: cos2 = 0.3090169943749474_dbl ! cos(2*pi/5)
  REAL ( dbl ), PARAMETER :: cos4 = - 0.8090169943749474_dbl !  cos(4*pi/5)
  REAL ( dbl ), PARAMETER :: sin2p = 0.9510565162951536_dbl ! sin(2*pi/5)
  REAL ( dbl ), PARAMETER :: sin4p = 0.5877852522924731_dbl ! sin(4*pi/5)

!-----------------------------------------------------------------------------!

  atn = after * now
  atb = after * before

  IF ( now == 4 ) THEN
    IF ( isign == 1 ) THEN
      ia = 1
      nin1 = ia - after
      nout1 = ia - atn
      DO ib = 1, before
        nin1 = nin1 + after
        nin2 = nin1 + atb
        nin3 = nin2 + atb
        nin4 = nin3 + atb
        nout1 = nout1 + atn
        nout2 = nout1 + after
        nout3 = nout2 + after
        nout4 = nout3 + after
        DO j = 1, nfft
          r1 = zin ( 1, j, nin1 )
          s1 = zin ( 2, j, nin1 )
          r2 = zin ( 1, j, nin2 )
          s2 = zin ( 2, j, nin2 )
          r3 = zin ( 1, j, nin3 )
          s3 = zin ( 2, j, nin3 )
          r4 = zin ( 1, j, nin4 )
          s4 = zin ( 2, j, nin4 )
          r = r1 + r3
          s = r2 + r4
          zout ( 1, j, nout1 ) = r + s
          zout ( 1, j, nout3 ) = r - s
          r = r1 - r3
          s = s2 - s4
          zout ( 1, j, nout2 ) = r - s
          zout ( 1, j, nout4 ) = r + s
          r = s1 + s3
          s = s2 + s4
          zout ( 2, j, nout1 ) = r + s
          zout ( 2, j, nout3 ) = r - s
          r = s1 - s3
          s = r2 - r4
          zout ( 2, j, nout2 ) = r + s
          zout ( 2, j, nout4 ) = r - s
        END DO
      END DO
      DO ia = 2, after
        ias = ia - 1
        IF ( 2*ias == after ) THEN
          nin1 = ia - after
          nout1 = ia - atn
          DO ib = 1, before
            nin1 = nin1 + after
            nin2 = nin1 + atb
            nin3 = nin2 + atb
            nin4 = nin3 + atb
            nout1 = nout1 + atn
            nout2 = nout1 + after
            nout3 = nout2 + after
            nout4 = nout3 + after
            DO j = 1, nfft
              r1 = zin ( 1, j, nin1 )
              s1 = zin ( 2, j, nin1 )
              r = zin ( 1, j, nin2 )
              s = zin ( 2, j, nin2 )
              r2 = ( r - s ) * rt2i
              s2 = ( r + s ) * rt2i
              r3 = - zin ( 2, j, nin3 )
              s3 = zin ( 1, j, nin3 )
              r = zin ( 1, j, nin4 )
              s = zin ( 2, j, nin4 )
              r4 = - ( r + s ) * rt2i
              s4 = ( r -  s ) * rt2i
              r = r1 + r3
              s = r2 + r4
              zout ( 1, j, nout1 ) = r + s
              zout ( 1, j, nout3 ) = r - s
              r = r1 - r3
              s = s2 - s4
              zout ( 1, j, nout2 ) = r - s
              zout ( 1, j, nout4 ) = r + s
              r = s1 + s3
              s = s2 + s4
              zout ( 2, j, nout1 ) = r + s
              zout ( 2, j, nout3 ) = r - s
              r = s1 - s3
              s = r2 - r4
              zout ( 2, j, nout2 ) = r + s
              zout ( 2, j, nout4 ) = r - s
            END DO
          END DO
        ELSE
          itt = ias * before
          itrig = itt + 1
          cr2 = trig ( 1, itrig )
          ci2 = trig ( 2, itrig )
          itrig = itrig + itt
          cr3 = trig ( 1, itrig )
          ci3 = trig ( 2, itrig )
          itrig = itrig + itt
          cr4 = trig ( 1, itrig )
          ci4 = trig ( 2, itrig )
          nin1 = ia - after
          nout1 = ia - atn
          DO ib = 1, before
            nin1 = nin1 + after
            nin2 = nin1 + atb
            nin3 = nin2 + atb
            nin4 = nin3 + atb
            nout1 = nout1 + atn
            nout2 = nout1 + after
            nout3 = nout2 + after
            nout4 = nout3 + after
            DO j = 1, nfft
              r1 = zin ( 1, j, nin1 )
              s1 = zin ( 2, j, nin1 )
              r = zin ( 1, j, nin2 )
              s = zin ( 2, j, nin2 )
              r2 = r * cr2 - s * ci2
              s2 = r * ci2 + s * cr2
              r = zin ( 1, j, nin3 )
              s = zin ( 2, j, nin3 )
              r3 = r * cr3 - s * ci3
              s3 = r * ci3 + s * cr3
              r = zin ( 1, j, nin4 )
              s = zin ( 2, j, nin4 )
              r4 = r * cr4 - s * ci4
              s4 = r * ci4 + s * cr4
              r = r1 + r3
              s = r2 + r4
              zout ( 1, j, nout1 ) = r + s
              zout ( 1, j, nout3 ) = r - s
              r = r1 - r3
              s = s2 - s4
              zout ( 1, j, nout2 ) = r - s
              zout ( 1, j, nout4 ) = r + s
              r = s1 + s3
              s = s2 + s4
              zout ( 2, j, nout1 ) = r + s
              zout ( 2, j, nout3 ) = r - s
              r = s1 - s3
              s = r2 - r4
              zout ( 2, j, nout2 ) = r + s
              zout ( 2, j, nout4 ) = r - s
            END DO
          END DO
        END IF
      END DO
    ELSE
      ia = 1
      nin1 = ia - after
      nout1 = ia - atn
      DO ib = 1, before
        nin1 = nin1 + after
        nin2 = nin1 + atb
        nin3 = nin2 + atb
        nin4 = nin3 + atb
        nout1 = nout1 + atn
        nout2 = nout1 + after
        nout3 = nout2 + after
        nout4 = nout3 + after
        DO j = 1, nfft
          r1 = zin ( 1, j, nin1 )
          s1 = zin ( 2, j, nin1 )
          r2 = zin ( 1, j, nin2 )
          s2 = zin ( 2, j, nin2 )
          r3 = zin ( 1, j, nin3 )
          s3 = zin ( 2, j, nin3 )
          r4 = zin ( 1, j, nin4 )
          s4 = zin ( 2, j, nin4 )
          r = r1 + r3
          s = r2 + r4
          zout ( 1, j, nout1 ) = r + s
          zout ( 1, j, nout3 ) = r - s
          r = r1 - r3
          s = s2 - s4
          zout ( 1, j, nout2 ) = r + s
          zout ( 1, j, nout4 ) = r - s
          r = s1 + s3
          s = s2 + s4
          zout ( 2, j, nout1 ) = r + s
          zout ( 2, j, nout3 ) = r - s
          r = s1 - s3
          s = r2 - r4
          zout ( 2, j, nout2 ) = r - s
          zout ( 2, j, nout4 ) = r + s
        END DO
      END DO
      DO ia = 2, after
        ias = ia - 1
        IF ( 2 * ias == after ) THEN
          nin1 = ia - after
          nout1 = ia - atn
          DO ib = 1, before
            nin1 = nin1 + after
            nin2 = nin1 + atb
            nin3 = nin2 + atb
            nin4 = nin3 + atb
            nout1 = nout1 + atn
            nout2 = nout1 + after
            nout3 = nout2 + after
            nout4 = nout3 + after
            DO j = 1, nfft
              r1 = zin ( 1, j, nin1 )
              s1 = zin ( 2, j, nin1 )
              r = zin ( 1, j, nin2 )
              s = zin ( 2, j, nin2 )
              r2 = ( r + s ) * rt2i
              s2 = ( s - r ) * rt2i
              r3 = zin ( 2, j, nin3 )
              s3 = - zin ( 1, j, nin3 )
              r = zin ( 1, j, nin4 )
              s = zin ( 2, j, nin4 )
              r4 = ( s - r ) * rt2i
              s4 = - ( r + s ) * rt2i
              r = r1 + r3
              s = r2 + r4
              zout ( 1, j, nout1 ) = r + s
              zout ( 1, j, nout3 ) = r - s
              r = r1 - r3
              s = s2 - s4
              zout ( 1, j, nout2 ) = r + s
              zout ( 1, j, nout4 ) = r - s
              r =s1 + s3
              s =s2 + s4
              zout ( 2, j, nout1 ) = r + s
              zout ( 2, j, nout3 ) = r - s
              r = s1 - s3
              s = r2 - r4
              zout ( 2, j, nout2 ) = r - s
              zout ( 2, j, nout4 ) = r + s
            END DO
          END DO
        ELSE
          itt = ias * before
          itrig = itt + 1
          cr2 = trig ( 1, itrig )
          ci2 = trig ( 2, itrig )
          itrig = itrig + itt
          cr3 = trig ( 1, itrig )
          ci3 = trig ( 2, itrig )
          itrig = itrig + itt
          cr4 = trig ( 1, itrig )
          ci4 = trig ( 2, itrig )
          nin1 = ia - after
          nout1 = ia - atn
          DO ib = 1, before
            nin1 = nin1 + after
            nin2 = nin1 + atb
            nin3 = nin2 + atb
            nin4 = nin3 + atb
            nout1 = nout1 + atn
            nout2 = nout1 + after
            nout3 = nout2 + after
            nout4 = nout3 + after
            DO j = 1, nfft
              r1 = zin ( 1, j, nin1 )
              s1 = zin ( 2, j, nin1 )
              r = zin ( 1, j, nin2 )
              s = zin ( 2, j, nin2 )
              r2 = r * cr2 - s * ci2
              s2 = r * ci2 + s * cr2
              r = zin ( 1, j, nin3 )
              s = zin ( 2, j, nin3 )
              r3 = r * cr3 - s * ci3
              s3 = r * ci3 + s * cr3
              r = zin ( 1, j, nin4 )
              s = zin ( 2, j, nin4 )
              r4 = r * cr4 - s * ci4
              s4 = r * ci4 + s * cr4
              r = r1 + r3
              s = r2 + r4
              zout ( 1, j, nout1 ) = r + s
              zout ( 1, j, nout3 ) = r - s
              r = r1 - r3
              s = s2 - s4
              zout ( 1, j, nout2 ) = r + s
              zout ( 1, j, nout4 ) = r - s
              r = s1 + s3
              s = s2 + s4
              zout ( 2, j, nout1 ) = r + s
              zout ( 2, j, nout3 ) = r - s
              r = s1 - s3
              s = r2 - r4
              zout ( 2, j, nout2 ) = r - s
              zout ( 2, j, nout4 ) = r + s
            END DO
          END DO
        END IF
      END DO
    END IF
  ELSE IF ( now == 8 ) THEN
    IF ( isign == -1 ) THEN
      ia = 1
      nin1 = ia - after
      nout1 = ia - atn
      DO ib = 1, before
        nin1 = nin1 + after
        nin2 = nin1 + atb
        nin3 = nin2 + atb
        nin4 = nin3 + atb
        nin5 = nin4 + atb
        nin6 = nin5 + atb
        nin7 = nin6 + atb
        nin8 = nin7 + atb
        nout1 = nout1 + atn
        nout2 = nout1 + after
        nout3 = nout2 + after
        nout4 = nout3 + after
        nout5 = nout4 + after
        nout6 = nout5 + after
        nout7 = nout6 + after
        nout8 = nout7 + after
        DO j = 1, nfft
          r1 = zin ( 1, j, nin1 )
          s1 = zin ( 2, j, nin1 )
          r2 = zin ( 1, j, nin2 )
          s2 = zin ( 2, j, nin2 )
          r3 = zin ( 1, j, nin3 )
          s3 = zin ( 2, j, nin3 )
          r4 = zin ( 1, j, nin4 )
          s4 = zin ( 2, j, nin4 )
          r5 = zin ( 1, j, nin5 )
          s5 = zin ( 2, j, nin5 )
          r6 = zin ( 1, j, nin6 )
          s6 = zin ( 2, j, nin6 )
          r7 = zin ( 1, j, nin7 )
          s7 = zin ( 2, j, nin7 )
          r8 = zin ( 1, j, nin8 )
          s8 = zin ( 2, j, nin8 )
          r = r1 + r5
          s = r3 + r7
          ap = r + s
          am = r - s
          r = r2 + r6
          s = r4 + r8
          bp = r + s
          bm = r - s
          r = s1 + s5
          s = s3 + s7
          cp = r + s
          cm = r - s
          r = s2 + s6
          s = s4 + s8
          dp = r + s
          dm = r - s
          zout ( 1, j, nout1 ) = ap + bp
          zout ( 2, j, nout1 ) = cp + dp
          zout ( 1, j, nout5 ) = ap - bp
          zout ( 2, j, nout5 ) = cp - dp
          zout ( 1, j, nout3 ) = am + dm
          zout ( 2, j, nout3 ) = cm - bm
          zout ( 1, j, nout7 ) = am - dm
          zout ( 2, j, nout7 ) = cm + bm
          r = r1 - r5
          s = s3 - s7
          ap = r + s
          am = r - s
          r = s1 - s5
          s = r3 - r7
          bp = r + s
          bm = r - s
          r = s4 - s8
          s = r2 - r6
          cp = r + s
          cm = r - s
          r = s2 - s6
          s = r4 - r8
          dp = r + s
          dm = r - s
          r = ( cp + dm ) * rt2i
          s = (-cp + dm ) * rt2i
          cp = ( cm + dp ) * rt2i
          dp = ( cm - dp ) * rt2i
          zout ( 1, j, nout2 ) = ap + r
          zout ( 2, j, nout2 ) = bm + s
          zout ( 1, j, nout6 ) = ap - r
          zout ( 2, j, nout6 ) = bm - s
          zout ( 1, j, nout4 ) = am + cp
          zout ( 2, j, nout4 ) = bp + dp
          zout ( 1, j, nout8 ) = am - cp
          zout ( 2, j, nout8 ) = bp - dp
        END DO
      END DO
    ELSE
      ia = 1
      nin1 = ia - after
      nout1 = ia - atn
      DO ib = 1, before
        nin1 = nin1 + after
        nin2 = nin1 + atb
        nin3 = nin2 + atb
        nin4 = nin3 + atb
        nin5 = nin4 + atb
        nin6 = nin5 + atb
        nin7 = nin6 + atb
        nin8 = nin7 + atb
        nout1 = nout1 + atn
        nout2 = nout1 + after
        nout3 = nout2 + after
        nout4 = nout3 + after
        nout5 = nout4 + after
        nout6 = nout5 + after
        nout7 = nout6 + after
        nout8 = nout7 + after
        DO j = 1, nfft
          r1 = zin ( 1, j, nin1 )
          s1 = zin ( 2, j, nin1 )
          r2 = zin ( 1, j, nin2 )
          s2 = zin ( 2, j, nin2 )
          r3 = zin ( 1, j, nin3 )
          s3 = zin ( 2, j, nin3 )
          r4 = zin ( 1, j, nin4 )
          s4 = zin ( 2, j, nin4 )
          r5 = zin ( 1, j, nin5 )
          s5 = zin ( 2, j, nin5 )
          r6 = zin ( 1, j, nin6 )
          s6 = zin ( 2, j, nin6 )
          r7 = zin ( 1, j, nin7 )
          s7 = zin ( 2, j, nin7 )
          r8 = zin ( 1, j, nin8 )
          s8 = zin ( 2, j, nin8 )
          r = r1 + r5
          s = r3 + r7
          ap = r + s
          am = r - s
          r = r2 + r6
          s = r4 + r8
          bp = r + s
          bm = r - s
          r = s1 + s5
          s = s3 + s7
          cp = r + s
          cm = r - s
          r = s2 + s6
          s = s4 + s8
          dp = r + s
          dm = r - s
          zout ( 1, j, nout1 ) = ap + bp
          zout ( 2, j, nout1 ) = cp + dp
          zout ( 1, j, nout5 ) = ap - bp
          zout ( 2, j, nout5 ) = cp - dp
          zout ( 1, j, nout3 ) = am - dm
          zout ( 2, j, nout3 ) = cm + bm
          zout ( 1, j, nout7 ) = am + dm
          zout ( 2, j, nout7 ) = cm - bm
          r = r1 - r5
          s = -s3 + s7
          ap = r + s
          am = r - s
          r = s1 - s5
          s = r7 - r3
          bp = r + s
          bm = r - s
          r = -s4 + s8
          s = r2 - r6
          cp = r + s
          cm = r - s
          r = -s2 + s6
          s = r4 - r8
          dp = r + s
          dm = r - s
          r = ( cp + dm ) * rt2i
          s = ( cp - dm ) * rt2i
          cp = ( cm + dp ) * rt2i
          dp = (-cm + dp ) * rt2i
          zout ( 1, j, nout2 ) = ap + r
          zout ( 2, j, nout2 ) = bm + s
          zout ( 1, j, nout6 ) = ap - r
          zout ( 2, j, nout6 ) = bm - s
          zout ( 1, j, nout4 ) = am + cp
          zout ( 2, j, nout4 ) = bp + dp
          zout ( 1, j, nout8 ) = am - cp
          zout ( 2, j, nout8 ) = bp - dp
        END DO
      END DO
    END IF
  ELSE IF ( now == 3 ) THEN
    ia = 1
    nin1 = ia - after
    nout1 = ia - atn
    DO ib = 1, before
      nin1 = nin1 + after
      nin2 = nin1 + atb
      nin3 = nin2 + atb
      nout1 = nout1 + atn
      nout2 = nout1 + after
      nout3 = nout2 + after
      DO j = 1, nfft
        r1 = zin ( 1, j, nin1 )
        s1 = zin ( 2, j, nin1 )
        r2 = zin ( 1, j, nin2 )
        s2 = zin ( 2, j, nin2 )
        r3 = zin ( 1, j, nin3 )
        s3 = zin ( 2, j, nin3 )
        r = r2 + r3
        s = s2 + s3
        zout ( 1, j, nout1 ) = r + r1
        zout ( 2, j, nout1 ) = s + s1
        r1 = r1 - 0.5_dbl * r
        s1 = s1 - 0.5_dbl * s
        r2 = bb * ( r2 - r3 )
        s2 = bb * ( s2 - s3 )
        zout ( 1, j, nout2 ) = r1 - s2
        zout ( 2, j, nout2 ) = s1 + r2
        zout ( 1, j, nout3 ) = r1 + s2
        zout ( 2, j, nout3 ) = s1 - r2
      END DO
    END DO
    DO ia = 2, after
      ias = ia - 1
      IF ( 4*ias == 3*after ) THEN
        IF ( isign == 1 ) THEN
          nin1 = ia - after
          nout1 = ia - atn
          DO ib = 1, before
            nin1 = nin1 + after
            nin2=nin1+atb
            nin3=nin2+atb
            nout1=nout1+atn
            nout2=nout1+after
            nout3=nout2+after
            DO j = 1, nfft
              r1 = zin ( 1, j, nin1 )
              s1 = zin ( 2, j, nin1 )
              r2 = -zin ( 2, j, nin2 )
              s2 = zin ( 1, j, nin2 )
              r3 = -zin ( 1, j, nin3 )
              s3 = -zin ( 2, j, nin3 )
              r = r2 + r3
              s = s2 + s3
              zout ( 1, j, nout1 ) = r + r1
              zout ( 2, j, nout1 ) = s + s1
              r1 = r1 - 0.5_dbl * r
              s1 = s1 - 0.5_dbl * s
              r2 = bb*(r2-r3)
              s2 = bb*(s2-s3)
              zout ( 1, j, nout2 ) = r1 - s2
              zout ( 2, j, nout2 ) = s1 + r2
              zout ( 1, j, nout3 ) = r1 + s2
              zout ( 2, j, nout3 ) = s1 - r2
            END DO
          END DO
        ELSE
          nin1 = ia - after
          nout1 = ia - atn
          DO ib = 1, before
            nin1 = nin1 + after
            nin2 = nin1 + atb
            nin3 = nin2 + atb
            nout1 = nout1 + atn
            nout2 = nout1 + after
            nout3 = nout2 + after
            DO j = 1, nfft
              r1 = zin ( 1, j, nin1 )
              s1 = zin ( 2, j, nin1 )
              r2 = zin ( 2, j, nin2 )
              s2 = -zin ( 1, j, nin2 )
              r3 = -zin ( 1, j, nin3 )
              s3 = -zin ( 2, j, nin3 )
              r = r2 + r3
              s = s2 + s3
              zout ( 1, j, nout1 ) = r + r1
              zout ( 2, j, nout1 ) = s + s1
              r1 = r1 - 0.5_dbl * r
              s1 = s1 - 0.5_dbl * s
              r2 = bb * ( r2 - r3 )
              s2 = bb * ( s2 - s3 )
              zout ( 1, j, nout2 ) = r1 - s2
              zout ( 2, j, nout2 ) = s1 + r2
              zout ( 1, j, nout3 ) = r1 + s2
              zout ( 2, j, nout3 ) = s1 - r2
            END DO
          END DO
        END IF
      ELSE IF ( 8 * ias == 3 * after ) THEN
        IF ( isign == 1 ) THEN
          nin1 = ia - after
          nout1 = ia - atn
          DO ib = 1, before
            nin1 = nin1 + after
            nin2 = nin1 + atb
            nin3 = nin2 + atb
            nout1 = nout1 + atn
            nout2 = nout1 + after
            nout3 = nout2 + after
            DO j = 1, nfft
              r1 = zin ( 1, j, nin1 )
              s1 = zin ( 2, j, nin1 )
              r = zin ( 1, j, nin2 )
              s = zin ( 2, j, nin2 )
              r2 = ( r - s ) * rt2i
              s2 = ( r + s ) * rt2i
              r3 = -zin ( 2, j, nin3 )
              s3 = zin ( 1, j, nin3 )
              r = r2 + r3
              s = s2 + s3
              zout ( 1, j, nout1 ) = r + r1
              zout ( 2, j, nout1 ) = s + s1
              r1 = r1 - 0.5_dbl * r
              s1 = s1 - 0.5_dbl * s
              r2 = bb * ( r2 - r3 )
              s2 = bb * ( s2 - s3 )
              zout ( 1, j, nout2 ) = r1 - s2
              zout ( 2, j, nout2 ) = s1 + r2
              zout ( 1, j, nout3 ) = r1 + s2
              zout ( 2, j, nout3 ) = s1 - r2
            END DO
          END DO
        ELSE
          nin1 = ia - after
          nout1 = ia - atn
          DO ib = 1, before
            nin1 = nin1 + after
            nin2 = nin1 + atb
            nin3 = nin2 + atb
            nout1 = nout1 + atn
            nout2 = nout1 + after
            nout3 = nout2 + after
            DO j = 1, nfft
              r1 = zin ( 1, j, nin1 )
              s1 = zin ( 2, j, nin1 )
              r = zin ( 1, j, nin2 )
              s = zin ( 2, j, nin2 )
              r2 = ( r + s ) * rt2i
              s2 = ( -r + s ) * rt2i
              r3 = zin ( 2, j, nin3 )
              s3 = -zin ( 1, j, nin3 )
              r = r2 + r3
              s = s2 + s3
              zout ( 1, j, nout1 ) = r + r1
              zout ( 2, j, nout1 ) = s + s1
              r1 = r1 - 0.5_dbl * r
              s1 = s1 - 0.5_dbl * s
              r2 = bb * ( r2 - r3 )
              s2 = bb * ( s2 - s3 )
              zout ( 1, j, nout2 ) = r1 - s2
              zout ( 2, j, nout2 ) = s1 + r2
              zout ( 1, j, nout3 ) = r1 + s2
              zout ( 2, j, nout3 ) = s1 - r2
            END DO
          END DO
        END IF
      ELSE
        itt = ias * before
        itrig = itt + 1
        cr2 = trig ( 1, itrig )
        ci2 = trig ( 2, itrig )
        itrig = itrig + itt
        cr3 = trig ( 1, itrig )
        ci3 = trig ( 2, itrig )
        nin1 = ia - after
        nout1 = ia - atn
        DO ib = 1, before
          nin1 = nin1 + after
          nin2 = nin1 + atb
          nin3 = nin2 + atb
          nout1 = nout1 + atn
          nout2 = nout1 + after
          nout3 = nout2 + after
          DO j = 1, nfft
            r1 = zin ( 1, j, nin1 )
            s1 = zin ( 2, j, nin1 )
            r = zin ( 1, j, nin2 )
            s = zin ( 2, j, nin2 )
            r2 = r * cr2 - s * ci2
            s2 = r * ci2 + s * cr2
            r = zin ( 1, j, nin3 )
            s = zin ( 2, j, nin3 )
            r3 = r * cr3 - s * ci3
            s3 = r * ci3 + s * cr3
            r = r2 + r3
            s = s2 + s3
            zout ( 1, j, nout1 ) = r + r1
            zout ( 2, j, nout1 ) = s + s1
            r1 = r1 - 0.5_dbl * r
            s1 = s1 - 0.5_dbl * s
            r2 = bb * ( r2 - r3 )
            s2 = bb * ( s2 - s3 )
            zout ( 1, j, nout2 ) = r1 - s2
            zout ( 2, j, nout2 ) = s1 + r2
            zout ( 1, j, nout3 ) = r1 + s2
            zout ( 2, j, nout3 ) = s1 - r2
          END DO
        END DO
      END IF
    END DO
  ELSE IF ( now == 5 ) THEN
    sin2 = isign * sin2p
    sin4 = isign * sin4p
    ia = 1
    nin1 = ia - after
    nout1 = ia - atn
    DO ib = 1, before
      nin1 = nin1 + after
      nin2 = nin1 + atb
      nin3 = nin2 + atb
      nin4 = nin3 + atb
      nin5 = nin4 + atb
      nout1 = nout1 + atn
      nout2 = nout1 + after
      nout3 = nout2 + after
      nout4 = nout3 + after
      nout5 = nout4 + after
      DO j = 1, nfft
        r1 = zin ( 1, j, nin1 )
        s1 = zin ( 2, j, nin1 )
        r2 = zin ( 1, j, nin2 )
        s2 = zin ( 2, j, nin2 )
        r3 = zin ( 1, j, nin3 )
        s3 = zin ( 2, j, nin3 )
        r4 = zin ( 1, j, nin4 )
        s4 = zin ( 2, j, nin4 )
        r5 = zin ( 1, j, nin5 )
        s5 = zin ( 2, j, nin5 )
        r25 = r2 + r5
        r34 = r3 + r4
        s25 = s2 - s5
        s34 = s3 - s4
        zout ( 1, j, nout1 ) = r1 + r25 + r34
        r = cos2 * r25 + cos4 * r34 + r1
        s = sin2 * s25 + sin4 * s34
        zout ( 1, j, nout2 ) = r - s
        zout ( 1, j, nout5 ) = r + s
        r = cos4 * r25 + cos2 * r34 + r1
        s = sin4 * s25 - sin2 * s34
        zout ( 1, j, nout3 ) = r - s
        zout ( 1, j, nout4 ) = r + s
        r25 = r2 - r5
        r34 = r3 - r4
        s25 = s2 + s5
        s34 = s3 + s4
        zout ( 2, j, nout1 ) = s1 + s25 + s34
        r = cos2 * s25 + cos4 * s34 + s1
        s = sin2 * r25 + sin4 * r34
        zout ( 2, j, nout2 ) = r + s
        zout ( 2, j, nout5 ) = r - s
        r = cos4 * s25 + cos2 * s34 + s1
        s = sin4 * r25 - sin2 * r34
        zout ( 2, j, nout3 ) = r + s
        zout ( 2, j, nout4 ) = r - s
      END DO
    END DO
    DO ia = 2, after
      ias = ia - 1
      IF ( 8 * ias == 5 * after ) THEN
        IF ( isign == 1 ) THEN
          nin1 = ia - after
          nout1 = ia - atn
          DO ib = 1, before
            nin1 = nin1 + after
            nin2 = nin1 + atb
            nin3 = nin2 + atb
            nin4 = nin3 + atb
            nin5 = nin4 + atb
            nout1 = nout1 + atn
            nout2 = nout1 + after
            nout3 = nout2 + after
            nout4 = nout3 + after
            nout5 = nout4 + after
            DO j = 1, nfft
              r1 = zin ( 1, j, nin1 )
              s1 = zin ( 2, j, nin1 )
              r = zin ( 1, j, nin2 )
              s = zin ( 2, j, nin2 )
              r2 = ( r - s ) * rt2i
              s2 = ( r + s ) * rt2i
              r3 = -zin ( 2, j, nin3 )
              s3 = zin ( 1, j, nin3 )
              r = zin ( 1, j, nin4 )
              s = zin ( 2, j, nin4 )
              r4 = -( r + s ) * rt2i
              s4 = ( r - s ) * rt2i
              r5 = -zin ( 1, j, nin5 )
              s5 = -zin ( 2, j, nin5 )
              r25 = r2 + r5
              r34 = r3 + r4
              s25 = s2 - s5
              s34 = s3 - s4
              zout ( 1, j, nout1 ) = r1 + r25 + r34
              r = cos2 * r25 + cos4 * r34 + r1
              s = sin2 * s25 + sin4 * s34
              zout ( 1, j, nout2 ) = r - s
              zout ( 1, j, nout5 ) = r + s
              r = cos4 * r25 + cos2 * r34 + r1
              s = sin4 * s25 - sin2 * s34
              zout ( 1, j, nout3 ) = r - s
              zout ( 1, j, nout4 ) = r + s
              r25 = r2 - r5
              r34 = r3 - r4
              s25 = s2 + s5
              s34 = s3 + s4
              zout ( 2, j, nout1 ) = s1 + s25 + s34
              r = cos2 * s25 + cos4 * s34 + s1
              s = sin2 * r25 + sin4 * r34
              zout ( 2, j, nout2 ) = r + s
              zout ( 2, j, nout5 ) = r - s
              r = cos4 * s25 + cos2 * s34 + s1
              s = sin4 * r25 - sin2 * r34
              zout ( 2, j, nout3 ) = r + s
              zout ( 2, j, nout4 ) = r - s
            END DO
          END DO
        ELSE
          nin1 = ia - after
          nout1 = ia - atn
          DO ib = 1, before
            nin1 = nin1 + after
            nin2 = nin1 + atb
            nin3 = nin2 + atb
            nin4 = nin3 + atb
            nin5 = nin4 + atb
            nout1 = nout1 + atn
            nout2 = nout1 + after
            nout3 = nout2 + after
            nout4 = nout3 + after
            nout5 = nout4 + after
            DO j = 1, nfft
              r1 = zin ( 1, j, nin1 )
              s1 = zin ( 2, j, nin1 )
              r = zin ( 1, j, nin2 )
              s = zin ( 2, j, nin2 )
              r2 = ( r + s ) * rt2i
              s2 = ( -r + s ) * rt2i
              r3 = zin ( 2, j, nin3 )
              s3 = -zin ( 1, j, nin3 )
              r = zin ( 1, j, nin4 )
              s = zin ( 2, j, nin4 )
              r4 = ( s - r ) * rt2i
              s4 = - ( r + s ) * rt2i
              r5 = -zin ( 1, j, nin5 )
              s5 = -zin ( 2, j, nin5 )
              r25 = r2 + r5
              r34 = r3 + r4
              s25 = s2 - s5
              s34 = s3 - s4
              zout ( 1, j, nout1 ) = r1 + r25 + r34
              r = cos2 * r25 + cos4 * r34 + r1
              s = sin2 * s25 + sin4 * s34
              zout ( 1, j, nout2 ) = r - s
              zout ( 1, j, nout5 ) = r + s
              r = cos4 * r25 + cos2 * r34 + r1
              s = sin4 * s25 - sin2 * s34
              zout ( 1, j, nout3 ) = r - s
              zout ( 1, j, nout4 ) = r + s
              r25 = r2 - r5
              r34 = r3 - r4
              s25 = s2 + s5
              s34 = s3 + s4
              zout ( 2, j, nout1) = s1 + s25 + s34
              r = cos2 * s25 + cos4 * s34 + s1
              s = sin2 * r25 + sin4 * r34
              zout ( 2, j, nout2 ) = r + s
              zout ( 2, j, nout5 ) = r - s
              r = cos4 * s25 + cos2 * s34 + s1
              s = sin4 * r25 - sin2 * r34
              zout ( 2, j, nout3 ) = r + s
              zout ( 2, j, nout4 ) = r - s
            END DO
          END DO
        END IF
      ELSE
        ias = ia - 1
        itt = ias * before
        itrig = itt + 1
        cr2 = trig ( 1, itrig )
        ci2 = trig ( 2, itrig )
        itrig = itrig + itt
        cr3 = trig ( 1, itrig )
        ci3 = trig ( 2, itrig )
        itrig = itrig + itt
        cr4 = trig ( 1, itrig )
        ci4 = trig ( 2, itrig )
        itrig = itrig + itt
        cr5 = trig ( 1, itrig )
        ci5 = trig ( 2, itrig )
        nin1 = ia - after
        nout1 = ia - atn
        DO ib = 1, before
          nin1 = nin1 + after
          nin2 = nin1 + atb
          nin3 = nin2 + atb
          nin4 = nin3 + atb
          nin5 = nin4 + atb
          nout1 = nout1 + atn
          nout2 = nout1 + after
          nout3 = nout2 + after
          nout4 = nout3 + after
          nout5 = nout4 + after
          DO j = 1, nfft
            r1 = zin ( 1, j, nin1 )
            s1 = zin ( 2, j, nin1 )
            r = zin ( 1, j, nin2 )
            s = zin ( 2, j, nin2 )
            r2 = r * cr2 - s * ci2
            s2 = r * ci2 + s * cr2
            r = zin ( 1, j, nin3 )
            s = zin ( 2, j, nin3 )
            r3 = r * cr3 - s * ci3
            s3 = r * ci3 + s * cr3
            r = zin ( 1, j, nin4 )
            s = zin ( 2, j, nin4 )
            r4 = r * cr4 - s * ci4
            s4 = r * ci4 + s * cr4
            r = zin ( 1, j, nin5 )
            s = zin ( 2, j, nin5 )
            r5 = r * cr5 - s * ci5
            s5 = r * ci5 + s * cr5
            r25 = r2 + r5
            r34 = r3 + r4
            s25 = s2 - s5
            s34 = s3 - s4
            zout ( 1, j, nout1 ) = r1 + r25 + r34
            r = cos2 * r25 + cos4 * r34 + r1
            s = sin2 * s25 + sin4 * s34
            zout ( 1, j, nout2 ) = r - s
            zout ( 1, j, nout5 ) = r + s
            r = cos4 * r25 + cos2 * r34 + r1
            s = sin4 * s25 - sin2 * s34
            zout ( 1, j, nout3 ) = r - s
            zout ( 1, j, nout4 ) = r + s
            r25 = r2 - r5
            r34 = r3 - r4
            s25 = s2 + s5
            s34 = s3 + s4
            zout ( 2, j, nout1 ) = s1 + s25 + s34
            r = cos2 * s25 + cos4 * s34 + s1
            s = sin2 * r25 + sin4 * r34
            zout ( 2, j, nout2 ) = r + s
            zout ( 2, j, nout5 ) = r - s
            r = cos4 * s25 + cos2 * s34 + s1
            s = sin4 * r25 - sin2 * r34
            zout ( 2, j, nout3 ) = r + s
            zout ( 2, j, nout4 ) = r - s
          END DO
        END DO
      END IF
    END DO
  ELSE IF ( now == 6 ) THEN
    ia = 1
    nin1 = ia - after
    nout1 = ia - atn
    DO ib = 1, before
      nin1 = nin1 + after
      nin2 = nin1 + atb
      nin3 = nin2 + atb
      nin4 = nin3 + atb
      nin5 = nin4 + atb
      nin6 = nin5 + atb
      nout1 = nout1 + atn
      nout2 = nout1 + after
      nout3 = nout2 + after
      nout4 = nout3 + after
      nout5 = nout4 + after
      nout6 = nout5 + after
      DO j = 1, nfft
        r2 = zin ( 1, j, nin3 )
        s2 = zin ( 2, j, nin3 )
        r3 = zin ( 1, j, nin5 )
        s3 = zin ( 2, j, nin5 )
        r = r2 + r3
        s = s2 + s3
        r1 = zin ( 1, j, nin1 )
        s1 = zin ( 2, j, nin1 )
        ur1 = r + r1
        ui1 = s + s1
        r1 = r1 - 0.5_dbl * r
        s1 = s1 - 0.5_dbl * s
        r = r2 - r3
        s = s2 - s3
        ur2 = r1 - s * bb
        ui2 = s1 + r * bb
        ur3 = r1 + s * bb
        ui3 = s1 - r * bb

        r2 = zin ( 1, j, nin6 )
        s2 = zin ( 2, j, nin6 )
        r3 = zin ( 1, j, nin2 )
        s3 = zin ( 2, j, nin2 )
        r = r2 + r3
        s = s2 + s3
        r1 = zin ( 1, j, nin4 )
        s1 = zin ( 2, j, nin4 )
        vr1 = r + r1
        vi1 = s + s1
        r1 = r1 - 0.5_dbl * r
        s1 = s1 - 0.5_dbl * s
        r = r2 - r3
        s = s2 - s3
        vr2 = r1 - s * bb
        vi2 = s1 + r * bb
        vr3 = r1 + s * bb
        vi3 = s1 - r * bb

        zout ( 1, j, nout1 ) = ur1 + vr1
        zout ( 2, j, nout1 ) = ui1 + vi1
        zout ( 1, j, nout5 ) = ur2 + vr2
        zout ( 2, j, nout5 ) = ui2 + vi2
        zout ( 1, j, nout3 ) = ur3 + vr3
        zout ( 2, j, nout3 ) = ui3 + vi3
        zout ( 1, j, nout4 ) = ur1 - vr1
        zout ( 2, j, nout4 ) = ui1 - vi1
        zout ( 1, j, nout2 ) = ur2 - vr2
        zout ( 2, j, nout2 ) = ui2 - vi2
        zout ( 1, j, nout6 ) = ur3 - vr3
        zout ( 2, j, nout6 ) = ui3 - vi3
      END DO
    END DO
  ELSE
    STOP 'Error fftstp'
  END If

!-----------------------------------------------------------------------------!

END SUBROUTINE fftstp

!-----------------------------------------------------------------------------!
!-----------------------------------------------------------------------------!
!   CP2K: A general program to perform molecular dynamics simulations         !
!   Copyright (C) 2000  CP2K developers group                                 !
!-----------------------------------------------------------------------------!

SUBROUTINE mltfftsg ( transa, transb, a, ldax, lday, b, ldbx, ldby, &
                      n, m, isign, scale )

  IMPLICIT NONE

  INTEGER, PARAMETER :: dbl = SELECTED_REAL_KIND ( 14, 200 )

! Arguments
  CHARACTER ( LEN = 1 ), INTENT ( IN ) :: transa*1, transb*1
  INTEGER, INTENT ( IN ) :: ldax, lday, ldbx, ldby, n, m, isign
  COMPLEX ( dbl ), INTENT ( INOUT ) :: a ( ldax, lday ), b ( ldbx, ldby )
  REAL ( dbl ), INTENT ( IN ) :: scale

! Variables
  INTEGER, SAVE :: ncache
  INTEGER :: after ( 20 ), now ( 20 ), before ( 20 )
  REAL ( dbl ) :: trig ( 2, 1024 )
  LOGICAL :: tscal
  COMPLEX ( dbl ), DIMENSION ( :, : ), ALLOCATABLE, SAVE :: z
  INTEGER :: length, isig, ic, lot, itr, nfft, i, inzee, j

!-----------------------------------------------------------------------------!

  IF( .NOT. ALLOCATED ( Z ) ) THEN
    ncache = get_cache_size ( )
    LENGTH = 2 * ( NCACHE / 4 + 1 )
    ALLOCATE ( Z ( LENGTH, 2 ) )
  END IF

  ISIG = -ISIGN
  TSCAL = ( ABS ( SCALE -1._dbl ) > 1.e-12_dbl )
  CALL CTRIG ( N, TRIG, AFTER, BEFORE, NOW, ISIG, IC )
  LOT = NCACHE / ( 4 * N )
  LOT = LOT - MOD ( LOT + 1, 2 )
  LOT = MAX ( 1, LOT )
  DO ITR = 1, M, LOT
    NFFT = MIN ( M - ITR + 1, LOT )
    IF ( TRANSA == 'N' .OR. TRANSA == 'n' ) THEN
      CALL FFTPRE ( NFFT, NFFT, LDAX, LOT, N, A ( 1, ITR ), Z ( 1, 1 ), &
                    TRIG, NOW ( 1 ), AFTER ( 1 ), BEFORE ( 1 ), ISIG )
    ELSE
      CALL FFTSTP ( LDAX, NFFT, N, LOT, N, A ( ITR, 1 ), Z ( 1, 1 ), &
                    TRIG, NOW ( 1 ), AFTER ( 1 ), BEFORE ( 1 ), ISIG )
    ENDIF
    IF ( TSCAL ) THEN
      IF ( LOT == NFFT ) THEN
        CALL DSCAL ( 2 * LOT * N, SCALE, Z ( 1, 1 ), 1 )
!         Z ( :lot*n, 1:2 ) = Z ( :lot*n, 1:2 ) * scale
      ELSE
        DO I = 1, N
          CALL DSCAL ( 2 * NFFT, SCALE, Z ( LOT * ( I - 1 ) + 1, 1 ), 1 )
!           Z ( LOT*(I-1)+1:LOT*(I-1)+NFFT, 1:2 ) &
!                = Z ( LOT*(I-1)+1:LOT*(I-1)+NFFT, 1:2 ) * SCALE
        END DO
      END IF
    END IF
    IF(IC.EQ.1) THEN
      IF(TRANSB == 'N'.OR.TRANSB == 'n') THEN
        CALL ZGETMO(Z(1,1),LOT,NFFT,N,B(1,ITR),LDBX)
      ELSE
        CALL MATMOV(NFFT,N,Z(1,1),LOT,B(ITR,1),LDBX)
      ENDIF
    ELSE
      INZEE=1
      DO I=2,IC-1
        CALL FFTSTP(LOT,NFFT,N,LOT,N,Z(1,INZEE), &
                    Z(1,3-INZEE),TRIG,NOW(I),AFTER(I), &
                    BEFORE(I),ISIG)
        INZEE=3-INZEE
      ENDDO
      IF(TRANSB == 'N'.OR.TRANSB == 'n') THEN
        CALL FFTROT(LOT,NFFT,N,NFFT,LDBX,Z(1,INZEE), &
              B(1,ITR),TRIG,NOW(IC),AFTER(IC),BEFORE(IC),ISIG)
      ELSE
        CALL FFTSTP(LOT,NFFT,N,LDBX,N,Z(1,INZEE), &
              B(ITR,1),TRIG,NOW(IC),AFTER(IC),BEFORE(IC),ISIG)
      ENDIF
    ENDIF
  ENDDO
  IF(TRANSB == 'N'.OR.TRANSB == 'n') THEN
    B(1:LDBX,M+1:LDBY) = CMPLX(0._dbl,0._dbl,dbl)
    B(N+1:LDBX,1:M) = CMPLX(0._dbl,0._dbl,dbl)
  ELSE
    B(1:LDBX,N+1:LDBY) = CMPLX(0._dbl,0._dbl,dbl)
    B(M+1:LDBX,1:M) = CMPLX(0._dbl,0._dbl,dbl)
  ENDIF

!******************************************************************************

  CONTAINS

!******************************************************************************

    SUBROUTINE matmov ( n, m, a, lda, b, ldb )
      IMPLICIT NONE
      INTEGER :: n, m, lda, ldb
      COMPLEX (dbl) :: a ( lda, * ), b ( ldb, * )
      b ( 1:n , 1:m ) = a ( 1:n, 1:m )
    END SUBROUTINE matmov

    SUBROUTINE zgetmo ( a, lda, m, n, b, ldb )
      IMPLICIT NONE
      INTEGER  :: lda, m, n, ldb
      COMPLEX(dbl) :: a ( lda, n ), b ( ldb, m )
      b ( 1:n, 1:m ) = TRANSPOSE ( a ( 1:m, 1:n ) )
    END SUBROUTINE zgetmo

    FUNCTION get_cache_size ( ) RESULT ( ncache )
      IMPLICIT NONE
      INTEGER ncache


#if defined ( __T3E )
      ncache = 1024*8
#elif defined ( __SX5 ) || defined ( __T90 )
      ncache = 1024*128
#elif defined ( __ALPHA )
      ncache = 1024*8
#elif defined ( __SGI )
      ncache = 1024*4
#elif defined ( __POWER2 ) || defined ( __OSX )
      ncache = 1024*10
#elif defined(__i386)
      ncache = 1024*10
#elif defined(__x86_64)
      ncache = 1024*6
#elif defined ( __HP )
      ncache = 1024*64
#else
      ncache = 1024*2
#endif

    END FUNCTION get_cache_size

!******************************************************************************

END SUBROUTINE mltfftsg

! BLAS subroutine dscal
!     scales a vector by a constant.
!     uses unrolled loops for increment equal to one.
!     jack dongarra, linpack, 3/11/78.
!     modified 3/93 to return if incx .le. 0.
!     modified 12/3/93, array(1) declarations changed to array(*)
!     modified 12/30/04 adapted to (kindof) fortran 90 

SUBROUTINE dscal(n,da,dx,incx)
      USE kinds, ONLY : dbl
      IMPLICIT NONE
! arguments
      INTEGER, INTENT ( IN ) :: n, incx

      REAL(dbl), INTENT ( IN ) :: da
      REAL(dbl), INTENT ( INOUT ) :: dx(n*incx)

! locals
      INTEGER :: i,x,m,mp1,nincx

      IF ( ( n <= 0 ) .or. ( incx <= 0 ) ) RETURN
      IF ( incx /= 1) THEN
!
!        code for increment not equal to 1
!
          nincx = n*incx
!$OMP parallel do private(I)
#ifdef _vpp_
!OCL NOALIAS
#endif
          DO  i = 1, nincx, incx
              dx(i) = da*dx(i)
          END DO

      ELSE
!
!        code for increment equal to 1
!
!
!        clean-up loop
!
          m = mod(n,5)
          IF ( m /= 0 ) THEN
!$OMP parallel do private(I)
#ifdef _vpp_
!OCL NOALIAS
#endif
            DO i = 1,m
              dx(i) = da*dx(i)
            END DO
          ENDIF

          IF ( n < 5 ) RETURN

          mp1 = m + 1
          DO i = mp1,n,5
            dx(i) = da*dx(i)
            dx(i + 1) = da*dx(i + 1)
            dx(i + 2) = da*dx(i + 2)
            dx(i + 3) = da*dx(i + 3)
            dx(i + 4) = da*dx(i + 4)
          END DO

      END IF

      RETURN

END SUBROUTINE dscal
