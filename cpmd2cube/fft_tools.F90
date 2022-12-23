
MODULE fft_tools
  
  USE grid_types, ONLY : grid_type
  USE kinds, ONLY : dbl
  
  IMPLICIT NONE
  
  PRIVATE
  PUBLIC :: fftchk, fwdfft, invfft
  
CONTAINS

!******************************************************************************

SUBROUTINE fwdfft ( f, nr1, nr2, nr3, n )
  
! Arguments
  COMPLEX ( dbl ), DIMENSION ( :, :, : ), INTENT ( INOUT ) :: f
  INTEGER, INTENT ( IN ), OPTIONAL :: n ( 3 )
  INTEGER, INTENT ( IN ), OPTIONAL :: nr1, nr2, nr3
  
! Locals
  INTEGER :: isign, m(3)
  INTEGER :: dim1, dim2, dim3
  REAL ( dbl ) :: scale
  
!------------------------------------------------------------------------------
  
  isign = 1

  dim1 = SIZE ( f, 1 )
  dim2 = SIZE ( f, 2 )
  dim3 = SIZE ( f, 3 )
  
  IF ( PRESENT ( n ) ) THEN
     m ( : ) = n ( : )
  ELSE IF ( PRESENT ( nr1 ) .AND. PRESENT ( nr2 ) .AND. PRESENT ( nr3 ) ) THEN
     m ( 1 ) = nr1
     m ( 2 ) = nr2
     m ( 3 ) = nr3
  ELSE
     m ( 1 ) = dim1
     m ( 2 ) = dim2
     m ( 3 ) = dim3
  END IF
  
  CALL phase ( f, m, nr1, nr2, nr3 )
  
  scale = 1.0_dbl / REAL ( m( 1 ) * m( 2 ) * m( 3 ), dbl )
  CALL fft3d ( f, dim1, dim2, dim3, m ( 1 ), m ( 2 ), m ( 3 ), isign, scale )
  
END SUBROUTINE fwdfft

!******************************************************************************

SUBROUTINE invfft ( f, nr1, nr2, nr3, n )
  
! Arguments
  COMPLEX ( dbl ), DIMENSION ( :, :, : ), INTENT ( INOUT ) :: f
  INTEGER, INTENT ( IN ), OPTIONAL :: n ( 3 )
  INTEGER, INTENT ( IN ), OPTIONAL :: nr1, nr2, nr3
  
! Locals
  INTEGER :: isign, m(3)
  INTEGER :: dim1, dim2, dim3
  REAL ( dbl ) :: scale
  
!------------------------------------------------------------------------------
  
  isign = -1
  scale = 1.0_dbl

  dim1 = SIZE ( f, 1 )
  dim2 = SIZE ( f, 2 )
  dim3 = SIZE ( f, 3 )
  
  IF ( PRESENT ( n ) ) THEN
     m ( : ) = n ( : )
  ELSE IF ( PRESENT ( nr1 ) .AND. PRESENT ( nr2 ) .AND. PRESENT ( nr3 ) ) THEN
     m ( 1 ) = nr1
     m ( 2 ) = nr2
     m ( 3 ) = nr3
  ELSE
     m ( 1 ) = dim1
     m ( 2 ) = dim2
     m ( 3 ) = dim3
  END IF
  
  CALL fft3d ( f, dim1, dim2, dim3, m ( 1 ), m ( 2 ), m ( 3 ), isign, scale )
  
  CALL phase ( f, m, nr1, nr2, nr3 )
  
END SUBROUTINE invfft

!******************************************************************************

SUBROUTINE phase ( f, n, nr1, nr2, nr3 )
  
  IMPLICIT NONE
  
! Arguments
  COMPLEX ( dbl ), DIMENSION ( :, :, : ), INTENT ( INOUT ) :: f
  INTEGER, INTENT ( IN ), OPTIONAL :: nr1, nr2, nr3
  INTEGER, OPTIONAL :: n ( 3 )
  
! Locals
  INTEGER :: k, j, i, ii, ijk, m ( 3 )
  REAL ( dbl ) :: pf ( 2 ) = (/ 1.0_dbl, -1.0_dbl /)
  
!------------------------------------------------------------------------------
  
  IF ( PRESENT ( n ) ) THEN
     m ( : ) = n ( : )
  ELSE IF ( PRESENT ( nr1 ) .AND. PRESENT ( nr2 ) .AND. PRESENT ( nr3 ) ) THEN
     m ( 1 ) = nr1
     m ( 2 ) = nr2
     m ( 3 ) = nr3
  ELSE
     STOP "phase, dimensions not given"
  END IF
  
  DO k = 1, m ( 3 )
     DO j = 1, m ( 2 )
        DO i = 1, m ( 1 )
           ii = i
           ijk = MOD ( k + j + i + 1, 2 ) + 1
           f ( ii, j, k ) = f ( ii, j, k ) * pf ( ijk )
        END DO
     END DO
  END DO
  
END SUBROUTINE phase

!******************************************************************************

#if defined ( FFT_ESSL )
SUBROUTINE fft3d ( x, ldx, ldy, ldz, nx, ny, nz, isign, scale )
  
  IMPLICIT NONE
  
! Arguments
  INTEGER, INTENT(in) :: ldx,ldy,ldz,nx,ny,nz,isign
  COMPLEX(dbl), INTENT(inout) :: x(ldx,ldy,ldz)
  REAL ( dbl ), INTENT(in) :: scale
  
! Locals
  INTEGER, PARAMETER:: naux = 60000
  REAL ( dbl ) :: aux(naux)
  
  CALL dcft3(x,ldx,ldx*ldy,x,ldx,ldx*ldy,nx,ny,nz,isign,scale,aux,naux)
  
END SUBROUTINE fft3d

#elif defined ( FFT_T3E )

SUBROUTINE fft3d ( x, ldx, ldy, ldz, nx, ny, nz, isign, scale )
  
  IMPLICIT NONE
  
! Arguments
  INTEGER, INTENT ( IN ) :: ldx, ldy, ldz, nx, ny, nz, isign
  COMPLEX ( dbl ), INTENT ( INOUT ) :: x ( ldx, ldy, ldz )
  REAL ( dbl ), INTENT ( IN ) :: scale
  
! Locals
  INTEGER :: isys ( 4 ) = (/ 3, 0, 0, 0 /), fft_sign
  REAL ( dbl ) :: work ( 2 * nx * ny * nx )
  REAL ( dbl ) :: table ( 2 * ( nx + ny + nz ) )
  
!------------------------------------------------------------------------------
  
! In order to initialise the tables
  CALL ccfft3d ( 0, nx, ny, nz, scale, x, ldx, ldy, x, ldx, ldy, &
       table, work, isys )
  
! minus because then it is consistent with dcft3/ESSL (and yields correct res.)
  fft_sign = - isign
  
  CALL ccfft3d ( fft_sign, nx, ny, nz, scale, x, ldx, ldy, x, ldx, ldy, &
       table, work, isys )
  
END SUBROUTINE fft3d

#elif defined ( FFT_FFTW )

SUBROUTINE fft3d ( x, ldx, ldy, ldz, nx, ny, nz, isign, scale )
  
  IMPLICIT NONE
  
! Arguments
  INTEGER, INTENT ( IN ) :: ldx, ldy, ldz, nx, ny, nz, isign
  COMPLEX ( dbl ), INTENT ( INOUT ) :: x ( ldx, ldy, ldz )
  REAL ( dbl ), INTENT ( IN ) :: scale
  
! Locals
  INTEGER :: isys ( 4 ) = (/ 3, 0, 0, 0 /), fft_sign
  REAL ( dbl ) :: work ( 2 * nx * ny * nx )
  REAL ( dbl ) :: table ( 2 * ( nx + ny + nz ) )
  
! integer_pointer_kind: kind parameter for pointer variables (addresses)
!                       this is usually an integer with 32 or 64 bit size
#if defined (__ABSOFT)
  INTEGER, PARAMETER :: integer_pointer_kind = 4
#else
  INTEGER, PARAMETER :: integer_pointer_kind = 8
#endif
  
  INTEGER ( KIND = integer_pointer_kind ) :: plan_a_fw, plan_a_bw

      integer FFTW_FORWARD,FFTW_BACKWARD
      parameter (FFTW_FORWARD=-1,FFTW_BACKWARD=1)
  
      integer FFTW_ESTIMATE,FFTW_MEASURE
      parameter (FFTW_ESTIMATE=0,FFTW_MEASURE=1)

      integer FFTW_OUT_OF_PLACE,FFTW_IN_PLACE,FFTW_USE_WISDOM
      parameter (FFTW_OUT_OF_PLACE=0)
      parameter (FFTW_IN_PLACE=8,FFTW_USE_WISDOM=16)
  
!------------------------------------------------------------------------------
  
  IF ( ldx /= nx .OR. ldy /= ny .OR. ldz /= nz ) THEN
     STOP "FFTW: Array has to be continuous"
  END IF
  
  CALL fftw3d_f77_create_plan ( plan_a_fw, nx, ny, nz, FFTW_FORWARD, &
       FFTW_ESTIMATE + FFTW_IN_PLACE )
  CALL fftw3d_f77_create_plan ( plan_a_bw, nx, ny, nz, FFTW_BACKWARD, &
       FFTW_ESTIMATE + FFTW_IN_PLACE )
  
  IF ( isign == +1 ) THEN
     CALL fftwnd_f77_one ( plan_a_fw, x, x )
  ELSE
     CALL fftwnd_f77_one ( plan_a_bw, x, x )
  END IF
  
  IF ( scale /= 1.0_dbl ) THEN
     x = x * scale
  END IF
  
END SUBROUTINE fft3d

#elif defined ( FFT_FFTW3 )

SUBROUTINE fft3d ( x, ldx, ldy, ldz, nx, ny, nz, isign, scale )

  IMPLICIT NONE

! Arguments
  INTEGER, INTENT ( IN ) :: ldx, ldy, ldz, nx, ny, nz, isign
  COMPLEX ( dbl ), INTENT ( INOUT ) :: x ( ldx, ldy, ldz )
  REAL ( dbl ), INTENT ( IN ) :: scale

! Locals
#if defined (__ABSOFT)
  INTEGER, PARAMETER :: integer_pointer_kind = 4
#else
  INTEGER, PARAMETER :: integer_pointer_kind = 8
#endif
  INTEGER ( KIND = integer_pointer_kind ) :: plan
  INTEGER :: ld( 3 ), n( 3 )
  INTEGER :: fftw_dir
  INTEGER, PARAMETER :: FFTW_FORWARD = -1, FFTW_BACKWARD = 1
  INTEGER, PARAMETER :: FFTW_ESTIMATE = 64
!------------------------------------------------------------------------------
  ld( 1 ) = ldx
  ld( 2 ) = ldy
  ld( 3 ) = ldz

  n( 1 ) = nx
  n( 2 ) = ny
  n( 3 ) = nz

  IF ( isign == +1 ) THEN
     fftw_dir = FFTW_FORWARD
  ELSE
     fftw_dir = FFTW_BACKWARD
  END IF

  CALL dfftw_plan_many_dft( plan, 3, n, 1, x, ld, 1, 1, x, ld, 1, 1, &
       fftw_dir, FFTW_ESTIMATE )

  CALL dfftw_execute_dft( plan, x, x )

  IF ( scale /= 1.0_dbl ) x = x * scale

  CALL dfftw_destroy_plan( plan )

END SUBROUTINE fft3d

#elif defined ( FFT_DEFAULT )

SUBROUTINE fft3d ( x, ldx, ldy, ldz, nx, ny, nz, isign, scale )
  
  USE fftsg_lib, ONLY : fft3d_sg
  
  IMPLICIT NONE
  
! Arguments
  INTEGER, INTENT ( IN ) :: ldx, ldy, ldz, nx, ny, nz
  INTEGER, INTENT ( INOUT ) :: isign
  COMPLEX ( dbl ), INTENT ( INOUT ) :: x ( ldx, ldy, ldz )
  REAL ( dbl ), INTENT ( IN ) :: scale
  
! Locals
  INTEGER :: n ( 3 )
  
!------------------------------------------------------------------------------
  
  IF ( ldx /= nx .OR. ldy /= ny .OR. ldz /= nz ) THEN
     STOP "FFTSG: Array has to be continuous"
  END IF
  
  n ( 1 ) = nx
  n ( 2 ) = ny
  n ( 3 ) = nz
  
! In order to initialise the tables
  CALL fft3d_sg ( isign, scale, n, x )
  
END SUBROUTINE fft3d

#endif

!******************************************************************************

! ----------------------------------------------------------------------
!     n < 0 : take the next smaller one
!     n = 1 : take the next bigger one
!     n = 2 : take the next bigger even one
! ----------------------------------------------------------------------

FUNCTION fftchk ( m, n )
  
  IMPLICIT NONE
  
! Return value
  INTEGER :: fftchk
  
! Arguments
  INTEGER :: m, n
  
! Locals
  INTEGER :: nmx,i,m1
  
#if defined(FFT_ESSL)
  PARAMETER ( nmx = 99 )
  INTEGER lft ( nmx )
  !! the following table list of acceptable values for == 
  !! the transform lengths in the fft is taken from pag. 758 == 
  !! of the essl manual (vol. 3) == 
  DATA lft /   2,   4,   6,   8,  10,  12,  14,  16,  18,  20,  &
       22,  24,  28,  30,  32,  36,  40,  42,  44,  48, &
       56,  60,  64,  66,  70,  72,  80,  84,  88,  90, &
       96, 110, 112, 120, 126, 128, 132, 140, 144, 154, &
       160, 168, 176, 180, 192, 198, 210, 220, 224, 240, &
       252, 256, 264, 280, 288, 308, 320, 330, 336, 352, &
       360, 384, 396, 420, 440, 448, 462, 480, 504, 512, &
       528, 560, 576, 616, 630, 640, 660, 672, 704, 720, &
       768, 770, 792, 840, 880, 896, 924, 960, 990,1008, &
       1024,1056,1120,1152,1232,1260,1280,1320,1344/
#elif defined ( FFT_DEFAULT ) || defined ( FFT_T3D ) || defined ( FFT_T3E ) || defined ( FFT_HP )
  PARAMETER(nmx = 100)
  INTEGER lft(nmx)
  !! the following table list of acceptable values for == 
  !! the transform lengths in the fft (roots 2, 3 and 5) == 
  DATA lft /   3,   4,   5,   6,   8,   9,  12,  15,  16,  18, &
       20,  24,  25,  27,  30,  32,  36,  40,  45,  48, &
       54,  60,  64,  72,  75,  80,  81,  90,  96, 100, &
       108, 120, 125, 128, 135, 144, 150, 160, 162, 180, &
       192, 200, 216, 225, 240, 243, 256, 270, 288, 300, &
       320, 324, 360, 375, 384, 400, 405, 432, 450, 480, &
       486, 500, 512, 540, 576, 600, 625, 640, 648, 675, &
       720, 729, 750, 768, 800, 810, 864, 900, 960, 972, &
       1000,1024,1080,1152,1200,1280,1296,1350,1440,1458,&
       1500,1536,1600,1620,1728,1800,1920,1944,2000,2048 /

#elif defined ( FFT_FFTW ) || defined ( FFT_FFTW3 ) 
      PARAMETER(NMX=316)
      INTEGER LFT(NMX)
!     ==================================================================
!     ==   The following table list of acceptable values for          ==
!     ==   the transform lengths in the FFT (roots 2, 3, 5, 7)        ==
!     ==================================================================
      DATA LFT /   2,   3,   4,   5,   6,   7,   8,   9,  10,  12, &
                 14,  15,  16,  18,  20,  21,  24,  25,  27,  28, &
                 30,  32,  35,  36,  40,  42,  45,  48,  49,  50, &
                 54,  56,  60,  63,  64,  70,  72,  75,  80,  81, &
                 84,  90,  96,  98, 100, 105, 108, 112, 120, 125, &
                126, 128, 135, 140, 144, 147, 150, 160, 162, 168, &
                175, 180, 189, 192, 196, 200, 210, 216, 224, 225, &
                240, 243, 245, 250, 252, 256, 270, 280, 288, 294, &
                300, 315, 320, 324, 336, 343, 350, 360, 375, 378, &
                384, 392, 400, 405, 420, 432, 441, 448, 450, 480, &
                486, 490, 500, 504, 512, 525, 540, 560, 567, 576, &
                588, 600, 625, 630, 640, 648, 672, 675, 686, 700, &
                720, 729, 735, 750, 756, 768, 784, 800, 810, 840, &
                864, 875, 882, 896, 900, 945, 960, 972, 980,1000, &
               1008,1024,1029,1050,1080,1120,1125,1134,1152,1176, &
               1200,1215,1225,1250,1260,1280,1296,1323,1344,1350, &
               1372,1400,1440,1458,1470,1500,1512,1536,1568,1575, &
               1600,1620,1680,1701,1715,1728,1750,1764,1792,1800, &
               1875,1890,1920,1944,1960,2000,2016,2025,2048,2058, &
               2100,2160,2187,2205,2240,2250,2268,2304,2352,2400, &
               2401,2430,2450,2500,2520,2560,2592,2625,2646,2688, &
               2700,2744,2800,2835,2880,2916,2940,3000,3024,3072, &
               3087,3125,3136,3150,3200,3240,3360,3375,3402,3430, &
               3456,3500,3528,3584,3600,3645,3675,3750,3780,3840, &
               3888,3920,3969,4000,4032,4050,4096,4116,4200,4320, &
               4374,4375,4410,4480,4500,4536,4608,4704,4725,4800, &
               4802,4860,4900,5000,5040,5103,5120,5145,5184,5250, &
               5292,5376,5400,5488,5600,5625,5670,5760,5832,5880, &
               6000,6048,6075,6125,6144,6174,6250,6272,6300,6400, &
               6480,6561,6615,6720,6750,6804,6860,6912,7000,7056, &
               7168,7200,7203,7290,7350,7500,7560,7680,7776,7840, &
               7875,7938,8000,8064,8100,8192  /
!     ==--------------------------------------------------------------==
#else
  
  PARAMETER(nmx = 1)
  INTEGER lft(nmx)
  DATA lft /   0/
  
#endif


  fftchk = 0
  m1 = m
  DO i = 1,nmx
     IF ( lft(i) >= m1) THEN
        IF ( n < 0) THEN
           m1 = lft(i-1)
           GOTO 10
        ELSE IF ( n == 1) THEN
           m1 = lft(i)
           GOTO 10
        ELSE IF ( n == 2) THEN
           IF ( MOD(lft(i),2) == 0) THEN
              m1 = lft(i)
              GOTO 10
           END IF
        END IF
     END IF
  END DO
  
  GO TO 999
  
10 CONTINUE
  fftchk = m1
  RETURN
  
999 CONTINUE
  WRITE(*,*) 'function fftchk     '
  WRITE(*,*) ' the minimal mesh size is larger than ',lft(nmx), &
       ' m = ',m
  WRITE(*,*) ' add larger mesh values in fftchk '
  STOP 'fftchk'
  
  
END FUNCTION fftchk

!******************************************************************************

END MODULE fft_tools
