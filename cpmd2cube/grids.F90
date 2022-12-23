! hello emacs this is -*- f90 -*-

MODULE grids
  
  USE grid_types, ONLY : grid_type
  USE kinds, ONLY : dbl
  USE util, ONLY : quicksort
  
  IMPLICIT NONE
  
  PRIVATE
  PUBLIC :: grid_form
  
! private to module
  REAL ( dbl ), PARAMETER :: epsgx = 1.0E-12_dbl
  
CONTAINS

!******************************************************************************

SUBROUTINE grid_init ( grid )
  
! Arguments
  TYPE ( grid_type ), INTENT ( INOUT ), TARGET :: grid
  
!------------------------------------------------------------------------------
  
  grid % formed = .FALSE.
  NULLIFY ( grid % inyh )
  NULLIFY ( grid % hg )
  
END SUBROUTINE grid_init

!******************************************************************************

SUBROUTINE grid_form ( gcut, gcutw, ibrav, celldm, nr1, nr2, nr3, grid )
  
  IMPLICIT NONE
  
! Arguments
  REAL ( dbl ), INTENT ( IN ) :: gcut, gcutw
  INTEGER, INTENT ( IN ) :: ibrav
  REAL ( dbl ), INTENT ( IN ) :: celldm ( 6 )
  INTEGER, INTENT ( IN ) :: nr1, nr2, nr3
  
  TYPE ( grid_type ), INTENT ( INOUT ), TARGET :: grid
  
! Local
  INTEGER :: ngw, j
  REAL ( dbl ) :: b1 ( 3 ), b2 ( 3 ), b3 ( 3 ), epsg
  
  INTEGER, POINTER :: nhg
  INTEGER, DIMENSION ( :, : ), POINTER :: inyh
  REAL ( dbl ), DIMENSION ( : ), POINTER :: hg
  REAL ( dbl ), DIMENSION ( : ), POINTER :: a1, a2, a3
  
!------------------------------------------------------------------------------
  
  IF ( grid % formed ) THEN
     IF ( ibrav == grid % ibrav .AND. &
       ALL ( celldm == grid % celldm ) .AND. gcut == grid % gcut .AND. &
       gcutw == grid % gcutw .AND. &
       ( nr1 == grid % nr1 .AND. nr2 == grid % nr2 &
       .AND. nr3 == grid % nr3 ) ) THEN
        
! The grid has not changed, nothing to do here
        RETURN
     END IF
  END IF
  
  a1 => grid % a1
  a2 => grid % a2
  a3 => grid % a3
  nhg => grid % nhg
  
  grid % nr1 = nr1
  grid % nr2 = nr2
  grid % nr3 = nr3
  grid % ibrav = ibrav
  grid % celldm = celldm
  grid % gcut = gcut
  grid % gcutw = gcutw
  
! First find the lattice vectors ...
  CALL latgen ( ibrav, celldm, a1, a2, a3, grid % volume )
  
! ... then the reci-procal vectors ...
  CALL recips ( celldm ( 1 ), a1, a2, a3, b1, b2, b3 )
  
! ... the number of g vectors ...
  CALL gcount ( gcut, gcutw, b1, b2, b3, ngw, nhg, &
       grid % nr1, grid % nr2, grid % nr3 )
  
  IF ( ASSOCIATED ( grid % inyh ) ) DEALLOCATE ( grid % inyh )
  IF ( ASSOCIATED ( grid % hg ) ) DEALLOCATE ( grid % hg )
  
  ALLOCATE ( grid % inyh ( 3, nhg ) )
  ALLOCATE ( grid % hg ( nhg ) )
  
  inyh => grid % inyh
  hg => grid % hg
  
! ... form the grid ...
  CALL gmake ( gcut, gcutw, b1, b2, b3, hg, inyh, &
       grid % nh1, grid % nh2, grid % nh3, &
       grid % nr1, grid % nr2, grid % nr3, &
       ngw, nhg, epsg )
  
! ... check the g vectors
  DO j = 2, nhg
     IF ( hg(j) - hg(j-1) < -epsg ) THEN
        WRITE ( *, '( "Error in ordering of g-vectors:" )' )
        WRITE ( *, '( 3X, I12, ":", E14.5, ";   ", I12, ":", E14.5 )' ) &
             j-1, hg(j-1), j, hg(j)
     END IF
  END DO
  
  grid % formed = .TRUE.
  
END SUBROUTINE grid_form

!******************************************************************************

! ----------------------------------------------------------------------
! subroutine latgen:
!   Sets up the translational vectors a1,a2, and a3.
!         ibrav
!           1     Simple Cubic
!           2     Face Centered Cubic
!           3     Body Centered Cubic
!           4     Hexagonal
!           5     Rhombohedral or trigonal
!           6     Tetragonal
!           7     Body Centred Tetragonal
!           8     Orthorhombic
!          12     Monoclinic (1 angle different from 90)
!          14     Triclinic (3 angles different from 90)
! ----------------------------------------------------------------------

SUBROUTINE latgen ( ibrav, celldm, a1, a2, a3, omega )
  
  IMPLICIT NONE
  
! Arguments
  INTEGER, INTENT ( IN ) :: ibrav
  REAL ( dbl ), INTENT ( IN ) :: celldm ( 6 )
  REAL ( dbl ), INTENT ( OUT ) :: a1 ( 3 ), a2 ( 3 ), a3 ( 3 ), omega
  
! Locals
  REAL ( dbl ) :: term, cbya, term1, term2, sin, singam, s
  INTEGER :: ir, i, j, k, iperm, l
  
  REAL ( dbl ), PARAMETER :: sr3 = 1.73205080756887729352_dbl
  
!------------------------------------------------------------------------------
  
  a1 ( : ) = 0.0_dbl
  a2 ( : ) = 0.0_dbl
  a3 ( : ) = 0.0_dbl
  
  IF ( celldm(1) == 0 ) &
       CALL STOPGM('LATGEN','THE LATTICE CONSTANT IS NULL!')
  
  IF ( ibrav == 1 ) THEN         ! Simple Cubic
     a1(1) = celldm(1)
     a2(2) = celldm(1)
     a3(3) = celldm(1)
     
  ELSE IF ( ibrav == 2) THEN     !face Centered Cubic == 
     term = celldm(1)/2.0_dbl
     a1(1) = -term
     a1(3) = term
     a2(2) = term
     a2(3) = term
     a3(1) = -term
     a3(2) = term
     
  ELSE IF ( ibrav == 3) THEN     !Body Centered Cubic == 
     term = celldm(1)/2.0_dbl
     do ir = 1,3
        a1(ir) = term
        a2(ir) = term
        a3(ir) = term
     end do
     a2(1) = -term
     a3(1) = -term
     a3(2) = -term
     
  ELSE IF ( ibrav == 4) THEN     !Hexagonal == 
     IF ( celldm(3) == 0.0_dbl) &
          CALL stopgm('latgen', 'c/a (third argument) is null')
     cbya = celldm(3)
     a1(1) = celldm(1)
     a2(1) = -celldm(1)/2.0_dbl
     a2(2) = celldm(1)*sr3/2.0_dbl
     a3(3) = celldm(1)*cbya
     
  ELSE IF ( ibrav == 5) THEN     !Trigonal or rhombohedral == 
     IF ( celldm(4) == 1.0_dbl) &
          CALL stopgm('latgen','the angle is null!')
     term1 = SQRT(1.0_dbl+2.0_dbl*celldm(4))
     term2 = SQRT(1.0_dbl-celldm(4))
     a2(2) = 1.414214_dbl*celldm(1)*term2/sr3
     a2(3) = celldm(1)*term1/sr3
     a1(1) = celldm(1)*term2/1.414214_dbl
     a1(2) = -a1(1)/sr3
     a1(3) = a2(3)
     a3(1) = -a1(1)
     a3(2) = a1(2)
     a3(3) = a2(3)
     
  ELSE IF ( ibrav == 6) THEN     !Tetragonal == 
     IF ( celldm(3) == 0.0_dbl) &
          CALL stopgm('latgen', 'c/a (third argument) is null')
     cbya = celldm(3)
     a1(1) = celldm(1)
     a2(2) = celldm(1)
     a3(3) = celldm(1)*cbya
     
  ELSE IF ( ibrav == 7) THEN     !Body Centred Tetragonal == 
     cbya = celldm(3)
     a2(1) = celldm(1)/2.0_dbl
     a2(2) = a2(1)
     a2(3) = cbya*celldm(1)/2.0_dbl
     a1(1) = a2(1)
     a1(2) = -a2(1)
     a1(3) = a2(3)
     a3(1) = -a2(1)
     a3(2) = -a2(1)
     a3(3) = a2(3)
     
  ELSE IF ( ibrav == 8) THEN     !Orthorhombic == 
     IF ( celldm(2) == 0.0_dbl) &
          call stopgm('latgen', 'the second argument is null')
     IF ( celldm(3) == 0.0_dbl) &
          call stopgm('latgen', 'the third argument is null')
     A1(1) = celldm(1)
     A2(2) = celldm(1)*celldm(2)
     A3(3) = celldm(1)*celldm(3)
     
  ELSE IF ( ibrav == 12) THEN    !Monoclinic (1 angle different from 90) == 
     IF ( celldm(2) == 0.0_dbl) &
          CALL stopgm('latgen', 'the second argument is null')
     IF ( celldm(3) == 0.0_dbl) &
          CALL stopgm('latgen', 'the third argument is null')
     IF ( celldm(4) == 1.0_dbl) &
          CALL stopgm('latgen','the angle is null!')
     SIN = SQRT(1.0_dbl-celldm(4)**2)
     A1(1) = celldm(1)
     A2(1) = celldm(1)*celldm(2)*celldm(4)
     A2(2) = celldm(1)*celldm(2)*SIN
     A3(3) = celldm(1)*celldm(3)
     
  ELSE IF ( ibrav == 14) THEN    !Triclinic (3 angles different from 90) == 
     IF ( celldm(2) == 0.0_dbl) &
          CALL stopgm('latgen', 'the second argument is null')
     IF ( celldm(3) == 0.0_dbl) &
          CALL stopgm('latgen', 'the third argument is null')
     IF ( celldm(4) == 1.0_dbl) &
          CALL stopgm('latgen','the first angle is null!')
     IF ( celldm(5) == 1.0_dbl) &
          CALL stopgm('latgen','the second angle is null!')
     IF ( celldm(6) == 1.0_dbl) &
          CALL stopgm('latgen','the third angle is null!')
!       Compatiblity between angle
     IF ( ACOS(celldm(4))+ACOS(celldm(5)) < ACOS(celldm(6)) ) &
          CALL stopgm('latgen','alpha + beta > = gamma')
     IF ( ACOS(celldm(5))+ACOS(celldm(6)) < ACOS(celldm(4)) ) &
          CALL stopgm('latgen','beta + gamma > = alpha')
     IF ( ACOS(celldm(6))+ACOS(celldm(4)) < ACOS(celldm(5)) ) &
          CALL stopgm('latgen','gamma + alpha > = beta')
     singam = SQRT ( 1.0_dbl - celldm(6) ** 2 )
     a1(1) = celldm(1)
     a2(1) = celldm(1)*celldm(2)*celldm(6)
     a2(2) = celldm(1)*celldm(2)*singam
     a3(1) = celldm(1)*celldm(3)*celldm(5)
     a3(2) = celldm(1)*celldm(3)*(celldm(4)-celldm(5)*celldm(6))/singam
     term = SQRT((1.0_dbl+2.0_dbl*celldm(4)*celldm(5)*celldm(6) &
          -celldm(4)**2-celldm(5)**2 &
          -celldm(6)**2)/(1.0_dbl-celldm(6)**2))
     A3(3) = celldm(1)*celldm(3)*TERM
     
! Not defined ibrav numbers (9, 10, 11, 13)
  ELSE IF ( ibrav == 9 .OR. ibrav == 10 &
       .OR. ibrav == 11 .OR. ibrav == 13 ) THEN
     WRITE(*,'(A,I3,A)') &
          ' bravais lattice', ibrav, ' not programmed. stopping'
     CALL stopgm ( 'latgen', ' ' )
  END IF
  
! Compute the volume of the Supercell (OMEGA)
  omega = 0.0_dbl
  s = 1.0_dbl
  i = 1
  j = 2
  k = 3
101 CONTINUE
  DO iperm = 1,3
     omega = omega+s*a1(i)*a2(j)*a3(k)
     l = i
     i = j
     j = k
     k = l
  END DO
  i = 2
  j = 1
  k = 3
  s = -s
  IF ( s < 0.0_dbl) GO TO 101
  
  omega = ABS ( omega )
  
END SUBROUTINE latgen

!******************************************************************************

! ----------------------------------------------------------------------
! subroutine recips
!   Generates the reciprocal lattice vectors b1,b2,b3 given the
!   real space vectors a1,a2,a3.  b in units of 2pi/a.
! ----------------------------------------------------------------------

SUBROUTINE recips ( a, a1, a2, a3, b1, b2, b3 )
  
  IMPLICIT NONE
  
! Arguments
  REAL ( dbl ), INTENT ( IN ) :: a, a1(3), a2(3), a3(3)
  REAL ( dbl ), INTENT ( OUT ) :: b1(3),b2(3),b3(3)
  
! Locals
  REAL ( dbl ) :: den, s
  INTEGER :: i,j,k,l,iperm,ir
  
!------------------------------------------------------------------------------
  
  den = 0.0_dbl
  i = 1
  j = 2
  k = 3
  s = 1.0_dbl
1 CONTINUE
  DO iperm = 1,3
     den = den+s*a1(i)*a2(j)*a3(k)
     l = i
     i = j
     j = k
     k = l
  END DO
  i = 2
  j = 1
  k = 3
  s = -s
  IF ( s < 0.0_dbl) GO TO 1
  i = 1
  j = 2
  k = 3
  den = a / abs ( den )
  DO ir = 1, 3
     b1(ir) = den*(a2(j)*a3(k)-a2(k)*a3(j))
     b2(ir) = den*(a3(j)*a1(k)-a3(k)*a1(j))
     b3(ir) = den*(a1(j)*a2(k)-a1(k)*a2(j))
     l = i
     i = j
     j = k
     k = l
  END DO
  
END SUBROUTINE recips

!******************************************************************************

! ----------------------------------------------------------------------
! counts the number of g-vectors
! ----------------------------------------------------------------------

SUBROUTINE gcount ( gcut, gcutw, b1, b2, b3, ngw, nhg, nr1, nr2, nr3 )
  
  IMPLICIT NONE
  
! Arguments
  REAL ( dbl ), INTENT ( IN ) :: gcut, gcutw
  REAL ( dbl ), INTENT ( IN ) :: b1 ( 3 ), b2 ( 3 ), b3 ( 3 )
  INTEGER, INTENT ( OUT ) :: ngw, nhg
  INTEGER, INTENT ( IN ) :: nr1, nr2, nr3
  
! Locals
  REAL ( dbl ) :: g2, t, sign
  INTEGER:: ig, i, j, k, jmin, jmax, kmin, kmax, ir
  
!------------------------------------------------------------------------------
  
! count g-vectors
  ig = 0
  ngw = 0
  nhg = 0
  DO i = 0,nr1-1
     jmin = -nr2+1
     jmax = nr2-1
     IF ( i == 0 ) jmin = 0
     DO j = jmin,jmax
        kmin = -nr3+1
        kmax = nr3-1
        IF ( i == 0 .AND. j == 0 ) kmin = 0
        DO k = kmin,kmax
           g2 = 0.0_dbl
           DO ir = 1,3
              t = DBLE(i)*b1(ir)+DBLE(j)*b2(ir)+DBLE(k)*b3(ir)
              g2 = g2+t*t
           END DO
           IF ( g2 < gcut ) THEN
              ig = ig+1
              nhg = nhg+1
              sign = +1.0_dbl
              IF ( g2 < gcutw ) THEN
                 ngw = ngw + 1
                 sign = -1.0_dbl
              END IF
           END IF
        END DO
     END DO
  END DO
  
END SUBROUTINE gcount

!******************************************************************************

! ----------------------------------------------------------------------
! builds and sorts the g-vectors and mappings
! ----------------------------------------------------------------------

SUBROUTINE gmake ( gcut, gcutw, b1, b2, b3, hg, inyh, &
     nh1, nh2, nh3, nr1, nr2, nr3, ngw, nhg, epsg )
  
  IMPLICIT NONE
  
! Arguments
  REAL ( dbl ), INTENT ( IN ) :: gcut, gcutw
  REAL ( dbl ), INTENT ( IN ) :: b1 ( 3 ), b2 ( 3 ), b3 ( 3 )
  REAL ( dbl ), DIMENSION ( : ), POINTER :: hg
  INTEGER, DIMENSION ( :, : ), POINTER :: inyh
  INTEGER, INTENT ( OUT ) :: nh1, nh2, nh3
  INTEGER, INTENT ( IN ) :: nr1, nr2, nr3
  INTEGER, INTENT ( OUT ) :: ngw
  INTEGER, INTENT ( INOUT ) :: nhg
  REAL ( dbl ), INTENT ( OUT ) :: epsg
  
! Locals
  INTEGER :: ig, i, j, k, jmin, jmax, kmin, kmax, ir, indy1, indy2, indy3
  REAL ( dbl ) :: g2, t, t1, t2, t3, sign
  
!------------------------------------------------------------------------------
  
  epsg = gcut * epsgx
  
!   count g-vectors
  
  nh1 = nr1 / 2 + 1
  nh2 = nr2 / 2 + 1
  nh3 = nr3 / 2 + 1
  
  ig = 0
  ngw = 0
  nhg = 0
  DO i = 0, nr1 - 1
     jmin = -nr2 + 1
     jmax = +nr2 - 1
     IF ( i == 0 ) jmin = 0
     DO j = jmin, jmax
        kmin = -nr3 + 1
        kmax = +nr3 - 1
        IF ( i == 0 .AND. j == 0 ) kmin = 0
        DO k = kmin, kmax
           t1=DBLE(i)*b1(1)+DBLE(j)*b2(1)+DBLE(k)*b3(1)
           t2=DBLE(i)*b1(2)+DBLE(j)*b2(2)+DBLE(k)*b3(2)
           t3=DBLE(i)*b1(3)+DBLE(j)*b2(3)+DBLE(k)*b3(3)
           g2=t1*t1+t2*t2+t3*t3
           
           IF ( g2 < gcut ) THEN
              ig = ig + 1
              nhg = nhg + 1
              sign = +1.0_dbl
              IF ( g2 < gcutw ) THEN
                 ngw = ngw + 1
                 sign = -1.0_dbl
              END IF
              hg ( ig ) = g2 + DSQRT ( DBLE ( ig - 1 ) ) * epsg * sign
              inyh(1,ig) = nh1 + i
              inyh(2,ig) = nh2 + j
              inyh(3,ig) = nh3 + k
           END IF
           
        END DO
     END DO
  END DO
  
  CALL gsort ( hg, inyh, nhg )
  
  CALL aliasing ( inyh, nh1, nh2, nh3, nr1, nr2, nr3, nhg )
  
  DO ig = 1,nhg
     i = inyh(1,ig)-nh1
     j = inyh(2,ig)-nh2
     k = inyh(3,ig)-nh3
     t1 = DBLE(i)*b1(1)+DBLE(j)*b2(1)+DBLE(k)*b3(1)
     t2 = DBLE(i)*b1(2)+DBLE(j)*b2(2)+DBLE(k)*b3(2)
     t3 = DBLE(i)*b1(3)+DBLE(j)*b2(3)+DBLE(k)*b3(3)
     hg(ig) = t1*t1+t2*t2+t3*t3
  END DO
  
END SUBROUTINE gmake

!******************************************************************************

! ----------------------------------------------------------------------
! Reorder the Gs in order of increasing magnitude
! ----------------------------------------------------------------------

SUBROUTINE gsort ( hg, inyh, nhg )
  
  IMPLICIT NONE
  
! Arguments
  REAL ( dbl ), DIMENSION ( : ), INTENT ( INOUT ) :: hg
  INTEGER, DIMENSION ( :, : ), INTENT ( INOUT ) :: inyh
  INTEGER, INTENT ( IN ) :: nhg
  
! Variables
  INTEGER, DIMENSION (:), ALLOCATABLE :: indx (:)
  INTEGER :: ig, icurr, it, ierror
  
!------------------------------------------------------------------------------
  
  ALLOCATE(indx(nhg),STAT=ierror)
  IF (ierror/=0) CALL STOPGM('gsort','Allocation problem')
!  CALL kb07ad ( hg, nhg, indx )
  CALL quicksort ( hg, nhg, indx )
  
  outer_loop: DO ig = 1, nhg - 1
     icurr = ig
     inner_loop: DO
        IF ( indx(icurr) /= ig ) THEN
           it = inyh(1,icurr)
           inyh(1,icurr) = inyh(1,indx(icurr))
           inyh(1,indx(icurr)) = it
           it = inyh(2,icurr)
           inyh(2,icurr) = inyh(2,indx(icurr))
           inyh(2,indx(icurr)) = it
           it = inyh(3,icurr)
           inyh(3,icurr) = inyh(3,indx(icurr))
           inyh(3,indx(icurr)) = it
           it = icurr
           icurr = indx(icurr)
           indx(it) = it
           IF ( indx(icurr) == ig ) THEN
              indx(icurr) = icurr
              CYCLE outer_loop
           END IF
        ELSE
           EXIT inner_loop
        END IF
     END DO inner_loop
  END DO outer_loop
  DEALLOCATE(indx,STAT=ierror)
  IF (ierror/=0) CALL STOPGM('gsort','Deallocation problem')
  
END SUBROUTINE gsort

!******************************************************************************

SUBROUTINE aliasing ( inyh, nh1, nh2, nh3, nr1, nr2, nr3, nhg )
  
  IMPLICIT NONE
  
! Arguments
  INTEGER, DIMENSION ( :, : ), INTENT ( IN ) :: inyh
  INTEGER, INTENT ( OUT ) :: nh1, nh2, nh3
  INTEGER, INTENT ( IN ) :: nr1, nr2, nr3
  INTEGER, INTENT ( IN ) :: nhg
  
! Variables
  INTEGER :: if1, if2, if3, ig, iri1, iri2, iri3, iff
  
!------------------------------------------------------------------------------
  
  nh1 = nr1/2+1
  nh2 = nr2/2+1
  nh3 = nr3/2+1
  if1 = 0
  if2 = 0
  if3 = 0
  DO ig = 1,nhg
     iri1 = inyh(1,ig)
     iri2 = inyh(2,ig)
     iri3 = inyh(3,ig)
     IF ( iri1 < 1 )   if1 = MAX(if1,ABS(iri1+1))
     IF ( iri2 < 1 )   if2 = MAX(if2,ABS(iri2+1))
     IF ( iri3 < 1 )   if3 = MAX(if3,ABS(iri3+1))
     IF ( iri1 > nr1 ) if1 = MAX(if1,iri1-nr1)
     IF ( iri2 > nr2 ) if2 = MAX(if2,iri2-nr2)
     IF ( iri3 > nr3 ) if3 = MAX(if3,iri3-nr3)
  END DO
  
  IF ( if1 > 0) WRITE(*,'(a,i3,a,i3,a)') &
       ' set mesh parameter nr1 to ',nr1+2*if1, &
       ' (current ',nr1,')'
  IF ( if2 > 0) WRITE(*,'(a,i3,a,i3,a)') &
       ' set mesh parameter nr2 to ',nr2+2*if2, &
       ' (current ',nr2,')'
  IF ( if3 > 0) WRITE(*,'(a,i3,a,i3,a)') &
       ' set mesh parameter nr3 to ',nr3+2*if3, &
       ' (current ',nr3,')'
  iff = if1+if2+if3
  IF ( iff > 0) THEN
     WRITE(*,'(a,3i4)') ' current mesh: ',nr1,nr2,nr3
     WRITE(*,'(a)') &
          ' use option mesh in system section'
     CALL stopgm('aliasing','mesh too small')
  END IF
  
END SUBROUTINE aliasing

!******************************************************************************

SUBROUTINE stopgm ( a, b )
  
  IMPLICIT NONE
  
! Arguments
  CHARACTER ( LEN = * ), INTENT ( IN ) :: a, b
  
!------------------------------------------------------------------------------
  
  WRITE ( *, '( /, /, 4A )' ) " PROGRAM STOPS IN SUBROUTINE ", a, "| ", b
  
  STOP 999
  
END SUBROUTINE stopgm

!******************************************************************************

END MODULE grids
