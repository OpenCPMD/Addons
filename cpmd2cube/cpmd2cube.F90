! hello emacs this is -*- f90 -*-
! read cpmd's Wannier-function files

MODULE cpmd2cube

  USE atom_types, ONLY : atom_type
  USE fft_tools, ONLY : fftchk, fwdfft, invfft
  USE grid_types, ONLY : grid_type, grid_init
  USE grids, ONLY : grid_form
  USE kinds, ONLY : dbl, sgl, bohrang, pi
  USE periodic_table, ONLY : init_periodic_table, ptable
  USE readwrites, ONLY : read_density_file, write_pdb, write_cube, &
       write_Bader_density, READ_ALL
  USE util, ONLY : wstrlen, file_exists, my_getarg

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: cpmd2cube_print

  CHARACTER ( LEN = 24 ), PARAMETER :: symm ( 0:14 ) = (/ &
       "ISOLATED                ", "SIMPLE CUBIC            ", &
       "FACE CENTRED CUBIC      ", "BODY CENTRED CUBIC      ", &
       "HEXAGONAL               ", "TRIGONAL or RHOMBOHEDRAL", &
       "TETRAGONAL              ", "BODY CENTRED TETRAGONAL ", &
       "ORTHORHOMBIC            ", "!!!ILLEGAL VALUE!!!     ", &
       "!!!ILLEGAL VALUE!!!     ", "!!!ILLEGAL VALUE!!!     ", &
       "MONOCLINIC              ", "!!!ILLEGAL VALUE!!!     ", &
       "TRICLINIC               "  /)

CONTAINS

!******************************************************************************

SUBROUTINE cpmd2cube_print ( )
  
  IMPLICIT NONE
  
! Locals
  CHARACTER ( LEN = 64 ) :: filesuff, sf, ss
  CHARACTER ( LEN = 132 ) :: header
  CHARACTER ( LEN = 80 ) :: fname_out_basis
  CHARACTER ( LEN = 128 ) :: fname_in, fname_pdb, fname_cube, fname_Bader
  CHARACTER :: stored
  LOGICAL :: singleprecision, inbox, shift2com, wave, dens, write_Bader
  LOGICAL :: halfmesh, normpot, nocoords
  INTEGER :: nrep ( 3 ), iarg, argc, nn ( 3 ), arg, ll ( 3 ), i, j, k
  INTEGER :: ibrav, nhg, nr1, nr2, nr3, allocstatus, info
  REAL ( dbl ) :: shft ( 3 ), origin ( 3 ), cbox ( 3 ), integral, celltrans(3)
  REAL ( dbl ) :: h_mat ( 3, 3 ), h_inv ( 3, 3 ), c0_sum, trimthresh
  REAL ( dbl ) :: celldm ( 6 ), gcut, gcutw, spacing ( 3, 3 ), ecut, tpiba
  TYPE ( atom_type ) :: atom
  TYPE ( grid_type ) :: grid
  
  REAL ( dbl ), DIMENSION ( :, :, : ), ALLOCATABLE :: rho
  COMPLEX ( dbl ), DIMENSION ( :, :, : ), ALLOCATABLE :: cfft
  
  COMPLEX ( dbl ), DIMENSION ( : ), POINTER :: c0
  
!------------------------------------------------------------------------------
  
! misc initialisations
  WRITE ( *, * )
  
  CALL args ( iarg, argc, nn, nrep, shft, singleprecision, fname_out_basis, info, &
    inbox, shift2com, wave, dens, write_Bader, halfmesh, trimthresh, normpot, nocoords)
  
  IF ( argc <= 0 ) THEN
     WRITE ( *, '( "need at least one arg (", I2, " passed)" )' ) argc
     CALL write_help ( 1 )
  END IF
  
  CALL nullify_pointers

  CALL init ( iarg, argc )
  
  CALL grid_init ( grid )
  celltrans ( : ) = 0.0d0
  
! read files
  DO arg = iarg, iarg + argc - 1
     
     CALL my_getarg ( arg, fname_in )
     
     CALL get_suffix ( fname_in, filesuff, sf )
     
     IF( info >= 10 ) WRITE ( *, '( ".read input file ... ", $ )' )
     CALL read_density_file ( fname_in, c0, singleprecision, atom, &
          ibrav, celldm, nr1, nr2, nr3, gcut, gcutw, nhg, &
          celltrans, key = READ_ALL )
     IF( info >= 10 ) WRITE ( *, '( "done" )' )
     
     c0_sum = 2.0_dbl * SUM ( c0 ( 2: ) * CONJG ( c0 ( 2: ) ) ) &
          + REAL ( c0 ( 1 ) * CONJG ( c0 ( 1 ) ) )
     
! assume a wavefunction was stored
     IF ( ABS ( c0_sum - 1.0_dbl ) < 1.0E-3_dbl ) THEN
        stored = 'w'
        WRITE(*,'("I think you stored a wavefunction (sum^2 =",e16.8,")")') &
             c0_sum
     ELSE
        stored = 'r'
        WRITE(*,'("I think you stored a density (sum^2 =",e16.8,")")') c0_sum
     END IF
! AK: the heuristics from above fail for low cutoff calculations. so we
!     write this warning and continue anyway. if people treat not perfectly
!     normalized wavefunctions as densities the result may be even worse.
!     i've already seen it happen. brrr.
     IF ( stored == 'r' .AND. wave ) THEN
        WRITE ( *, '( "WARNING in file ", A )') TRIM ( fname_in )
        WRITE ( *, '( "Wannier-functions stored as density, you can not" )' )
        WRITE ( *, '( "   create a cube-file with the wavefunction" )' )
!        STOP
     END IF
     
! build g-vectors
     IF( info >= 10 ) WRITE ( *, '( ".building g vectors ... ", $ )' )
     
     CALL grid_form ( gcut, gcutw, ibrav, celldm, nr1, nr2, nr3, grid )
     
     IF ( grid % nhg /= nhg ) THEN
        WRITE ( *, '( /,' // &
             '"calculated and stored number of components do not agree" )' )
        WRITE ( *, '( "   calculated:", I9, ";   stored:", I9 )' ) &
             grid % nhg, nhg
        WRITE ( *, '( "   for file: ", A )' ) TRIM ( fname_in )
        STOP
     END IF
     
     IF( info >= 10 ) WRITE ( *, '( "done" )' )
     
! cell
     cbox ( : ) = 0.5_dbl * ( grid % a1 ( : ) + grid % a2 ( : ) + grid % a3 ( : ) )
     
     CALL pbc_setup ( grid % a1, grid % a2, grid % a3, h_mat, h_inv )
     
     CALL do_geom ( atom, cbox, origin, h_mat, h_inv, &
          inbox, info, shift2com , shft )
     
!! create pdb file
     IF ( LEN( TRIM( fname_out_basis ) ) == 0 ) THEN ! no -o option
        fname_pdb = TRIM ( fname_in ) // '.pdb'
     ELSE
        fname_pdb = TRIM ( fname_out_basis ) // TRIM ( sf ) // '.pdb'
     END IF
     
     IF( info >= 10 ) WRITE ( header, '( A, A )' ) &
          "pdb file created from cpmd Wannier file: ", &
          TRIM ( fname_in )
     
     CALL write_pdb ( fname_pdb, header, atom, &
          grid % a1, grid % a2, grid % a3, nrep )
     
     IF( info >= 10 ) WRITE ( *, '( ".handling file ", A)' ) TRIM ( fname_in )
     
!! compute density
     ALLOCATE ( cfft ( grid % nr1, grid % nr2, grid % nr3 ) )
     ALLOCATE ( rho ( grid % nr1, grid % nr2, grid % nr3 ) )
     
     CALL do_dens ( c0, cfft, origin, h_inv, grid, nn, ll )
     
     IF ( ASSOCIATED ( c0 ) ) THEN
        DEALLOCATE ( c0, STAT = allocstatus )
        IF ( allocstatus /= 0 ) STOP "cpmd2cube_print, error deallocating c0"
     END IF
     
     spacing ( 1, : ) = grid % a1 ( : ) / REAL ( nn ( 1 ) )
     spacing ( 2, : ) = grid % a2 ( : ) / REAL ( nn ( 2 ) )
     spacing ( 3, : ) = grid % a3 ( : ) / REAL ( nn ( 3 ) )
     
     integral = 0.0_dbl
     DO k = 1, grid % nr3
        DO j = 1, grid % nr2
           DO i = 1, grid % nr1
              integral = integral + ABS ( REAL ( cfft ( i, j, k ) ) )
           END DO
        END DO
     END DO
     WRITE ( *, * ) "Integral|| = ", integral, grid % volume, &
          grid % nr1 * grid % nr2 * grid % nr3
     
     integral = 0.0_dbl
     DO k = 1, grid % nr3
        DO j = 1, grid % nr2
           DO i = 1, grid % nr1
              integral = integral + cfft ( i, j, k )
           END DO
        END DO
     END DO
     WRITE ( *, * ) "Integral() = ", integral, grid % volume, &
          grid % nr1 * grid % nr2 * grid % nr3

     IF ( normpot ) THEN  ! correct for the integrated el. pot. != 0.0, if requested
          integral = integral / DBLE(grid % nr1 * grid % nr2 * grid % nr3)
          WRITE ( *, * ) "Correction for ES Potential:", integral
          cfft(:,:,:) = cfft(:,:,:) - integral
     ENDIF
     IF( stored == 'w' ) &     ! normalize so that \int_V dV |psi|^2 == 1
          cfft = cfft * SQRT( 1.0_dbl / grid % volume )
     IF ( stored == 'w' .AND. dens ) THEN
        rho = ABS ( cfft ) ** 2
     ELSE
        DO k = 1, grid % nr3
           DO j = 1, grid % nr2
              DO i = 1, grid % nr1
                 rho ( i, j, k ) = cfft ( i, j, k )
              END DO
           END DO
        END DO
     END IF
     
     DEALLOCATE ( cfft )
     
!*apsi* Take care of ll(:) HERE!!!
     
!! create cube file
     IF ( LEN( TRIM( fname_out_basis ) ) == 0 ) THEN ! no -o option
        fname_cube = TRIM ( fname_in ) // '.cube'
     ELSE
        fname_cube = TRIM ( fname_out_basis ) // TRIM ( filesuff ) // '.cube'
     END IF
     
     IF ( wave ) THEN
        ss = "WAVEFUNCTION"
     ELSE
        ss = "DENSITY"
     END IF
     
     WRITE ( header, '( A, A, A )' ) TRIM ( ss ), &
          ": cube file created from cpmd Wannier file: ", &
          TRIM ( fname_in )
     
     CALL write_cube ( fname_cube, header, atom, origin, rho, spacing,    &
          nn, nrep, halfmesh, grid % a1, grid % a2, grid % a3, trimthresh, &
          nocoords, celltrans )
     
!! create density file for the program with Bader zero-flux surface
     IF ( LEN( TRIM( fname_out_basis ) ) == 0 ) THEN ! no -o option
        fname_Bader = TRIM ( fname_in ) // '.flux'
     ELSE
        fname_Bader = TRIM ( fname_out_basis ) // TRIM ( filesuff ) // '.flux'
     END IF
     
     tpiba = 2.0_dbl * pi / celldm ( 1 )
     ecut = gcut * tpiba ** 2
     
     IF( write_Bader ) CALL write_Bader_density ( fname_Bader, rho, &
          grid % a1, grid % a2, grid % a3, ecut )
     
     DEALLOCATE( rho )
     
  END DO
  
  WRITE ( *, * )

  CONTAINS

  SUBROUTINE nullify_pointers()

    implicit none

    !
    ! if c0 is not nullified, allocation in read_write fails with recent Intel compilers (> 15.0)
    !
    nullify(c0)
    return

  END SUBROUTINE nullify_pointers
  
END SUBROUTINE cpmd2cube_print

!******************************************************************************

SUBROUTINE init ( iarg, argc )
  
  IMPLICIT NONE
  
! Arguments
  INTEGER, INTENT ( IN ) :: iarg, argc
  
! Locals
  
!------------------------------------------------------------------------------
  
  CALL init_periodic_table()
  
! check filenames
  CALL checknames ( iarg, argc )
  
END SUBROUTINE init

!******************************************************************************

SUBROUTINE do_geom ( atom, cbox, origin, h_mat, h_inv, &
     inbox, info, shift2com, shft )
  
  IMPLICIT NONE
  
! Arguments
  TYPE ( atom_type ), INTENT ( IN ), TARGET :: atom
  REAL ( dbl ), INTENT ( IN ) :: cbox ( 3 )
  REAL ( dbl ), INTENT ( INOUT ) :: shft ( 3 )
  REAL ( dbl ), INTENT ( IN ) :: h_mat ( 3, 3 ), h_inv ( 3, 3 )
  REAL ( dbl ), INTENT ( OUT ) :: origin ( 3 )
  LOGICAL, INTENT ( IN ) :: inbox, shift2com
  INTEGER, INTENT ( IN ) :: info
  
! Locals
  INTEGER :: ia, is
  REAL ( dbl ) :: m, mm
  REAL ( dbl ) :: com ( 3 )
  
  REAL ( dbl ), DIMENSION ( : ), POINTER :: tau
  
!------------------------------------------------------------------------------
  
  IF ( info >= 20 ) WRITE ( *, '( I6, / )' ) atom % nat
  
  com ( : ) = 0.0_dbl
  mm = 0.0_dbl


  IF ( shift2com ) THEN           ! centre box around centre of mass of system
     ! get center of mass  
     DO is = 1, atom % nsp
        m = ptable ( atom % iatyp ( is ) ) % mass
        DO ia = 1, atom % na ( is )
           tau => atom % tau0 ( :, ia, is )
           mm = mm + m
           com ( : ) = com ( : ) + m * tau ( : )
        END DO
     END DO

     com ( : ) = com ( : ) / mm
     IF ( info >= 20 ) THEN
        WRITE ( *, '( /, "centre of mass:", 3F9.4, " bohr = ", 3F9.4," Ang" )' ) &
             com ( : ), bohrang * com ( : )
        WRITE ( *, '( /, "additional shift:", 3F9.4 )' ) MATMUL( h_inv, com ) - 0.5_dbl
     END IF

     ! translate com into fractional coordinates and add to shift. we need
     ! to add a shift of (-0.5,-0.5,-0.5) to get the the proper origin.
     shft = shft + MATMUL ( h_inv, com ) - 0.5_dbl
  ENDIF

  origin ( : ) = MATMUL( h_mat, shft )
  IF ( info >= 20 ) THEN
     WRITE ( *, '( /, "origin:", 3F9.4, " bohr = ", 3F9.4," Ang" )' ) &
             origin ( : ), bohrang * origin ( : )
  ENDIF

  DO is = 1, atom % nsp
     DO ia = 1, atom % na ( is )
        tau => atom % tau0 ( :, ia, is )

        ! put atom inside unit cell 
        ! we need to add (0.5,0.5,0.5) to the shift, since we 
        ! want to wrap around the box center.
        IF ( inbox ) CALL pbc ( tau, h_mat, h_inv, shft + 0.5_dbl )
        
        IF ( info >= 20 ) WRITE ( *, '( A6, 3F9.4 )' ) &
             ptable ( atom % iatyp ( is ) ) % symbol, &
             bohrang * tau ( : )
     END DO
  END DO
  
  IF ( info > 20 ) STOP "do_geom: Stop due to info > 20"

END SUBROUTINE do_geom

!******************************************************************************

SUBROUTINE do_dens ( c0, cfft, origin, h_inv, grid, nn, ll )
  
  IMPLICIT NONE
  
! Arguments
  COMPLEX ( dbl ), DIMENSION ( : ), INTENT ( IN ) :: c0
  COMPLEX ( dbl ), DIMENSION ( :, :, : ), INTENT ( OUT ) :: cfft
  REAL ( dbl ), INTENT ( IN ) :: origin ( 3 )
  REAL ( dbl ), INTENT ( IN ) :: h_inv ( 3, 3 )
  TYPE ( grid_type ), INTENT ( IN ) :: grid
  INTEGER, INTENT ( INOUT ) :: nn ( 3 )
  INTEGER, INTENT ( OUT ) :: ll ( 3 )
  
! Locals
  INTEGER :: ip, jp, kp, ig, in, jn, kn, hh ( 3 ), i, j, k
  INTEGER :: old_nr1, old_nr2, old_nr3, nr1, nr2, nr3
  REAL ( dbl ) :: d, a(3), b(3), s(3)
  COMPLEX ( dbl ) :: z
  COMPLEX ( dbl ) :: z1 ( grid % nr1 ), z2 ( grid % nr2 ), z3 ( grid % nr3 )
  
!------------------------------------------------------------------------------
  
  nr1 = grid % nr1
  nr2 = grid % nr2
  nr3 = grid % nr3
  
  old_nr1 = nr1
  old_nr2 = nr2
  old_nr3 = nr3
  
! if grid was made bigger, simply change nr
  IF ( nn ( 1 ) > nr1 ) nr1 = nn ( 1 )
  IF ( nn ( 2 ) > nr2 ) nr2 = nn ( 2 )
  IF ( nn ( 3 ) > nr3 ) nr3 = nn ( 3 )
  
! if nn not specified, use nr
  IF ( nn ( 1 ) == 0 .OR. nn ( 2 ) == 0 .OR. nn ( 3 ) == 0 ) THEN
     nn ( 1 ) = nr1
     nn ( 2 ) = nr2
     nn ( 3 ) = nr3
  END IF
  
  WRITE ( *, '( "real-space grid was:", I4, 2I9, ";   using:", 3I9 )' ) &
       old_nr1, old_nr2, old_nr3, nn ( 1 ), nn ( 2 ), nn ( 3 )
  
  ll ( 1 ) = ( nr1 - nn ( 1 ) ) / 2 + 1
  ll ( 2 ) = ( nr2 - nn ( 2 ) ) / 2 + 1
  ll ( 3 ) = ( nr3 - nn ( 3 ) ) / 2 + 1
  
  hh ( 1 ) = ll ( 1 ) - 1 + nn ( 1 )
  hh ( 2 ) = ll ( 2 ) - 1 + nn ( 2 )
  hh ( 3 ) = ll ( 3 ) - 1 + nn ( 3 )
  
  s = - MATMUL ( h_inv, origin )
  
! shift cell by multiplying with phase factor in g-space
  CALL shiftfac ( z1, grid % nh1, ll ( 1 ), nn ( 1 ), s ( 1 ) )
  CALL shiftfac ( z2, grid % nh2, ll ( 2 ), nn ( 2 ), s ( 2 ) )
  CALL shiftfac ( z3, grid % nh3, ll ( 3 ), nn ( 3 ), s ( 3 ) )
  
  WRITE ( *, '(" .calculate density ... ", $ )' )
  
#if defined ( __IFC )
  cfft ( :, :, : ) = ( 0.0, 0.0 )
#else
  cfft ( :, :, : ) = ( 0.0_dbl, 0.0_dbl )
#endif
  
  IF ( nn ( 1 ) < nr1 .OR. nn ( 2 ) < nr2 .OR. nn ( 3 ) < nr3 ) THEN
     DO ig = 1, grid % nhg
        ip = grid % inyh(1,ig)
        jp = grid % inyh(2,ig)
        kp = grid % inyh(3,ig)
        IF ( ip >= ll ( 1 ) .AND. ip <= hh ( 1 ) .AND. &
             jp >= ll ( 2 ) .AND. jp <= hh ( 2 ) .AND. &
             kp >= ll ( 3 ) .AND. kp <= hh ( 3 ) ) THEN
           in = 2 * grid % nh1 - ip
           jn = 2 * grid % nh2 - jp
           kn = 2 * grid % nh3 - kp
           
           cfft ( ip, jp, kp ) = c0 ( ig ) * z1 ( ip ) * z2 ( jp ) * z3 ( kp )
           cfft ( in, jn, kn ) = CONJG ( cfft ( ip, jp, kp ) )
        END IF
     END DO
  ELSE
     DO ig = 1, grid % nhg
        ip = grid % inyh(1,ig)
        jp = grid % inyh(2,ig)
        kp = grid % inyh(3,ig)
        in = 2 * grid % nh1 - ip
        jn = 2 * grid % nh2 - jp
        kn = 2 * grid % nh3 - kp
        cfft ( ip, jp, kp ) = c0 ( ig ) * z1 ( ip ) * z2 ( jp ) * z3 ( kp )
        cfft ( in, jn, kn ) = CONJG ( cfft ( ip, jp, kp ) )
     END DO
  END IF
  
  CALL invfft ( cfft, nr1, nr2, nr3 )
  
  WRITE ( *, '( "done   " )' )
  
END SUBROUTINE do_dens

!******************************************************************************

! ----------------------------------------------------------------------
! set up phase factors to shift function by inverse fft
! shift is rounded to multiple of grid spacing
! zz: vector with phase vectors
! nr: number of grid point along current direction
! nz: index of G = 0 point
! f:  fraction of translational vector by which to shift
! ----------------------------------------------------------------------

SUBROUTINE shiftfac ( zz, nz, l, nr, f )
  
  IMPLICIT NONE
  
! Arguments
  INTEGER, INTENT ( IN ) :: nr                             ! number of points
  COMPLEX ( dbl ), DIMENSION ( nr ), INTENT ( OUT ) :: zz  ! phase factors
  INTEGER, INTENT ( IN ) :: nz                             ! position of zero
  INTEGER, INTENT ( IN ) :: l                         ! position of first value
  REAL ( dbl ), INTENT ( IN ) :: f                 ! fraction by which to shift
  
! Locals
  COMPLEX(dbl) :: z
  INTEGER :: i,j,p,h
  REAL ( dbl ) :: d
  
!------------------------------------------------------------------------------
  
  d = -2.0 * pi * REAL ( NINT ( f * nr ), dbl ) / REAL ( nr, dbl )
  z = CMPLX ( COS ( d ), SIN ( d ) )
  zz ( nz ) = 1.0_dbl
  p = nz
  h = l+nr-1
  
  DO i = 1,nr-1
     j = p + 1
     IF ( j > h ) j = l
     zz ( j ) = z * zz ( p )
     p = j
  END DO
  
END SUBROUTINE shiftfac

!******************************************************************************

! ----------------------------------------------------------------------
! finds suffix of the form ([0-9]+\.)?[0-9]+ in "filename"
!                          \__sf_/
!                          \___filesuff____/
! ----------------------------------------------------------------------

SUBROUTINE get_suffix ( filename, filesuff, sf )
  
  IMPLICIT NONE
  
! Arguments
  CHARACTER ( LEN = * ) :: filename, filesuff, sf
  
! Locals
  INTEGER :: ln, lp, j
  
!------------------------------------------------------------------------------
  
  filesuff = ''
  sf = ''
  ln = wstrlen ( filename )
  j = ln
  IF ( ln <= 0 ) THEN
     WRITE ( *, '( "wrong filename passed: ",a)') TRIM ( filename )
     STOP
  END IF
  
  DO WHILE ( j > 0 .AND. filename(j:j) >= '0' .AND. filename(j:j) <= '9' )
     j = j-1
  END DO
  
  IF ( j+1 > ln ) RETURN
  
  IF ( j == 0 .or. filename(j:j) /= '.' ) THEN
     filesuff = filename(j+1:ln)
     RETURN
  END IF
  
  lp = j
  j = j-1
  DO WHILE ( j > 0 .AND. filename(j:j) >= '0' .AND. filename(j:j) <= '9' )
     j = j-1
  END DO
  
  filesuff = filename(j+1:ln)
  sf = filename(j+1:lp-1)
  
END SUBROUTINE get_suffix

!******************************************************************************

SUBROUTINE checknames ( iarg, argc )
  
  IMPLICIT NONE
  
! Arguments
  INTEGER, INTENT(in) :: iarg, argc
  
! Local
  INTEGER :: jf
  CHARACTER ( LEN = 80 ) :: fname_in, filesuff, sf
  
!------------------------------------------------------------------------------
  
! check filenames
  DO jf = iarg, iarg + argc - 1
     CALL my_getarg ( jf, fname_in )
     
     IF ( .NOT. file_exists ( fname_in ) ) THEN
        WRITE ( *, '( "could not find file ",a)') TRIM ( fname_in )
        STOP
     END IF
     
     CALL get_suffix ( fname_in, filesuff, sf )
     
     IF ( wstrlen ( filesuff ) == 0 .AND. argc > 1 ) THEN
        WRITE ( *, '( "could not extract a suffix from file ",a)') &
             TRIM ( fname_in )
        WRITE ( *, '( "you therefore can only specify one file")')
        STOP
     END IF
     
  END DO
  
END SUBROUTINE checknames

!******************************************************************************

SUBROUTINE pbc_setup ( a1, a2, a3, h_mat, h_inv )
  
  IMPLICIT NONE
  
! Arguments
  REAL ( dbl ), INTENT ( IN ) :: a1 ( 3 ), a2 ( 3 ), a3 ( 3 )
  REAL ( dbl ), INTENT ( OUT ) :: h_mat ( 3, 3 ), h_inv ( 3, 3 )
  
! Locals
  REAL ( dbl ) :: id, det
  
!------------------------------------------------------------------------------
  
  h_mat(:,1) = a1(:)
  h_mat(:,2) = a2(:)
  h_mat(:,3) = a3(:)
  
  det = a1(1)*a2(2)*a3(3) + a2(1)*a3(2)*a1(3) + a3(1)*a1(2)*a2(3) &
       - a1(1)*a3(2)*a2(3) - a2(1)*a1(2)*a3(3) - a3(1)*a2(2)*a1(3)
  id = 1.0_dbl / det
  h_inv(1,1) = id * ( a2(2)*a3(3)-a2(3)*a3(2) )
  h_inv(2,1) = id * ( a1(3)*a3(2)-a1(2)*a3(3) )
  h_inv(3,1) = id * ( a1(2)*a2(3)-a1(3)*a2(2) )
  h_inv(1,2) = id * ( a2(3)*a3(1)-a2(1)*a3(3) )
  h_inv(2,2) = id * ( a1(1)*a3(3)-a1(3)*a3(1) )
  h_inv(3,2) = id * ( a1(3)*a2(1)-a1(1)*a2(3) )
  h_inv(1,3) = id * ( a2(1)*a3(2)-a2(2)*a3(1) )
  h_inv(2,3) = id * ( a1(2)*a3(1)-a1(1)*a3(2) )
  h_inv(3,3) = id * ( a1(1)*a2(2)-a1(2)*a2(1) )
  
END SUBROUTINE pbc_setup

!******************************************************************************

SUBROUTINE mat3v ( M, v, r )
  
! Arguments
  REAL ( dbl ), INTENT ( IN ) :: M(3,3), v(3)
  REAL ( dbl ), INTENT ( OUT ) :: r(3)
  
! Locals
  REAL ( dbl ) :: w ( 3 )       ! use temporary w in case v and r coincide
  
!------------------------------------------------------------------------------
  
  w(1) = M(1,1)*v(1)+M(1,2)*v(2)+M(1,3)*v(3)
  w(2) = M(2,1)*v(1)+M(2,2)*v(2)+M(2,3)*v(3)
  w(3) = M(3,1)*v(1)+M(3,2)*v(2)+M(3,3)*v(3)
  r(:) = w(:)
  
END SUBROUTINE mat3v

!******************************************************************************

SUBROUTINE pbc ( r, h_mat, h_inv, shft )
  
  IMPLICIT NONE
  
! Arguments
  REAL ( dbl ), INTENT ( INOUT ) :: r ( 3 )
  REAL ( dbl ), INTENT ( IN ) :: h_mat ( 3, 3 ), h_inv ( 3, 3 )
  REAL ( dbl ), INTENT ( IN ) :: shft ( 3 )
  
! Locals
  REAL ( dbl ) :: s(3)
  INTEGER :: i
  
!------------------------------------------------------------------------------
  
  s = MATMUL ( h_inv,  r ) - shft
  s = s - NINT ( s )
  r = MATMUL ( h_mat, s + shft)
  
END SUBROUTINE pbc

!******************************************************************************

SUBROUTINE args ( iarg, argc, nn, nrep, shft, singleprecision, &
     fname_out_basis, info, inbox, shift2com, wave, dens, write_Bader, &
     halfmesh, trimthresh, normpot, nocoords)
  
  IMPLICIT NONE
  
! Arguments
  LOGICAL, INTENT ( OUT ) :: singleprecision
  INTEGER :: nn ( 3 ), nrep ( 3), iarg, argc
  REAL ( dbl ) :: shft(3), trimthresh
  CHARACTER ( LEN = * ) :: fname_out_basis
  LOGICAL, INTENT ( OUT ) :: inbox, shift2com, wave, dens, write_Bader, halfmesh, normpot, nocoords
  INTEGER, INTENT ( OUT ) :: info
  
! Locals
  INTEGER :: iargc

  INTEGER :: j
  LOGICAL :: args_left
  CHARACTER ( LEN = 80 ) :: str
  
!------------------------------------------------------------------------------

#if defined(__GFORTRAN)
  argc = COMMAND_ARGUMENT_COUNT()
#else  
  argc = iargc()
#endif

  iarg = 1
  
  fname_out_basis = ''        ! default values
  args_left = .TRUE.
  singleprecision = .TRUE.
  info = 10
  inbox = .FALSE.
  nocoords = .FALSE.
  shift2com = .FALSE.
  wave = .TRUE.
  dens = .FALSE.
  write_Bader = .FALSE.
  halfmesh = .TRUE.
  normpot  = .FALSE.
  nn ( : ) = (/ 0, 0, 0 /)
  nrep ( : ) = (/ 1, 1, 1 /)
  shft ( : ) = (/ 0.0_dbl, 0.0_dbl, 0.0_dbl /)
  trimthresh = -1.0_dbl
  
  DO WHILE ( iarg <= argc .AND. args_left )
     
     CALL my_getarg ( iarg, str )
     
     IF ( str == '-n' ) THEN       ! change mesh
        ! AK: enlarging the mesh segfaults and
        !     with a smaller mesh the density does not fit inside the box.
        !     as an alternative, we can simply skip writing the mesh points
        !     while writing the cubefile (as done in cpmd/proppt.F/util_p.F).
        WRITE ( *, '( "ERROR: changing the size of the real space grid &
             &via fft does not work currently." )' )
        STOP 'use -fullmesh/-halfmesh to use all/half the grid points in each direction'

        IF ( argc-iarg<3 ) THEN
           WRITE ( *, '( "ERROR: please specify three int args after -n option" )' )
           STOP
        END IF
        
        DO j = 1, 3
           CALL my_getarg ( iarg + j, str )
           READ ( str, * ) nn ( j )
        END DO
        iarg = iarg + 4
        
     ELSE IF ( str == '-v' ) THEN ! specify verbosity
        CALL my_getarg ( iarg + 1, str )
        READ ( str, * ) info
        SELECT CASE( info )
        CASE( 0 )
           info = 0
        CASE( 1 )
           info = 10
        CASE( 2 )
           info =20
        CASE( 3 )
           info =30
        CASE default
           WRITE( *, '("Illegal value passed to option -v")')
           STOP
        END SELECT
        iarg = iarg + 2
        
     ELSE IF ( str == '-double' ) THEN ! change mesh
        singleprecision = .FALSE.
        iarg = iarg + 1
        
     ELSE IF ( str == '-halfmesh' ) THEN ! change mesh when writing the cubefile
        halfmesh = .TRUE.
        iarg = iarg + 1
        
     ELSE IF ( str == '-fullmesh' ) THEN ! use full mesh.
        halfmesh = .FALSE.
        iarg = iarg + 1
        
     ELSE IF ( str == '-normpot' ) THEN ! correct electrostatic potential
        normpot = .TRUE.
        iarg = iarg + 1
        
     ELSE IF ( str == '-nocoords' ) THEN ! don't write coordinates
        nocoords = .TRUE.
        iarg = iarg + 1
        
     ELSE IF ( str == '-o' ) THEN  ! prefix of output files
        IF ( argc - iarg < 1 ) THEN
           WRITE ( *, '( "ERROR: option -o <outputfile> needs one arg")' )
           STOP
        END IF
        
        CALL my_getarg ( iarg + 1, fname_out_basis )
        iarg = iarg + 2
        
     ELSE IF ( str == '-rep' ) THEN ! replicate cell and atoms by periodicity
        IF ( argc-iarg<3 ) THEN
           WRITE ( *, '( "ERROR: need three int args after -rep option")')
           STOP
        END IF
        DO j = 1, 3
           CALL my_getarg ( iarg + j, str )
           READ ( str, * ) nrep ( j )
           IF ( nrep ( j ) <= 0 .OR. nrep ( j ) > 4 ) THEN
              WRITE ( *, '( "illegal value for option -rep (",i4,")")') &
                   nrep ( j)
              STOP
           END IF
        END DO
        iarg = iarg+4
        
     ELSE IF ( str == '-shift' ) THEN ! shift unit cell
        IF ( argc-iarg<3 ) THEN
           WRITE ( *, '( "ERROR: need three real args after -shift option")')
           STOP
        END IF
        DO j = 1,3
           CALL my_getarg ( iarg + j, str )
           READ ( str, * ) shft(j)
           IF ( ABS ( shft ( j ) ) > 10.0_dbl ) THEN
              WRITE ( *, &
                   '( "illegal value for option -shift (", F9.4, ")" )' ) &
                   shft ( j )
              STOP
           END IF
        END DO
        iarg = iarg+4

! trim cubefile to minimal size        
     ELSE IF ( str == '-trim' ) THEN 
        IF ( argc-iarg<1 ) THEN
           WRITE ( *, '( "ERROR: need one real arg after -trim option")')
           STOP
        END IF
        CALL my_getarg ( iarg + 1, str )
        READ ( str, * ) trimthresh
        IF ( trimthresh <= 0.0_dbl ) THEN
              WRITE ( *, &
                   '( "illegal value for option -trim ", F9.4, ". must be > 0.0" )' ) &
                   trimthresh
              STOP
           END IF
        iarg = iarg+2
        
! write Bader density
     ELSE IF ( str == '-bader' ) THEN
        write_Bader = .TRUE.
        iarg = iarg+1
        
! put atoms inside simulation cell
     ELSE IF ( str == '-inbox' ) THEN
        inbox = .TRUE.
        iarg = iarg+1
        
! do NOT put density around com of atoms
     ELSE IF ( str == '-centre' .or. str == '-center' ) THEN
        shift2com = .TRUE.
        iarg = iarg+1
        
! read density instead of wavefunction
     ELSE IF ( str == '-rho' .or. str == '-dens' ) THEN
        wave = .FALSE.
        dens = .TRUE.
        iarg = iarg+1
        
! read wavefunction instead of density
     ELSE IF ( str == '-wave' .or. str == '-psi' ) THEN
        wave = .TRUE.
        dens = .FALSE.
        iarg = iarg + 1
        
! no options follow this
     ELSE IF ( str == '--' ) THEN
        iarg = iarg+1
        args_left = .FALSE.
        
! get some help
     ELSE IF ( str == '-?' .OR. str == '-h' .OR. &
          str == '-help' .OR. str == '--help' ) THEN
        call write_help ( 0 )
        
     ELSE                      ! found no option
        args_left = .FALSE.
     END IF
     
  END DO

! sanity check
  IF ( (trimthresh > 0.0) .AND. ( ( nrep(1) /=  1 ) .OR. &
       ( nrep(2) /=  1 ) .OR.( nrep(3) /=  1 ))) THEN
     WRITE(*, '("options -trim and -rep are incompatible.")')
     STOP
  END IF
  argc = argc - iarg + 1
  
END SUBROUTINE args

!******************************************************************************

SUBROUTINE write_help ( exval )
  
  IMPLICIT NONE
  
! Arguments
  INTEGER, INTENT ( IN ), OPTIONAL :: exval
  
!------------------------------------------------------------------------------
  WRITE(*,999)
999 FORMAT ( "cpmd2cube: Convert CPMD's Wannier-function files to cube",/,                   &
         "usage is: cpmd2cube [options] Wannier_file [Wannier_file...]",/,                   &
         "   If you specify more than one Wannier file, they MUST have the",/,               &
         "   same g-vectors and (for the moment) atom positions",/,                          &
         "   The program will create one cube file for each Wannier file",/,                 &
         "   and one pdb file with the atom positions",/,                                    &
         "Example:",/,                                                                       &
         "   cpmd2cube WANNIER_1.*",/,                                                       &
         "possible options are",/,                                                           &
         "   -bader:",/,                                                                     &
         "      write Bader density to a file",/,                                            &
         "   -v <verbosity>:",/,                                                             &
         "      <verbosity> is 0-3 (default is 1)",/,                                        &
         "   -double:",/,                                                                    &
         "      Read the density in double precision (default is single)",/,                 &
         "   -fullmesh:",/,                                                                  &
         "      create full mesh cube file (default is -halfmesh)",/,                        &
         "   -halfmesh:",/,                                                                  &
         "      leave out half the grid points in each direction.",/,                        &
         "      Reduces the file size by 1/8th",/,                                           &
         "   -trim <threshold>:",/,                                                          &
         "      write a minimal cube file including all data points which have an",/,        &
         "      absolute value above <treshold>. Can reduce the file size dramatically",/,   &
         "      for isolated molecules and localized orbitals.",/,                           &
         "   -normpot:",/,                                                                   &
         "      correct for the fact, that the integrated electrostatic potential.",/,       &
         "      is not zero. To be used when converting ELPOT files",/,                      &
         "   -nocoords:",/,                                                                  &
         "      do not output any coordinates to the cube file. only puts a dummy ",/,       &
         "      atom at the origin, since some codes need at least one atom.",/,             &
         "      main application is to generate small files on QM/MM runs.",/,               &
         "   -n <n1> <n2> <n3>:",/,                                                          &
         "      change the REAL-space mesh. Default is to take the same mesh as CPMD",/,     &
         "   -o <prefix>:",/,                                                                &
         "      specify the prefix of the name used for the cube and pdb-files",/,           &
         "   -rep <n1> <n2> <n3>:",/,                                                        &
         "      replicate the cell n<j> times along the <j>-th direction by periodicity",/,  &
         "   -shift <r1> <r2> <r3>:",/,                                                      &
         "      shift cube density by r1*a1+r2*a2+r3*a3",/,                                  &
         "   -centre:", /, "   -center:",/,                                                  &
         "      centre density around centre of mass of system.",/,                          &
         "   -inbox:",/,                                                                     &
         "      put atoms inside unit cell centred around origin",/,                         &
         "   -rho:", /, "   -dens:",/,                                                       &
         "      convert a density instead of a wavefunction into a cube file.",/,            &
         "   -psi:", /, "   -wave:",/,                                                       &
         "      convert a wavefunction instead of a density into a cube file.",/,            &
         "   --:",/,                                                                         &
         "      last option. Useful if you have a file with the same name as an option",/,   &
         "   -h  or  -?  or  -help  or  --help  or no files:",/,                             &
         "      write this help" )
  
  IF ( PRESENT ( exval ) ) STOP
  
END SUBROUTINE write_help

!******************************************************************************

END MODULE cpmd2cube
