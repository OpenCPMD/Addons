! hello emacs this is -*- f90 -*-

MODULE readwrites
  
  USE atom_types, ONLY : atom_type
  USE grid_types, ONLY : grid_type
  USE kinds, ONLY : dbl, sgl, bohrang
  USE periodic_table, ONLY : ptable
  
  IMPLICIT NONE
  
  PRIVATE
  PUBLIC :: read_density_file, write_pdb, write_cube, write_Bader_density
  PUBLIC :: READ_HEADER, READ_ALL
  
  INTEGER, PARAMETER :: READ_HEADER = 101, READ_ALL = 102
  
CONTAINS

!******************************************************************************

! read density/Wannier file

SUBROUTINE read_density_file ( fname_in, c0, singleprecision, atom, &
     ibrav, celldm, nr1, nr2, nr3, gcut, gcutw, nhg, celltrans, key )
  
  IMPLICIT NONE
  
! Arguments
  CHARACTER ( LEN = * ) :: fname_in
  COMPLEX ( dbl ), DIMENSION ( : ), POINTER :: c0
  LOGICAL, INTENT ( IN ) :: singleprecision
  TYPE ( atom_type ), INTENT ( OUT ) :: atom
  INTEGER, INTENT ( OUT ) :: nhg, nr1, nr2, nr3, ibrav
  REAL ( dbl ), INTENT ( OUT ) :: celldm ( 6 ), gcut, gcutw, celltrans( 3 )
  INTEGER, INTENT ( IN ) :: key
  
! Locals
  INTEGER :: iochannel = 99
  INTEGER :: ia, sp, ig, allocstatus
  COMPLEX ( sgl ), DIMENSION ( : ), ALLOCATABLE :: c0x
  
!------------------------------------------------------------------------------
  
  OPEN ( UNIT = iochannel, FILE = fname_in, &
       STATUS = "OLD", FORM = "UNFORMATTED" )
  
  READ ( iochannel ) ibrav
  READ ( iochannel ) celldm
  READ ( iochannel ) nr1, nr2, nr3
  READ ( iochannel ) gcut, nhg, gcutw
  READ ( iochannel ) atom % nsp
  READ ( iochannel ) ( atom % na ( sp ), sp = 1, atom % nsp )
  
  atom % nat = 0
  DO sp = 1, atom % nsp
     atom % nat = atom % nat + atom % na ( sp )
     DO ia = 1, atom % na ( sp )
        READ ( iochannel ) &
             atom % tau0 ( 1, ia, sp ), atom % tau0 ( 2, ia, sp ), &
             atom % tau0 ( 3, ia, sp ), atom % iatyp ( sp )
     END DO
  END DO
  
! read in the density/wave function
  IF ( .NOT. ASSOCIATED ( c0 ) ) THEN
     ALLOCATE ( c0 ( nhg ), STAT = allocstatus )
     IF ( allocstatus /= 0 ) STOP "read_density_file, error allocating c0"
  END IF
  
  IF ( SIZE ( c0 ) /= nhg ) STOP "read_density_file, c0 incorrectly sized"
  
  IF ( singleprecision ) THEN
     
     ALLOCATE ( c0x ( nhg ), STAT = allocstatus )
     IF ( allocstatus /= 0 ) STOP "read_density_file, error allocating c0x"
     
     READ ( iochannel ) ( c0x ( ig ), ig = 1, nhg )
     
     c0 = c0x
     
     DEALLOCATE ( c0x, STAT = allocstatus )
     IF ( allocstatus /= 0 ) STOP "read_density_file, error deallocating c0x"
     
  ELSE
     
     READ ( iochannel ) ( c0 ( ig ), ig = 1, nhg )
     
  END IF
! AK: the cell translation vector is a new addition to cpmd for QM/MM.
!     we poll iostat to be able to ignore it, if it is not present. 
  READ ( iochannel , IOSTAT=ig ) ( celltrans ( sp ), sp = 1, 3 )
  CLOSE ( UNIT = iochannel )
  
END SUBROUTINE read_density_file

!******************************************************************************

SUBROUTINE write_pdb ( fname_pdb, title, atom, a1, a2, a3, nrep_a )
  
  IMPLICIT NONE
  
! Arguments
  CHARACTER ( LEN = * ), INTENT ( IN ) :: fname_pdb
  CHARACTER ( LEN = * ), INTENT ( IN ) :: title
  TYPE ( atom_type ), INTENT ( IN ) :: atom
  REAL ( dbl ), INTENT ( IN ), OPTIONAL :: a1 ( 3 ), a2 ( 3 ), a3 ( 3 )
  INTEGER, INTENT ( IN ), OPTIONAL :: nrep_a ( 3 )
  
! Locals
  CHARACTER ( LEN = 6 ) :: molname = "MOL"
  INTEGER :: iat, ia, sp, i, j, k, iochannel = 99, nrep( 3 )
  REAL ( dbl ) :: rr ( 3 )
  
!------------------------------------------------------------------------------
  
  nrep = (/ 1, 1, 1 /)
  IF ( PRESENT ( nrep_a ) ) nrep = nrep_a
  
  IF ( PRESENT ( nrep_a ) ) THEN
     IF ( .NOT. &
          ( PRESENT ( a1 ) .AND. PRESENT ( a2 ) .AND. PRESENT ( a3 ) ) ) THEN
        STOP "write_pdb, both replicate and lattice vectors must be present"
     END IF
  ELSE
     IF ( PRESENT ( a1 ) .OR. PRESENT ( a2 ) .OR. PRESENT ( a3 ) ) THEN
        STOP "write_pdb, both replicate and lattice vectors must be present"
     END IF
  END IF
  
  WRITE ( *, '( " create pdb file ", A, "... ", $ )' ) TRIM ( fname_pdb )
  
  OPEN ( UNIT = iochannel, FILE = fname_pdb, &
       FORM = "FORMATTED", STATUS = "UNKNOWN" )
  
  WRITE ( iochannel, '( 2A )' ) "REMARK ", TRIM ( title )
  
  iat = 0
  DO sp = 1, atom % nsp
     DO ia = 1, atom % na ( sp )
        
        DO i = 0, nrep ( 1 ) - 1
          DO j = 0, nrep ( 2 ) - 1
            DO k = 0, nrep ( 3 ) - 1

              rr(1:3) = atom % tau0 ( 1:3, ia, sp ) &
                   + REAL(i)*a1(1:3) + REAL(j)*a2(1:3) + REAL(k)*a3(1:3)
              rr ( 1:3 ) = rr ( 1:3 ) * bohrang
                    
              iat = iat + 1
              WRITE ( iochannel, &
                   '( "HETATM", I5, 1X, A4, 1X, A6, "  1    ", &
                   & 3(1X,F7.3), "  1.00  0.00" )' ) &
                   iat, ptable ( atom % iatyp ( sp ) ) % symbol, &
                   molname, rr ( 1 ), rr ( 2 ), rr ( 3 )

            END DO
          END DO
        END DO
           
     END DO
  END DO
   
  WRITE ( iochannel, &
       '( "TER     ", I3, 5X, " MOL    99")' ) iat+1
  WRITE ( iochannel, '( "END" )' )
   
  CLOSE ( UNIT = iochannel )
  
  WRITE ( *, '( "done" )' )
  
END SUBROUTINE write_pdb

!******************************************************************************

SUBROUTINE write_cube ( fname_cube, title, atom, origin, rho, spacing, &
     nn, nrep_a, halfmesh, a1, a2, a3, trimthresh, nocoords, celltrans )
  
  IMPLICIT NONE
  
! Arguments
  CHARACTER ( LEN = * ), INTENT ( IN ) :: fname_cube
  CHARACTER ( LEN = * ), INTENT ( IN ) :: title
  TYPE ( atom_type ), INTENT ( IN ) :: atom
  REAL ( dbl ), INTENT ( IN ) :: origin ( 3 )
  REAL ( dbl ), DIMENSION ( :, :, : ), INTENT ( IN ) :: rho
  REAL ( dbl ), INTENT ( IN ) :: spacing ( 3, 3 ), trimthresh, celltrans ( 3 )
  REAL ( dbl ), INTENT ( IN ), OPTIONAL :: a1 ( 3 ), a2 ( 3 ), a3 ( 3 )
  INTEGER, INTENT ( IN ) :: nn ( 3 )
  INTEGER, INTENT ( IN ), OPTIONAL :: nrep_a ( 3 )
  LOGICAL, INTENT ( IN ), OPTIONAL :: halfmesh, nocoords
  
! Locals
  CHARACTER ( LEN = 32 ) :: title2 = " Total SCF Density"
  INTEGER :: iochannel = 99
  INTEGER :: i, j, k, i1, i2, i3, ia, sp, nrep( 3 ), step
  REAL ( dbl ) :: rr ( 3 )
  LOGICAL :: docoords
! for trimming
  INTEGER :: maxa, maxb, maxc, mina, minb, minc, nra, nrb, nrc
  REAL ( dbl ) :: trimorigin ( 3 )
  
!------------------------------------------------------------------------------
  step = 1
  IF ( PRESENT (halfmesh) ) THEN
     IF ( halfmesh ) THEN
        step = 2
     END IF
  END IF

  nrep = (/ 1, 1, 1 /)
  IF ( PRESENT ( nrep_a ) ) nrep = nrep_a
  
  IF ( PRESENT ( nrep_a ) ) THEN
     IF ( .NOT. &
          ( PRESENT ( a1 ) .AND. PRESENT ( a2 ) .AND. PRESENT ( a3 ) ) ) THEN
        STOP "write_cube, both replicate and lattice vectors must be present"
     END IF
     IF (trimthresh > 0.0_dbl) THEN
        IF (( nrep_a ( 1 ) > 1) .OR. ( nrep_a ( 2 ) > 1) .OR. ( nrep_a ( 3 ) > 1)) THEN
           STOP "write_cube, replicate and trimming are incompatible"
        END IF
     END IF
  ELSE
     IF ( PRESENT ( a1 ) .OR. PRESENT ( a2 ) .OR. PRESENT ( a3 ) ) THEN
        STOP "write_cube, both replicate and lattice vectors must be present"
     END IF
  END IF

! initialize variable defaults for no trimming
  nra = nn ( 1 )   
  nrb = nn ( 2 )
  nrc = nn ( 3 )
  mina = 1
  minb = 1
  minc = 1
  maxa = nn ( 1 )
  maxb = nn ( 2 )
  maxc = nn ( 3 )
  trimorigin ( : ) = origin ( : )
  docoords = .TRUE.
  IF ( PRESENT( nocoords ) ) THEN
     docoords = .NOT. nocoords
  ENDIF

  IF ( trimthresh > 0.0_dbl ) THEN
     WRITE ( *, '( "trimming cube with threshold: ", F8.4)' ) trimthresh
! initialize min/max indices to non-sensical values,
! so they all get updated on the first match.
     mina = nn ( 1 ) + 1
     minb = nn ( 2 ) + 1
     minc = nn ( 3 ) + 1
     maxa = -1
     maxb = -1
     maxc = -1

     DO i1 = 1, nn ( 1 ), step
        DO i2 = 1, nn ( 2 ), step
           DO i3 = 1, nn ( 3 ), step
              IF ( ABS (rho ( i1, i2, i3 )) > trimthresh) THEN
                 IF (i1 < mina) mina = i1
                 IF (i2 < minb) minb = i2
                 IF (i3 < minc) minc = i3
                 IF (i1 > maxa) maxa = i1
                 IF (i2 > maxb) maxb = i2
                 IF (i3 > maxc) maxc = i3
              END IF
           END DO
        END DO
     END DO

! check if there was any data point above the threshold
     IF (maxa < 0) THEN
        STOP 'trimming threshold is too high. refusing to write an empty cube file.'
     END IF

     nra = maxa - mina + step
     nrb = maxb - minb + step
     nrc = maxc - minc + step
     trimorigin ( 1 ) = origin ( 1 ) &
          + (mina-1) * spacing ( 1, 1 ) &
          + (minb-1) * spacing ( 2, 1 ) &
          + (minc-1) * spacing ( 3, 1 ) 
     trimorigin ( 2 ) = origin ( 2 ) &
          + (mina-1) * spacing ( 1, 2 ) &
          + (minb-1) * spacing ( 2, 2 ) &
          + (minc-1) * spacing ( 3, 2 ) 
     trimorigin ( 3 ) = origin ( 3 ) &
          + (mina-1) * spacing ( 1, 3 ) &
          + (minb-1) * spacing ( 2, 3 ) &
          + (minc-1) * spacing ( 3, 3 ) 

  WRITE ( *, '( "trimmed full real-space grid now:", 3I9)' ) nra, nrb, nrc
     WRITE (6, '("number of data points:",I10,"      after trimming:",I10)') &
          (nn ( 1 ) * nn ( 2 ) * nn ( 3 ))/step, (nra * nrb * nrc)/step 
     WRITE (6, '("compression ratio: ",F6.1,":1 ")') &
          DBLE(nn ( 1 ) * nn ( 2 ) * nn ( 3 )) / DBLE(nra * nrb * nrc)
  END IF

  
  WRITE ( *, '( " creating cube file ", A, "... ", $ )' ) TRIM ( fname_cube )
  
  OPEN ( UNIT = iochannel, FILE = fname_cube, &
       FORM = "FORMATTED", STATUS = "UNKNOWN" )
  
  WRITE ( iochannel, '( A )' ) TRIM ( title )
  
  WRITE ( iochannel, '( A )' ) TRIM ( title2 )

  IF (docoords) THEN  
     WRITE ( iochannel, '( I5, 3F12.6 )' ) atom % nat * nrep(1) * nrep(2) * nrep(3), &
          trimorigin ( 1:3 ) - celltrans( 1:3 )
  
     WRITE ( iochannel, '( I5, 3F12.6 )' ) &
          nrep ( 1 ) * nra / step , DBLE(step) * spacing ( 1, : )
     WRITE ( iochannel, '( I5, 3F12.6 )' ) &
          nrep ( 2 ) * nrb / step , DBLE(step) * spacing ( 2, : )
     WRITE ( iochannel, '( I5, 3F12.6 )' ) &
          nrep ( 3 ) * nrc / step , DBLE(step) * spacing ( 3, : )
  
     DO sp = 1, atom % nsp
        DO ia = 1, atom % na ( sp )
           DO i = 0, nrep ( 1 ) - 1
              DO j = 0, nrep ( 2 ) - 1
                 DO k = 0, nrep ( 3 ) - 1
                    rr(1:3) = atom % tau0 ( 1:3, ia, sp ) &
                         + REAL(i)*a1(1:3) + REAL(j)*a2(1:3) + REAL(k)*a3(1:3)
                    WRITE ( iochannel, '( I5, 4F12.6 )' ) &
                         atom % iatyp ( sp ), DBLE(atom % iatyp ( sp )), &
                         rr ( 1 ), rr ( 2 ), rr ( 3 )
                 END DO
              END DO
           END DO
        END DO
     END DO
  ELSE
! no coordinates cube file. so we just write a dummy atom at the position
! of the origin. some codes choke, if there is not atom at all
     WRITE ( iochannel, '( I5, 3F12.6 )' ) 1, trimorigin ( 1:3 ) - celltrans ( 1:3 )
  
     WRITE ( iochannel, '( I5, 3F12.6 )' ) &
          nrep ( 1 ) * nra / step , DBLE(step) * spacing ( 1, : )
     WRITE ( iochannel, '( I5, 3F12.6 )' ) &
          nrep ( 2 ) * nrb / step , DBLE(step) * spacing ( 2, : )
     WRITE ( iochannel, '( I5, 3F12.6 )' ) &
          nrep ( 3 ) * nrc / step , DBLE(step) * spacing ( 3, : )
  
     WRITE ( iochannel, '( I5, 4F12.6 )' ) 0, 0.0, trimorigin( 1:3 )
  ENDIF

! write grid
  DO i = 1, nrep ( 1 )
     DO i1 = mina, maxa, step
        DO j = 1, nrep ( 2 )
           DO i2 = minb, maxb, step
              WRITE ( iochannel, '( 6E13.5 )' ) ( ( REAL ( rho ( i1, i2, i3 ) ), &
                   i3 = minc, maxc, step ), k = 1, nrep ( 3 ) )
           END DO
        END DO
     END DO
  END DO
  
  CLOSE ( UNIT = iochannel )
  
  WRITE ( *, '( "done" )' )
  
END SUBROUTINE write_cube

!******************************************************************************

SUBROUTINE write_Bader_density ( fname, n, a1, a2, a3, ecut_density )
  
  IMPLICIT NONE
  
! Arguments
  CHARACTER ( LEN = * ), INTENT ( IN ) :: fname
  REAL ( dbl ), DIMENSION ( :, :, : ), INTENT ( IN ) :: n
  REAL ( dbl ), INTENT ( IN ) :: a1 ( 3 ), a2 ( 3 ), a3 ( 3 )
  REAL ( dbl ), INTENT ( IN ) :: ecut_density

! Local
  INTEGER :: nr1d, nr2d, nr3d, iochannel = 99
  
!------------------------------------------------------------------------------
  
  OPEN ( UNIT = iochannel, FILE = fname, &
       FORM = "FORMATTED", ACTION = "WRITE" )
  
  WRITE ( iochannel, '( 3F15.10 )' ) a1
  WRITE ( iochannel, '( 3F15.10 )' ) a2
  WRITE ( iochannel, '( 3F15.10 )' ) a3
  
  WRITE ( iochannel, '( F15.8 )' ) ecut_density
  
  WRITE ( iochannel, '( 3I5 )' ) SIZE ( n, 1 ), SIZE ( n, 2 ), SIZE ( n, 3 )
  
  WRITE ( iochannel, '( F12.8 )' ) n ( :, :, : )
  
  CLOSE ( UNIT = iochannel )
  
END SUBROUTINE write_Bader_density

!******************************************************************************

END MODULE readwrites

