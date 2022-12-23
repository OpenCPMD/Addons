PROGRAM cutcube 
! --------------------------------------------------------------------
!   reads in cubefile and writes out a cubefile with only gridpoints
!   around given atoms copied and the rest set to zero.
! --------------------------------------------------------------------
  USE kinds, ONLY : dbl, angbohr  
  USE periodic_table, ONLY : init_periodic_table, ptable

  IMPLICIT NONE
  CHARACTER ( LEN = 80  ) :: filename_in, filename_out
  CHARACTER ( LEN = 132 ) :: junk
  INTEGER :: natoms
  INTEGER , PARAMETER :: iu_cube = 25
  INTEGER , PARAMETER :: iu_out = 35
  INTEGER :: nx,ny,nz
  INTEGER :: na, nx2, ny2,nz2 , nx0, ny0,nz0
  INTEGER :: i,j,k,l,n
  REAL (dbl), ALLOCATABLE, DIMENSION(:,:,:) :: CUBE, NCUBE
  REAL (dbl) :: gx,gxy,gxz,gy,gyx,gyz,gz,gzx,gzy
  REAL (dbl) :: origin(3), point(3), dist,v,v1,v2,v3
  INTEGER, ALLOCATABLE, DIMENSION(:) :: atoms 
  REAL (dbl), ALLOCATABLE :: coor (:,:) , rcut (:)
  REAL (dbl) ::  mass, r, anumber

! -------------------------------------------------------------------
!  the program 
! -------------------------------------------------------------------
  
  WRITE(*,'(/)')
  WRITE(*,*)'   ----------------------------------------'
  WRITE(*,*)'    this program reads in a cube-file      '
  WRITE(*,*)'    and writes a new cube around selected  '
  WRITE(*,*)'    atoms given as input                   ' 
  WRITE(*,*)'   (c) 2006 by katrin spiegel, axel kohlmeyer'
  WRITE(*,*)'   ----------------------------------------'
  WRITE(*,*)

  WRITE(*,*) 'give input cube filename:'
  READ *, filename_in
  WRITE(*,*) 'give output cube filename:'
  READ *, filename_out
  WRITE(*,*) 'give number of atoms, steepness (0.0 < flat < normal=1.0 < steep):' 
  READ *, natoms, r
  ALLOCATE (atoms(natoms))
  DO i=1, natoms
    WRITE(*,*)'give index number for atom ',i  
    READ *, atoms(i)
  END DO

  CALL init_periodic_table()

  OPEN (iu_cube,FILE=TRIM(filename_in),STATUS='OLD')
  OPEN (iu_out,FILE=TRIM(filename_out),STATUS='REPLACE')

! -------------------------------------------------------
!  read header of cube file, determine smallest spacing
!  write header of new cube file
! -------------------------------------------------------
   READ (iu_cube,*) junk
   WRITE (iu_out,*) TRIM(junk)
   READ (iu_cube,*) junk
   WRITE (iu_out,*) TRIM(junk)
   READ (iu_cube,900,END=200,ERR=200) na, origin(1), origin(2), origin(3)
   WRITE (iu_out,900) na, origin(1), origin(2), origin(3) 
   READ (iu_cube,900,END=200,ERR=200) nx,gx,gxy,gxz
   WRITE (iu_out,900) nx,gx,gxy,gxz
   READ (iu_cube,900,END=200,ERR=200) ny,gyx,gy,gyz
   WRITE (iu_out,900) ny,gyx,gy,gyz 
   READ (iu_cube,900,END=200,ERR=200) nz,gzx,gzy,gz
   WRITE (iu_out,900) nz,gzx,gzy,gz

!--------------------------------------------------------
!  read in atom centers and write them to new cubefile
!--------------------------------------------------------
   ALLOCATE(coor(na,3),rcut(na))
   DO n=1,na
      READ(iu_cube,910,END=200,ERR=200) anumber, mass, (coor(n,i),i=1,3)
      WRITE(iu_out,910)                 anumber, mass, (coor(n,i),i=1,3)
      rcut(n)=ptable(NINT(anumber+0.1))%vdw_radius
   END DO

!------------------------------------------------------------------
!  allocate cube-file array and read in cube-file
!------------------------------------------------------------------
   ALLOCATE (CUBE (nx,ny,nz), NCUBE (nx,ny,nz))
   DO n=1,nx
      DO l=1, ny
         READ(iu_cube,FMT=920,END=200,ERR=200) (CUBE(n,l,j),j=1,nz)
      END DO
   END DO 
!------------------------------------------------------------------
!  cube file format statements
!------------------------------------------------------------------
900 FORMAT (I5,3F12.6)
910 FORMAT (I5,4F12.6)
920 FORMAT (6E13.6)
!-------------------------------------------------------------------
!   loop over grid points and compute the position of each point.
!   determine the distance of the point from the selected atoms and 
!   use an erfc() style additive switching function to get a masking factor.
!   bracket factor between 0 and 1 and multiply with original data.
!-------------------------------------------------------------------
   DO n=1,nx
      DO l=1, ny
         DO k=1, nz
            point(1)=origin(1)+gx*n+gyx*l+gzx*k
            point(2)=origin(2)+gxy*n+gy*l+gzy*k
            point(3)=origin(3)+gxz*n+gyz*l+gz*k
            v=0.0_dbl
            DO i=1, natoms
               j=atoms(i)
               v1=coor(j,1)-point(1)
               v2=coor(j,2)-point(2)
               v3=coor(j,3)-point(3)
               dist=DSQRT(v1*v1+v2*v2+v3*v3)
#if defined(_NO_DERFC)
! DERFC is not implemented on intel v7
! yet would be a better choice...
               v=v+DERFC(r*(dist - rcut(j)))
#else
               v=v-TANH(r*(dist - rcut(j)))+1.0
#endif
            END DO
            IF (v > 1.0_dbl) v=1.0_dbl
            IF (v < 0.0001_dbl) v=0.0_dbl
            NCUBE(n,l,k)=v*CUBE(n,l,k)
         END DO
      END DO
   END DO
!--------------------------------------------------------
!   write new density to output file 
!--------------------------------------------------------
   WRITE(*,*) 'writing new cube file'
   DO n=1,nx
      DO l=1, ny
         WRITE(iu_out,920) (NCUBE(n,l,j),j=1,nz) 
      END DO
   END DO
   CLOSE(iu_cube)
   CLOSE(iu_out)
   DEALLOCATE (CUBE,NCUBE)
   STOP 'done' 

200 CONTINUE
   STOP 'premature end of file, or read error'

END PROGRAM cutcube 
