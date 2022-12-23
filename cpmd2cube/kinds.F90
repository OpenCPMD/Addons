
MODULE kinds

! ----------------------------------------------------------------------
!! Data type definitions tested on :
!!     - IBM AIX xlf90
!!     - IBM SP3 xlf90
!!     - IBM SP4 xlf90
!!     - SGI IRIX  f90
!!     - CRAY T3E  f90
!!     - DEC ALPHA f90
!!     - NAG_F90
!!     - SUN
!!     - SGI Origin 2000
!!     - HITACHI SR8000 (Europe)
!!     - HITACHI SR8000 (Japan)
!!     - HITACHI SR11000 - K1 model (Japan)
!!     - HITACHI SR11000 - J1 model (Japan)
!!     - FUJITSU VPP5000 (Japan)
! ----------------------------------------------------------------------
  
  IMPLICIT NONE
  
  PRIVATE
  PUBLIC :: qpr, dbl, dbl_size, sgl
  PUBLIC :: pi, bohrang, angbohr
  PUBLIC :: print_kind_info
  
  INTEGER, PARAMETER :: dbl = selected_real_kind(14,200)
  INTEGER, PARAMETER :: sgl = selected_real_kind(6,30)
  INTEGER, PARAMETER :: qpr = selected_real_kind(24,60)
  INTEGER, PARAMETER :: dbl_size = 8
  
  REAL ( dbl ), PARAMETER:: pi=3.14159265358979323846_dbl, &
       bohrang=0.529177249_dbl, angbohr=1._dbl/bohrang
  
CONTAINS

!******************************************************************************

! Print informations about the used data types.

SUBROUTINE print_kind_info
  
  IMPLICIT NONE
  
  WRITE (6,'(/,T2,A)') 'DATA TYPE INFORMATION:'
  
  WRITE (6,'(/,T2,A,T78,A,2(/,T2,A,T75,I6),3(/,T2,A,T67,E14.8))') &
       'REAL: Data type name:', 'dbl', '      Kind value:', kind(0.0_dbl), &
       '      Precision:', precision(0.0_dbl), &
       '      Smallest nonnegligible quantity relative to 1:', &
       epsilon(0.0_dbl), '      Smallest positive number:', tiny(0.0_dbl), &
       '      Largest representable number:', huge(0.0_dbl)
  WRITE (6,'(/,T2,A,T78,A,2(/,T2,A,T75,I6),3(/,T2,A,T67,E14.8))') &
       '      Data type name:', 'sgl', '      Kind value:', kind(0.0_sgl), &
       '      Precision:', precision(0.0_sgl), &
       '      Smallest nonnegligible quantity relative to 1:', &
       epsilon(0.0_sgl), '      Smallest positive number:', tiny(0.0_sgl), &
       '      Largest representable number:', huge(0.0_sgl)
  WRITE (6,'(/,T2,A,T72,A,4(/,T2,A,T61,I20))') &
       'INTEGER: Data type name:', '(default)', '         Kind value:', &
       kind(0), '         Bit size:', bit_size(0), &
       '         Largest representable number:', huge(0)
  WRITE (6,'(/,T2,A,T72,A,/,T2,A,T75,I6,/)') 'LOGICAL: Data type name:', &
       '(default)', '         Kind value:', kind(.TRUE.)
  WRITE (6,'(/,T2,A,T72,A,/,T2,A,T75,I6,/)') &
       'CHARACEER: Data type name:', '(default)', '           Kind value:', &
       kind('C')
  
END SUBROUTINE print_kind_info

!******************************************************************************

END MODULE kinds
