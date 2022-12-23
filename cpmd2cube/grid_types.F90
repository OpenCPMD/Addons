
MODULE grid_types
  
  USE kinds, ONLY : dbl
  
  IMPLICIT NONE
  
  PRIVATE
  PUBLIC :: grid_type, grid_init
  
  TYPE grid_type
     LOGICAL :: formed
     INTEGER :: ibrav
     REAL ( dbl ) :: gcut
     REAL ( dbl ) :: gcutw
     REAL ( dbl ) :: celldm ( 6 )
     REAL ( dbl ) :: volume
     REAL ( dbl ) :: a1 ( 3 ), a2 ( 3 ), a3 ( 3 )
     INTEGER :: nhg
     INTEGER, DIMENSION ( :, : ), POINTER :: inyh
     REAL ( dbl ), DIMENSION ( : ), POINTER :: hg
     INTEGER :: nh1, nh2, nh3, nr1, nr2, nr3
  END TYPE grid_type
  
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

END MODULE grid_types
