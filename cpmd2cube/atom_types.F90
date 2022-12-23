
MODULE atom_types
  
  USE kinds, ONLY : dbl, sgl, bohrang, pi
  
  IMPLICIT NONE
  
  PRIVATE
  PUBLIC :: atom_type
  
  INTEGER, PARAMETER :: maxsp = 50, maxat = 2000
  
  TYPE atom_type
     INTEGER :: nsp, na ( maxsp ), nat, iatyp ( maxsp )
     REAL ( dbl ) :: tau0 ( 3, maxat, maxsp )
  END TYPE atom_type
  
END MODULE atom_types
