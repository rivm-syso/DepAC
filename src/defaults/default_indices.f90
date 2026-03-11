module default_indices
    implicit none (type, external)

    ! Defined component indices for easier reference
    enum, bind(c)
      enumerator :: COMP_NH3 = 1
      enumerator :: COMP_O3 = 2
      enumerator :: COMP_SO2 = 3
      enumerator :: COMP_NO2 = 4
      enumerator :: COMP_NO = 5
      enumerator :: COMP_HNO3 = 6
   end enum

   ! Defined land use indices for easier reference
   enum, bind(c)
      enumerator :: LU_GRASS = 1
      enumerator :: LU_ARABLE = 2
      enumerator :: LU_PERMANENT_CROPS = 3
      enumerator :: LU_CONIFEROUS_FOREST = 4
      enumerator :: LU_DECIDUOUS_FOREST = 5
      enumerator :: LU_WATER = 6
      enumerator :: LU_URBAN = 7
      enumerator :: LU_OTHER = 8
      enumerator :: LU_DESERT = 9
   end enum

   public
end module default_indices