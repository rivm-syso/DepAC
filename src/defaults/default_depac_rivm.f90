 !------------------------------------------------------------------------------
 !  default_depac_config_rivm.f90
 !
 !  Author: Marte Voorneveld
 !  Description: Default configuration for DepAC land use and component types
 !               (RIVM version)
 !  Created:    2024
 !------------------------------------------------------------------------------

module default_depac_config_rivm
   use t_depac_component, only: depac_component
   use t_depac_land_use, only: depac_land_use
   use t_depac_setup, only: depac_setup

   use m_depac_params, only: gsoil_default, gw_default, gstom_default, comp_point_default, &
      rc_special_default, rinc_default, rinc_no_path, rinc_no_resistance,&
       csoil_default, csoil_water, &
       gw_nh3_sutton, gw_so2, gstom_emberson, comp_point_ammonia, rc_tot_nitric_acid, &
       rc_tot_nitric_oxide, rc_tot_fixed

   use m_depac_factory, only: make_component, make_land_use, make_setup, &
      make_stom_params, make_rc_r_params


   implicit none (type, external)

   ! Defined component indices for easier reference
    enum, bind(c)
      enumerator :: RIVM_COMP_NH3 = 1
      enumerator :: RIVM_COMP_O3 = 2
      enumerator :: RIVM_COMP_SO2 = 3
      enumerator :: RIVM_COMP_NO2 = 4
      enumerator :: RIVM_COMP_NO = 5
      enumerator :: RIVM_COMP_HNO3 = 6
   end enum

   ! Defined land use indices for easier reference
   enum, bind(c)
      enumerator :: RIVM_LU_GRASS = 1
      enumerator :: RIVM_LU_ARABLE = 2
      enumerator :: RIVM_LU_PERMANENT_CROPS = 3
      enumerator :: RIVM_LU_CONIFEROUS_FOREST = 4
      enumerator :: RIVM_LU_DECIDUOUS_FOREST = 5
      enumerator :: RIVM_LU_WATER = 6
      enumerator :: RIVM_LU_URBAN = 7
      enumerator :: RIVM_LU_OTHER = 8
      enumerator :: RIVM_LU_DESERT = 9
   end enum

   type(depac_component), dimension(:), allocatable :: default_components
   type(depac_land_use), dimension(:), allocatable :: default_land_uses
   type(depac_setup), dimension(:,:), allocatable :: default_depac_setup

   private
   public :: init_default_depac_config_rivm, finalize_default_depac_config_rivm, &
   default_depac_setup, RIVM_COMP_NH3, RIVM_COMP_O3, RIVM_COMP_SO2, RIVM_COMP_NO2, RIVM_COMP_NO, RIVM_COMP_HNO3, &
   RIVM_LU_GRASS, RIVM_LU_ARABLE, RIVM_LU_PERMANENT_CROPS, RIVM_LU_CONIFEROUS_FOREST, RIVM_LU_DECIDUOUS_FOREST, &
   RIVM_LU_WATER, RIVM_LU_URBAN, RIVM_LU_OTHER, RIVM_LU_DESERT

contains

   subroutine init_default_depac_config_rivm()
      integer :: i, j
      logical, dimension(6,9), save :: use_gw_nh3_sutton = .false.
      logical, dimension(6,9), save :: use_gw_so2 = .false.

      logical, dimension(6,9), save :: use_gstom_emberson = .false.
      logical, dimension(6,9), save :: use_comp_point_ammonia = .false.

      logical, dimension(6,9), save :: use_rc_tot_nitric_acid = .false.
      logical, dimension(6,9), save :: use_rc_tot_nitric_oxide = .false.
      logical, dimension(6,9), save :: use_rc_tot_fixed = .false.

      logical, dimension(6,9), save :: use_rinc_no_resistance = .false.
      logical, dimension(6,9), save :: use_rinc_no_path = .false.

      real, dimension(9, 6), save :: default_rsoil_matrix


      use_gw_nh3_sutton(RIVM_COMP_NH3,:) = .true.
      use_gw_so2(RIVM_COMP_SO2,:) = .true.

      use_gstom_emberson(RIVM_COMP_NH3, :) = .true.
      use_gstom_emberson(RIVM_COMP_O3, :) = .true.
      use_gstom_emberson(RIVM_COMP_SO2, :) = .true.
      use_gstom_emberson(RIVM_COMP_NO2, :) = .true.

      use_comp_point_ammonia(RIVM_COMP_NH3, :) = .true.


      use_rc_tot_nitric_acid(RIVM_COMP_HNO3, :) = .true.
      use_rc_tot_nitric_oxide(RIVM_COMP_NO, :) = .true.

      use_rc_tot_fixed(RIVM_COMP_NO, RIVM_LU_WATER) = .true.

      use_rinc_no_resistance(:, RIVM_LU_WATER) = .true.
      use_rinc_no_resistance(:, RIVM_LU_URBAN) = .true.
      use_rinc_no_resistance(:, RIVM_LU_DESERT) = .true.

      use_rinc_no_path(:, RIVM_LU_OTHER) = .true.
      use_rinc_no_path(:, RIVM_LU_GRASS) = .true.







      ! Matrix of rsoil values for each land use and component type
      default_rsoil_matrix = reshape([ &
      ! NH3,   O3,   SO2,   NO2,   NO,  HNO3
         100.0, 1000.0, 1000.0, 1000.0, -999.0, -999.0, &   ! grass
         100.0,  200.0, 1000.0, 1000.0, -999.0, -999.0, &   ! arable
         100.0,  200.0, 1000.0, 1000.0, -999.0, -999.0, &   ! permanent_crops
         100.0,  200.0, 1000.0, 1000.0, -999.0, -999.0, &   ! coniferous_forest
         100.0,  200.0, 1000.0, 1000.0, -999.0, -999.0, &   ! deciduous_forest
         10.0, 2000.0,   10.0, 2000.0, 2000.0, -999.0, &   ! water
         100.0,  400.0, 1000.0, 1000.0, 1000.0, -999.0, &   ! urban
         100.0,  400.0, 1000.0, 1000.0, -999.0, -999.0, &   ! other
         100.0, 2000.0, 1000.0, 1000.0, 2000.0, -999.0  &   ! desert
         ], [9, 6])
      ! This subroutine can be used to initialize any additional data structures
      ! or perform checks on the default configuration if needed.
      allocate(default_components(6))

      default_components = [ &
         make_component( &
         name = 'NH3', &
         index = RIVM_COMP_NH3, &
         diffc = 0.21e-4, &
         rw_val = -999.0, &
         ipar_snow = 2, &
         rsoil_frozen = 1000.0, &
         rsoil_wet = 10.0), &
         make_component( &
         name = 'O3', &
         index = RIVM_COMP_O3, &
         diffc = 0.13e-4, &
         rw_val = 1000.0, &
         ipar_snow = 1, &
         rsoil_frozen = 2000.0, &
         rsoil_wet = 2000.0), &
         make_component( &
         name = 'SO2', &
         index = RIVM_COMP_SO2, &
         diffc = 0.11e-4, &
         rw_val = -999.0, &
         ipar_snow = 2, &
         rsoil_frozen = 500.0, &
         rsoil_wet = 10.0), &
         make_component( &
         name = 'NO2', &
         index = RIVM_COMP_NO2, &
         diffc = 0.13e-4, &
         rw_val = 2000.0, &
         ipar_snow = 1, &
         rsoil_frozen = 2000.0, &
         rsoil_wet = 2000.0), &
         make_component( &
         name = 'NO', &
         index = RIVM_COMP_NO, &
         diffc = 0.16e-4, &
         rw_val = -999.0, &
         ipar_snow = 1, &
         rsoil_frozen = -999.0, &
         rsoil_wet = -999.0), &
         make_component( &
         name = 'HNO3', &
         index = RIVM_COMP_HNO3, &
         diffc = -999.0, &
         rw_val = -999.0, &
         ipar_snow = -999, &
         rsoil_frozen = -999.0, &
         rsoil_wet = -999.0) &
         ]

      allocate(default_land_uses(9))



      default_land_uses = [ &
         make_land_use( &
         name = 'grass', &
         index = RIVM_LU_GRASS, &
         gamma_stom_c_fac = 362.0, &
         gamma_soil_c_fac = -999.0, &
         rsoil = -999.0, &
         stom_par = make_stom_params( &
         F_min = 0.01, &
         alpha = 4.57*0.009, &
         Topt = 26.0, &
         Tmin = 12.0, &
         Tmax = 40.0, &
         g_max = 270.0/41000, &
         vpd_max = 1.3, &
         vpd_min = 3.0)), &
         make_land_use( &
         name = 'arable', &
         index = RIVM_LU_ARABLE, &
         gamma_stom_c_fac = 362.0, &
         gamma_soil_c_fac = -999.0, &
         rsoil = -999.0, &
         stom_par = make_stom_params( &
         F_min = 0.01, &
         alpha = 4.57*0.009, &
         Topt = 26.0, &
         Tmin = 12.0, &
         Tmax = 40.0, &
         g_max = 300.0/41000, &
         vpd_max = 0.9, &
         vpd_min = 2.8), &
         rc_rinc = make_rc_r_params( &
         b = 14.0, &
         h = 1.0)), &
         make_land_use( &
         name = 'permanent_crops', &
         index = RIVM_LU_PERMANENT_CROPS, &
         gamma_stom_c_fac = 362.0, &
         gamma_soil_c_fac = -999.0, &
         rsoil = -999.0, &
         stom_par = make_stom_params( &
         F_min = 0.01, &
         alpha = 4.57*0.009, &
         Topt = 26.0, &
         Tmin = 12.0, &
         Tmax = 40.0, &
         g_max = 300.0/41000, &
         vpd_max = 0.9, &
         vpd_min = 2.8), &
         rc_rinc = make_rc_r_params( &
         b = 14.0, &
         h = 1.0)), &
         make_land_use( &
         name = 'coniferous_forest', &
         index = RIVM_LU_CONIFEROUS_FOREST, &
         gamma_stom_c_fac = 362.0, &
         gamma_soil_c_fac = -999.0, &
         rsoil = -999.0, &
         stom_par = make_stom_params( &
         F_min = 0.1, &
         alpha = 4.57*0.006, &
         Topt = 18.0, &
         Tmin = 0.0, &
         Tmax = 36.0, &
         g_max = 140.0/41000, &
         vpd_max = 0.5, &
         vpd_min = 3.0), &
         rc_rinc = make_rc_r_params( &
         b = 14.0, &
         h = 20.0)), &
         make_land_use( &
         name = 'deciduous_forest', &
         index = RIVM_LU_DECIDUOUS_FOREST, &
         gamma_stom_c_fac = 362.0, &
         gamma_soil_c_fac = -999.0, &
         rsoil = -999.0, &
         stom_par = make_stom_params( &
         F_min = 0.1, &
         alpha = 4.57*0.006, &
         Topt = 20.0, &
         Tmin = 0.0, &
         Tmax = 35.0, &
         g_max = 150.0/41000, &
         vpd_max = 1.0, &
         vpd_min = 3.25), &
         rc_rinc = make_rc_r_params( &
         b = 14.0, &
         h = 20.0)), &
         make_land_use( &
         name = 'water', &
         index = RIVM_LU_WATER, &
         gamma_stom_c_fac = -999.0, &
         gamma_soil_c_fac = 430.0, &
         rsoil = -999.0, &
         stom_par = make_stom_params( &
         F_min = -999.0, &
         alpha = -999.0, &
         Topt = -999.0, &
         Tmin = -999.0, &
         Tmax = -999.0, &
         g_max = -999.0, &
         vpd_max = -999.0, &
         vpd_min = -999.0)), &
         make_land_use( &
         name = 'urban', &
         index = RIVM_LU_URBAN, &
         gamma_stom_c_fac = -999.0, &
         gamma_soil_c_fac = -999.0, &
         rsoil = -999.0, &
         stom_par = make_stom_params( &
         F_min = -999.0, &
         alpha = -999.0, &
         Topt = -999.0, &
         Tmin = -999.0, &
         Tmax = -999.0, &
         g_max = -999.0, &
         vpd_max = -999.0, &
         vpd_min = -999.0)), &
         make_land_use( &
         name = 'other', &
         index = RIVM_LU_OTHER, &
         gamma_stom_c_fac = 362.0, &
         gamma_soil_c_fac = -999.0, &
         rsoil = -999.0, &
         stom_par = make_stom_params( &
         F_min = 0.01, &
         alpha = 4.57*0.009, &
         Topt = 26.0, &
         Tmin = 12.0, &
         Tmax = 40.0, &
         g_max = 270.0/41000, &
         vpd_max = 1.3, &
         vpd_min = 3.0)), &
         make_land_use( &
         name = 'desert', &
         index = RIVM_LU_DESERT, &
         gamma_stom_c_fac = -999.0, &
         gamma_soil_c_fac = -999.0, &
         rsoil = -999.0, &
         stom_par = make_stom_params( &
         F_min = -999.0, &
         alpha = -999.0, &
         Topt = -999.0, &
         Tmin = -999.0, &
         Tmax = -999.0, &
         g_max = -999.0, &
         vpd_max = -999.0, &
         vpd_min = -999.0)), &
         make_land_use( &
         name = 'desert', &
         index = RIVM_LU_DESERT, &
         gamma_stom_c_fac = -999.0, &
         gamma_soil_c_fac = -999.0, &
         rsoil = -999.0, &
         stom_par = make_stom_params( &
         F_min = -999.0, &
         alpha = -999.0, &
         Topt = -999.0, &
         Tmin = -999.0, &
         Tmax = -999.0, &
         g_max = -999.0, &
         vpd_max = -999.0, &
         vpd_min = -999.0)) &
         ]

      allocate(default_depac_setup(9, 6))


      do i = 1, 9
         do j = 1, 6

            default_depac_setup(i, j) = make_setup( &
               component = default_components(j), &
               land_use = default_land_uses(i) &
            )

            default_depac_setup(i, j)%land_use%rsoil = default_rsoil_matrix(i, j)

            if(use_gw_nh3_sutton(j, i)) then
               deallocate(default_depac_setup(i, j)%gw_param)
               allocate(default_depac_setup(i, j)%gw_param, source=gw_nh3_sutton())
            end if

            if(use_gw_so2(j, i)) then
               deallocate(default_depac_setup(i, j)%gw_param)
               allocate(default_depac_setup(i, j)%gw_param, source=gw_so2())
            end if

            if(use_gstom_emberson(j, i)) then
               deallocate(default_depac_setup(i, j)%gstom_param)
               allocate(default_depac_setup(i, j)%gstom_param, source=gstom_emberson())
            end if

            if(use_comp_point_ammonia(j, i)) then
               deallocate(default_depac_setup(i, j)%comp_point_param)
               allocate(default_depac_setup(i, j)%comp_point_param, source=comp_point_ammonia())
            end if

            if(use_rc_tot_nitric_acid(j, i)) then
               deallocate(default_depac_setup(i, j)%rc_special_param)
               allocate(default_depac_setup(i, j)%rc_special_param, source=rc_tot_nitric_acid())
            end if

            if(use_rc_tot_nitric_oxide(j, i)) then
               deallocate(default_depac_setup(i, j)%rc_special_param)
               allocate(default_depac_setup(i, j)%rc_special_param, source=rc_tot_nitric_oxide())
            end if

            if(use_rc_tot_fixed(j, i)) then
               deallocate(default_depac_setup(i, j)%rc_special_param)
               allocate(default_depac_setup(i, j)%rc_special_param, source=rc_tot_fixed())
            end if

            if(use_rinc_no_resistance(j, i)) then
               deallocate(default_depac_setup(i, j)%rinc_param)
               allocate(default_depac_setup(i, j)%rinc_param, source=rinc_no_resistance())
            end if

            if(use_rinc_no_path(j, i)) then
               deallocate(default_depac_setup(i, j)%rinc_param)
               allocate(default_depac_setup(i, j)%rinc_param, source=rinc_no_path())
            end if
         end do
      end do

   end subroutine init_default_depac_config_rivm

   subroutine finalize_default_depac_config_rivm()
      if (allocated(default_components)) then
         deallocate(default_components)
      end if
      if (allocated(default_land_uses)) then
         deallocate(default_land_uses)
      end if
      if (allocated(default_depac_setup)) then
         deallocate(default_depac_setup)
      end if
   end subroutine finalize_default_depac_config_rivm

end module default_depac_config_rivm

