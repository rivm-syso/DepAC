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

   use c_params, only: gsoil_default, gw_default, gstom_default, comp_point_default, &
      rc_special_default, rinc_default, rinc_no_path, rinc_no_resistance, csoil_default, csoil_water, &
       gw_nh3_sutton, gw_so2, gstom_emberson, comp_point_ammonia, rc_tot_nitric_acid, &
       rc_tot_nitric_oxide, rc_tot_fixed

   use default_indices, only: COMP_NH3, COMP_O3, COMP_SO2, COMP_NO2, COMP_NO, COMP_HNO3, &
      LU_GRASS, LU_ARABLE, LU_PERMANENT_CROPS, LU_CONIFEROUS_FOREST, LU_DECIDUOUS_FOREST, &
      LU_WATER, LU_URBAN, LU_OTHER, LU_DESERT
   use m_depac_factory, only: make_component, make_land_use, make_setup, make_stom_params, make_rc_r_params


   implicit none (type, external)

   ! SEE default_indices.f90 for indices of land use and component types
   ! These are used here in the default configuration and can be used by
   ! users to refer to these types in their input.




   type(depac_component), dimension(:), allocatable, public :: default_components
   type(depac_land_use), dimension(:), allocatable, public :: default_land_uses
   type(depac_setup), dimension(:,:), allocatable, public :: default_depac_setup


   

   public

contains

   subroutine init_default_depac_config_rivm()
      integer :: i, j
      logical, dimension(6,9) :: use_gw_nh3_sutton = .false.
      logical, dimension(6,9) :: use_gw_so2 = .false.

      logical, dimension(6,9) :: use_gstom_emberson = .false.
      logical, dimension(6,9) :: use_comp_point_ammonia = .false.
      
      logical, dimension(6,9) :: use_rc_tot_nitric_acid = .false.
      logical, dimension(6,9) :: use_rc_tot_nitric_oxide = .false.
      logical, dimension(6,9) :: use_rc_tot_fixed = .false.

      logical, dimension(6,9) :: use_rinc_no_resistance = .false.
      logical, dimension(6,9) :: use_rinc_no_path = .false.
      
      real, dimension(9, 6) :: default_rsoil_matrix


      use_gw_nh3_sutton(COMP_NH3,:) = .true.
      use_gw_so2(COMP_SO2,:) = .true.
      
      use_gstom_emberson(COMP_NH3, :) = .true.
      use_gstom_emberson(COMP_O3, :) = .true.
      use_gstom_emberson(COMP_SO2, :) = .true.
      use_gstom_emberson(COMP_NO2, :) = .true.

      use_comp_point_ammonia(COMP_NH3, :) = .true.

      
      use_rc_tot_nitric_acid(COMP_HNO3, :) = .true.
      use_rc_tot_nitric_oxide(COMP_NO, :) = .true.

      use_rc_tot_fixed(COMP_NO, LU_WATER) = .true.

      use_rinc_no_resistance(:, LU_WATER) = .true.
      use_rinc_no_resistance(:, LU_URBAN) = .true.
      use_rinc_no_resistance(:, LU_DESERT) = .true.

      use_rinc_no_path(:, LU_OTHER) = .true.
      use_rinc_no_path(:, LU_GRASS) = .true.







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
         index = COMP_NH3, &
         diffc = 0.21e-4, &
         rw_val = -999.0, &
         ipar_snow = 2, &
         rsoil_frozen = 1000.0, &
         rsoil_wet = 10.0), &
         make_component( &
         name = 'O3', &
         index = COMP_O3, &
         diffc = 0.13e-4, &
         rw_val = 1000.0, &
         ipar_snow = 1, &
         rsoil_frozen = 2000.0, &
         rsoil_wet = 2000.0), &
         make_component( &
         name = 'SO2', &
         index = COMP_SO2, &
         diffc = 0.11e-4, &
         rw_val = -999.0, &
         ipar_snow = 2, &
         rsoil_frozen = 500.0, &
         rsoil_wet = 10.0), &
         make_component( &
         name = 'NO2', &
         index = COMP_NO2, &
         diffc = 0.13e-4, &
         rw_val = 2000.0, &
         ipar_snow = 1, &
         rsoil_frozen = 2000.0, &
         rsoil_wet = 2000.0), &
         make_component( &
         name = 'NO', &
         index = COMP_NO, &
         diffc = 0.16e-4, &
         rw_val = -999.0, &
         ipar_snow = 1, &
         rsoil_frozen = -999.0, &
         rsoil_wet = -999.0), &
         make_component( &
         name = 'HNO3', &
         index = COMP_HNO3, &
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
         index = LU_GRASS, &
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
         index = LU_ARABLE, &
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
         index = LU_PERMANENT_CROPS, &
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
         index = LU_CONIFEROUS_FOREST, &
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
         index = LU_DECIDUOUS_FOREST, &
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
         index = LU_WATER, &
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
         index = LU_URBAN, &
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
         index = LU_OTHER, &
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
         index = LU_DESERT, &
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
         index = LU_DESERT, &
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



end module default_depac_config_rivm

