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
   use t_depac_land_use, only: depac_land_use, depac_stomatal_params, depac_rc_r_params
   use t_depac_error, only: ERR_INPUT, depac_error
   use m_depac_error, only: set_error
   use m_logger, only: log_error


   use m_gw_param, only: gw_default, gw_so2, gw_nh3_sutton
   use m_gstom_param, only: gstom_default, gstom_emberson
   use m_comp_point_param, only: comp_point_ammonia, csoil_default, csoil_water
   use m_gsoil_param, only:  rinc_no_resistance, rinc_default, rinc_no_path
   use m_rc_special_param, only: rc_tot_nitric_acid, rc_tot_nitric_oxide, rc_tot_fixed


   use default_indices, only: COMP_NH3, COMP_O3, COMP_SO2, COMP_NO2, COMP_NO, COMP_HNO3, &
      LU_GRASS, LU_ARABLE, LU_PERMANENT_CROPS, LU_CONIFEROUS_FOREST, LU_DECIDUOUS_FOREST, &
      LU_WATER, LU_URBAN, LU_OTHER, LU_DESERT
   use m_depac_factory, only: make_component, make_land_use, make_stom_params, make_rc_r_params


   implicit none (type, external)

   ! SEE default_indices.f90 for indices of land use and component types
   ! These are used here in the default configuration and can be used by
   ! users to refer to these types in their input.

   ! Default component types
   type(depac_component), dimension(:), allocatable, public :: default_component_types


   type(depac_land_use), dimension(:), allocatable, public :: default_landuse_types

   ! Default land use types with associated parameters used by RIVM



   real, dimension(9, 6) :: default_rsoil_matrix
   type(depac_land_use), dimension(9,6), public :: default_land_use_matrix
   type(depac_component), dimension(9,6), public :: default_component_matrix
   public

contains

   subroutine init_default_depac_config_rivm()
      integer :: i, j

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
      allocate(default_component_types(6))
      default_component_types =  [ &
         make_component( &
         name = 'NH3', &
         index = COMP_NH3, &
         diffc = 0.21e-4, &
         rw_val = -999.0, &
         ipar_snow = 2, &
         rsoil_frozen = 1000.0, &
         rsoil_wet = 10.0, &
         gw_param = gw_nh3_sutton(), &
         gstom_param = gstom_emberson(), &
         comp_point_param = comp_point_ammonia() &
      ), &
         make_component( &
         name = 'O3', &
         index = COMP_O3, &
         diffc = 0.13e-4, &
         rw_val = 1000.0, &
         ipar_snow = 1, &
         rsoil_frozen = 2000.0, &
         rsoil_wet = 2000.0, &
         gw_param = gw_so2(), &
         gstom_param = gstom_emberson()), &
         make_component( &
         name = 'SO2', &
         index = COMP_SO2, &
         diffc = 0.11e-4, &
         rw_val = -999.0, &
         ipar_snow = 2, &
         rsoil_frozen = 500.0, &
         rsoil_wet = 10.0, &
         gstom_param = gstom_emberson()), &
         make_component( &
         name = 'NO2', &
         index = COMP_NO2, &
         diffc = 0.13e-4, &
         rw_val = 2000.0, &
         ipar_snow = 1, &
         rsoil_frozen = 2000.0, &
         rsoil_wet = 2000.0, &
         gstom_param = gstom_emberson()), &
         make_component( &
         name = 'NO', &
         index = COMP_NO, &
         diffc = 0.16e-4, &
         rw_val = -999.0, &
         ipar_snow = 1, &
         rsoil_frozen = -999.0, &
         rsoil_wet = -999.0, &
         rc_special = rc_tot_nitric_oxide()), &
         make_component( &
         name = 'HNO3', &
         index = COMP_HNO3, &
         diffc = -999.0, &
         rw_val = -999.0, &
         ipar_snow = -999, &
         rsoil_frozen = -999.0, &
         rsoil_wet = -999.0, &
         rc_special = rc_tot_nitric_acid()) &
         ]

      allocate(default_landuse_types(9))



      default_landuse_types = [ &
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
         vpd_min = 3.0), &
         rc_rinc = make_rc_r_params( &
            rinc_param =  rinc_no_path() &
         )), &
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
         vpd_min = -999.0, &
         csoil_param = csoil_water()), &
         rc_rinc = make_rc_r_params( &
         rinc_param = rinc_no_resistance())), &
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
         vpd_min = -999.0), &
         rc_rinc = make_rc_r_params( &
         rinc_param = rinc_no_resistance())), &
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
         vpd_min = 3.0), &
         rc_rinc = make_rc_r_params( &
         rinc_param = rinc_no_path() &
         )), &
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
         vpd_min = -999.0), &
         rc_rinc = make_rc_r_params( &
            rinc_param = rinc_no_resistance())) &
         ]

      ! Initialize the default_land_use_matrix with rsoil values from the matrix

      do i = 1, 9
         do j = 1, 6
            default_land_use_matrix(i, j) = default_landuse_types(i)
            default_land_use_matrix(i, j)%rsoil = default_rsoil_matrix(i, j)
         end do
      end do

      ! Initialize the default_component_matrix with the default component types
      do i = 1, 9
         do j = 1, 6
            default_component_matrix(i, j) = default_component_types(j)
            ! we use a fixed total canopy resistance of 2000 s/m for NO on water and wet surfaces
            if (default_component_matrix(i, j)%index == COMP_NO .and. &
               default_land_use_matrix(i, j)%index == LU_WATER) then
               deallocate(default_component_matrix(i, j)%rc_special)
               allocate(default_component_matrix(i, j)%rc_special, source=rc_tot_fixed())
            end if
         end do
      end do

   end subroutine init_default_depac_config_rivm


end module default_depac_config_rivm
