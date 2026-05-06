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




   use default_indices, only: COMP_NH3, COMP_O3, COMP_SO2, COMP_NO2, COMP_NO, COMP_HNO3, &
      LU_GRASS, LU_ARABLE, LU_PERMANENT_CROPS, LU_CONIFEROUS_FOREST, LU_DECIDUOUS_FOREST, &
      LU_WATER, LU_URBAN, LU_OTHER, LU_DESERT
   use m_depac_factory, only: make_component


   implicit none (type, external)

   ! SEE default_indices.f90 for indices of land use and component types
   ! These are used here in the default configuration and can be used by
   ! users to refer to these types in their input.

   ! Default component types
   type(depac_component), dimension(:), allocatable, public :: default_component_types

   ! Default land use types with associated parameters used by RIVM
   type(depac_land_use), dimension(9), public :: default_landuse_types = [ &
      depac_land_use( &
      name = 'grass', &
      index = LU_GRASS, &
      gamma_stom_c_fac = 362.0, &
      gamma_soil_c_fac = -999.0, &
      rsoil = -999.0, &
      stom_par = depac_stomatal_params( &
      F_min = 0.01, &
      alpha = 4.57*0.009, &
      Topt = 26.0, &
      Tmin = 12.0, &
      Tmax = 40.0, &
      g_max = 270.0/41000, &
      vpd_max = 1.3, &
      vpd_min = 3.0), &
      rc_rinc = depac_rc_r_params( &
      b = -999.0, &
      h = -999.0)), &
      depac_land_use( &
      name = 'arable', &
      index = LU_ARABLE, &
      gamma_stom_c_fac = 362.0, &
      gamma_soil_c_fac = -999.0, &
      rsoil = -999.0, &
      stom_par = depac_stomatal_params( &
      F_min = 0.01, &
      alpha = 4.57*0.009, &
      Topt = 26.0, &
      Tmin = 12.0, &
      Tmax = 40.0, &
      g_max = 300.0/41000, &
      vpd_max = 0.9, &
      vpd_min = 2.8), &
      rc_rinc = depac_rc_r_params( &
      b = 14.0, &
      h = 1.0)), &
      depac_land_use( &
      name = 'permanent_crops', &
      index = LU_PERMANENT_CROPS, &
      gamma_stom_c_fac = 362.0, &
      gamma_soil_c_fac = -999.0, &
      rsoil = -999.0, &
      stom_par = depac_stomatal_params( &
      F_min = 0.01, &
      alpha = 4.57*0.009, &
      Topt = 26.0, &
      Tmin = 12.0, &
      Tmax = 40.0, &
      g_max = 300.0/41000, &
      vpd_max = 0.9, &
      vpd_min = 2.8), &
      rc_rinc = depac_rc_r_params( &
      b = 14.0, &
      h = 1.0)), &
      depac_land_use( &
      name = 'coniferous_forest', &
      index = LU_CONIFEROUS_FOREST, &
      gamma_stom_c_fac = 362.0, &
      gamma_soil_c_fac = -999.0, &
      rsoil = -999.0, &
      stom_par = depac_stomatal_params( &
      F_min = 0.1, &
      alpha = 4.57*0.006, &
      Topt = 18.0, &
      Tmin = 0.0, &
      Tmax = 36.0, &
      g_max = 140.0/41000, &
      vpd_max = 0.5, &
      vpd_min = 3.0), &
      rc_rinc = depac_rc_r_params( &
      b = 14.0, &
      h = 20.0)), &
      depac_land_use( &
      name = 'deciduous_forest', &
      index = LU_DECIDUOUS_FOREST, &
      gamma_stom_c_fac = 362.0, &
      gamma_soil_c_fac = -999.0, &
      rsoil = -999.0, &
      stom_par = depac_stomatal_params( &
      F_min = 0.1, &
      alpha = 4.57*0.006, &
      Topt = 20.0, &
      Tmin = 0.0, &
      Tmax = 35.0, &
      g_max = 150.0/41000, &
      vpd_max = 1.0, &
      vpd_min = 3.25), &
      rc_rinc = depac_rc_r_params( &
      b = 14.0, &
      h = 20.0)), &
      depac_land_use( &
      name = 'water', &
      index = LU_WATER, &
      gamma_stom_c_fac = -999.0, &
      gamma_soil_c_fac = 430.0, &
      rsoil = -999.0, &
      stom_par = depac_stomatal_params( &
      F_min = -999.0, &
      alpha = -999.0, &
      Topt = -999.0, &
      Tmin = -999.0, &
      Tmax = -999.0, &
      g_max = -999.0, &
      vpd_max = -999.0, &
      vpd_min = -999.0), &
      rc_rinc = depac_rc_r_params( &
      b = -999.0, &
      h = -999.0)), &
      depac_land_use( &
      name = 'urban', &
      index = LU_URBAN, &
      gamma_stom_c_fac = -999.0, &
      gamma_soil_c_fac = -999.0, &
      rsoil = -999.0, &
      stom_par = depac_stomatal_params( &
      F_min = -999.0, &
      alpha = -999.0, &
      Topt = -999.0, &
      Tmin = -999.0, &
      Tmax = -999.0, &
      g_max = -999.0, &
      vpd_max = -999.0, &
      vpd_min = -999.0), &
      rc_rinc = depac_rc_r_params( &
      b = -999.0, &
      h = -999.0)), &
      depac_land_use( &
      name = 'other', &
      index = LU_OTHER, &
      gamma_stom_c_fac = 362.0, &
      gamma_soil_c_fac = -999.0, &
      rsoil = -999.0, &
      stom_par = depac_stomatal_params( &
      F_min = 0.01, &
      alpha = 4.57*0.009, &
      Topt = 26.0, &
      Tmin = 12.0, &
      Tmax = 40.0, &
      g_max = 270.0/41000, &
      vpd_max = 1.3, &
      vpd_min = 3.0), &
      rc_rinc = depac_rc_r_params( &
      b = -999.0, &
      h = -999.0)), &
      depac_land_use( &
      name = 'desert', &
      index = LU_DESERT, &
      gamma_stom_c_fac = -999.0, &
      gamma_soil_c_fac = -999.0, &
      rsoil = -999.0, &
      stom_par = depac_stomatal_params( &
      F_min = -999.0, &
      alpha = -999.0, &
      Topt = -999.0, &
      Tmin = -999.0, &
      Tmax = -999.0, &
      g_max = -999.0, &
      vpd_max = -999.0, &
      vpd_min = -999.0), &
      rc_rinc = depac_rc_r_params( &
      b = -999.0, &
      h = -999.0)) &
      ]

   ! Matrix of rsoil values for each land use and component type
   real, dimension(9, 6), public :: default_rsoil_matrix = reshape([ &
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

   public

contains

   subroutine init_default_depac_config_rivm()
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
            gw_param = gw_nh3_sutton), &
         make_component( &
            name = 'O3', &
            index = COMP_O3, &
            diffc = 0.13e-4, &
            rw_val = 1000.0, &
            ipar_snow = 1, &
            rsoil_frozen = 2000.0, &
            rsoil_wet = 2000.0, &
            gw_param = gw_so2), &
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




   end subroutine init_default_depac_config_rivm

   ! Subroutine to obtain component and land use configurations based on indices
   subroutine obtain_config(lu_int, comp_int, dp_comp, dp_lu, err)
      integer, intent(in) :: lu_int, comp_int
      type(depac_component), intent(out) :: dp_comp
      type(depac_land_use), intent(out) :: dp_lu
      type(depac_error), intent(inout) :: err

      if (lu_int < 1 .or. lu_int > size(default_landuse_types)) then
         call set_error(err, ERR_INPUT, 'land use index out of range')
         call log_error('land use index out of range')
         return
      endif

      if (comp_int < 1 .or. comp_int > size(default_component_types)) then
         call set_error(err, ERR_INPUT, 'component index out of range')
         call log_error('component index out of range')
         return
      endif

      dp_comp = default_component_types(comp_int)
      dp_lu = default_landuse_types(lu_int)
      dp_lu%rsoil = default_rsoil_matrix(lu_int, comp_int)
   end subroutine obtain_config

   ! Subroutine to find component index based on name
   subroutine find_component_index(name, comp_index, err)
      character(len=*), intent(in) :: name
      integer, intent(out) :: comp_index
      type(depac_error), intent(inout) :: err
      integer :: i

      comp_index = -1
      do i = 1, size(default_component_types)
         if (trim(default_component_types(i)%name) == trim(name)) then
            comp_index = i
            return
         end if
      end do
      call set_error(err, ERR_INPUT, 'Component name not found: '//trim(name))
      call log_error('Component name not found: '//trim(name))
   end subroutine find_component_index

   ! Subroutine to find land use index based on name
   subroutine find_landuse_index(name, lu_index, err)
      character(len=*), intent(in) :: name
      integer, intent(out) :: lu_index
      type(depac_error), intent(inout) :: err
      integer :: i

      lu_index = -1
      do i = 1, size(default_landuse_types)
         if (trim(default_landuse_types(i)%name) == trim(name)) then
            lu_index = i
            return
         end if
      end do
      call set_error(err, ERR_INPUT, 'Land use name not found: '//trim(name))
      call log_error('Land use name not found: '//trim(name))
   end subroutine find_landuse_index

end module default_depac_config_rivm
