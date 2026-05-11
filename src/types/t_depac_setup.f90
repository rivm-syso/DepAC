module t_depac_setup
   use t_depac_config_core, only: depac_config_core
   use t_depac_land_use, only: depac_land_use
   use t_depac_component, only: depac_component

   use c_depac_param_types, only: depac_comp_point_param, depac_csoil_param, &
      depac_gsoil_param, &
      depac_gstom_param, depac_gw_param, depac_rc_special_param, &
      depac_rinc_param

   implicit none (type, external)
   private
   public :: depac_setup

   type :: depac_setup

      type(depac_config_core) :: config
      type(depac_land_use) :: land_use
      type(depac_component) :: component

      ! here the parameterisations are added
      class(depac_gsoil_param), allocatable :: gsoil_param
      class(depac_csoil_param), allocatable :: csoil_param
      class(depac_rinc_param), allocatable :: rinc_param
      class(depac_gw_param), allocatable :: gw_param
      class(depac_gstom_param), allocatable :: gstom_param
      class(depac_comp_point_param), allocatable :: comp_point_param
      class(depac_rc_special_param), allocatable :: rc_special_param

   end type depac_setup

end module t_depac_setup
