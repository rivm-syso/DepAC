!------------------------------------------------------------------------------
! Module:     t_depac_component
! Author:     Marte Voorneveld, RIVM
! Created:    2025-11-28
! Updated:    2026-02-27
! Description:
!   This module defines derived types for atmospheric deposition modeling
!   components. It includes the 'lu_rsoil' type for land use-specific soil
!   resistance values, and the 'component' type for chemical species and
!   their associated parameters such as diffusion coefficient, snow/soil
!   resistance, and land use-dependent soil resistance.
!------------------------------------------------------------------------------

module t_depac_component
   use t_depac_meteorology, only: depac_meteorology
   use t_depac_config, only: depac_config
   use t_depac_error, only: depac_error
   use t_depac_component_core, only: depac_component_core
   use t_depac_land_use_core, only: depac_land_use_core
   use t_depac_land_use, only: depac_stomatal_params, depac_land_use
   use t_depac_output, only: depac_output

   implicit none (type, external)

   private
   public :: depac_component, t_gw_parameterisation, &
      t_gstom_parameterisation, t_comp_point_parameterisation, t_rc_special_param


   ! The main component type that includes parameterisation function pointers for gw and gstom.
   type, extends(depac_component_core) :: depac_component
      class(t_gw_parameterisation), allocatable :: gw_param
      class(t_gstom_parameterisation), allocatable :: gstom_param
      class(t_comp_point_parameterisation), allocatable :: comp_point_param
      class(t_rc_special_param), allocatable :: rc_special
   end type depac_component

   type, abstract :: t_gw_parameterisation
   contains
      procedure(i_gw_parameterisation), deferred :: apply
   end type t_gw_parameterisation

   type, abstract :: t_rc_special_param
   contains
      procedure(i_rc_special), deferred :: apply
   end type t_rc_special_param

   type, abstract :: t_gstom_parameterisation
   contains
      procedure(i_gstom_parameterisation), deferred :: apply
   end type t_gstom_parameterisation

   type, abstract :: t_comp_point_parameterisation
   contains
      procedure(i_comp_point_parameterisation), deferred :: apply
   end type t_comp_point_parameterisation




   abstract interface
      subroutine i_rc_special(this, meteo, comp, dp_conf, dp_out, err, ready)
         import :: t_rc_special_param
         import :: depac_meteorology
         import :: depac_component_core
         import :: depac_config
         import :: depac_output
         import :: depac_error
         implicit none (type, external)
         class(t_rc_special_param), intent(in) :: this
         type(depac_meteorology), intent(in) :: meteo
         class(depac_component_core), intent(in) :: comp
         type(depac_config), intent(in) :: dp_conf
         type(depac_output), intent(inout) :: dp_out
         type(depac_error), intent(inout) :: err
         logical, intent(inout) :: ready
      end subroutine i_rc_special
      function i_gw_parameterisation(this, meteo, comp, dp_conf, err) result(gw)
         import :: t_gw_parameterisation
         import :: depac_meteorology, depac_component_core, depac_config, depac_error
         implicit none (type, external)

         class(t_gw_parameterisation), intent(in) :: this
         type(depac_meteorology), intent(in) :: meteo
         class(depac_component_core), intent(in) :: comp
         type(depac_config), intent(in) :: dp_conf
         type(depac_error), intent(inout) :: err

         real :: gw
      end function i_gw_parameterisation


      pure function i_gstom_parameterisation(this, comp, stom_par, meteo, dp_conf) result(gstom)
         import :: t_gstom_parameterisation
         import :: depac_component_core, depac_stomatal_params, depac_meteorology, depac_config
         implicit none (type, external)
         class(t_gstom_parameterisation), intent(in) :: this
         class(depac_component_core), intent(in) :: comp
         type(depac_stomatal_params), intent(in) :: stom_par
         type(depac_meteorology), intent(in) :: meteo
         type(depac_config), intent(in) :: dp_conf

         real :: gstom
      end function i_gstom_parameterisation

      pure function i_comp_point_parameterisation(this, meteo, lu_conf, dp_conf, dp_out) result(ccomp_tot)
         import :: t_comp_point_parameterisation
         import :: depac_meteorology, depac_land_use, depac_config, depac_output
         implicit none (type, external)
         class(t_comp_point_parameterisation), intent(in) :: this
         type(depac_meteorology), intent(in) :: meteo
         type(depac_land_use), intent(in) :: lu_conf
         type(depac_config), intent(in) :: dp_conf
         type(depac_output), intent(in) :: dp_out

         real :: ccomp_tot
      end function i_comp_point_parameterisation


   end interface

end module t_depac_component
