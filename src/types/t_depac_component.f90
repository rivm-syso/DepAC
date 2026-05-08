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
   public :: depac_component, gw_parameterisation, &
      gstom_parameterisation, comp_point_parameterisation, t_rc_special


   ! The main component type that includes parameterisation function pointers for gw and gstom.
   type, extends(depac_component_core) :: depac_component
      procedure(gw_parameterisation), pointer, nopass :: gw_param => null()
      procedure(gstom_parameterisation), pointer, nopass :: gstom_param => null()
      procedure(comp_point_parameterisation), pointer, nopass :: comp_point_param => null()
      class(t_rc_special), allocatable :: rc_special
   end type depac_component


    type, abstract :: t_rc_special
    contains
        procedure(rc_special_interface), deferred :: apply
    end type t_rc_special

    abstract interface
        subroutine rc_special_interface(this, meteo, comp, dp_conf, dp_out, err, ready)
            import :: t_rc_special
            import :: depac_meteorology
            import :: depac_component_core
            import :: depac_config
            import :: depac_output
            import :: depac_error
            implicit none (type, external)
            class(t_rc_special), intent(in) :: this
            type(depac_meteorology), intent(in) :: meteo
            class(depac_component_core), intent(in) :: comp
            type(depac_config), intent(in) :: dp_conf
            type(depac_output), intent(inout) :: dp_out
            type(depac_error), intent(inout) :: err
            logical, intent(inout) :: ready
        end subroutine rc_special_interface
    end interface


   abstract interface
      function gw_parameterisation(meteo, comp, dp_conf, err) result(gw)
         import :: depac_meteorology, depac_component_core, depac_config, depac_error
         implicit none (type, external)

         type(depac_meteorology), intent(in) :: meteo
         class(depac_component_core), intent(in) :: comp
         type(depac_config), intent(in) :: dp_conf
         type(depac_error), intent(inout) :: err

         real :: gw
      end function gw_parameterisation


      pure function gstom_parameterisation(comp, stom_par, meteo, dp_conf) result(gstom)
         import :: depac_component_core, depac_stomatal_params, depac_meteorology, depac_config
         implicit none (type, external)
         class(depac_component_core), intent(in) :: comp
         type(depac_stomatal_params), intent(in) :: stom_par
         type(depac_meteorology), intent(in) :: meteo
         type(depac_config), intent(in) :: dp_conf

         real :: gstom
      end function gstom_parameterisation

      pure function comp_point_parameterisation(meteo, lu_conf, dp_conf, dp_out) result(ccomp_tot)
         import :: depac_meteorology, depac_land_use, depac_config, depac_output
         implicit none (type, external)
         type(depac_meteorology), intent(in) :: meteo
         type(depac_land_use), intent(in) :: lu_conf
         type(depac_config), intent(in) :: dp_conf
         type(depac_output), intent(in) :: dp_out

         real :: ccomp_tot
      end function comp_point_parameterisation


   end interface

end module t_depac_component
