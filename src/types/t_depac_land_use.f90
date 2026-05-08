!------------------------------------------------------------------------------
! Module:     t_depac_land_use
! Author:     Marte Voorneveld, RIVM
! Created:    2025-11-13
! Updated:    2026-02-27
! Description:
!   This module defines derived types for land use parameters used in
!   atmospheric deposition modeling. It includes types for stomatal parameters,
!   resistance parameters, and the main land use type containing all relevant
!   properties.
!------------------------------------------------------------------------------

module t_depac_land_use
   use t_depac_meteorology, only: depac_meteorology
   use t_depac_config, only: depac_config
   use t_depac_component_core, only: depac_component_core
   use t_depac_land_use_core, only: depac_land_use_core

   implicit none (type, external)
   private
   public :: depac_land_use, depac_stomatal_params, depac_rc_r_params, t_gsoil_parameterisation, &
      t_rinc_parameterisation, t_csoil_parameterisation


   type, abstract :: t_gsoil_parameterisation
   contains
      procedure(i_gsoil_parameterisation), deferred :: apply
   end type t_gsoil_parameterisation

   type, abstract :: t_rinc_parameterisation
   contains
      procedure(i_rinc_parameterisation), deferred :: apply
   end type t_rinc_parameterisation

   type, abstract :: t_csoil_parameterisation
   contains
      procedure(i_csoil_parameterisation), deferred :: apply
   end type t_csoil_parameterisation

   !> Type representing stomatal conductance parameters.
   !! Contains the following fields:
   !! - F_min: Minimum stomatal conductance (typical values 0.01, default -999.0).
   !! - alpha: Alpha for F_light calculation (default -999.0).
   !! - Topt: Optimal temperature for F_temp calculation (default -999.0).
   !! - Tmin: Minimum temperature for F_temp calculation (default -999.0).
   !! - Tmax: Maximum temperature for F_temp calculation (default -999.0).
   !! - g_max: Maximum stomatal conductance (default -999.0).
   !! - vpd_max: Upper VPD limit for stomatal conductance reduction (default -999.0).
   !! - vpd_min: Lower VPD limit for stomatal conductance reduction (default -999.0).
   !! Note: The default values (-999.0) indicate missing or undefined data.
   type :: depac_stomatal_params
      real :: F_min = -999.0
      real :: alpha = -999.0
      real :: Topt = -999.0
      real :: Tmin = -999.0
      real :: Tmax = -999.0
      real :: g_max = -999.0
      real :: vpd_max = -999.0
      real :: vpd_min = -999.0
      class(t_csoil_parameterisation), allocatable :: csoil_param
   end type depac_stomatal_params

   !> Type representing parameters for rinc calculation.
   !! Contains the following fields:
   !! - b: Rinc parameter b (default -999.0).
   !! - h: Rinc parameter h (default -999.0).
   !! Note: The default values (-999.0) indicate missing or undefined data.
   type :: depac_rc_r_params
      real :: b = -999.0
      real :: h = -999.0

      class(t_rinc_parameterisation), allocatable :: rinc_param
   end type depac_rc_r_params


   type , extends(depac_land_use_core) :: depac_land_use
      class(t_gsoil_parameterisation), allocatable :: gsoil_param
      type(depac_stomatal_params) :: stom_par
      type(depac_rc_r_params) :: rc_rinc
   end type depac_land_use

   abstract interface
      pure function i_rinc_parameterisation(this, rc_rinc, meteo, dp_conf) result(rinc)
         import :: t_rinc_parameterisation
         import :: depac_rc_r_params, depac_meteorology, depac_config
         implicit none (type, external)
         class(t_rinc_parameterisation), intent(in) :: this
         type(depac_rc_r_params), intent(in) :: rc_rinc
         type(depac_meteorology), intent(in) :: meteo
         type(depac_config), intent(in) :: dp_conf

         real :: rinc
      end function i_rinc_parameterisation

      pure function i_gsoil_parameterisation(this, lu_conf, comp, meteo, dp_conf) result(gsoil)
         import :: t_gsoil_parameterisation
         import :: depac_land_use, depac_component_core, depac_meteorology, depac_config
         implicit none (type, external)
         class(t_gsoil_parameterisation), intent(in) :: this
         type(depac_land_use), intent(in) :: lu_conf
         class(depac_component_core), intent(in) :: comp
         type(depac_meteorology), intent(in) :: meteo
         type(depac_config), intent(in) :: dp_conf

         real :: gsoil
      end function i_gsoil_parameterisation

      pure function i_csoil_parameterisation(this, lu_conf, dp_conf, tfac) result(csoil)

         import :: t_csoil_parameterisation
         import :: depac_land_use_core, depac_config
         implicit none (type, external)
         class(t_csoil_parameterisation), intent(in) :: this
         class(depac_land_use_core), intent(in) :: lu_conf
         type(depac_config), intent(in) :: dp_conf
         real, intent(in) :: tfac

         real :: csoil
      end function i_csoil_parameterisation
   end interface

end module t_depac_land_use
