
!------------------------------------------------------------------------------
! Module:     m_depac_calc
! Author:     Marte Voorneveld, RIVM
! Description:
!   This module provides routines for calculating canopy resistance (Rc) and related
!   resistances in the DepAC atmospheric deposition model. It supports partial and
!   full calculation flows, with input validation and modular resistance components.
!   Created:    January 15, 2026
!   Last update: January 15, 2026
!------------------------------------------------------------------------------
module m_depac_calc
   use t_depac_location, only: depac_location
   use t_depac_land_use, only: depac_land_use
   use t_depac_component, only: depac_component
   use t_depac_meteorology, only: depac_meteorology
   use t_depac_config, only: depac_config
   use t_depac_output, only: depac_output

   ! Helper modules
   use m_depac_error, only: clear_error, has_error, set_error
   use t_depac_error, only: ERR_INPUT, depac_error
   use m_version, only: VERSION, BUILD_DATE
   use m_logger, only: set_log_level
   use m_check_depac_input, only: &
      check_component_input, &
      check_land_use_input, &
      check_depac_config, &
      check_meteorology_input
   use default_depac_config_rivm, only: default_landuse_types, default_component_types, &
      default_rsoil_matrix
   use m_helpers, only: missing

   ! Calculation modules
   use m_rc_special, only: rc_special
   use m_rc_gw, only: rc_gw
   use m_rc_gstom, only: rc_gstom
   use m_rc_gsoil, only: rc_gsoil
   use m_rc_tot, only: rc_tot
   use m_comp_points, only: rc_comp_point
   use m_rc_eff, only: rc_eff

   implicit none (type, external)
   private
   public :: depac_calc, depac_calc_partial, depac_calc_finish
contains
   subroutine depac_calc_partial(comp, lu, meteo, dp_conf, dp_out, err)
      type(depac_component), intent(in) :: comp
      type(depac_land_use), intent(in) :: lu
      type(depac_meteorology), intent(inout) :: meteo
      type(depac_config), intent(inout) :: dp_conf
      type(depac_output), intent(out) :: dp_out
      type(depac_error), intent(out) :: err

      logical :: result   ! Error status flag
      logical :: ready   ! Flag indicating if Rc_special is sufficient


      dp_out%rc_tot = -999.0
      !------------------------------------------------------------------
      ! Input validation checks (only disable for performance runs!)
      !------------------------------------------------------------------
      if (dp_conf%check_input) then
         call check_component_input(comp, err)
         if (has_error(err)) return

         call check_land_use_input(lu, err)
         if (has_error(err)) return

         call check_depac_config(dp_conf, err)
         if (has_error(err)) return

         call check_meteorology_input(meteo, err)
         if (has_error(err)) return

      end if

      !------------------------------------------------------------------
      ! Clear any previous error and set log level
      !------------------------------------------------------------------
      call clear_error(err)
      call set_log_level(dp_conf%log_level)

      !------------------------------------------------------------------
      ! Determine the presence of leaves and vegetation
      !------------------------------------------------------------------
      dp_conf%has_leaves = (dp_conf%lai > 0.0)
      ! Vegetation is present if there are leaves or if SAI > 0
      dp_conf%has_vegetation = (dp_conf%sai > 0.0)

      !------------------------------------------------------------------
      ! Calculate special canopy resistance (Rc_special)
      ! ready = .true. if Rc_special is sufficient, else further calculation needed
      !------------------------------------------------------------------
      call rc_special(comp, lu, meteo, dp_conf, dp_out, ready, err)

      if (has_error(err)) return

      if (.not. ready) then
         !--------------------------------------------------------------
         ! Compute External Conductance
         !--------------------------------------------------------------
         call rc_gw(comp, meteo, dp_conf, dp_out, err)      ! Ground/wet resistance
         if (has_error(err)) return

         !--------------------------------------------------------------
         ! Compute stomatal Conductance
         !--------------------------------------------------------------
         call rc_gstom(comp, lu, meteo, dp_conf, dp_out, err) ! Stomatal resistance
         if (has_error(err)) return

      end if

      ! Always set version and build date
      dp_out%version = VERSION
      dp_out%build_date = BUILD_DATE
   end subroutine depac_calc_partial

   subroutine depac_calc_finish(comp, lu, meteo, dp_conf, dp_out, err)
      type(depac_component), intent(in) :: comp
      type(depac_land_use), intent(in) :: lu
      type(depac_meteorology), intent(inout) :: meteo
      type(depac_config), intent(inout) :: dp_conf
      type(depac_output), intent(inout) :: dp_out
      type(depac_error), intent(out) :: err

      call clear_error(err)

      ! the gw and gstom should already be calculated in depac_calc_partial
      ! now calculate gsoil, rc_tot, and optionally comp points and rc_eff (requires u* in meteo)

      if(missing(dp_out%rc_tot)) then

         !--------------------------------------------------------------
         ! Compute (effective) Soil Conductance
         !--------------------------------------------------------------
         call rc_gsoil(lu, meteo, comp, dp_conf, dp_out, err) ! Soil resistance
         if (has_error(err)) return

         !--------------------------------------------------------------
         ! Compute total canopy resistance and conductance
         !--------------------------------------------------------------
         call rc_tot(dp_out, err)
         if (has_error(err)) return

         !--------------------------------------------------------------
         ! Optional: Compensation point calculations
         !--------------------------------------------------------------
         if (dp_conf%calc_comp_points) then

            !--------------------------------------------------------------
            ! Compute compensation points
            !--------------------------------------------------------------
            call rc_comp_point(comp, lu, meteo, dp_conf, dp_out, err)
            if (has_error(err)) return

            !----------------------------------------------------------
            ! Optional: Effective resistance calculation based on
            ! Compensation points
            !----------------------------------------------------------
            if (dp_conf%calc_effective_rc) then

               call rc_eff(dp_out, dp_conf, err)
               if (has_error(err)) return
            end if
         end if
      end if
   end subroutine depac_calc_finish

   !------------------------------------------------------------------------------
   ! Submodule:  depac_calc (main calculation routine)
   ! Description:
   !   This subroutine performs the full DepAC calculation flow, combining partial
   !   and finish steps for canopy resistance and related resistances.
   !------------------------------------------------------------------------------
   subroutine depac_calc(comp, lu, meteo, dp_conf, dp_out, err)
      type(depac_component), intent(in) :: comp
      type(depac_land_use), intent(in) :: lu
      type(depac_meteorology), intent(inout) :: meteo
      type(depac_config), intent(inout) :: dp_conf
      type(depac_output), intent(out) :: dp_out
      type(depac_error), intent(out) :: err

      call depac_calc_partial(comp, lu, meteo, dp_conf, dp_out, err)
      if (has_error(err)) return
      call depac_calc_finish(comp, lu, meteo, dp_conf, dp_out, err)
      if (has_error(err)) return
   end subroutine depac_calc

end module m_depac_calc
