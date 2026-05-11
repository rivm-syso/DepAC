
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
   use t_depac_setup, only: depac_setup
   use t_depac_context, only: depac_context

   ! Helper modules
   use m_depac_error, only: clear_error, has_error
   use m_version, only: VERSION, BUILD_DATE
   use m_logger, only: set_log_level
   use m_check_depac_input, only: &
      check_land_use_input, &
      check_depac_config, &
      check_meteorology_input
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
   subroutine depac_calc_partial(setup, ctx)
      type(depac_setup), intent(in) :: setup
      type(depac_context), intent(inout) :: ctx

      logical :: ready   ! Flag indicating if Rc_special is sufficient


      ! initialeze with no deposition path
      ctx%output%rc_tot = -999.0
      !------------------------------------------------------------------
      ! Input validation checks (only disable for performance runs!)
      !------------------------------------------------------------------

      if (setup%config%check_input) then

         call check_land_use_input(setup,ctx)
         if (has_error(ctx%error)) return

         call check_depac_config(setup, ctx)
         if (has_error(ctx%error)) return

         call check_meteorology_input(ctx)
         if (has_error(ctx%error)) return
      end if

      !------------------------------------------------------------------
      ! Clear any previous error and set log level
      !------------------------------------------------------------------
      call clear_error(ctx%error)
      call set_log_level(setup%config%log_level)

      !------------------------------------------------------------------
      ! Determine the presence of leaves and vegetation
      !------------------------------------------------------------------
      ctx%has_leaves = (setup%config%lai > 0.0)
      ! Vegetation is present if there are leaves or if SAI > 0
      ctx%has_vegetation = (setup%config%sai > 0.0)

      !------------------------------------------------------------------
      ! Calculate special canopy resistance (Rc_special)
      ! ready = .true. if Rc_special is sufficient, else further calculation needed
      !------------------------------------------------------------------
      call rc_special(setup, ctx, ready)

      if (has_error(ctx%error)) return

      if (.not. ready) then
         !--------------------------------------------------------------
         ! Compute External Conductance
         !--------------------------------------------------------------
         call rc_gw(setup, ctx)      ! Ground/wet resistance
         if (has_error(ctx%error)) return

         !--------------------------------------------------------------
         ! Compute stomatal Conductance
         !--------------------------------------------------------------
         call rc_gstom(setup, ctx) ! Stomatal resistance
         if (has_error(ctx%error)) return

      end if

      ! Always set version and build date
      ctx%output%version = VERSION
      ctx%output%build_date = BUILD_DATE
   end subroutine depac_calc_partial

   subroutine depac_calc_finish(setup, ctx)
      type(depac_setup), intent(in) :: setup
      type(depac_context), intent(inout) :: ctx

      call clear_error(ctx%error)

      ! the gw and gstom should already be calculated in depac_calc_partial
      ! now calculate gsoil, rc_tot, and optionally comp points and rc_eff (requires u* in meteo)

      if(missing(ctx%output%rc_tot)) then

         !--------------------------------------------------------------
         ! Compute (effective) Soil Conductance
         !--------------------------------------------------------------
         call rc_gsoil(setup, ctx) ! Soil resistance
         if (has_error(ctx%error)) return

         !--------------------------------------------------------------
         ! Compute total canopy resistance and conductance
         !--------------------------------------------------------------
         call rc_tot(ctx) ! Total canopy resistance
         if (has_error(ctx%error)) return

         !--------------------------------------------------------------
         ! Optional: Compensation point calculations
         !--------------------------------------------------------------
         if (setup%config%calc_comp_points) then

            !--------------------------------------------------------------
            ! Compute compensation points
            !--------------------------------------------------------------
            call rc_comp_point(setup, ctx)

            if (has_error(ctx%error)) return

            !----------------------------------------------------------
            ! Optional: Effective resistance calculation based on
            ! Compensation points
            !----------------------------------------------------------
            if (setup%config%calc_effective_rc) then

               call rc_eff(setup, ctx)
               if (has_error(ctx%error)) return
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
   subroutine depac_calc(setup, ctx)
      type(depac_setup), intent(in) :: setup
      type(depac_context), intent(inout) :: ctx

      call depac_calc_partial(setup, ctx)
      if (has_error(ctx%error)) return
      call depac_calc_finish(setup, ctx)
      if (has_error(ctx%error)) return
   end subroutine depac_calc

end module m_depac_calc
