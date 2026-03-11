!------------------------------------------------------------------------------
! Module:     m_rc_gw
! Authors:    Marte Voorneveld, RIVM
!             Addo van Pul, Jan Willem Erisman,
!             Ferd Sauter, Margreet van Zanten, Roy Wichink Kruit
! Created:    November 14 2025
! Updated:    November 20 2025
! Description:
!   This module provides routines for calculating external leaf resistance
!   (gw) for atmospheric deposition modeling components. The main subroutine,
!   rc_gw, computes conductance for NO, NO2, O3, SO2, and NH3, using
!   meteorological, component, and configuration parameters. Includes
!   parameterizations for SO2 and NH3 based on literature and RIVM defaults.
!------------------------------------------------------------------------------
module m_rc_gw
   use t_depac_component, only: depac_component
   use t_depac_config, only: depac_config
   use t_depac_meteorology, only: depac_meteorology
   use t_depac_output, only: depac_output
   use m_logger, only: log_info, log_warn, log_error
   use m_depac_error, only: set_error
   use t_depac_error, only: ERR_INPUT, depac_error
   use m_helpers, only: missing
   use default_indices, only: COMP_NO, COMP_NO2, COMP_O3, COMP_SO2, COMP_NH3

   implicit none (type, external)
   public
contains
   subroutine rc_gw(comp, meteo, dp_conf, dp_out, err)
      type(depac_component), intent(in) :: comp      ! current computed component
      type(depac_config), intent(in) :: dp_conf ! depac config
      type(depac_meteorology), intent(in) :: meteo     ! current computed meteo
      type(depac_output), intent(inout) :: dp_out
      type(depac_error), intent(inout) :: err

      select case(comp%index)
       case(COMP_NO, COMP_O3, COMP_NO2)
         ! Check for missing rw_val (assuming missing is a function or value)
         ! For NO, O3, and NO2, use the provided rw_val if available;
         ! For NO see Erisman et al, 1994 section 3.2.3
         if(.not. missing(comp%rw_val)) then
            if (dp_conf%has_vegetation) then
               dp_out%gw = 1.0/comp%rw_val
            else
               dp_out%gw = 0.0
            endif
         else
            dp_out%gw = -999.0 ! For NO see Erisman et al, 1994 section 3.2.3
            call log_warn('rw_val missing: gw set to -999')
         endif

       case(COMP_SO2)
         call rw_so2(meteo, dp_conf, dp_out, err)

       case(COMP_NH3)
         call rw_nh3_sutton(meteo, dp_conf, dp_out)

       case default
         call set_error(err, ERR_INPUT, 'Component '//trim(comp%name)//' not supported in rc_gw')
         call log_error('Component '//trim(comp%name)//' not supported in rc_gw')
         return
      end select

   end subroutine rc_gw

   subroutine rw_so2(meteo, dp_conf, dp_out, err)
      type(depac_meteorology), intent(in) :: meteo
      type(depac_config), intent(in) :: dp_conf
      type(depac_output), intent(inout) :: dp_out
      type(depac_error), intent(inout) :: err

      real :: rw

      if (dp_conf%has_vegetation) then
         if (meteo%nwet == 0) then
            ! dry surface
            if (meteo%t > -1.0) then
               if (meteo%rh < 81.3) then
                  rw = 25000.0 * exp(-0.0693 * meteo%rh)
               else
                  rw = 0.58e12 * exp(-0.278 * meteo%rh) + 10.0
               endif
            else
               if (meteo%t > -5.0) then
                  rw = 200.0
               else
                  rw = 500.0
               endif
            endif
         else
            ! wet surface
            ! see Table 5, Erisman et al, 1994 Atm. Environment,
            ! 0 is impl. as 10
            rw = 10.0
         endif

         ! very low NH3/SO2 ratio
         if (dp_conf%comp_point%iratns == 3) rw = rw + 50.0

         dp_out%gw = 1.0 / rw
      else
         dp_out%gw = 0.0
      endif

   end subroutine rw_so2

   !------------------------------------------------------------------------------
   ! Subroutine: rw_nh3_sutton
   ! Reference:  Sutton, M.A. & Fowler, D. (1993). A Model for NO, NO2 and NH3
   !             Exchange Between Vegetation and the Atmosphere. In: Exchange of
   !             Trace Gases Between Terrestrial Ecosystems and the Atmosphere,
   !             Andreae & Schimel (eds.), Wiley, pp. 301-313.
   ! Description:
   !   Calculates the external leaf resistance (rw) for NH3 using the parameterization
   !   from Sutton & Fowler (1993). The resistance is a function of relative humidity
   !   and vegetation state. For frozen soil, a fixed value is used. For unfrozen
   !   conditions, the resistance decreases exponentially with increasing relative
   !   humidity, reflecting enhanced cuticular uptake at higher humidity.
   !------------------------------------------------------------------------------
   subroutine rw_nh3_sutton(meteo, dp_conf, dp_out)
      type(depac_meteorology), intent(in) :: meteo
      type(depac_config), intent(in) :: dp_conf
      type(depac_output), intent(inout) :: dp_out

      real :: rw                ! external leaf resistance (s/m)


      !                  100 - rh
      !  rw = 2.0 * exp(----------)
      !                    12


      if (dp_conf%has_vegetation) then
         if (meteo%t < 0.0) then
            ! Frozen soil (from Depac v1):
            dp_out%gw = dp_conf%sai / 200.0
         else
            ! Use the gamma_w parameterization for NH3:
            rw = dp_conf%sai_grass_haarweg * 2.0 *exp((100.0 - meteo%rh) / 12.0)
            dp_out%gw = dp_conf%sai / rw
         end if
      else
         ! no vegetation:
         dp_out%gw = 0.0
      endif

   end subroutine rw_nh3_sutton
end module m_rc_gw
