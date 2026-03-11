 !-------------------------------------------------------------------------------
 ! File:        m_rb.f90
 ! Module:      m_rb
 ! Purpose:     Functions for calculating aerodynamic resistance (Rb)
 !              Currently: B.B. Hicks parameterization
 !              Future: McNaughton, Rigden, Bluff parameterizations
 ! Author:      Marte Voorneveld, RIVM
 ! Created:     November 13, 2025
 !-------------------------------------------------------------------------------
module m_rb
   use t_depac_meteorology, only: depac_meteorology
   use t_depac_component, only: depac_component
   use t_depac_config, only: depac_config
   use m_logger, only: log_debug
   implicit none (type, external)

   public
contains

   !--------------------------------------------------------------------------
   ! Function:   depac_calc_rb_hicks
   ! Author:     Marte Voorneveld, RIVM
   ! Created:    November 13 2025
   ! Updated:    November 13 2025
   ! Description:
   !   This function calculates the aerodynamic resistance (rb) using the
   !   Hicks parametrization based on a simple u star formula.
   ! with diffusion coefficient adjustment.
   !--------------------------------------------------------------------------
   function depac_calc_rb_hicks(meteo, comp) result(rb)
      type(depac_meteorology), intent(in) :: meteo
      type(depac_component), intent(in) :: comp
      real :: rb
      real, parameter :: thk = 0.20e-4   ! thermal diffusivity of dry air 20 C


        if (meteo%ust <= 0.0) then
             rb = -999.0
             call log_debug('Invalid friction velocity (ust <= 0). Returning -999.0 for rb.')
             return
        end if

        if (comp%diffc <= 0.0) then
             rb = -999.0
             call log_debug('Invalid diffusion coefficient (diffc <= 0). Returning -999.0 for rb.')
             return
        end if


      rb = 5.0 / meteo%ust

      rb = rb*(thk/comp%diffc)**.67

   end function depac_calc_rb_hicks

end module m_rb
