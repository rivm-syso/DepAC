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
   use t_depac_context, only: depac_context
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
   function depac_calc_rb_hicks(ctx, ust) result(rb)
      type(depac_context), intent(in) :: ctx
      real, intent(in) :: ust
      real :: rb
      real, parameter :: thk = 0.20e-4   ! thermal diffusivity of dry air 20 C


        if (ust <= 0.0) then
             rb = -999.0
             call log_debug('Invalid friction velocity (ust <= 0). Returning -999.0 for rb.')
             return
        end if

        if (ctx%comp%diffc <= 0.0) then
             rb = -999.0
             call log_debug('Invalid diffusion coefficient (diffc <= 0). Returning -999.0 for rb.')
             return
        end if


      rb = 5.0 / ust

      rb = rb*(thk/ctx%comp%diffc)**.67

   end function depac_calc_rb_hicks

end module m_rb
