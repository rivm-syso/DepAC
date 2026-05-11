module m_rinc_no_resistance
   use c_depac_param_types, only: depac_rinc_param
   use t_depac_setup, only: depac_setup
   use t_depac_context, only: depac_context

   implicit none (type, external)
   private
   public :: rinc_no_resistance
   type, extends(depac_rinc_param) :: rinc_no_resistance
   contains
      procedure :: apply => rinc_no_resistance_apply
   end type rinc_no_resistance
contains
   pure function rinc_no_resistance_apply(this, setup, ctx) result(rinc)
      class(rinc_no_resistance), intent(in) :: this
      class(*), intent(in) :: setup
      type(depac_context), intent(in) :: ctx
      real :: rinc

      select type (setup)
       type is (depac_setup)
         ! This parameterisation returns zero rinc value to effectively remove soil moisture resistance.
         rinc = 0.0
       class default
         ! If the setup is not of type depac_setup, return missing value
         rinc = -999.0
      end select
   end function rinc_no_resistance_apply

end module m_rinc_no_resistance
