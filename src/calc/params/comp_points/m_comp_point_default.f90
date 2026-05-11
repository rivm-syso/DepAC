module m_comp_point_default
   use c_depac_param_types, only: depac_comp_point_param
   use t_depac_setup, only: depac_setup
   use t_depac_context, only: depac_context

   implicit none (type, external)
   private
   public :: comp_point_default
   type, extends(depac_comp_point_param) :: comp_point_default
   contains
      procedure :: apply => comp_point_default_apply
   end type comp_point_default

contains
   pure function comp_point_default_apply(this, setup, ctx) result(ccomp_tot)
      class(comp_point_default), intent(in) :: this
      class(*), intent(in) :: setup
      type(depac_context), intent(in) :: ctx

      real :: ccomp_tot

      select type (setup)
       type is (depac_setup)
         ! Default component point parameterisation: returns 0.0 for all components and conditions
         ! This can be used as a placeholder or for components that do not require a specific parameterisation.
         ccomp_tot = 0.0
       class default
         ! If the setup is not of type depac_setup, return missing value
         ccomp_tot = -999.0
      end select

   end function comp_point_default_apply

end module m_comp_point_default
