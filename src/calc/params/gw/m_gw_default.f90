module m_gw_default
   use c_depac_params, only: depac_gw_param
   use t_depac_setup, only: depac_setup
   use t_depac_context, only: depac_context

   implicit none (type, external)
   private
   public :: gw_default

   type, extends(depac_gw_param) :: gw_default
   contains
      procedure :: apply => gw_default_apply
   end type gw_default
contains
   function gw_default_apply(this, setup, ctx) result(gw)
      class(gw_default), intent(in) :: this
      type(depac_setup), intent(in) :: setup
      type(depac_context), intent(in) :: ctx
      real :: gw

      ! Default gw parameterisation: returns 0.0 for all components and conditions
      ! This can be used as a placeholder or for components that do not require gw.
      if(missing(setup%component%rw_val)) then

         gw = 0.0
         return
      end if


      if(ctx%has_vegetation) then
         gw = 1.0/setup%component%rw_val
      else
         gw = 0.0
      endif


   end function gw_default_apply
end module m_gw_default
