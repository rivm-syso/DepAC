module m_rinc_no_path
   use c_depac_params, only: depac_rinc_param
   use t_depac_setup, only: depac_setup
   use t_depac_context, only: depac_context

   implicit none (type, external)
   private
   public :: rinc_no_path

   type, extends(depac_rinc_param) :: rinc_no_path
   contains
      procedure :: apply => rinc_no_path_apply
   end type rinc_no_path
contains
   pure function rinc_no_path_apply(this, setup, ctx) result(rinc)
      class(rinc_no_path), intent(in) :: this
      type(depac_setup), intent(in) :: setup
      type(depac_context), intent(in) :: ctx

      real :: rinc

      rinc = -999.0

   end function rinc_no_path_apply

end module m_rinc_no_path
