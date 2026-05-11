module m_gw_so2
   use c_depac_param_types, only: depac_gw_param
   use t_depac_setup, only: depac_setup
   use t_depac_context, only: depac_context

   implicit none (type, external)
   private
   public :: gw_so2
   type, extends(depac_gw_param) :: gw_so2
   contains
      procedure :: apply => gw_so2_apply
   end type gw_so2
contains

   pure function gw_so2_apply(this, setup, ctx)  result(gw)
      class(gw_so2), intent(in) :: this
      class(*), intent(in) :: setup
      type(depac_context), intent(in) :: ctx

      real :: rw ! external leaf resistance (s/m)
      real :: gw ! external leaf conductance (m/s)

      select type (setup)
       type is (depac_setup)
         associate(meteo => ctx%meteo, dp_conf => setup%config)
            if (ctx%has_vegetation) then
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

               gw = 1.0 / rw
            else
               gw = 0.0
            endif

         end associate
       class default
         ! If the setup is not of type depac_setup, return missing value
         gw = -999.0
      end select
   end function gw_so2_apply
end module m_gw_so2
