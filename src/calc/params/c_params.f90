module c_params
    use m_comp_point_param, only: comp_point_ammonia, comp_point_default, csoil_default, csoil_water
    use m_gsoil_param, only: gsoil_default, rinc_default, rinc_no_path, rinc_no_resistance
    use m_gstom_param, only: gstom_default, gstom_emberson
    use m_gw_param, only: gw_default, gw_so2, gw_nh3_sutton
    use m_rc_special_param, only: rc_tot_fixed, rc_tot_nitric_acid, rc_tot_nitric_oxide, &
        rc_special_default

    implicit none (type, external)
    private
    public :: comp_point_ammonia, comp_point_default, csoil_default, csoil_water, &
       gsoil_default, rinc_default, rinc_no_path, rinc_no_resistance, &
       gstom_default, gstom_emberson, &
       gw_default, gw_so2, gw_nh3_sutton, &
       rc_tot_fixed, rc_tot_nitric_acid, rc_tot_nitric_oxide, rc_special_default
end module c_params
