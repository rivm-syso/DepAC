module m_comp_point_param
    use m_comp_point_default, only: comp_point_default
    use m_comp_point_ammonia, only: comp_point_ammonia, csoil_default, csoil_water
    
    implicit none (type, external)
    private
    public :: comp_point_ammonia, comp_point_default, csoil_default, csoil_water
end module m_comp_point_param