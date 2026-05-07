module m_gsoil_param
    use m_gsoil_default
    use m_rinc_default
    use m_rinc_no_path
    use m_rinc_no_resistance
    implicit none (type, external)
    private
    public :: gsoil_default, rinc_default, rinc_no_path, rinc_no_resistance
end module m_gsoil_param
