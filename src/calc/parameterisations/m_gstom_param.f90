module m_gstom_param
    use m_gstom_default, only: gstom_default
    use m_gstom_emberson, only: gstom_emberson

    implicit none (type, external)
    private
    public :: gstom_default, gstom_emberson
end module m_gstom_param