module binaaripuu
    implicit none
    
    type :: solmu
        type (solmu), pointer :: vasen, oikea, vanhempi
        integer :: lukumaara
        character(len=:), allocatable :: sana(:)
    end type solmu

contains
    integer recursive function korkeus(s)
        type (solmu), pointer, intent(in) :: s
        integer :: k1, k2
        integer, intent(out) :: korkeus
              
        if (associated(s%vasen)) then
            k1 = korkeus(s%vasen)
        else
            k1 = -1
        endif
                
        if (associated(s%oikea)) then
            k2 = korkeus(s%oikea)
        else
            k2 = -1
        endif
                
        if (k1 .gt. k2) then
            korkeus = k1 + 1
        else
            korkeus = k2 + 1
        endif
        end function korkeus

    type (solmu) recursive function etsi(pituus, s, sana)
        integer, intent(in) :: pituus
        type (solmu), pointer,  intent(in) :: s
        character, intent(in) :: sana(pituus)
        type (solmu), pointer, intent(out) :: palautus
        if (sana .eq. s%sana) then
            palautus = s
        else if (sana .lt. s%sana) then !TODO oikeasti osaat tehdä myös nulleilla lapsilla, kts. lisaa
            if (associated(s%vasen)) then
                palautus => etsi(s%vasen, sana)
            else
                nullify(palautus)
            endif
        else
            if (associated(s%oikea)) then
                palautus => etsi(s%oikea, sana)
            else
                nullify(palautus)
            endif
        endif 
    end function etsi

    type (solmu) recursive function lisaa(pituus, sana, s, vanhempi)
        integer, intent(in) :: pituus
        character, intent(in) :: sana(pituus)
        type (solmu), pointer, intent(in) :: s
        type (solmu), pointer, intent(out) :: vanhempi
        type (solmu), pointer, intent(out) :: lisatty

        if (.not. allocated(s)) then
            allocate(s)
            s%sana = sana
            s%lukumaara = 1
            nullify(s%vasen)
            nullify(s%oikea)
            s%vanhempi => vanhempi
        else if (sana .lt. s%sana) then
            lisatty => lisaa(pituus, sana, s%vasen, s)
        else
           lisatty => lisaa(pituus, sana, s%oikea, s)
       endif
       !TODO tasapainotus
    end function lisaa

end module
