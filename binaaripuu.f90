! Fortran-toteutus tasapainotetulle binääripuulle
!
module AVLpuu
    implicit none
   
    ! Puun solmu 
    type :: solmu
        type (solmu), pointer :: vasen, oikea, vanhempi
        integer :: lukumaara = 1
        character(len=50) :: sana(50) !TODO voiko tehdä fiksummin?
    end type solmu

contains

    ! Palauttaa pisimmän matkan annetusta solmusta lehteen
    integer recursive function korkeus(s) result(palautus)
        implicit none
        type (solmu), pointer, intent(in) :: s
        integer :: k1, k2

        if (.not. associated(s)) then
            palautus = -1
            return
        else
            if (associated(s%vasen)) then
                k1 = korkeus(s%vasen)
            else
                k1 = -1
            end if
                    
            if (associated(s%oikea)) then
                k2 = korkeus(s%oikea)
            else
                k2 = -1
            end if
                    
            if (k1 .gt. k2) then
                palautus = k1 + 1
            else
                palautus = k2 + 1
            end if
        end if
    end function korkeus

    ! Palauttaa viitteen solmun s lapseen, jonka arvo on annettu sana tai null-viitteen jos solmua ei löydy
    type (solmu) recursive function etsi(pituus, s, sana) result(palautus)
        integer, intent(in) :: pituus
        type (solmu), pointer,  intent(in) :: s
        character, intent(in) :: sana(pituus)

        if (.not. associated(s)) then
            nullify(palautus)
        else if (sana .eq. s%sana) then
            palautus = s
        else if (sana .lt. s%sana) then
            if (associated(s%vasen)) then
                palautus => etsi(len(sana), s%vasen, sana)
            else
                nullify(palautus)
            end if
        else
            if (associated(s%oikea)) then
                palautus => etsi(len(sana), s%oikea, sana)
            else
                nullify(palautus)
            end if
        end if 
    end function etsi

    ! Lisää puuhun, jonka juuri on s, solmun, johon talletetaan annettu sana. Mikäli tällainen solmu on jo olemassa, kasvattaa
    ! solmun laskuria yhdellä. Palauttaa viitteen lisättyyn solmuun.
    type (solmu) recursive function lisaa(pituus, sana, s, vanhempi, juuri) result(lisatty)
        integer, intent(in) :: pituus
        character, intent(in) :: sana(pituus)
        type (solmu), pointer, intent(inout) :: s
        type (solmu), pointer, intent(in) :: vanhempi
        type (solmu), pointer, intent(in) :: juuri

        if (.not. (associated(s)) then
            allocate(s)
            s%sana = sana
            s%lukumaara = 1
            nullify(s%vasen)
            nullify(s%oikea)
            s%vanhempi => vanhempi
            call tasapainota_lisays(s, juuri)
        else if (sana .eq. s%sana) then ! solmu löytyy jo puusta
            s%lukumaara = s%lukumaara + 1
        else if (sana .lt. s%sana) then
            lisatty => lisaa(pituus, sana, s%vasen, s, juuri)
        else
           lisatty => lisaa(pituus, sana, s%oikea, s, juuri)
       end if
    end function lisaa

    ! Tasapainottaa puun lisäyksen jälkeen
    recursive subroutine tasapainota_lisays(s, juuri)
        type (solmu), pointer, intent(in) :: s
        type (solmu), pointer :: juuri
        type (solmu), pointer :: vanhempi
        type (solmu), pointer :: isovanhempi
        type (solmu), pointer :: alipuu
        vanhempi => s%vanhempi
        do while (associated(vanhempi))
            if (korkeus(vanhempi%vasen) .eq. korkeus(vanhempi%oikea) + 2 ) then ! epätasapaino vasemmasta lapsesta johtuen
                isovanhempi = vanhempi%vanhempi
                
                if (korkeus(vanhempi%vasen%vasen) .gt. korkeus(vanhempi%vasen%oikea)) then
                    alipuu = kiertoOikea(vanhempi)
                else
                    alipuu = kiertoVasenOikea(vanhempi)
                end if

                if (.not. associated(isovanhempi)) then
                    juuri => alipuu
                else if (isovanhempi%vasen%sana .eq. vanhempi%sana) then
                    isovanhempi%vasen => alipuu
                else
                    isovanhempi%oikea => alipuu
                end if

                return
            else if (korkeus(vanhempi%oikea) .eq. korkeus(vanhempi%vasen) + 2) then !epätasapaino oikeasta lapsesta johtuen
                isovanhempi = vanhempi%vanhempi

                if (korkeus(vanhempi%oikea) .gt. korkeus(vanhempi%vasen)) then
                    alipuu = kiertoVasen(vanhempi)
                else
                    alipuu = kiertoOikeaVasen(vanhempi)
                end if

                if (.not. associated(isovanhempi)) then
                    juuri => alipuu
                else if (isovanhempi%vasen%sana .eq. vanhempi%sana) then
                    isovanhempi%vasen => alipuu
                else
                    isovanhempi%oikea => alipuu
                end if
            end if
            vanhempi => vanhempi%vanhempi
        end do
    end subroutine tasapainota_lisays

    type (solmu) function kiertoOikea(s1)
        type (solmu), pointer, intent(in) :: s1
        type (solmu), pointer, intent(out) :: s2
        s2 => s1%vasen
        s2%vanhempi => s1%vanhempi
        s1%vanhempi => s2
        s1%vasen => s2%oikea
        s2%oikea = s1
        if (associated(s1%vasen)) then
            s1%vasen%vanhempi => s1
        end if
    end function kiertoOikea

    type (solmu) function kiertoVasen(s1)
        type (solmu), pointer, intent(in) :: s1
        type (solmu), pointer, intent(out) :: s2
        s2 => s1%oikea
        s2%vanhempi => s1%vanhempi
        s1%vanhempi => s2
        s1%oikea => s2%vasen
        s2%vasen = s1
        if (associated(s1%oikea)) then
            s1%oikea%vanhempi => s1
        end if
    end function kiertoVasen

    type (solmu) function kiertoOikeaVasen(s1)
        type (solmu), pointer, intent(in) :: s1
        type (solmu), pointer, intent(out) :: s2
        s1%oikea => kiertoOikea(s2)
        s2 => kiertoVasen(s2)
    end function kiertoOikeaVasen

    type (solmu) function kiertoVasenOikea(s1)
        type (solmu), pointer, intent(in) :: s1
        type (solmu), pointer, intent(out) :: s2
        s2 => s1%vasen
        s1%vasen = kiertoVasen(s2)
        s2 => kiertoOikea(s1)
    end function kiertoVasenOikea
end module
