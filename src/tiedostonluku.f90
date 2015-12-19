module tiedostonluku

    implicit none
    character(len=50), private :: rivi !TODO legitimpi mitta
    integer, private :: alkukohta
    !character(len=*), private, parameter :: sanan_kirjaimet = "a"
    character(len=*), private, parameter :: sanan_kirjaimet = "abcdefghijklmnopqrstuvwxyz1234567890-"

contains

    function avaa(pituus, tiedostonimi) result(onnistui)
        integer, intent(in) :: pituus
        character(len=pituus), intent(in) :: tiedostonimi
        logical :: onnistui
        integer :: ios
        logical :: auki
        
        ! suljetaan jo mahdollisesti auki oleva tiedosto
        inquire(unit=1, opened=auki)
        if (auki) then
            close(1)
        end if

        ! avataan pyydetty tiedosto
        open(unit=1, file=tiedostonimi, iostat=ios, status='old', form='formatted')
        if (ios/=0) then
            write(*,"(a,a)") "virhe avattaessa tiedostoa ", trim(tiedostonimi)
            write(*,"(a,i3)") "iostat: ", ios
            onnistui = .false.
        else
            onnistui = .true.
            alkukohta = len(rivi)
            !read(1,*) rivi
        end if
    end function avaa

    function lue_sana() result(sana)
        character(len=50) :: sana
        integer :: indeksi
!        integer :: kohta
        integer :: i
        integer :: edellinen
        logical :: auki
        integer :: ios

        inquire(unit=1, opened=auki)
        if (.not. auki) then
            write(*,*) "tiedosto tulee avata lue-funktiolla ennen sanojen lukemista."
            sana = ''
            return
        end if
       
        !TODO jos pilkku tms, lukee edeltävän sanan kahdesti
        do while (.true.) !todo ruma hack, fiksaa
            if(alkukohta .eq. len(rivi)) then
                !write(*,*) "riviä ei ole"
                read(1,"(a)", iostat=ios) rivi
                call tolower(rivi)

                if (ios /= 0) then
                    write(*,*) "riviä ei voitu lukea"
                    return
                end if
                !write(*,*) rivi
                alkukohta = 1
                edellinen = 1
            end if

            do i=alkukohta,len_trim(rivi)
                !write(*,*) "alkukohta:", alkukohta
                !write(*,*) "i:", i
                indeksi = index(sanan_kirjaimet, rivi(i:i))
                !write(*,*) "indeksi:", indeksi
                !write(*,*) "edellinen:", edellinen
                if (indeksi .eq. 0 .and. edellinen .gt. 0) then
                    read (rivi(alkukohta:i-1), *, iostat=ios) sana
                    alkukohta = i+1
                    edellinen = indeksi
                    return
                else if (indeksi .gt. 0 .and. edellinen .eq. 0) then
                    alkukohta = i
                end if

                edellinen = indeksi
            end do
            alkukohta =len(rivi)
        end do
        
!        if(alkukohta .eq. len(rivi)) then
!            !write(*,*) "riviä ei ole"
!            read(1,"(a)", iostat=ios) rivi
!            if (ios /= 0) then
!                write(*,*) "riviä ei voitu lukea"
!                return
!            end if
!            write(*,*) rivi
!            alkukohta = 0
!        end if
!         
!        kohta = alkukohta
!        edellinen = alkukohta
!        
!        do while(kohta .lt. len(rivi))
!            write(*,"(a,a)") "tutkitaan ",rivi(kohta:kohta)
!            write(*,*) "kohta: ", kohta
!            indeksi = index(sanan_kirjaimet, rivi(kohta:kohta))
!            write(*,*) "indeksi:", indeksi, "edellinen:", edellinen
!            if (indeksi .eq. 0 .and. edellinen .gt. 0) then
!                !write(*,*) "1"
!                read(rivi(alkukohta:kohta-1), *, iostat=ios) sana
!                if (ios /= 0) then
!                    write(*,*) "merkkiä ei voitu lukea"
!                    return
!                end if
!                alkukohta = kohta
!                edellinen = indeksi
!                kohta = kohta + 1
!                write(*,*) sana
!                return
!            else if (indeksi .gt. 0 .and. edellinen .eq. 0) then
!                !write(*,*) "2"
!                alkukohta = kohta
!            end if
!            edellinen = indeksi
!            kohta = kohta + 1
!        end do
        
    end function lue_sana

    subroutine tolower(str)
        character(*), intent(inout) :: str
        integer :: i

        do i=1, len(str)
            select case(str(i:i))
                case('A':'Z')
                    str(i:i) = achar(iachar(str(i:i))+32)
            end select
        end do
    end subroutine tolower

end module
