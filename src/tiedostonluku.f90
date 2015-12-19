module tiedostonluku

    implicit none
    character(len=1000), private :: rivi
    integer, private :: alkukohta
    character(len=*), private, parameter :: sanan_kirjaimet = "abcdefghijklmnopqrstuvwxyz1234567890-" ! merkit, joista sana voi koostua

contains

    ! Avaa tiedoston luettavaksi ja siirtää lukukohdan tiedoston alkuun.
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
        end if
    end function avaa

    ! Lukee seuraavan lukemattoman sanan. Jos sanoja ei ole, palauttaa tyhjän merkkijonon.
    function lue_sana() result(sana)
        character(len=50) :: sana
        integer :: indeksi
        integer :: i
        integer :: edellinen
        logical :: auki
        integer :: ios

        inquire(unit=1, opened=auki)
        if (.not. auki) then
            write(*,*) "tiedosto tulee avata avaa-funktiolla ennen sanojen lukemista."
            sana = ''
            return
        end if
       
        do while (.true.) ! luetaan rivejä kunnes löytyy sana

            if(alkukohta .eq. len(rivi)) then ! jos ollaan rivin lopussa,  viahdetaan riviä
                read(1,"(a)", iostat=ios) rivi

                if (ios /= 0) then
                    sana = ''
                    return
                end if
                
                call tolower(rivi)
                alkukohta = 1
                edellinen = 1
                call seuraavaan_sanaan()
            end if

            do i=alkukohta,len(rivi) ! loopataan koko rivin yli
                indeksi = index(sanan_kirjaimet, rivi(i:i))
                if (indeksi .eq. 0 .and. edellinen .gt. 0) then ! sanan loppu
                    read (rivi(alkukohta:i-1), *, iostat=ios) sana
                    alkukohta = i+1
                    edellinen = indeksi
                    call seuraavaan_sanaan()
                    return
                end if
                edellinen = indeksi
            end do

            read (rivi(alkukohta:len_trim(rivi)), *, iostat=ios) sana ! luetaan rivin viimeinen sana 
            alkukohta =len(rivi)
            if (.not. sana .eq. '') then ! jos rivin viimeinen sana ei ole tyhjä, palautetaan se
                return
            end if
        end do
    end function lue_sana

    ! Muuttaa annetun merkkijonon isot kirjaimet pieniksi. Muuntaa vain kirjaimet A-Z.
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

    ! siirtää lukukohdan seuraavan sanan alkuun
    subroutine seuraavaan_sanaan()
        do while (index(sanan_kirjaimet, rivi(alkukohta:alkukohta)) .eq. 0 .and. alkukohta .lt. len(rivi))
            alkukohta = alkukohta+1
        end do
    end subroutine seuraavaan_sanaan
end module
