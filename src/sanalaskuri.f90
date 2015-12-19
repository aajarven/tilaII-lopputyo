! Lukee tiedoston ja tulostaa tiedoston sanojen määrän aakkosjärjestyksessä. Yksittäisen sanan maksimipituus on 50
! merkkiä.
program sanalaskuri
    use avlpuu
    use tiedostonluku
    
    implicit none
    integer :: i
    character(len=500) :: tiedostonimi
    character(len=50) :: sana
    type (solmu), target :: juuri
    type (solmu), pointer :: lisatty
    type (solmu), pointer :: jp
    jp => juuri

    call get_command_argument(1, tiedostonimi)
    if (len_trim(tiedostonimi) .eq. 0) then
        write(*,*) "Anna argumenttina tutkittava tiedosto"
        stop
    end if

    if (.not. avaa(len_trim(tiedostonimi), tiedostonimi)) then
        stop
    end if

    sana = lue_sana()
    if (sana .eq. '') then
        write(*,*) "tyhjä tiedosto"
        stop
    end if
    juuri = solmu(NULL(), NULL(), NULL(), 1, sana)
    jp => juuri
    sana = lue_sana()
    do while (.not. sana .eq. '')
        lisatty => lisaa(len(sana), sana, jp, NULL(), jp)
        !write(*,*) trim(sana)
        sana = lue_sana()
    end do

    call tulosta_kaikki(jp)   
end program
