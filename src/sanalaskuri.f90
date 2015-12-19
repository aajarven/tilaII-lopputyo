! Lukee tiedoston ja tulostaa tiedoston sanojen määrän aakkosjärjestyksessä. Yksittäisen sanan maksimipituus on 50
! merkkiä.
program testi
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

    !write(*,*) avaa(len("run/simple.txt"), "run/simple.txt")
    !write(*,*) avaa(len("run/kalevala.txt"), "run/kalevala.txt")
    !if (.not. avaa(len("run/shakespear.txt"), "run/shakespear.txt")) then
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

!    type (solmu), pointer :: jp
!    type (solmu), target :: lapsi
!    type (solmu), pointer :: lp
!    type (solmu), pointer :: lisatty, lisatty2
!    juuri = solmu(NULL(), NULL(), NULL(), 1, "a")
!    jp => juuri
!    lisatty => lisaa(1, "c", jp, NULL(), jp)
!    lisatty => lisaa(1, "b", jp, NULL(), jp)
!    write(*,*) jp%sana
!    write(*,*) jp%vasen%sana
!    write(*,*) jp%oikea%sana
end program
