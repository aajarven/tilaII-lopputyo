! tiedostonluvun testausta
program testi
    !use AVLpuu
    use tiedostonluku
    implicit none
    integer :: i
    !write(*,*) avaa(len("run/simple.txt"), "run/simple.txt")
    !write(*,*) avaa(len("run/kalevala.txt"), "run/kalevala.txt")
    if (.not. avaa(len("run/shakespear.txt"), "run/shakespear.txt")) then
        stop
    end if

    do i=1,20
        write(*,*) lue_sana() 
    end do
   
    !type (solmu), target :: juuri
    !type (solmu), pointer :: jp
    !type (solmu), target :: lapsi
    !type (solmu), pointer :: lp
    !type (solmu), pointer :: lisatty, lisatty2
    !juuri = solmu(NULL(), NULL(), NULL(), 1, "a")
    !jp => juuri
    !lisatty => lisaa(1, "c", jp, NULL(), jp)
    !lisatty => lisaa(1, "b", jp, NULL(), jp)
    !write(*,*) jp%sana
    !write(*,*) jp%vasen%sana
    !write(*,*) jp%oikea%sana
end program
