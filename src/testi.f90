! binääripuun testausta
program testi
    use AVLpuu
    implicit none
    type (solmu), target :: juuri
    type (solmu), pointer :: jp
    type (solmu), target :: lapsi
    type (solmu), pointer :: lp
    type (solmu), pointer :: lisatty, lisatty2
    juuri = solmu(NULL(), NULL(), NULL(), 1, "a")
    jp => juuri
    lisatty => lisaa(1, "c", jp, NULL(), jp)
    lisatty => lisaa(1, "b", jp, NULL(), jp)
    write(*,*) jp%sana
    write(*,*) jp%vasen%sana
    write(*,*) jp%oikea%sana
end program
