program main
    implicit none
    integer :: option

    call clearTerminal()

    do
        print *, "************** Pixel Print Studio. **************"
        print *, "      1. Iniciar sesión"
        print *, "      2. Registrarse"
        print *, "      3. Salir"
        print *, "Ingrese su opción: "
        print *, "*************************************************"
        read(*,*) option

        select case(option)
            case(1)
                call login()
            case(2)
                call register()
            case(3)
                print *, "Saliendo del programa."
                exit
            case default
                print *, "Opción no válida."
        end select
    end do
end program main


subroutine clearTerminal()
    character(len=128) :: command
    command = 'cls'
    call system(command)
end subroutine clearTerminal


subroutine login()
    implicit none
    character(len=20) :: username, password
    integer :: attempts_left
    logical :: login_successful

    attempts_left = 3
    login_successful = .false.

    do while (attempts_left > 0 .and. .not. login_successful)
        print *, "Ingrese su nombre de usuario: "
        read(*,*) username

        print *, "Ingrese su contraseña: "
        read(*,*) password

        if (trim(username) == "admin" .and. trim(password) == "EDD2024") then
            login_successful = .true.
            print *, "¡Inicio de sesión exitoso!"
            call adminMode()
        else
            attempts_left = attempts_left - 1
            print *, "Nombre de usuario o contraseña incorrectos. Intentos restantes:", attempts_left
        end if
    end do

    if (.not. login_successful) then
        print *, "Se han agotado los intentos. Cerrando el programa."
        call sleep(4) 
    end if
end subroutine login


subroutine register()
    implicit none

    integer :: dpi
    character(len=50) :: nombre, password

    print *, "Ingrese su DPI: "
    read(*,*) dpi
    print *, "Ingrese su nombre: "
    read(*,*) nombre
    print *, "Ingrese su contraseña: "
    read(*,*) password
end subroutine register

subroutine adminMode()

    use Record


    implicit none
    integer :: option

    call clearTerminal()
    call LoadArchive()

    print *, " *********** Bienvenido al modo administrador. ***********"
    print *, "  1. Ver reporte de usuarios."
    print *, "  2. Editar usuarios"
    print *, "  3. Carga masiva de usuarios."
    print *, "  4. Salir."
    print *, "**********************************************************"

    do
        print *, "Ingrese su opción: "
        read(*,*) option

        select case(option)
            case(1)
            
            case(2)

            case(3)
                
            case(4)
                print *, "Saliendo del modo administrador."
                call sleep(1)
                call clearTerminal()
                exit
            case default
                print *, "Opción no válida."
        end select
    end do
end subroutine adminMode