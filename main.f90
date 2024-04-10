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

    character(len=13) :: dpi
    character(len=50) :: nombre, password

    print *, "Ingrese su DPI: "
    read(*,*) dpi
    print *, "Ingrese su nombre: "
    read(*,*) nombre
    print *, "Ingrese su contraseña: "
    read(*,*) password
end subroutine register

subroutine adminMode()

    implicit none
    integer :: option
    character(len=250) :: file

    call clearTerminal()

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
                print *, "Ingresar el nombre del archivo: "
                read(*,*) file
                call AddClient(file)
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

subroutine ClientMode()

    implicit none
    integer :: option
    character(len=250) :: file

    call clearTerminal()

    print *, " *********** Bienvenido ***********"
    print *, "  1. Ver reportes."
    print *, "  2. Navegación y gestión de imágenes"
    print *, "  3. Carga masiva."
    print *, "  4. Salir."
    print *, "************************************"

    do
        print *, "Ingrese su opción: "
        read(*,*) option

        select case(option)
            case(1)
                call ReportUsers()
            case(2)

            case(3)
                call BulkLoad()
            case(4)
                print *, "Saliendo del modo administrador."
                call sleep(1)
                call clearTerminal()
                exit
            case default
                print *, "Opción no válida."
        end select
    end do
end subroutine ClientMode

subroutine ReportUsers()
    implicit none
    character(len=250) :: file
    integer :: option

    print *, "Seleccione el archivo que desea visualizar: "
    print *, "1. Top 5 de imágenes con más número de capas. "
    print *, "2. Todas las capas que son hojas"
    print *, "3. Profundidad de árbol de capas"
    print *, "4. Listar las capas. "
    print *, "5. Salir"
    read(*,*) option

    select case(option)
        case(1)
            
        case(2)
            
        case(3)
        
        case(4)
            call ListLayout()
        case(5)
            print *, "Saliendo del los reportes."
            call sleep(1)
            call clearTerminal()
        case default
            print *, "Opción no válida."
    end select
end subroutine ReportUsers

subroutine ListLayout()
    implicit none
    character(len=250) :: file
    integer :: option

    print *, "Seleccione el tipo de lista: "
    print *, "1. Preorden. "
    print *, "2. Inorden."
    print *, "3. Postorden."
    print *, "4. Salir. "
    read(*,*) option

    select case(option)
        case(1)
            
        case(2)
            
        case(3)
        
        case(4)
            print *, "Saliendo de las listas."
            call sleep(1)
            call clearTerminal()
        case default
            print *, "Opción no válida."
    end select
end subroutine ListLayout

subroutine BulkLoad()
    implicit none
    character(len=250) :: file
    integer :: option

    print *, "Seleccione el tipo de carga a realizar: "
    print *, "1. Capas. "
    print *, "2. Imagenes."
    print *, "3. Albumes."
    print *, "4. Salir. "
    read(*,*) option

    select case(option)
        case(1)
            print *, "Ingresar el nombre del archivo: "
            read(*,*) file
            call AddLayer(file)
        case(2)
            print *, "Ingresar el nombre del archivo: "
            read(*,*) file
            call AddImage(file)
        case(3)
            print *, "Ingresar el nombre del archivo: "
            read(*,*) file
            call AddAlbum(file)
        case(4)
            print *, "Saliendo de las listas."
            call sleep(1)
            call clearTerminal()
        case default
            print *, "Opción no válida."
    end select
end subroutine BulkLoad


!==================== Lectura del archivo de Clientes ====================
subroutine AddClient(file)
    use json_module
    character(len=250) :: file
    type(json_file) :: json
    type(json_value), pointer :: list_p, client_p, attribute_p
    type(json_core) :: jsonc
    character(:), allocatable :: dpi, nombre, password
    integer :: i, size
    logical :: found
      
    call json%initialize()
    call json%load(filename=file)
    call json%info('', n_children=size)
    call json%get_core(jsonc)
    call json%get('', list_p, found)
        
    do i = 1, size
        call jsonc%get_child(list_p, i, client_p, found=found)

        call jsonc%get_child(client_p, 'dpi', attribute_p, found=found)
        if(found) call jsonc%get(attribute_p, dpi)
      
        call jsonc%get_child(client_p, 'nombre_cliente', attribute_p, found=found)
        if(found) call jsonc%get(attribute_p, nombre)
      
        call jsonc%get_child(client_p, 'password', attribute_p, found=found)
        if(found) call jsonc%get(attribute_p, password)
      
        print '(10a)', 'DPI: ', trim(dpi), ' Nombre: ', trim(nombre), ' Password: ', trim(password)
    end do  
    call json%destroy()
end subroutine AddClient

    !==================== Lectura del archivo de Imagenes =================================================================================
    !======================== Albunes =====================================
subroutine AddAlbum(file)
    use json_module
    character(len=250) :: file
    type(json_file) :: json
    type(json_value), pointer :: list_p, album_p, attribute_p
    type(json_core) :: jsonc
    character(:), allocatable :: nombre_album, imgs
    integer :: i, size
    logical :: found
      
    call json%initialize()
    call json%load(filename=file)
    call json%info('', n_children=size)
    call json%get_core(jsonc)
    call json%get('', list_p, found)
        
    do i = 1, size
        call jsonc%get_child(list_p, i, album_p, found=found)

        call jsonc%get_child(album_p, 'nombre_album', attribute_p, found=found)
        if(found) call jsonc%get(attribute_p, nombre_album)
      
        call jsonc%get_child(album_p, 'imgs', attribute_p, found=found)
        if(found) call jsonc%get(attribute_p, imgs)

        print '(10a)', 'Nombre album: ', trim(nombre_album), ' Imgs: ', trim(imgs)
    end do  
    call json%destroy()
end subroutine AddAlbum

    !======================= Imagenes =====================================
subroutine AddImage(file)
    use json_module
    character(len=250) :: file
    type(json_file) :: json
    type(json_value), pointer :: list_p, image_p, attribute_p
    type(json_core) :: jsonc
    character(:), allocatable :: id, capas
    integer :: i, size
    logical :: found
      
    call json%initialize()
    call json%load(filename=file)
    call json%info('', n_children=size)
    call json%get_core(jsonc)
    call json%get('', list_p, found)
        
    do i = 1, size
        call jsonc%get_child(list_p, i, image_p, found=found)

        call jsonc%get_child(image_p, 'id', attribute_p, found=found)
        if(found) call jsonc%get(attribute_p, id)
      
        call jsonc%get_child(image_p, 'capas', attribute_p, found=found)
        if(found) call jsonc%get(attribute_p, capas)

        print '(10a)', 'ID: ', trim(id), ' Capas: ', trim(capas)
    end do  
    call json%destroy()
end subroutine AddImage

    !======================= Capas ========================================
subroutine AddLayer(file)
    use json_module
    character(len=250) :: file
    type(json_file) :: json
    type(json_value), pointer :: list_p, layer_p, attribute_p
    type(json_core) :: jsonc
    character(:), allocatable :: id_capa, fila, columna, color
    integer :: i, size
    logical :: found
      
    call json%initialize()
    call json%load(filename=file)
    call json%info('', n_children=size)
    call json%get_core(jsonc)
    call json%get('', list_p, found)
        
    do i = 1, size
        call jsonc%get_child(list_p, i, layer_p, found=found)

        call jsonc%get_child(layer_p, 'id_capa', attribute_p, found=found)
        if(found) call jsonc%get(attribute_p, id_capa)

        print '(10a)', 'ID: ', trim(id_capa)
    end do  
    call json%destroy()
end subroutine AddLayer
