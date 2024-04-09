module Record
    implicit none
contains

    subroutine LoadArchive()
        character(256) :: filename
        character(:), allocatable :: jsonDocument
        logical :: success
   
        print *, "Ingrese la ruta del archivo JSON: "
        read(*,*) filename
    
        if (index(trim(filename), '.json') /= 0) then
            
            call ReadJSONFile(trim(filename), jsonDocument, success)
            if (success) then
                print *, "Contenido del archivo JSON:"
                print *, jsonDocument
            else
                print *, "No se pudo leer el archivo JSON."
            end if
        else
            print *, 'Archivo no compatible.'
        end if
    end subroutine LoadArchive


    subroutine ReadJSONFile(filename, document, success)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable, intent(out) :: document
        logical, intent(out) :: success
        character(len=256) :: line
        integer :: unit, iostat

        document = ''
        success = .false.

        open(unit, file=filename, status='old', action='read', iostat=iostat)
        if (iostat /= 0) then
            print *, 'Error al abrir el archivo: ', filename
            return
        end if
        do
            read(unit, '(A)', iostat=iostat) line
            if (iostat /= 0) then
                exit
            else
                document = document // trim(line) // ' '
            end if
        end do
        close(unit)
        success = .true.
    end subroutine ReadJSONFile

end module Record
