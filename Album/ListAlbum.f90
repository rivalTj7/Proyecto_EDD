module ListAlbum
  use AVL_Tree_M
  implicit none
  type circle_list
    type(node), pointer :: root => null()
    contains
      procedure :: print
      procedure :: graphList
      procedure :: remove
  end type circle_list

  type node
    character(len=100) :: name
    type(TreeAVL) :: imgs = TreeAVL()
    type(node), pointer :: next => null()
    type(node), pointer :: prev => null()
  end type node

  contains

  function add(self, Name) result(root)
    class(circle_list), intent(inout) :: self
    character(len=100), intent(in) :: Name
    type(node), pointer :: newNode, root, finish
    allocate(newNode)
    newNode%name = Name
    if(.not. associated(self%root)) then
      newNode%next => newNode
      newNode%prev => newNode
      self%root => newNode
      root => self%root
    else
      finish => self%root%prev
      newNode%next => self%root
      newNode%prev => finish
      root%prev => newNode
      finish%next => newNode
      self%root => newNode
      root => self%root
    end if
  end function add

  subroutine print(self)
    class(circle_list), intent(inout) :: self
    type(node), pointer :: tmp
    tmp => self%root
    if (.not. associated(tmp)) then
        print *, "Empty list :)"
        return
    else
        do while (associated(tmp) .and. tmp /= self%root)
            if (associated(tmp%name)) then
                print *, "Name: ", tmp%name
            end if
            tmp => tmp%next
        end do
    end if
  end subroutine print

  function contar(self) result(cantidad)
    class(circle_list), intent(inout) :: self
    type(node), pointer :: tmp
    integer :: cantidad
    cantidad = 0
    
    if(associated(self%root)) then
      tmp => self%root
      do while (associated(tmp) .and. tmp /= self%root)
        cantidad = cantidad + 1
        tmp => tmp%next
      end do
    end if
  end function contar

  subroutine remove(self, pos)
    class(circle_list), intent(inout) :: self
    integer, intent(in) :: pos

  end subroutine remove

  subroutine graphList(self)
    class(circle_list), intent(inout) :: self
    character(len=10000) :: grafo

    grafo = "digraph G" //new_line('a')  // "{" // new_line('a')
    grafo = grafo // 'graph[label=' // '" ' // 'Lista de Albunes'  // ' "'// ', fontcolor=Black];'  //new_line('a')
    grafo = grafo // 'rankdir=TB;' // new_line('a')

    grafo = grafo 

    grafo = grafo // '}' // new_line('a')

  end subroutine graphList

  function travel()
    
  end function travel

end module ListAlbum