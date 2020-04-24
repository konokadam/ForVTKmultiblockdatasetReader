module xml_data_multiblockdataset
    use READ_XML_PRIMITIVES
    use XMLPARSE

    implicit none
    integer, private :: lurep_
    logical, private :: strict_

    character(50), private :: type
    character(4), private :: version
    character(16), private :: byte_order
    character(16), private :: header_type

    type, abstract :: Block_DataSet_abstract_type
        integer :: index
        character(len=512) :: name
    contains
        procedure(input_proc_int), deferred, pass(dvar) :: input
        procedure(open_item_proc_int), deferred, pass(dvar) :: open
        procedure(close_item_proc_int), deferred, pass(dvar) :: close
    end type Block_DataSet_abstract_type

    abstract interface
        subroutine input_proc_int( dvar, info, starttag, endtag, attribs, noattribs, data, nodata, &
                has_dvar )
            import :: XML_PARSE, Block_DataSet_abstract_type
            class(Block_DataSet_abstract_type), intent(inout)  :: dvar
            type(XML_PARSE)                                 :: info
            character(len=*), intent(in)                    :: starttag
            logical, intent(inout)                          :: endtag
            character(len=*), dimension(:,:), intent(inout) :: attribs
            integer, intent(inout)                          :: noattribs
            character(len=*), dimension(:), intent(inout)   :: data
            integer, intent(inout)                          :: nodata
            logical, intent(inout)                       :: has_dvar
        end subroutine
        subroutine open_item_proc_int(dvar, index, u)
            import :: Block_DataSet_abstract_type
            class(Block_DataSet_abstract_type), intent(inout) :: dvar
            integer, intent(in) :: index
            integer, intent(in) :: u
        end subroutine
        subroutine close_item_proc_int(dvar, u)
            import :: Block_DataSet_abstract_type
            class(Block_DataSet_abstract_type), intent(in) :: dvar
            integer, intent(in) :: u
        end subroutine
    end interface

    type :: Block_DataSet_container_type
        class(Block_DataSet_abstract_type), pointer :: p => null()
    end type Block_DataSet_container_type

    type, extends(Block_DataSet_abstract_type) :: DataSet_type
        character(len=512) :: file
    contains
        procedure :: input => read_xml_type_DataSet_type
        procedure :: open => open_DataSet
        procedure :: close => close_DataSet
    end type DataSet_type

    type, extends(Block_DataSet_abstract_type) :: Block_type
        type(Block_DataSet_container_type), dimension(:), allocatable :: item
    contains
        procedure :: input => read_xml_type_Block_type
        procedure :: open => open_Block
        procedure :: close => close_Block
    end type Block_type


    type vtkMultiBlockDataSet_type
        type(Block_DataSet_container_type), dimension(:), allocatable :: item
    end type vtkMultiBlockDataSet_type

contains

    subroutine allocate_to_Block_or_DataSet(dvar, tip)

        class(block_dataset_abstract_type), pointer, intent(inout) :: dvar
        character(*), intent(in) :: tip

        select case(tip)
            case("Block")
                allocate(Block_type :: dvar)
            case("DataSet")
                allocate(DataSet_type :: dvar)
        end select

    end subroutine allocate_to_Block_or_DataSet

    subroutine read_xml_type_Block_DataSet_container_type_array( tip, &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar, has_dvar )
        character(*), intent(in) :: tip
        type(XML_PARSE)                                 :: info
        character(len=*), intent(inout)                 :: tag
        logical, intent(inout)                          :: endtag
        character(len=*), dimension(:,:), intent(inout) :: attribs
        integer, intent(inout)                          :: noattribs
        character(len=*), dimension(:), intent(inout)   :: data
        integer, intent(inout)                          :: nodata
        type(Block_DataSet_container_type), dimension(:), allocatable :: dvar
        logical, intent(inout)                       :: has_dvar

        integer                                      :: newsize
        type(Block_DataSet_container_type), dimension(:), allocatable :: newvar

        if(.not.allocated(dvar)) allocate(dvar(0))
        newsize = size(dvar) + 1
        call move_alloc(dvar, newvar)
        allocate(dvar(newsize))
        dvar(1:size(newvar)) = newvar

        call allocate_to_Block_or_DataSet(dvar(newsize)%p, tip)

        call dvar(newsize)%p%input(info, tag, endtag, attribs, noattribs, data, nodata, &
            has_dvar )


    end subroutine read_xml_type_Block_DataSet_container_type_array

    subroutine read_xml_type_DataSet_type( dvar, info, starttag, endtag, attribs, noattribs, data, nodata, &
            has_dvar )
        class(DataSet_type), intent(inout)  :: dvar
        type(XML_PARSE)                                 :: info
        character(len=*), intent(in)                    :: starttag
        logical, intent(inout)                          :: endtag
        character(len=*), dimension(:,:), intent(inout) :: attribs
        integer, intent(inout)                          :: noattribs
        character(len=*), dimension(:), intent(inout)   :: data
        integer, intent(inout)                          :: nodata
        logical, intent(inout)                       :: has_dvar

        integer                                      :: att_
        integer                                      :: noatt_
        logical                                      :: error
        logical                                      :: endtag_org
        character(len=80)                            :: tag
        logical                                         :: has_index
        logical                                         :: has_name
        logical                                         :: has_file
        has_index                            = .false.
        has_name                             = .false.
        has_file                             = .false.

        has_dvar = .true.
        error  = .false.
        att_   = 0
        noatt_ = noattribs+1
        endtag_org = endtag
        do
            if ( nodata .ne. 0 ) then
                noattribs = 0
                tag = starttag
            elseif ( att_ .lt. noatt_ .and. noatt_ .gt. 1 ) then
                att_      = att_ + 1
                if ( att_ .le. noatt_-1 ) then
                    tag       = attribs(1,att_)
                    data(1)   = attribs(2,att_)
                    noattribs = 0
                    nodata    = 1
                    endtag    = .false.
                else
                    tag       = starttag
                    noattribs = 0
                    nodata    = 0
                    endtag    = .true.
                    cycle
                endif
            else
                if ( endtag_org ) then
                    return
                else
                    call xml_get( info, tag, endtag, attribs, noattribs, data, nodata )
                    if ( xml_error(info) ) then
                        write(lurep_,*) 'Error reading input file!'
                        error = .true.
                        return
                    endif
                endif
            endif
            if ( endtag .and. tag .eq. starttag ) then
                exit
            endif
            if ( endtag .and. noattribs .eq. 0 ) then
                if ( xml_ok(info) ) then
                    cycle
                else
                    exit
                endif
            endif
            select case( tag )
                case('index')
                    call read_xml_integer( &
                        info, tag, endtag, attribs, noattribs, data, nodata, &
                        dvar%index, has_index )
                case('name')
                    call read_xml_word( &
                        info, tag, endtag, attribs, noattribs, data, nodata, &
                        dvar%name, has_name )
                case('file')
                    call read_xml_word( &
                        info, tag, endtag, attribs, noattribs, data, nodata, &
                        dvar%file, has_file )
                case ('comment', '!--')
                    ! Simply ignore
                case default
                    if ( strict_ ) then
                        error = .true.
                        call xml_report_errors( info, &
                            'Unknown or wrongly placed tag: ' // trim(tag))
                    endif
            end select
            nodata = 0
            if ( .not. xml_ok(info) ) exit
        end do
        if ( .not. has_index ) then
            has_dvar = .false.
            call xml_report_errors(info, 'Missing data on index')
        endif
        if ( .not. has_name ) then
            has_dvar = .false.
            call xml_report_errors(info, 'Missing data on name')
        endif
        if ( .not. has_file ) then
            has_dvar = .false.
            call xml_report_errors(info, 'Missing data on file')
        endif
    end subroutine read_xml_type_DataSet_type

    subroutine read_xml_type_Block_type( dvar, info, starttag, endtag, attribs, noattribs, data, nodata, &
            has_dvar )
        class(Block_type), intent(inout)  :: dvar
        type(XML_PARSE)                                 :: info
        character(len=*), intent(in)                    :: starttag
        logical, intent(inout)                          :: endtag
        character(len=*), dimension(:,:), intent(inout) :: attribs
        integer, intent(inout)                          :: noattribs
        character(len=*), dimension(:), intent(inout)   :: data
        integer, intent(inout)                          :: nodata
        logical, intent(inout)                       :: has_dvar

        integer                                      :: att_
        integer                                      :: noatt_
        logical                                      :: error
        logical                                      :: endtag_org
        character(len=80)                            :: tag
        logical                                         :: has_index
        logical                                         :: has_name
        logical                                         :: has_DataSet
        logical                                         :: has_Block
        has_index                            = .false.
        has_name                             = .false.
        has_DataSet                          = .false.
        has_Block                            = .false.

        has_dvar = .true.
        error  = .false.
        att_   = 0
        noatt_ = noattribs+1
        endtag_org = endtag
        do
            if ( nodata .ne. 0 ) then
                noattribs = 0
                tag = starttag
            elseif ( att_ .lt. noatt_ .and. noatt_ .gt. 1 ) then
                att_      = att_ + 1
                if ( att_ .le. noatt_-1 ) then
                    tag       = attribs(1,att_)
                    data(1)   = attribs(2,att_)
                    noattribs = 0
                    nodata    = 1
                    endtag    = .false.
                else
                    tag       = starttag
                    noattribs = 0
                    nodata    = 0
                    endtag    = .true.
                    cycle
                endif
            else
                if ( endtag_org ) then
                    return
                else
                    call xml_get( info, tag, endtag, attribs, noattribs, data, nodata )
                    if ( xml_error(info) ) then
                        write(lurep_,*) 'Error reading input file!'
                        error = .true.
                        return
                    endif
                endif
            endif
            if ( endtag .and. tag .eq. starttag ) then
                exit
            endif
            if ( endtag .and. noattribs .eq. 0 ) then
                if ( xml_ok(info) ) then
                    cycle
                else
                    exit
                endif
            endif
            select case( tag )
                case('index')
                    call read_xml_integer( &
                        info, tag, endtag, attribs, noattribs, data, nodata, &
                        dvar%index, has_index )
                case('name')
                    call read_xml_word( &
                        info, tag, endtag, attribs, noattribs, data, nodata, &
                        dvar%name, has_name )
                case('DataSet')
                    call read_xml_type_Block_DataSet_container_type_array( "DataSet", &
                        info, tag, endtag, attribs, noattribs, data, nodata, &
                        dvar%item, has_DataSet )
                case('Block')
                    call read_xml_type_Block_DataSet_container_type_array( "Block", &
                        info, tag, endtag, attribs, noattribs, data, nodata, &
                        dvar%item, has_Block )
                case ('comment', '!--')
                    ! Simply ignore
                case default
                    if ( strict_ ) then
                        error = .true.
                        call xml_report_errors( info, &
                            'Unknown or wrongly placed tag: ' // trim(tag))
                    endif
            end select
            nodata = 0
            if ( .not. xml_ok(info) ) exit
        end do
        if ( .not. has_index ) then
            has_dvar = .false.
            call xml_report_errors(info, 'Missing data on index')
        endif
        if ( .not. has_name ) then
            has_dvar = .false.
            call xml_report_errors(info, 'Missing data on name')
        endif
        if ( .not. has_DataSet .and. .not. has_Block) then
            has_dvar = .false.
            call xml_report_errors(info, 'Missing data on DataSet or Block')
        endif
    end subroutine read_xml_type_Block_type

    subroutine read_xml_type_vtkMultiBlockDataSet_type( info, starttag, endtag, attribs, noattribs, data, nodata, &
            dvar, has_dvar )
        type(XML_PARSE)                                 :: info
        character(len=*), intent(in)                    :: starttag
        logical, intent(inout)                          :: endtag
        character(len=*), dimension(:,:), intent(inout) :: attribs
        integer, intent(inout)                          :: noattribs
        character(len=*), dimension(:), intent(inout)   :: data
        integer, intent(inout)                          :: nodata
        type(vtkMultiBlockDataSet_type), intent(inout)  :: dvar
        logical, intent(inout)                       :: has_dvar

        integer                                      :: att_
        integer                                      :: noatt_
        logical                                      :: error
        logical                                      :: endtag_org
        character(len=80)                            :: tag
        logical                                         :: has_Block
        logical                                         :: has_DataSet
        has_Block                            = .false.
        has_DataSet                          = .false.

        has_dvar = .true.
        error  = .false.
        att_   = 0
        noatt_ = noattribs+1
        endtag_org = endtag
        do
            if ( nodata .ne. 0 ) then
                noattribs = 0
                tag = starttag
            elseif ( att_ .lt. noatt_ .and. noatt_ .gt. 1 ) then
                att_      = att_ + 1
                if ( att_ .le. noatt_-1 ) then
                    tag       = attribs(1,att_)
                    data(1)   = attribs(2,att_)
                    noattribs = 0
                    nodata    = 1
                    endtag    = .false.
                else
                    tag       = starttag
                    noattribs = 0
                    nodata    = 0
                    endtag    = .true.
                    cycle
                endif
            else
                if ( endtag_org ) then
                    return
                else
                    call xml_get( info, tag, endtag, attribs, noattribs, data, nodata )
                    if ( xml_error(info) ) then
                        write(lurep_,*) 'Error reading input file!'
                        error = .true.
                        return
                    endif
                endif
            endif
            if ( endtag .and. tag .eq. starttag ) then
                exit
            endif
            if ( endtag .and. noattribs .eq. 0 ) then
                if ( xml_ok(info) ) then
                    cycle
                else
                    exit
                endif
            endif
            select case( tag )
                case('Block')
                    call read_xml_type_Block_DataSet_container_type_array( "Block", &
                        info, tag, endtag, attribs, noattribs, data, nodata, &
                        dvar%item, has_Block )
                case('DataSet')
                    call read_xml_type_Block_DataSet_container_type_array( "DataSet", &
                        info, tag, endtag, attribs, noattribs, data, nodata, &
                        dvar%item, has_DataSet )
                case ('comment', '!--')
                    ! Simply ignore
                case default
                    if ( strict_ ) then
                        error = .true.
                        call xml_report_errors( info, &
                            'Unknown or wrongly placed tag: ' // trim(tag))
                    endif
            end select
            nodata = 0
            if ( .not. xml_ok(info) ) exit
        end do
        if ( .not. has_Block .and. .not. has_DataSet) then
            has_dvar = .false.
            call xml_report_errors(info, 'Missing data on Block or DataSet')
        endif

    end subroutine read_xml_type_vtkMultiBlockDataSet_type

    subroutine read_xml_file_multiblockdataset_reader(vtkMultiBlockDataSet, fname, lurep, errout)
        type(vtkMultiBlockDataSet_type) :: vtkMultiBlockDataSet
        character(len=*), intent(in)           :: fname
        integer, intent(in), optional          :: lurep
        logical, intent(out), optional         :: errout

        type(XML_PARSE)                        :: info
        logical                                :: error
        character(len=80)                      :: tag
        character(len=80)                      :: starttag
        logical                                :: endtag
        character(len=80), dimension(1:2,1:20) :: attribs
        integer                                :: noattribs
        character(len=200), dimension(1:100)   :: data
        integer                                :: nodata
        logical                                         :: has_vtkMultiBlockDataSet
        integer                                      :: att_
        integer                                      :: noatt_
        logical                                      :: endtag_org
        logical :: has_type
        logical :: has_version
        logical :: has_byte_order
        logical :: has_header_type
        has_vtkMultiBlockDataSet             = .false.

        call xml_open( info, fname, .true. )
        call xml_options( info, report_errors=.true., ignore_whitespace=.true.)
        lurep_ = 0
        if ( present(lurep) ) then
            lurep_ = lurep
            call xml_options( info, report_lun=lurep )
        endif
        do
            call xml_get( info, starttag, endtag, attribs, noattribs, &
                data, nodata)
            if ( starttag .ne. '!--' ) exit
        enddo
        if ( starttag .ne. "VTKFile" ) then
            call xml_report_errors( info, &
                'XML-file should have root element "VTKFile"')
            error = .true.
            call xml_close(info)
            return
        endif
        strict_ = .true.
        error  = .false.
        att_   = 0
        noatt_ = noattribs+1
        endtag_org = endtag
        do
            if ( nodata .ne. 0 ) then
                noattribs = 0
                tag = starttag
            elseif ( att_ .lt. noatt_ .and. noatt_ .gt. 1 ) then
                att_      = att_ + 1
                if ( att_ .le. noatt_-1 ) then
                    tag       = attribs(1,att_)
                    data(1)   = attribs(2,att_)
                    noattribs = 0
                    nodata    = 1
                    endtag    = .false.
                else
                    tag       = starttag
                    noattribs = 0
                    nodata    = 0
                    endtag    = .true.
                    cycle
                endif
            else
                if ( endtag_org ) then
                    return
                else
                    call xml_get( info, tag, endtag, attribs, noattribs, data, nodata )
                    if ( xml_error(info) ) then
                        write(lurep_,*) 'Error reading input file!'
                        error = .true.
                        return
                    endif
                endif
            endif
            if ( endtag .and. tag .eq. starttag ) then
                exit
            endif
            if ( endtag .and. noattribs .eq. 0 ) then
                if ( xml_ok(info) ) then
                    cycle
                else
                    exit
                endif
            endif
            select case( tag )
                case('type')
                    call read_xml_word( &
                        info, tag, endtag, attribs, noattribs, data, nodata, &
                        type, has_type )
                case('version')
                    call read_xml_word( &
                        info, tag, endtag, attribs, noattribs, data, nodata, &
                        version, has_version )
                case('byte_order')
                    call read_xml_word( &
                        info, tag, endtag, attribs, noattribs, data, nodata, &
                        byte_order, has_byte_order )
                case('header_type')
                    call read_xml_word( &
                        info, tag, endtag, attribs, noattribs, data, nodata, &
                        header_type, has_header_type )
                case('vtkMultiBlockDataSet')
                    call read_xml_type_vtkMultiBlockDataSet_type( &
                        info, tag, endtag, attribs, noattribs, data, nodata, &
                        vtkMultiBlockDataSet, has_vtkMultiBlockDataSet )
                case ('comment', '!--')
                    ! Simply ignore
                case default
                    if ( strict_ ) then
                        error = .true.
                        call xml_report_errors( info, &
                            'Unknown or wrongly placed tag: ' // trim(tag))
                    endif
            end select
            nodata = 0
            if ( .not. xml_ok(info) ) exit
        end do
        if ( .not. has_vtkMultiBlockDataSet ) then
            error = .true.
            call xml_report_errors(info, 'Missing data on vtkMultiBlockDataSet')
        endif
        if ( present(errout) ) errout = error
    end subroutine

    recursive subroutine traverse_block(item, index, u)

        type(Block_DataSet_container_type), intent(inout) :: item
        integer, intent(in) :: index
        integer, intent(in) :: u

        call item%p%open(index, u)
        call item%p%close(u)

    end subroutine

    subroutine write_vtkMultiBlockDataSet(vtkMultiBlockDataSet, dosya)

        type(vtkMultiBlockDataSet_type), intent(inout) :: vtkMultiBlockDataSet
        character(*), intent(in) :: dosya

        integer :: u

        open(newunit=u, file = trim(dosya))
        write(u,'(a)') '<?xml version="1.0" encoding="utf-8" ?>'
        write(u,'(a)') '<VTKFile '//'type="'//trim(type)//'" version="'//trim(version)&
            &//'" byte_order="'//trim(byte_order)//'" header_type="'//trim(header_type)//'">'
        write(u,'(a)') '<vtkMultiBlockDataSet>'
        call traverse_block(vtkMultiBlockDataSet%item(1), 0, u)
        write(u,'(a)') '</vtkMultiBlockDataSet>'
        write(u,'(a)') '</VTKFile>'
        close(u)

    end subroutine


    subroutine open_Block(dvar, index, u)

        class(Block_type), intent(inout) :: dvar
        integer, intent(in) :: u
        integer, intent(in) :: index
        integer :: i
        character(10) :: str

        dvar%index = index
        write(str,'(1i3)') index
        write(u,'(a)') '<Block '//'index="'//trim(adjustl(str))//'" name="'//trim(dvar%name)//'">'
        if(size(dvar%item) > 0) then
            do i = 1, size(dvar%item)
                call traverse_block(dvar%item(i), i - 1, u)
            enddo
        endif

    end subroutine

    subroutine close_Block(dvar, u)

        class(Block_type), intent(in) :: dvar
        integer, intent(in) :: u

        write(u,'(a)') '</Block>'

    end subroutine close_Block

    subroutine open_DataSet(dvar, index, u)

        class(DataSet_type), intent(inout) :: dvar
        integer, intent(in) :: index
        integer, intent(in) :: u
        character(10) :: str

        dvar%index = index
        write(str,'(1i3)') index
        !            write(u,'(a)') '<DataSet '//'index="'//trim(adjustl(str))&
            !            &//'" name="'//trim(dvar%name)//'" file="'//trim(dvar%file)//'">'
        write(u,'(a)') '<DataSet '//'index="'//trim(adjustl(str))&
            &//'" name="'//trim(dvar%name)//'" file="'//trim(dvar%file)//'"/>'
        !            write(u,'(a)') '<DataSet '//'index="'//trim(adjustl(str))&
            !            &//'" name="'//trim(dvar%name)//'" file="'//trim(dvar%file)//'"></DataSet>'

    end subroutine

    subroutine close_DataSet(dvar, u)

        class(DataSet_type), intent(in) :: dvar
        integer, intent(in) :: u

        !            write(u,'(a)') '</DataSet>'

    end subroutine close_DataSet

end module
