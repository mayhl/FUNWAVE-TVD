!------------------------------------------------------------------------------------
!
!      FILE mod_parallel_field_io.F
!
!      This file is part of the FUNWAVE-TVD program under the Simplified BSD license
!
!-------------------------------------------------------------------------------------
! 
!    Copyright (c) 2016, FUNWAVE Development Team
!
!    (See http://www.udel.edu/kirby/programs/funwave/funwave.html
!     for Development Team membership)
!
!    All rights reserved.
!
!    FUNWAVE_TVD is free software: you can redistribute it and/or modify
!    it under the terms of the Simplified BSD License as released by
!    the Berkeley Software Distribution (BSD).
!
!    Redistribution and use in source and binary forms, with or without
!    modification, are permitted provided that the following conditions are met:
!
!    1. Redistributions of source code must retain the above copyright notice, this
!       list of conditions and the following disclaimer.
!    2. Redistributions in binary form must reproduce the above copyright notice,
!    this list of conditions and the following disclaimer in the documentation
!    and/or other materials provided with the distribution.
!
!    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
!    ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
!    WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
!    DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
!    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
!    (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
!    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
!    ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
!    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
!    SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
!  
!    The views and conclusions contained in the software and documentation are those
!    of the authors and should not be interpreted as representing official policies,
!    either expressed or implied, of the FreeBSD Project.
!  
!-------------------------------------------------------------------------------------
!
!    PARALLEL_FIELD_IO is a module that contains subroutines for outputting
!                      field data in various data format
!
!    HISTORY:
!      12/06/2017 Michael-Angelo Y.-H. Lam
!-------------------------------------------------------------------------------------



MODULE PARALLEL_FIELD_IO

      
USE GLOBAL

      LOGICAL :: FILE_CLOSED = .TRUE.
      LOGICAL :: HAS_SHARED_FILE_NAME = .FALSE.
      CHARACTER(80) :: SHARED_FILE_NAME = ''
      
      INTEGER :: ncid
      INTEGER, DIMENSION(2) :: dimids

# if defined (PARALLEL)
  
CONTAINS

!------------------------------------------------------
!     ASCII Output
!------------------------------------------------------
      
SUBROUTINE PutFileASCII(VAR_NAME,FILE_NUMBER,PHI)
    
     IMPLICIT NONE
     CHARACTER(LEN=80), INTENT(IN) :: VAR_NAME
     CHARACTER(LEN=10), INTENT(IN) :: FILE_NUMBER
     REAL(SP),DIMENSION(Mloc,Nloc),INTENT(IN) :: PHI
      
     CHARACTER(LEN=80) :: FILE

      
![-------ykchoi (06/May/2017)
      !INTEGER :: l
      ! could be max. procs
      !INTEGER,DIMENSION(NumberProcessor) :: npxs,npys
	!REAL(SP),DIMENSION(NumberProcessor) :: xx
     INTEGER :: irank, lenx, leny, lenxy, ireq
     INTEGER, ALLOCATABLE :: iistas(:), iiends(:), jjstas(:), jjends(:)
     INTEGER :: istatus(mpi_status_size)

     REAL(SP), ALLOCATABLE :: LocalPHI(:,:)
     REAL(SP), ALLOCATABLE :: xx(:,:)
     
      !REAL(SP),DIMENSION(MGlob+2*Nghost,NGlob+2*Nghost) :: PHIGLOB   
     REAL(SP),DIMENSION( MGlob, NGlob ) :: PHIGLOB
!-------ykchoi (06/May/2017)]
     LOGICAL :: FirstCallPutFile = .TRUE.
     SAVE  FirstCallPutFile

      ! Filter off file number for depth and breakwater 
      IF (TRIM(FILE_NUMBER)=='') THEN
         FILE =  TRIM(RESULT_FOLDER)//TRIM(VAR_NAME)
      ELSE
         FILE =  TRIM(RESULT_FOLDER)//TRIM(VAR_NAME)//'_'//TRIM(FILE_NUMBER)
      ENDIF

      
! first time call 
     IF(FirstCallPutFile)THEN
        FirstCallPutFile = .FALSE.
! format length
        write(FORMAT_LEN(1:1),'(A1)') '('
        write(FORMAT_LEN(2:8),'(I7)') Mglob
        write(FORMAT_LEN(9:13),'(A5)') 'E16.6'
        write(FORMAT_LEN(14:14),'(A1)') ')'
     ENDIF

![-------ykchoi (06/May/2017)
      !call MPI_Gather(npx,1,MPI_INTEGER,npxs,1,MPI_INTEGER,&
      !    0,MPI_COMM_WORLD,ier)
      !call MPI_Gather(npy,1,MPI_INTEGER,npys,1,MPI_INTEGER,&
      !    0,MPI_COMM_WORLD,ier)
     ALLOCATE( LocalPHI(Mloc-2*Nghost, Nloc-2*Nghost) )
     LocalPHI=PHI(Ibeg:Iend,Jbeg:Jend)

     allocate( iistas(nprocs), iiends(nprocs), jjstas(nprocs), jjends(nprocs) ) 

     call MPI_Gather( iista, 1, MPI_INTEGER, iistas, 1, MPI_INTEGER, &
                      0, MPI_COMM_WORLD, ier )
     call MPI_Gather( iiend, 1, MPI_INTEGER, iiends, 1, MPI_INTEGER, &
                      0, MPI_COMM_WORLD, ier )
     call MPI_Gather( jjsta, 1, MPI_INTEGER, jjstas, 1, MPI_INTEGER, &
                      0, MPI_COMM_WORLD, ier )
     call MPI_Gather( jjend, 1, MPI_INTEGER, jjends, 1, MPI_INTEGER, &
                      0, MPI_COMM_WORLD, ier )
      
     if( myid == 0 ) then
	 PHIGLOB( iista:iiend, jjsta:jjend ) = LocalPHI
     endif

     do irank=1, px*py-1  	  
	  
	  if( myid == 0 ) then
	    lenx = iiends(irank+1) - iistas(irank+1) + 1 
	    leny = jjends(irank+1) - jjstas(irank+1) + 1
	    lenxy = lenx*leny
	    allocate( xx(lenx, leny) )
	  
	    call mpi_irecv( xx, lenxy, mpi_sp, irank, 1, mpi_comm_world, ireq, ier )
	    call mpi_wait( ireq, istatus, ier )
	    
		PHIGLOB( iistas(irank+1):iiends(irank+1), jjstas(irank+1):jjends(irank+1) ) = xx
		deallocate( xx )

        elseif( myid == irank ) then
	    
		lenxy = ( iiend-iista+1 )*( jjend-jjsta+1 )
	    call mpi_isend( LocalPHI, lenxy, mpi_sp, 0, 1, mpi_comm_world, ireq, ier )
	    call mpi_wait( ireq, istatus, ier )

        endif	  

     enddo

     deallocate(LocalPHI, iistas, iiends, jjstas, jjends)

     if (myid.eq.0) then
        OPEN(1,FILE=TRIM(FILE))
	  DO J=1,NGlob,OUTPUT_RES
	     WRITE(1,FORMAT_LEN)(real(PHIGLOB(I,J)),I=1,MGlob,OUTPUT_RES)
	  ENDDO
        !DO J=Nghost+1,NGlob+NGhost,OUTPUT_RES
        !   WRITE(1,FORMAT_LEN)(real(PHIGLOB(I,J)),I=Nghost+1,MGlob+Nghost,OUTPUT_RES)
        !ENDDO
!100  FORMAT(5000E16.6)
!100   FORMAT(FORMAT_LEN)
        CLOSE(1)
     endif
!-------ykchoi (06/May/2017)]

END SUBROUTINE PutfileASCII


!------------------------------------------------------
!     Binary Output
!------------------------------------------------------
      
! Code derived from William D. Gropp lecture slides 33 
! http://wgropp.cs.illinois.edu/courses/cs598-s16/lectures/lecture33.pdf
SUBROUTINE PutFileBinary(VAR_NAME,FILE_NUMBER,PHI)
    
     IMPLICIT NONE
     CHARACTER(LEN=80), INTENT(IN) :: VAR_NAME
     CHARACTER(LEN=10), INTENT(IN) :: FILE_NUMBER
     REAL(SP),DIMENSION(Mloc,Nloc), INTENT(IN) :: PHI

     CHARACTER(LEN=80) :: FILE
      
     INTEGER ::  file_handler, ierr , istatus  , mem_type_loc , file_type_glob
     INTEGER(8) :: offset
     INTEGER , DIMENSION(ndims) ::  sizes_loc , subsizes_loc , start_index_loc
     INTEGER , DIMENSION(ndims) ::  sizes_glob , subsizes_glob , start_index_glob


      ! Filter off file number for depth and breakwater 
      IF (TRIM(FILE_NUMBER)=='') THEN
         FILE =  TRIM(RESULT_FOLDER)//TRIM(VAR_NAME)
      ELSE
         FILE =  TRIM(RESULT_FOLDER)//TRIM(VAR_NAME)//'_'//TRIM(FILE_NUMBER)
      ENDIF
      
     offset = 0


     ! Defines each nodes subdomain 'PHI' and the surrounding ghost points
     ! Used to extract data minus ghost points
     sizes_loc(1) = Mloc 
     sizes_loc(2) = Nloc

     subsizes_loc(1) =  sizes_loc(1) - 2*Nghost
     subsizes_loc(2) =  sizes_loc(2) - 2*Nghost

     start_index_loc(1) = Nghost  ! 0 start index even in Fortran
     start_index_loc(2) = Nghost  ! 0 start index even in Fortran

     ! Defines the mapping between subdomains (minus ghost points)
     ! to the global domain
     sizes_glob(1) = Mglob
     sizes_glob(2) = Nglob

     subsizes_glob(1) =   subsizes_loc(1) 
     subsizes_glob(2) =   subsizes_loc(2)

     start_index_glob(1) = iista - 1 ! 0 start index even in Fortran 
     start_index_glob(2) = jjsta - 1 ! 0 start index even in Fortran


     CALL MPI_TYPE_CREATE_SUBARRAY(ndims, sizes_loc , subsizes_loc , &
                                   start_index_loc , MPI_ORDER_FORTRAN, mpi_sp , mem_type_loc, ierr)
     CALL MPI_TYPE_COMMIT(mem_type_loc, ierr)


     CALL MPI_TYPE_CREATE_SUBARRAY(ndims, sizes_glob , subsizes_glob , &
                                   start_index_glob , MPI_ORDER_FORTRAN, mpi_sp , file_type_glob , ierr)
     CALL MPI_TYPE_COMMIT(file_type_glob, ierr)


     CALL MPI_FILE_OPEN(MPI_COMM_WORLD, FILE , & 
                           MPI_MODE_WRONLY + MPI_MODE_CREATE, & 
                           MPI_INFO_NULL, file_handler , ierr) 

     CALL MPI_FILE_SET_VIEW( file_handler , offset , mpi_sp , file_type_glob , 'native' , MPI_INFO_NULL, ierr)


     CALL MPI_FILE_WRITE_ALL(file_handler, PHI , 1, mem_type_loc , MPI_STATUS_IGNORE, ierr)

     CALL MPI_FILE_CLOSE(file_handler, ierr )


END SUBROUTINE PutfileBinary

# if defined (EXTERNAL_PACKAGES)


!------------------------------------------------------
!     NetCDF Output
!------------------------------------------------------
      
! Wrapper to fixed 'Error NetCDF: Variable not found'
! that appeared to be system dependent    
SUBROUTINE NetCDFAttWrapper(NAME,VALUE)

      USE netcdf
      IMPLICIT NONE
  !    INTEGER, INTENT(IN) :: ncid
      CHARACTER(LEN=*), INTENT(IN) :: NAME
      REAL(SP) , INTENT(IN) :: VALUE
     
      INTEGER :: dummy_id
            
      CALL CheckNetCDF(nf90_put_att(ncid, dummy_id, NAME ,VALUE))
      
END SUBROUTINE

      
SUBROUTINE CreateFileNetCDF(FILE_NAME)

      USE netcdf
      IMPLICIT NONE
      
      CHARACTER(LEN=80), INTENT(IN) :: FILE_NAME
    !  INTEGER, INTENT(OUT) :: ncid
     ! INTEGER, DIMENSION(ndims), INTENT(OUT) :: dimids
      INTEGER :: mode_flag

      INTEGER :: x_dimid , y_dimid
      
      mode_flag = IOR(NF90_MPIIO, NF90_CLOBBER)
      mode_flag = IOR(mode_flag, NF90_NETCDF4)
         
      CALL CheckNetCDF(nf90_create_par(FILE_NAME, mode_flag , &
                         MPI_COMM_WORLD,  MPI_INFO_NULL, ncid)) 

# if defined (CARTESIAN)
      CALL CheckNetCDF(nf90_def_dim(ncid, "x", Mglob , x_dimid))
      CALL CheckNetCDF(nf90_def_dim(ncid, "y", Nglob , y_dimid))
# else
      CALL CheckNetCDF(nf90_def_dim(ncid, "Dphi", Mglob , x_dimid))
      CALL CheckNetCDF(nf90_def_dim(ncid, "Dtheta", Nglob , y_dimid))
# endif
      
      dimids = (/ x_dimid, y_dimid /)
  
      CALL NetCDFAttWrapper( "TIME" , TIME )
      CALL NetCDFAttWrapper( "TOTAL_TIME" , TOTAL_TIME )
      
      CALL NetCDFAttWrapper( "PLOT_INTV" , PLOT_INTV )
      CALL NetCDFAttWrapper( "PLOT_INTV_STATION" , PLOT_INTV_STATION )
      CALL NetCDFAttWrapper( "SCREEN_INTV" , SCREEN_INTV )
      
# if defined (CARTESIAN)
      CALL NetCDFAttWrapper( "DX" , DX )
      CALL NetCDFAttWrapper( "DY" , DY )
# else
      CALL NetCDFAttWrapper( "Lon_West", Lon_West))
      CALL NetCDFAttWrapper( "Lat_South", Lat_South))
      CALL NetCDFAttWrapper( "Dphi", Dphi))
      CALL NetCDFAttWrapper( "Dtheta", Dtheta))
      
# endif
      
      CALL CheckNetCDF(nf90_enddef(ncid))
      
END SUBROUTINE CreateFileNetCDF

   
SUBROUTINE PutFileNetCDF(VAR_NAME,FILE_NUMBER,PHI)
      
      USE netcdf
      IMPLICIT NONE
      CHARACTER(LEN=80), INTENT(IN) :: VAR_NAME
      CHARACTER(LEN=10), INTENT(IN) :: FILE_NUMBER
      REAL(SP),DIMENSION(Mloc,Nloc),INTENT(IN) :: PHI

      
      CHARACTER(LEN=80) :: FILE_NAME
      REAL(SP) , DIMENSION(Mloc- 2*Nghost  , Nloc - 2*Nghost) :: TEST_DATA
      INTEGER :: mode_flag, x_dimid , y_dimid, varid
      INTEGER :: dummy_id
      INTEGER, DIMENSION(ndims) :: start, count, chunk_size
      
!     Filter off file number for depth and breakwater
      IF (TRIM(FILE_NUMBER)=='') THEN
         FILE_NAME =  TRIM(RESULT_FOLDER)//TRIM(VAR_NAME)
      ELSE
         IF ( COMBINE_FIELD_IO.EQV..TRUE. ) THEN
            FILE_NAME =  TRIM(RESULT_FOLDER)//TRIM(SHARED_FILE_NAME)//'_'//TRIM(FILE_NUMBER)
         ELSE
            FILE_NAME =  TRIM(RESULT_FOLDER)//TRIM(VAR_NAME)//'_'//TRIM(FILE_NUMBER)
         END IF
      END IF

      IF ( FILE_CLOSED.EQV..TRUE. ) THEN
         CALL CreateFileNetCDF(FILE_NAME)
         FILE_CLOSED = .FALSE.   
      END IF
      
      CALL CheckNetCDF(nf90_redef(ncid)) 
      
# if defined (DOUBLE_PRECISION)
      CALL CheckNetCDF(nf90_def_var( &
      ncid, &
      VAR_NAME , &
      NF90_DOUBLE, &
      dimids, &
      varid))
# else
       CALL CheckNetCDF(nf90_def_var( &
      ncid, &
      VAR_NAME , &
      NF90_FLOAT, &
      dimids, &
      varid))
# endif  
            
      CALL CheckNetCDF(nf90_enddef(ncid))
       
      start = (/ iista , jjsta /)
      count = (/ Mloc-2*Nghost , Nloc-2*Nghost /) ! chunk_size
      CALL CheckNetCDF(nf90_var_par_access(ncid, varid, nf90_collective))
       
      CALL CheckNetCDF(nf90_put_var(ncid, varid, &
      PHI( (Nghost+1):(Mloc-Nghost) ,(Nghost+1):(Nloc-Nghost) ) , & 
    !  TEST_DATA , & 
      start = start, &
      count = count)) 

END SUBROUTINE PutFileNetCDF

      
SUBROUTINE ClosePutFileNetCDF()
      
      USE netcdf
      IMPLICIT NONE
      CALL CheckNetCDF( nf90_close(ncid) )
      
      FILE_CLOSED=.TRUE.
      SHARED_FILE_NAME = ''

END SUBROUTINE ClosePutFileNetCDF

      
SUBROUTINE CheckNetCDF(status)

      USE netcdf
      IMPLICIT NONE
      INTEGER, INTENT (IN) :: status
      INTEGER :: errorcode, ierr
      
      IF(status /=  nf90_noerr) THEN
         WRITE (*,*) "Error " ,  trim(nf90_strerror(status))
         CALL MPI_ABORT (MPI_COMM_WORLD, errorcode , ierr) 
         
      END IF
  
END SUBROUTINE CheckNetCDF

      
!------------------------------------------------------
!     XDMF Output
!------------------------------------------------------

SUBROUTINE PutFileXDMF(VAR_NAME,FILE_NUMBER,PHI)
      
      USE HDF5
      IMPLICIT NONE
      CHARACTER(LEN=80), INTENT(IN) :: VAR_NAME
      CHARACTER(LEN=10), INTENT(IN) :: FILE_NUMBER
      REAL(SP),DIMENSION(Mloc,Nloc),INTENT(IN) :: PHI

    
      CHARACTER(LEN=80) :: FILE_NAME
      INTEGER :: error, error_n

      INTEGER :: mpi_info


      INTEGER(HID_T) :: file_id       ! File identifier 
      INTEGER(HID_T) :: dset_id       ! Dataset identifier 
      INTEGER(HID_T) :: filespace     ! Dataspace identifier in file 
      INTEGER(HID_T) :: memspace      ! Dataspace identifier in memory
      INTEGER(HID_T) :: plist_id      ! Property list identifier 

      INTEGER(HSIZE_T), DIMENSION(ndims) :: dimsf , chunk_dims

      INTEGER(HSIZE_T),  DIMENSION(ndims) :: count  
      INTEGER(HSSIZE_T), DIMENSION(ndims) :: offset 
      INTEGER(HSIZE_T),  DIMENSION(ndims) :: stride
      INTEGER(HSIZE_T),  DIMENSION(ndims) :: block
      

    ! Filter off file number for depth and breakwater 
      IF (TRIM(FILE_NUMBER)=='') THEN
         FILE_NAME =  TRIM(RESULT_FOLDER)//TRIM(VAR_NAME)
      ELSE
         FILE_NAME =  TRIM(RESULT_FOLDER)//TRIM(VAR_NAME)//'_'//TRIM(FILE_NUMBER)
      ENDIF

      IF (myid.eq.0) WRITE(*,*) "HERE 1"
      
      dimsf = (/Mglob,Nglob/) 
      chunk_dims = (/Mloc-2*Nghost,Nloc-2*Nghost/) 
      ! Initialize HDF5 library and Fortran interfaces.
      CALL h5open_f(error)

      ! Setup file access property list with parallel I/O access.
      mpi_info = MPI_INFO_NULL 
      CALL h5pcreate_f(H5P_FILE_ACCESS_F, plist_id, error)
      CALL h5pset_fapl_mpio_f(plist_id, MPI_COMM_WORLD , mpi_info , error)

      ! Create the file collectively.
      CALL h5fcreate_f(FILE_NAME, H5F_ACC_TRUNC_F, file_id, error, access_prp = plist_id)
      CALL h5pclose_f(plist_id, error)

      ! Create the data space for the  dataset. 
      CALL h5screate_simple_f(ndims, dimsf, filespace, error)
      CALL h5screate_simple_f(ndims, chunk_dims, memspace, error)

      IF (myid.eq.0) WRITE(*,*) "HERE 2"
     ! Create chunked dataset.
     !
      CALL h5pcreate_f(H5P_DATASET_CREATE_F, plist_id, error)
      CALL h5pset_chunk_f(plist_id, ndims, chunk_dims, error)
# if defined (DOUBLE_PRECISION)
      CALL h5dcreate_f(file_id, TRIM(VAR_NAME), H5T_NATIVE_DOUBLE, filespace, &
      dset_id, error, plist_id)
# else
      CALL h5dcreate_f(file_id, TRIM(VAR_NAME), H5T_NATIVE_REAL , filespace, &
      dset_id, error, plist_id)
# endif       
      CALL h5sclose_f(filespace, error)

      stride(1) = 1 
      stride(2) = 1 
      count(1) =  1 
      count(2) =  1 
      block(1) = chunk_dims(1)
      block(2) = chunk_dims(2)
      offset(1) = iista - 1 ! 0 start index even in Fortran 
      offset(2) = jjsta - 1 ! 0 start index even in Fortran

      ! Select hyperslab in the file.
      CALL h5dget_space_f(dset_id, filespace, error)
      CALL h5sselect_hyperslab_f (filespace, H5S_SELECT_SET_F, offset, count, error, &
                                 stride, block)
      ! Create property list for collective dataset write
      CALL h5pcreate_f(H5P_DATASET_XFER_F, plist_id, error) 
      CALL h5pset_dxpl_mpio_f(plist_id, H5FD_MPIO_COLLECTIVE_F, error)

      
      IF (myid.eq.0) WRITE(*,*) "HERE 3"
      ! Write the dataset collectively. 
# if defined (DOUBLE_PRECISION)
      CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, PHI( (Nghost+1):(Mloc-Nghost) ,(Nghost+1):(Nloc-Nghost)) , dimsf, error, &
                     file_space_id = filespace, mem_space_id = memspace, xfer_prp = plist_id)
# else
      CALL h5dwrite_f(dset_id, H5T_NATIVE_REAL , PHI( (Nghost+1):(Mloc-Nghost),(Nghost+1):(Nloc-Nghost)) , dimsf, error, &
                     file_space_id = filespace, mem_space_id = memspace, xfer_prp = plist_id)
# endif
                     
      ! Close dataspaces.
      CALL h5sclose_f(filespace, error)
      CALL h5sclose_f(memspace, error)
      
      ! Close the dataset.
      CALL h5dclose_f(dset_id, error)

      
      ! Close the property list.
      CALL h5pclose_f(plist_id, error)
      
      IF (myid.eq.0) WRITE(*,*) "HERE 4"
      ! Close the file.
      CALL h5fclose_f(file_id, error)

      IF (myid.eq.0) WRITE(*,*) "HERE 5"
      
      ! Close FORTRAN interfaces and HDF5 library.
      CALL h5close_f(error)
      
END SUBROUTINE PutFileXDMF
      
# endif
! end external packages
      
# endif
! end parallel
      
END MODULE PARALLEL_FIELD_IO 



