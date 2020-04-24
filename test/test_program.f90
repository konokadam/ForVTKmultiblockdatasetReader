! Test program for generated code
program test

   use xml_data_multiblockdataset

   implicit none
   type(vtkMultiBlockDataSet_type) :: vtkMultiBlockDataSet

   open(152, file = "xmlread.log")
   call read_xml_file_multiblockdataset_reader(vtkMultiBlockDataSet, "test.vtm", lurep=152)

   close(152)

   call write_vtkMultiBlockDataSet(vtkMultiBlockDataSet, "test_write.vtm")

end program

