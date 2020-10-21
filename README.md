# vtk_multiblockdataset_reader

This is pure Fortran code for reading vtk multiblock dataset (.vtm) files (only vtm file not the dataset files (model files)).

For reading XML file, xml-fortran of Arjen Marcus is a great source, which is very simple to learn (http://xml-fortran.sourceforge.net/). The basic part of the main module (module_multiblockdataset.f90) was generated by means of xmlreader of xml-fortran. Then it was chaged to be more object oriented style and added tree traversal subroutine. In addition, since xml-fortran uses Fortran 90, the derived type arrays are generated as pointer, however since Fortran 2003+ allows to use allocatable arrays for derived types and the allocatable arrays are safer in terms of memory leak, pointer derived type arrays were changed to allocatable.
