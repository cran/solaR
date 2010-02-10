.First.lib <- function(lib, pkg){
	
	require(vcd, quietly = TRUE)
	
	if (!require(lattice, quietly = TRUE)) 
		warning('lattice package could not be loaded. Some funcionality of solaR may not be available')
	if (!require(latticedl, quietly = TRUE)) 
		warning('latticedl package could not be loaded. Some funcionality of solaR may not be available')
	if (!require(latticeExtra, quietly = TRUE)) 
		warning('latticeExtra package could not be loaded. Some funcionality of solaR may not be available')
	#if (!(require(gWidgets, quietly = TRUE) && 
	#	(require(gWidgetstcltk, quietly = TRUE) || require(gWidgetsRGtk2, quietly = TRUE))))
	#	warning('gWidgets package or their associated packages could not be loaded. GUI funcionality of solaR will not be available')
}
