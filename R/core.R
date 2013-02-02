# ---------------------------------------------------
# Copyright 2012 Taras Zakharko
# 
# This file is part of the ToolboxSearch R package.
# 
# ToolboxSearch is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the Free
# Software Foundation, either version 2 of the License, or (at your option)
# any later version.
# 
# ToolboxSearch is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
# more details.
# ---------------------------------------------------


.VERSION <- "0.1-0"


.onLoad <- function(libname, pkgname)
{
	library.dynam("ToolboxSearch", package=pkgname, lib.loc=libname)
	cat("ToolboxSearch", .VERSION, "loaded.\n")	
	
}
