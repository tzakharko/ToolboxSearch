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

###########################
#
#  Corpus format descriptor
#
###########################

toolboxFormat <- function(id, ...){
	.eframe. <- parent.frame()
	
	
	.evalarg. <- function(arg) 
	{
		a0 <- tryCatch(eval(parse(text=arg), enclos=.eframe.), error=function(e) arg)
		if(!is.character(a0)) arg else a0
	}
	
	
	c <- function(...) 
		sapply(sapply(substitute(list(...))[-1], deparse), .evalarg., USE.NAMES=F)
	
	
	
	expp <- sapply(match.call()[-1], deparse)
	
	fmt <- lapply(expp, .evalarg.)
	
	fmt <- unlist(lapply(names(fmt), function(n) {
					t <- rep(as.character(n), length(fmt[[n]]))
					names(t) <- as.character(fmt[[n]])
					t
				}))
	
	structure(fmt, class = "toolbox.format.descriptor")			
}

as.character.toolbox.format.descriptor <- function(x)
{
	x <- unclass(x)

	lvls <- unique(x)
	lnum <- length(lvls)
	names(x) <- paste("\\", names(x), sep="")
	
	
	paste("Toolbox format descriptor with ", lnum, if(lnum==1) " level\n" else " levels\n",
		  "  record marker ", names(x)[1], "\n",
		  paste(sapply(lvls, function(lvl)
			paste("  @", lvl, ": ", paste(names(x[x==lvl]), collapse=" "), sep="")
			), sep="", collapse="\n"), sep="")
}

print.toolbox.format.descriptor <- function(x, ...) cat(as.character(x),"\n")

levels.toolbox.format.descriptor <- function(x) unique(unique(x))

"$.toolbox.format.descriptor" <- function(x, i) x[i]

"[.toolbox.format.descriptor" <- function(x, i) {
	x <- unclass(x)

	t <-  unique(x)
	
	if(is.numeric(i))
	{
		if(length(i) != 1) stop("Invalid level index\n")
		if(i < 1 || i > length(t)) stop("Invalid level index\n")
		names(x[x == t[i]])
	} else
	if(is.character(i))
	{
		if(!i %in% t) stop("Invalid level name\n")
		names(x[x == i])
	} else stop("Invalid level index\n")
}

format.descriptor <- function(corpus) attr(corpus, "format.descriptor")

"format.descriptor<-" <- function(corpus, value)
{
	old <- format.descriptor(corpus)
	new <- value
	
	old.levels <- unique(old)
	new.levels <- unique(value)
	
	# the relative order of levels must match!
	common.levels <- intersect(old.levels, new.levels)
	if(!identical(old.levels[old.levels %in% common.levels], new.levels[new.levels %in% common.levels]))
		stop("The levels of the new format descriptor must keep the relative hierarchy of the currect descriptor:\n    (", paste(old.levels[old.levels %in% common.levels], collapse=", "), ") instead (", paste(new.levels[new.levels %in% common.levels], collapse=", "), ")\n")
		
	# we only allow deletion of a level if everything below this level is also deleted
	for(deleted.level in which(!(old.levels %in% new.levels)))
		if(any(old.levels[deleted.level:(length(old))] %in% new.levels))
			stop("Cannot delete level *", old.levels[deleted.level], "* as there are levels below it!\n")
	
	# and delete the corresponding levels from the token table
	corpus$tok.table <- corpus$tok.table[intersect(names(corpus$tok.table), new.levels)]
	
	# create new levels
	# each new level simply copies the data of the higher one
	# if there is no higher one, the level consists of a single object
	for(lvl.i in seq_along(new.levels))
	if(!new.levels[lvl.i] %in% names(corpus$tok.table))
	{
		if(lvl.i==1) 
			stop("Cannot add a new top level *", new.levels[lvl.i],"*!\n", )
			#corpus$tok.table[[new.levels[lvl.i]]] <- as.integer(rep(1L, length(corpus$tok.table[[1]])))

			prev <- names(corpus$tok.table)[lvl.i - 1]
			corpus$tok.table[[new.levels[lvl.i]]] <- corpus$tok.table[[prev]]
	}
	
	# update the tier list
	corpus$tiers <- corpus$tiers[intersect(names(corpus$tiers), names(value))]
	
	for(tier in names(value))
	if(!tier %in% names(corpus$tiers))
	{
		corpus$tiers[[tier]] <- as.character(rep(NA, length(unique(na.omit(corpus$tok.table[[value[[tier]]]])))))
	}
	
	corpus$tiers <- corpus$tiers[names(unclass(value))]
	
	attr(corpus, "format.descriptor") <- value
	
	corpus
}

###########################
#
#  Corpus display utilities
#
###########################
record.to.toolbox <- function(corpus, id, order = NULL, skip.invisibles = T, use.bytes = T)
{
	fmt <- unclass(attr(corpus, "format.descriptor"))
	index <-  which(corpus$tok.table[[1]] %in% id)

	static.tiers <- which(fmt == fmt[1])
	aligned.tiers <- which(fmt != fmt[1])
	
	static.tiers.s <- sapply(static.tiers, function(tier.i)  corpus$tiers[[tier.i]][id])
	names(static.tiers.s) <- names(fmt)[static.tiers]
	
	
    aligned.tiers.s <- lapply(aligned.tiers, function(tier.i)  {
		i <- corpus$tok.table[[fmt[tier.i]]][index]
		i[duplicated(i)] <- NA
		i <- corpus$tiers[[tier.i]][i]
		i[is.na(i)] <- ""
		i
	}) 
	
	if(length(aligned.tiers.s)>0)
	{
		aligned.tiers.s <- .Call("write_aligned_strings", aligned.tiers.s, skip.invisibles , use.bytes)
		names(aligned.tiers.s) <- names(fmt)[aligned.tiers]
	}
		
	tiers <- c(static.tiers.s, aligned.tiers.s)
	
	if(is.null(order)) order <- names(fmt)
	
	tiers <- tiers[order]
	
	
	tiers <- tiers[tiers != ""]
	tiers <- tiers[!is.na(tiers)]
	#tiers <- tiers[!is.na(names(tiers))]
	
	tiers <- paste("\\", names(tiers), " ", tiers, sep="", collapse="\n")
	
	paste(tiers, "\n", sep="")
}

writeToolbox <- function(corpus, file, order = NULL, header = attr(corpus, "source.header"), skip.invisibles =T, use.bytes = T)
{
	text <- sapply(seq_along(corpus$tiers[[1]]), function(id) record.to.toolbox(corpus, id, order, skip.invisibles, use.bytes))
	
	text <- c(header, text)
	
	text <- paste(text, collapse="\n\n")
	
	
	cat(text, file = file)
}

###########################
#
#  Corpus subset
#
###########################

index.corpus <- function(index=NULL, level=NULL)
	structure(as.integer(sort(index)), level=level, class="language.corpus.subset")


"+.language.corpus.subset" <- function(a, b)
{
	if(attr(a, "level") != attr(b, "level"))
		stop("Must be at the same level!")
		
	index.corpus(c(a, b), attr(a, "level"))
}

"*.language.corpus.subset" <- function(a, b)
{
	if(attr(a, "level") != attr(b, "level"))
		stop("Must be at the same level!")

	index.corpus(intersect(a, b), attr(a, "level"))
}

"-.language.corpus.subset" <- function(a, b)
{
	if(class(a)=="language.corpus")
		a <- index.corpus(1:length.corpus(a, attr(b, "level")), attr(b, "level"))
	
	if(attr(a, "level") != attr(b, "level"))
		stop("Must be at the same level!")

	index.corpus(setdiff(a, b), attr(a, "level"))
}	
	
print.language.corpus.subset <- function(x)
{
	txt <- .Call("_format_seq", x, 30L)
	if(!grepl("elements", txt))
		txt <- paste(txt, " (", length(x), " elements)", sep="")
		
	cat("Corpus subset", "@", attr(x, "level"), ": ", txt, "\n", sep="") 
}

"[.language.corpus" <- function(corpus, index=1:3) {
	if(class(index) == "language.corpus.subset")
	{
		return(subset(corpus, index))
	} else
	{
		print(corpus, index)
		return(invisible(NULL))
	}
} 

print.language.corpus <- function(corpus, records=1:3)
{
	records <- records[records>0]
	records <- records[records<=length(corpus$tiers[[1]])]
	
	records <- as.integer(unique(records))
			
	cat("Corpus with ", length(corpus$tiers[[1]]), " entries (", levels(attr(corpus, "format.descriptor"))[1], ") showing ", .Call("_format_seq", records, 30L), ":\n", sep="")
	
	d <- ""
	for(i in records)
	{ 
		d <- paste(d, "\n----------@", i ,"\n", sep="")
		d <- paste(d, record.to.toolbox(corpus, i, use.bytes=F), sep="")
	}
	cat(d)
}


###########################
#
#  Corpus access
#
###########################



corpus.to.df <- function(corpus)
{
	fmt <- unclass(attr(corpus, "format.descriptor"))
	
	df.top   <- corpus$tok.table
	df.tiers <- lapply(seq_along(fmt), function(tier.i) corpus$tiers[[tier.i]][df.top[[fmt[tier.i]]]])
	df <- c(df.top, df.tiers)
				
	df <- structure(df, class="data.frame", row.names=c(NA, as.integer(length(df.top[[1]]))))

	names(df) <- c(paste(unique(fmt), "id", sep="."), names(fmt))		
	
	df
}



as.data.frame.language.corpus <- function(corpus, ids=NULL)
{
	if(is.null(ids)) return(corpus.to.df(corpus))
	
	fmt <- unclass(attr(corpus, "format.descriptor"))
	
	
	ids <-  which(corpus$tok.table[[1]] %in% ids)
	
	df.top   <- lapply(unique(fmt), function(level)  corpus$tok.table[[level]][ids])
	names(df.top) <- unique(fmt)
	
	df.tiers <- lapply(seq_along(fmt), function(tier.i) corpus$tiers[[tier.i]][df.top[[fmt[tier.i]]]])
	
	df <- c(df.top, df.tiers)
			
	df <- structure(df, class="data.frame", row.names=c(NA, as.integer(length(df.top[[1]]))))
	
	names(df) <- c(paste(unique(fmt), "id", sep="."), names(fmt))		
	
	df
}

concat.corpus <- function(...)
{
	.combine.fmt <- function(a, b)
	{
		if(!setequal(unique(a), unique(b)) || names(a[1]) != names(b[1]))
			stop("Unable to combine corpora of different types!")
			
		common <- intersect(names(a), names(b))
		a.common <- a[common]	
		b.common <- b[common]	
		
		if(!all(a.common == b.common))
		{
			common <- common[a.common != b.common]
			if(length(common) > 1)
				stop("Unable to combine corpora: tiers ", paste("'", common, "'", sep="", collapse=", "), " belong to different levels in both corpora!")
			else	
				stop("Unable to combine corpora: tier ", paste("'", common, "'", sep=""), " belongs to different levels in both corpora!")
		}
			
		fmt <- 	do.call(c, lapply(unique(a), function(lvl) {
							l <- unique(c(names(a[a == lvl]), names(b[b == lvl])))
							lvl <- rep(lvl, length(l))
							names(lvl) <- l
							lvl
						}))
								
		fmt
		
	}
	
	.concat <- function(a, b)
	{
		fmt <- .combine.fmt(unclass(attr(a, "format.descriptor")), unclass(attr(b, "format.descriptor")))

		a_level_len <- as.integer(sapply(unique(fmt), function(lvl) {
			f <- attr(a, "format.descriptor")
			length(a$tiers[[f[lvl][1]]])
		}))
		names(a_level_len) <- unique(fmt)

		b_level_len <- as.integer(sapply(unique(fmt), function(lvl) {
			f <- attr(b, "format.descriptor")
			length(b$tiers[[f[lvl][1]]])
		}))
		names(b_level_len) <- unique(fmt)
		
		a_tier_len <- a_level_len[fmt]
		names(a_tier_len) <- names(fmt)
		b_tier_len <- b_level_len[fmt]
		names(b_tier_len) <- names(fmt)
		
				
		.Call("_fast_combine_corpus", a, b, as.character(names(fmt)), as.character(unique(fmt)), a_level_len, a_tier_len, b_level_len, b_tier_len,
		structure(fmt, class = "toolbox.format.descriptor"),
		parse.log = c(attr(a, "parse.log"), attr(b, "parse.log")),
		source.header = attr(a, "header")
		)
	}
	
	arg <- list(...)
	if(length(arg) ==1 && class(arg[[1]]) == 'language.corpus')
		return(arg[[1]])
	else if(length(arg)==1 && is.list(arg[[1]])) 
		do.call(concat.corpus, arg[[1]])
	else
		Reduce(.concat, list(...)[-1], list(...)[[1]])
}

parse.log <- function(corpus) attr(corpus, "parse.log")

length.corpus <- function(corpus, level=levels(attr(corpus, "format.descriptor"))[1]) {
	fmt <- attr(corpus, "format.descriptor")
	length(unique(na.omit(corpus$tok.table[[level]])))#[[fmt[level][1]]])
}

subset.language.corpus <- function(corpus, ...)
{
	fmt <- unclass(attr(corpus, "format.descriptor"))
		
	args <- list(...)
		
	if(any(names(args)[1] %in% unlist(fmt)))
	{
		level = names(args)[1]
		index   = args[[1]]
		
		if(length(args)>1) {
			args <- names(args)[-1]
			args <- args[args != args]
			warning("Ignoring unknown parameters ", paste(args, collapse=", "))
		}
	} else
	if(any(names(args)[1] == "level"))
	{
		level <- args[[1]]
		index = NULL
		if(length(args)>1) {
			args <- names(args)[-1]
			args <- args[args != args]
			warning("Ignoring unknown parameters ", paste(args, collapse=", "))
		}
	} else
	if(length(args)==1)
	{
		index <- args[[1]]
		
		if(class(index) == "language.corpus.subset")
		{
			level = attr(index, "level")
			index = unclass(index)
		} 
		else
		level=levels(attr(corpus, "format.descriptor"))[1]
		
		
		if(!is.null(names(args)))
			if(!(any(names(args)[1] == "index") || any(names(args)[1] == ""))) stop("Unknown paremeters")
	} else
	if(length(args)>1)
	{
		if(!is.null(names(args))) if(!names(args)[1] %in% c("index", "")) stop("Unknown paremeters")
		if(!is.null(names(args))) if(!names(args)[2] %in% c("level", "")) stop("Unknown paremeters")
		
		index <- args[[1]]
		level <- args[[2]]
		
		if(length(args)>2) {
			args <- names(args)[-c(1, 2)]
			args <- args[args != args]
			warning("Ignoring unknown parameters ", paste(args, collapse=", "))
		}	
	} else stop("Unknown paremeters")
		
	if(!level %in% levels(attr(corpus, "format.descriptor")))
		stop("Unknown level", level)
	
	
	newlevels <- unique(fmt)[(which(unique(fmt) == level)[1]):length(unique(fmt))]
	
	
	# subset the token table
	corpus$tok.table <- corpus$tok.table[newlevels]
	if(!is.null(index))
	{
		w <- corpus$tok.table[[level]] %in% index
		for(level in unique(fmt)) corpus$tok.table[[level]] <- corpus$tok.table[[level]][w]	
	}
	
	# remove all unused tiers
	corpus$tiers <- corpus$tiers[names(fmt[fmt %in% newlevels])]
	
	fmt <- fmt[fmt %in% newlevels]
		
	# and remap the tier tokens and indices
	for(level in unique(fmt))
	{
		items <- corpus$tok.table[[level]]
		r <- rle(items)
		items <- unique(items)
		
								
		for(tier in names(fmt[fmt == level]))
			corpus$tiers[[tier]] <- corpus$tiers[[tier]][na.omit(items)]
		
		if(any(!is.na(r$values)))
			r$values[!is.na(r$values)] <- 1:sum(!is.na(r$values))

		corpus$tok.table[[level]] <- inverse.rle(r)
	}	
		
				
	attr(corpus, "format.descriptor") <- structure(fmt, class = "toolbox.format.descriptor")
	
	corpus
}


`[<-.language.corpus` <- function(corpus, index, tier, value) {
	stopifnot(inherits(corpus, "language.corpus"))
	stopifnot(inherits(index, "language.corpus.subset"))

	# decompose the index
	level <- attr(index, "level")
	index  <- unclass(index) 

	# validate the tier
	fmt <- unclass(attr(corpus, "format.descriptor"))
	(tier %in% names(fmt)) || stop(paste0("unknown tier '", tier, "'"))
	(fmt[[tier]] == level) || stop(paste0("tier '", tier, "' is not in level '", level, "'"))

	corpus$tiers[[tier]][index] <- value

	corpus
}


"[.language.corpus" <- function(corpus, ...) subset.language.corpus(corpus, ...)

###########################
#
#  Corpus loader
#
###########################

do_readToolbox <- function(file, format.descriptor, ..., unknown.tiers.as.top = F, enable.gaps=F, strict=F,  encoding="UTF-8")
{
	# do some validation
	if(!is.logical(enable.gaps)) stop("enable.gaps must be a logical scalar!\n")
	if(!is.logical(strict)) stop("strict must be a logical scalar!\n")	
	if(!inherits(format.descriptor,"toolbox.format.descriptor")) stop("format.descriptor must be a toolbox file format descriptor!\n")
	
	
	text <- readLines(file, warn=F, encoding=encoding)
	text <- iconv(text, "UTF-8", "UTF-8") # bypass the incorrect UTF-8 codepoints toolbox sometimes produces
	text <- gsub('\t', ' ', text)
	header <- paste(paste(grep("^\\\\_", text, value=T), collapse="\n"), "\n", sep="")
	text[grep("^\\\\_", text)] <- ""
	
	if(unknown.tiers.as.top)
	{
		alltiers <- grep("^\\\\[^[:space:]]", text, perl=T, useBytes=T, value=T)
		alltiers <- unique(sub("^\\\\([^[:space:]]+).*$", "\\1", alltiers, perl=T, useBytes=T))
		alltiers <- alltiers[alltiers != ""]
		unknown <- setdiff(alltiers, names(unclass(format.descriptor)))
		
		if(length(unknown) != 0)
		{ 
			msg <- paste("Adding", paste(unknown, collapse=", "), "to the top level\n")
			message(msg)
		
			u <- rep(levels(format.descriptor)[1], length(unknown))
			names(u) <- unknown
			
			format.descriptor <- unclass(format.descriptor)
			
			format.descriptor <- c(format.descriptor[1], u, format.descriptor[-1])
			
			format.descriptor <- structure(format.descriptor, class = "toolbox.format.descriptor")
		}
	}
	
		
	# process load parameters
	load.params <- replicate(length(levels(format.descriptor)), list(mode="position", conn=""), simplify=F)
	load.params[[1]]$mode <- "single"
	names(load.params) <- levels(format.descriptor)
	
	load.params0 <- list(...)
	for(n in names(load.params0))
	{		
		if(!n %in% levels(format.descriptor)) stop(n, " is not a valid level name!\n")
		
		p <- load.params0[[n]]

		mode <- if(is.list(p)) p$mode else p;
		
		switch(mode,
			position = {},
			sequence = {
				conn <- if(is.list(p)) p$conn else NULL
				if(is.null(conn)) conn<- c("-", "=")
				if(!is.character(conn) || length(conn) == 0) stop("conn must be a character vector containing single-character elements!\n")
				if(any(sapply(conn, nchar)>1)) stop("conn must be a character vector containing single-character elements!\n")
				
				load.params[[n]] <- list(mode="sequence", conn=conn)
			},
			stop("invalid match mode '", paste(as.character(p), collapse=""), "' for level ", n, "!\n")	
			)
	}
		
	# convert load parameters into the tier_prop and connector_chars vectors
	tier_prop <- lapply(levels(format.descriptor), function(level)
			switch(load.params[[level]]$mode,
				single = rep(1L, length(format.descriptor[level])),
				position = rep(2L, length(format.descriptor[level])),
				sequence = c(5L, rep(3L, length(format.descriptor[level])-1))
			))
			
	tier_prop <- unlist(tier_prop)
			
	connector_chars <- lapply(levels(format.descriptor), function(level) replicate(length(format.descriptor[level]), load.params[[level]]$conn, simplify=F))
	connector_chars <-  unlist(connector_chars, recursive=F)
	
	# compute the tier-to level vector
	tl <- seq_along(levels(format.descriptor))
	names(tl) <- levels(format.descriptor)
	tl <- tl[format.descriptor]
	
	# do the black magic
	crp_dta <- .Call("read_toolbox", 
					 text,
					 list(names(format.descriptor), tl, tier_prop, as.integer(length(levels(format.descriptor))), connector_chars),
					 "", 
					 !enable.gaps,
					 strict)
		
	tiers <- crp_dta[[1]]
	tiers <- lapply(tiers, function(x) {x[is.na(x)] <- ""; x})
	names(tiers) <- names(unclass(format.descriptor))
	
	
	
	tok.table <- matrix(crp_dta[[2]], nrow=length(levels(format.descriptor)))
	tok.table <- lapply(seq_along(levels(format.descriptor)), function(i) tok.table[i, ])
	names(tok.table) <- levels(format.descriptor)
	
	log <- crp_dta[[3]]
	names(log) <- tiers[[1]]
	
	structure(list(tiers = tiers, tok.table = tok.table), format.descriptor = format.descriptor, parse.log = log, source = file, source.header = header, class="language.corpus")
}


readToolbox <- function(files, format.descriptor, ..., unknown.tiers.as.top = F, enable.gaps=F, strict=F,  encoding="UTF-8")
{
	if (inherits(files, "connection")) files <- list(files)
		
	
	corpora <- lapply(files, function(file) do_readToolbox(file, format.descriptor =format.descriptor, ..., unknown.tiers.as.top= unknown.tiers.as.top, enable.gaps = enable.gaps, strict = strict,  encoding = encoding))
	
	if (length(corpora)==1) return(corpora[[1]]) else concat.corpus(corpora)
}
