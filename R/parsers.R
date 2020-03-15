#' @noRd
getCols <- function(line, colstart, colstop=colstart){
  return(substr(line, colstart, colstop))
}

#' @noRd
parseInt <- function(int){
  if (trimws(int)==""){
    return(NA)
  }
  else{
    return(as.integer(trimws(int)))
  }
}

#' @noRd
parseString <- function(string){
  return(trimws(string))
}

#' @noRd
parseYear <- function(year){
  if (as.integer(year) < 100){
    year <- as.integer(paste("2", year, sep=""))
  }
  else{
    year <- as.integer(paste("1", year, sep=""))
  }
  return(year)
}

readForm <- function(filename, formcode, encoding="latin1"){
  f <- file(filename, open = "r")
  lines <- readLines(f, encoding = encoding)
  close(f)

  for (l in lines){
    if (substr(l,1,1) == formcode){
      uselines <- c(uselines, l)
    }
  }

  return(uselines)
}

#' Parses line from S-form (spesified in SPD handbook jan. 2010)
#' @noRd
parseSline <- function(line){
  if (getCols(line,1,1) != "S"){
    stop("Not an S-line")
  }

  st.avvik.apning=parseInt(getCols(line,90,91))
  if (!is.na(st.avvik.apning)){
    st.avvik.apning <- st.avvik.apning/10
  }

  st.avvik.dorspredning=parseInt(getCols(line,95,97))
  if (!is.na(st.avvik.dorspredning)){
    st.avvik.dorspredning <- st.avvik.dorspredning/10
  }

  return(data.table::data.table(aar=parseYear(getCols(line,2,4)),
                                land=parseString(getCols(line, 5,6)),
                                skipskode=parseString(getCols(line, 7)),
                                skip=parseString(getCols(line, 8,13)),
                                mnd=parseInt(getCols(line, 14,15)),
                                dag=parseInt(getCols(line, 16,17)),
                                st.nr=parseString(getCols(line, 18,21)),
                                serie.nr=parseString(getCols(line, 22,26)),
                                st.type=parseString(getCols(line, 27)),
                                bredde.gr=parseInt(getCols(line, 28,29)),
                                bredde.min=parseInt(getCols(line, 30,31)),
                                bredde.des.min=parseInt(getCols(line, 32)),
                                lengde.gr=parseInt(getCols(line, 33,35)),
                                lengde.min=parseInt(getCols(line, 36,37)),
                                lengde.des.min=parseInt(getCols(line, 38)),
                                nsov=parseString(getCols(line,39)),
                                system=parseString(getCols(line,40)),
                                omrade=parseString(getCols(line,41,42)),
                                lokalitet=parseString(getCols(line,43,45)),
                                bunndyp=parseInt(getCols(line,46,49)),
                                ant.redskap=parseInt(getCols(line,50,51)),
                                redskap.kode=parseString(getCols(line,52,55)),
                                redskap.nr=parseString(getCols(line,56,57)),
                                retning=parseInt(getCols(line,58,59)),
                                fart=parseInt(getCols(line,60,61)),
                                start.tid.timer=parseInt(getCols(line,62,63)),
                                start.tid.min=parseInt(getCols(line,64,65)),
                                start.logg=parseInt(getCols(line,66,69)),
                                stopp.tid.timer=parseInt(getCols(line,70,71)),
                                stopp.tid.min=parseInt(getCols(line,72,73)),
                                distanse=parseInt(getCols(line,74,76)),
                                tilstand=parseString(getCols(line,77)),
                                kvalitet=parseString(getCols(line,78)),
                                fiskedyp.maks=parseInt(getCols(line,79,82)),
                                fiskedyp.min=parseInt(getCols(line,83,86)),
                                apning=parseInt(getCols(line,87,89)),
                                st.avvik.apning=st.avvik.apning,
                                dorspredning=parseInt(getCols(line,92,94)),
                                st.avvik.dorspredning=st.avvik.dorspredning,
                                spesialkode=parseString(getCols(line,98,99)),
                                wirelengde=parseInt(getCols(line,100,103)),
                                kvalitetsmerking=parseString(getCols(line,122)),
                                gjeldende.kvalitetsprosedyre=parseString(getCols(line,123,124)),
                                omkodingsprogram=parseString(getCols(line,125,126)),
                                originalt.format=parseString(getCols(line,127,128)),
                                gjeldende.format=parseString(getCols(line,129,130))
                                ))
}

#' Parses line from T-form (spesified in SPD handbook jan. 2010)
#' @noRd
parseTline <- function(line){
  if (getCols(line,1,1) != "T"){
    stop("Not a T-line")
  }

  return(data.table::data.table(aar=parseYear(getCols(line,2,4)),
                                land=parseString(getCols(line, 5,6)),
                                skipskode=parseString(getCols(line, 7)),
                                skip=parseString(getCols(line, 8,13)),
                                mnd=parseInt(getCols(line, 14,15)),
                                dag=parseInt(getCols(line, 16,17)),
                                st.nr=parseString(getCols(line, 18,21)),
                                serie.nr=parseString(getCols(line, 22,26)),
                                artskode=parseString(getCols(line, 27)),
                                art=parseString(getCols(line,28,39)),
                                del.nr=parseString(getCols(line,40)),
                                provetype=parseString(getCols(line,41,42)),
                                gruppe=parseString(getCols(line,43,44)),
                                kons=parseString(getCols(line,45)),
                                mal.fangst=parseString(getCols(line,46)),
                                fangst.vekt.vol=parseInt(getCols(line,47,53)),
                                fangst.antall=parseInt(getCols(line,54,59)),
                                mal.prove=parseString(getCols(line,60)),
                                lengdemal=parseString(getCols(line,61)),
                                lengdeprove.vekt.vol=parseInt(getCols(line,62,67)),
                                lengdeprove.antall=parseInt(getCols(line,68,71)),
                                individprove.antall=parseInt(getCols(line,72,75)),
                                ot.skj=parseString(getCols(line, 76)),
                                parasitt=parseString(getCols(line, 77)),
                                mage=parseString(getCols(line, 78)),
                                genetikk=parseString(getCols(line, 79))
                                ))
}

#' Parses line from U-form (spesified in SPD handbook jan. 2010)
#' @noRd
parseUline <- function(line){
  if (getCols(line,1,1) != "U"){
    stop("Not a U-line")
  }

  return(data.table::data.table(aar=parseYear(getCols(line,2,4)),
                                land=parseString(getCols(line, 5,6)),
                                skipskode=parseString(getCols(line, 7)),
                                skip=parseString(getCols(line, 8,13)),
                                mnd=parseInt(getCols(line, 14,15)),
                                dag=parseInt(getCols(line, 16,17)),
                                st.nr=parseString(getCols(line, 18,21)),
                                serie.nr=parseString(getCols(line, 22,26)),
                                artskode=parseString(getCols(line, 27)),
                                art=parseString(getCols(line,28,39)),
                                del.nr=parseString(getCols(line,40)),
                                intervall=parseString(getCols(line,41)),
                                kjonn=parseString(getCols(line,42)),
                                minste.lengdegr=parseInt(getCols(line,43,45)),
                                lengdefrekvenser.i.antall=parseInt(getCols(line,40))

  ))
}

#' Parses line from V-form (spesified in SPD handbook jan. 2010)
#' @noRd
parseVline <- function(line){
  if (getCols(line,1,1) != "V"){
    stop("Not a V-line")
  }

  return(data.table::data.table(aar=parseYear(getCols(line,2,4)),
                                land=parseString(getCols(line, 5,6)),
                                skipskode=parseString(getCols(line, 7)),
                                skip=parseString(getCols(line, 8,13)),
                                mnd=parseInt(getCols(line, 14,15)),
                                dag=parseInt(getCols(line, 16,17)),
                                st.nr=parseString(getCols(line, 18,21)),
                                serie.nr=parseString(getCols(line, 22,26)),
                                artskode=parseString(getCols(line, 27)),
                                art=parseString(getCols(line,28,39)),
                                del.nr=parseString(getCols(line,40)),
                                fisk.nr=parseInt(getCols(line,41,43)),
                                vekt.vol.kode=parseString(getCols(line,44)),
                                vekt.volum=parseInt(getCols(line,45,49)),
                                lengdeenhet=parseString(getCols(line,50)),
                                lengde=parseInt(getCols(line,51,53)),
                                fett=parseString(getCols(line,54)),
                                kjonn=parseString(getCols(line,55)),
                                stadium=parseString(getCols(line,56)),
                                spesial.stadium=parseString(getCols(line,57,58)),
                                magefyll=parseString(getCols(line,59)),
                                ford.grad=parseString(getCols(line,60)),
                                lever=parseString(getCols(line,61)),
                                parasitt=parseString(getCols(line,62)),
                                sepsialkode=parseString(getCols(line,63,66)),
                                virvler=parseInt(getCols(line,67,68)),
                                alder=parseInt(getCols(line,69,70)),
                                gytealder=parseInt(getCols(line,71,72)),
                                gytesoner=parseInt(getCols(line,73,74)),
                                lesbarhet=parseString(getCols(line,75)),
                                type=parseString(getCols(line,76)),
                                rand=parseString(getCols(line,77)),
                                kjerne=parseString(getCols(line,78)),
                                kalibrering=parseInt(getCols(line,79,80)),
                                vekstsoner=parseString(getCols(line,81,100)),
                                merketype=parseString(getCols(line,101)),
                                seriekode=parseString(getCols(line,102,103)),
                                merkenummer=parseString(getCols(line,104,109)),
                                vekt.vol.lode=parseString(getCols(line,110)),
                                gonademengde=parseInt(getCols(line,111,114)),
                                levermengde=parseInt(getCols(line,115,118)),
                                sloyd.vekt.vol=parseInt(getCols(line,119,123)),
                                magevekt=parseInt(getCols(line,124,127))
  ))
}

#' Parses line from W-form (spesified in SPD handbook jan. 2010)
#' @noRd
parseWline <- function(line){
  stop("Not implemented")
}

#'  Parses SPD froms (spesified in SPD handbook jan. 2010)
#'  Units are only standardized when interpretation does not depend on other fields.
parseSPD <- function(filename, encoding="latin1"){
  Sform <- NULL
  Tform <- NULL
  Uform <- NULL
  Vform <- NULL
  Wform <- NULL

  f <- file(filename, open = "r")
  lines <- readLines(f, encoding = encoding)
  close(f)

  for (l in lines){
    if (getCols(l,1) == "S"){
      Sform <- rbind(Sform, parseSline(l))
    }
    if (getCols(l,1) == "T"){
      Tform <- rbind(Tform, parseTline(l))
    }
    if (getCols(l,1) == "U"){
      Uform <- rbind(Uform, parseUline(l))
    }
    if (getCols(l,1) == "V"){
      Vform <- rbind(Vform, parseVline(l))
    }
    if (getCols(l,1) == "W"){
      Wform <- rbind(Wform, parseWline(l))
    }

  }

  ret <- list()
  ret$Sform <- Sform
  ret$Tform <- Tform
  ret$Uform <- Uform
  ret$Vform <- Vform
  ret$Wform <- Wform

  return(ret)
}
