
#' @noRd
fixYear <- function(year){
  y <- year
  y[year<100] <- year[year<100] + 2000
  y[year>100] <- year[year>100] + 1000
  return(y)
}

#' @noRd
readForm <- function(filename, formcode, colspec, coltypes, encoding="latin1"){

  message("...... extracting lines ...")
  loc <- readr::default_locale()
  loc$encoding <- encoding
  lines <- readr::read_fwf(filename, readr::fwf_cols(type=c(1,1), rest=c(2,NA)), col_types = "cc", trim_ws = F, locale = loc)
  lines <- lines[lines$type==formcode,]
  #write lines to tempfile
  tempf <- tempfile()
  readr::write_delim(lines[,2], tempf, delim="", col_names = F)

  #read from tempfile
  message("...... parsing ...")
  colspec$begin <- colspec$begin -1
  colspec$end <- colspec$end -1
  form <- readr::read_fwf(tempf, colspec, col_types = coltypes)

  file.remove(tempf)

  return(data.table::as.data.table(form))
}

#' Parses all S-lines from file (spesified in SPD handbook jan. 2010)
#' @param file path to file (SPD-format)
#' @return data.table
#' @export
readSform <-function (file){
  coltypes = "iccciiccciiiiiicccciicciiiiiiiicciiididciccccc"
  colspec <- readr::fwf_cols(
           aar=c(2,4),

           land=c(5,6),
           skipskode=c(7,7),
           skip=c(8,13),

           mnd=c(14,15),
           dag=c(16,17),

           st.nr=c(18,21),
           serie.nr=c(22,26),
           st.type=c(27,27),

           bredde.gr=c(28,29),
           bredde.min=c(30,31),
           bredde.des.min=c(32,32),
           lengde.gr=c(33,35),
           lengde.min=c(36,37),
           lengde.des.min=c(38,38),

           nsov=c(39,39),
           system=c(40,40),
           omrade=c(41,42),
           lokalitet=c(43,45),

           bunndyp=c(46,49),
           ant.redskap=c(50,51),

           redskap.kode=c(52,55),
           redskap.nr=c(56,57),

           retning=c(58,59),
           fart=c(60,61),
           start.tid.timer=c(62,63),
           start.tid.min=c(64,65),
           start.logg=c(66,69),
           stopp.tid.timer=c(70,71),
           stopp.tid.min=c(72,73),
           distanse=c(74,76),

           tilstand=c(77,77),
           kvalitet=c(78,78),

           fiskedyp.maks=c(79,82),
           fiskedyp.min=c(83,86),
           apning=c(87,89),
           st.avvik.apning=c(90,91),
           dorspredning=c(92,94),
           st.avvik.dorspredning=c(95,96),

           spesialkode=c(98,99),

           wirelengde=c(100,103),

           kvalitetsmerking=c(122,122),
           gjeldende.kvalitetsprosedyre=c(123,124),
           omkodingsprogram=c(125,126),
           originalt.format=c(127,128),
           gjeldende.format=c(129,130))

  stopifnot(nrow(colspec) == nchar(coltypes))

  sform <- readForm(file, "S", colspec, coltypes = coltypes)

  if (nrow(sform) > 0){
    sform$aar <- fixYear(sform$aar)
    sform$st.avvik.apning <- sform$st.avvik.apning/10
    sform$st.avvik.dorspredning <- sform$st.avvik.dorspredning/10
  }


  return(sform)
}

#' Parses all T-lines from file (spesified in SPD handbook (missing mage, genetikk))
#' @param file path to file (SPD-format)
#' @return data.table
#' @export
readTform <-function (file){
  coltypes = "iccciiccccccccciicciiicccc"
  colspec <- readr::fwf_cols(aar=c(2,4),

                             land=c(5,6),
                             skipskode=c(7,7),
                             skip=c(8,13),

                             mnd=c(14,15),
                             dag=c(16,17),

                             st.nr=c(18,21),
                             serie.nr=c(22,26),
                             artskode=c(27,27),
                             art=c(28,39),
                             del.nr=c(40,40),
                             provetype=c(41,42),
                             gruppe=c(43,44),
                             kons=c(45,45),
                             mal.fangst=c(46,46),

                             fangst.vekt.vol=c(47,53),
                             fangst.antall=c(54,59),

                             mal.prove=c(60,60),
                             lengdemal=c(61,61),

                             lengdeprove.vekt.vol=c(62,67),
                             lengdeprove.antall=c(68,71),
                             individprove.antall=c(72,75),

                             ot.skj=c(76,76),
                             parasitt=c(77,77),
                             mage=c(78,78),
                             genetikk=c(79,79))

  stopifnot(nrow(colspec) == nchar(coltypes))

  tform <- readForm(file, "T", colspec, coltypes = coltypes)

  if (nrow(tform) > 0){
    tform$aar <- fixYear(tform$aar)
  }


  return(tform)
}

#' Parses all U-lines from file (spesified in SPD handbook jan. 2010)
#' @param file path to file (SPD-format)
#' @return data.table
#' @export
readUform <-function (file){
  coltypes = "iccciicccccccii"
  colspec <- readr::fwf_cols(aar=c(2,4),

                             land=c(5,6),
                             skipskode=c(7,7),
                             skip=c(8,13),

                             mnd=c(14,15),
                             dag=c(16,17),

                             st.nr=c(18,21),
                             serie.nr=c(22,26),
                             artskode=c(27,27),
                             art=c(28,39),
                             del.nr=c(40,40),

                             intervall=c(41,41),
                             kjonn=c(42,42),
                             minste.lengdegr=c(43,45),
                             lengdefrekvenser.i.antall=c(40,40)
                             )

  stopifnot(nrow(colspec) == nchar(coltypes))

  uform <- readForm(file, "U", colspec, coltypes = coltypes)

  if (nrow(uform) > 0){
    uform$aar <- fixYear(uform$aar)
  }

  return(uform)
}

#' Parses all V-lines from file (spesified in SPD handbook jan. 2010)
#' @param file path to file (SPD-format)
#' @return data.table
#' @export
readVform <-function (file){
  coltypes = "iccciiccccccciciccccccccciiiicccciccccciiii"
  colspec <- readr::fwf_cols(aar=c(2,4),

                             land=c(5,6),
                             skipskode=c(7,7),
                             skip=c(8,13),

                             mnd=c(14,15),
                             dag=c(16,17),

                             st.nr=c(18,21),
                             serie.nr=c(22,26),
                             artskode=c(27,27),
                             art=c(28,39),
                             del.nr=c(40,40),
                             fisk.nr=c(41,43),
                             vekt.vol.kode=c(44,44),

                             vekt.volum=c(45,49),

                             lengdeenhet=c(50,50),

                             lengde=c(51,53),

                             fett=c(54,54),
                             kjonn=c(55,55),
                             stadium=c(56,56),
                             spesial.stadium=c(57,58),
                             magefyll=c(59,59),
                             ford.grad=c(60,60),
                             lever=c(61,61),
                             parasitt=c(62,62),
                             sepsialkode=c(63,66),

                             virvler=c(67,68),
                             alder=c(69,70),
                             gytealder=c(71,72),
                             gytesoner=c(73,74),

                             lesbarhet=c(75,75),
                             type=c(76,76),
                             rand=c(77,77),
                             kjerne=c(78,78),

                             kalibrering=c(79,80),

                             vekstsoner=c(81,100),
                             merketype=c(101,101),
                             seriekode=c(102,103),
                             merkenummer=c(104,109),
                             vekt.vol.lode=c(110,110),

                             gonademengde=c(111,114),
                             levermengde=c(115,118),
                             sloyd.vekt.vol=c(119,123),
                             magevekt=c(124,127)
  )

  stopifnot(nrow(colspec) == nchar(coltypes))

  vform <- readForm(file, "V", colspec, coltypes = coltypes)
  if (nrow(vform) > 0){
    vform$aar <- fixYear(vform$aar)
  }

  return(vform)
}

#' Parses all V-lines from file (spesified in SPD handbook jan. 2010)
#' @param file path to file (SPD-format)
#' @return data.table
#' @export
readWform <-function (file){
  coltypes = "iccciicccccccccciciccic"
  colspec <- readr::fwf_cols(aar=c(2,4),

                             land=c(5,6),
                             skipskode=c(7,7),
                             skip=c(8,13),

                             mnd=c(14,15),
                             dag=c(16,17),

                             st.nr=c(18,21),
                             serie.nr=c(22,26),
                             artskode=c(27,27),
                             art=c(28,39),
                             del.nr=c(40,40),
                             fisk.nr=c(41,43),
                             byttedyrkode=c(44,44),
                             byttedyr=c(45,46),
                             ford.gr=c(57,57),
                             antall.enhet=c(58,58),

                             antall=c(59,62),

                             vektenhet=c(63,63),

                             byttedyr.vekt=c(64,69),

                             intervall.utviklingstrinn=c(70,70),
                             lengdemal=c(71,71),

                             minste.lengde=c(72,74),
                             lengdefrekvens.i.antall=c(75,118)


  )

  stopifnot(nrow(colspec) == nchar(coltypes))

  wform <- readForm(file, "W", colspec, coltypes = coltypes)
  if (nrow(wform) > 0){
    wform$aar <- fixYear(wform$aar)
  }

  return(wform)
}

#' SPD parser
#' @description Parses SPD froms (spesified in SPD handbook jan. 2010)
#' @details
#'  Units are only standardized when interpretation does not depend on other fields.
#'  Incomplete forms will produce warnings (parsing failures). Missing columns will be interpreted as NAs
#' @param filename path to file (SPD-format)
#' @param encoding encoding for the file
#' @return list with elements:
#'  \describe{
#'   \item{Sform}{S-form (stations / hauls)}
#'   \item{Tform}{T-form (catch samples)}
#'   \item{Uform}{U-form (length frequencies (fish))}
#'   \item{Vform}{V-form (individual measurements (fish))}
#'   \item{Wform}{W-form (length frequencies / devstage frequencies (prey))}
#'  }
#' @export
parseSPD <- function(filename, encoding="latin1"){
  Sform <- NULL
  Tform <- NULL
  Uform <- NULL
  Vform <- NULL
  Wform <- NULL

  message("... parsing S forms ...")
  Sform <- readSform(filename)
  message("... parsing T forms ...")
  Tform <- readTform(filename)
  message("... parsing U forms ...")
  Uform <- readUform(filename)
  message("... parsing V forms ...")
  Vform <- readVform(filename)
  message("... parsing W forms ...")
  Wform <- readWform(filename)

  ret <- list()
  ret$Sform <- Sform
  ret$Tform <- Tform
  ret$Uform <- Uform
  ret$Vform <- Vform
  ret$Wform <- Wform

  return(ret)
}
