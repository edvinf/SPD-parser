testfile <- system.file("extdata", "testresources", "fisk2018_20181221_trunc.spd", package="SPD")
testfileW <- system.file("extdata", "testresources", "ex_w.spd", package="SPD")

parsed <- expect_warning(parseSPD(testfile))

context("Test S form")
expect_true(all(parsed$Sform$aar == 2018))
expect_true(all(!is.na(parsed$Sform$redskap.kode)))
expect_equal(ncol(parsed$Sform), 46)

context("Test T form")
expect_true(all(parsed$Tform$aar == 2018))
expect_true(all(!is.na(parsed$Tform$artskode)))
expect_equal(ncol(parsed$Tform), 26)

context("Test U form")
expect_true(all(parsed$Uform$aar == 2018))
expect_true(all(!is.na(parsed$Uform$artskode)))
expect_true(all(!is.na(parsed$Uform$intervall)))
expect_equal(ncol(parsed$Uform), 15)

context("Test V form")
expect_true(all(parsed$Vform$aar == 2018))
expect_true(all(!is.na(parsed$Vform$artskode)))
expect_true(all(!is.na(parsed$Vform$lengde)))
expect_equal(ncol(parsed$Vform), 43)

parsed <- expect_warning(parseSPD(testfileW))
context("Test W form")
expect_true(all(parsed$Wform$aar == 2018))
expect_true(all(!is.na(parsed$Wform$artskode)))
expect_true(all(!is.na(parsed$Wform$byttedyr.vekt)))
expect_equal(ncol(parsed$Wform), 23)
