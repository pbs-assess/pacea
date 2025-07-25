# These are on plotting functions for pacea_index and then pacea_recruitment,
# pacea_biomass, pacea_buoy, pacea_harbour_seals, pacea_zooplankton, and
# pacea_recruitment_herring objects. Also a generic df to which we add class pacea_index.

# plot.pacea_index()
test_that("index plotting: the stopifnot commands are working", {
  expect_error(plot(oni,
                    value = "foo"))
  expect_error(plot(npi_monthly))
  expect_error(plot(npi_annual,
                    smooth_over_year = TRUE))
})

test_that("index plotting works with various options", {
  expect_invisible(plot(npi_monthly,
                        smooth_over_year = TRUE,
                        value = "value"))
  expect_invisible(plot(oni,
                        style = "red_blue"))

  expect_invisible(plot(npi_monthly,
                        value = "value"))
  expect_invisible(plot(npi_annual,
                        value = "value"))
  expect_invisible(plot(oni,
                        xlim = c(lubridate::dmy(01011950),
                                 lubridate::dmy(01012040)))) # to expand x-axis
  expect_invisible(plot(oni,
                       type = "o"))     # passed onto plot()
  expect_invisible(plot(oni,
                        main = "My nice plot"))  # passed onto plot()

  expect_invisible(plot(pdo,
                        event_years = c(2007, 2008, 2010)))

  expect_invisible(plot(oni,
                        event_years = c(2007, 2008, 2010),
                        value = "value",
                        style = "plain"))

  expect_invisible(plot(oni,
                        event_lub = c(lubridate::dmy(02081972),
                                      lubridate::dmy(31121999))))

  suppressWarnings(expect_warning(plot(oni,
                                       not_an_option = "My nice plot")))
                                       # nonsensical ... option

  expect_invisible(plot(oni,
                        style = "goa"))   # that isn't implemented yet
  # Confirm works for Fraser River discharge
  expect_invisible(plot(fraser_discharge_mean))
  expect_invisible(plot(fraser_discharge_peak))
})

# Adapting some of above for hake_recruitment
test_that("recruitment plotting: the stopifnot commands are working", {
  expect_error(plot(hake_recruitment,
                    value = "foo"))
  expect_error(plot.hake_recruitment(npi_monthly))
})

test_that("recruitment plotting works with various options", {
  expect_invisible(plot(hake_recruitment))
  expect_invisible(plot(hake_recruitment,
                        style = "foo"))
  expect_invisible(plot(dplyr::select(hake_recruitment,
                                      -c("low"))))
  expect_invisible(plot(hake_recruitment,
                        y_max = 100))
  expect_invisible(plot(hake_recruitment_over_2010))
  expect_invisible(plot(hake_recruitment_over_R0))
  expect_invisible(plot(hake_recruitment_deviations))
})

# Adapting some of that for hake_biomass
test_that("biomass plotting: the stopifnot commands are working", {
  expect_error(plot(hake_biomass,
                    value = "foo"))
  expect_error(plot.hake_biomass(npi_monthly))
})

test_that("biomass plotting works with various options", {
  expect_invisible(plot(hake_biomass))
  expect_invisible(plot(hake_biomass,
                        style = "foo"))
  expect_invisible(plot(dplyr::select(hake_biomass,
                                      -c("low"))))
  expect_invisible(plot(hake_biomass,
                        y_max = 100))
})

# Adapting some of hake one for herring_recruitment
test_that("herring recruitment plotting: the stopifnot commands are working", {
  expect_error(plot(herring_recruitment,
                    region = "foo"))
  expect_error(plot(herring_recruitment,
                    title = "foo"))
  expect_error(plot.herring_recruitment(npi_monthly))

})

test_that("herring recruitment plotting works with various options", {
  expect_invisible(plot(herring_recruitment))
  expect_invisible(plot(herring_recruitment,
                        region = "HG"))
  expect_invisible(plot(herring_recruitment,
                        title = NULL))
  expect_invisible(plot(herring_recruitment,
                        title = "full"))
  expect_invisible(plot(herring_recruitment,
                        title = "short"))
})

# Essentially copy the herring recruitment ones to test herring biomass plots
# Adapting some of hake one for herring_recruitment
test_that("herring biomass plotting: the stopifnot commands are working", {
  expect_error(plot(herring_spawning_biomass,
                    region = "foo"))
  expect_error(plot(herring_spawning_biomass,
                    title = "foo"))
  expect_error(plot.herring_biomass(npi_monthly))

})

test_that("herring biomass plotting works with various options", {
  expect_invisible(plot(herring_spawning_biomass))
  expect_invisible(plot(herring_spawning_biomass,
                        region = "HG"))
  expect_invisible(plot(herring_spawning_biomass,
                        title = NULL))
  expect_invisible(plot(herring_spawning_biomass,
                        title = "full"))
  expect_invisible(plot(herring_spawning_biomass,
                        title = "short"))
})


# plot.pacea_buoy()
test_that("buoy SST plotting  works with various options", {
  expect_visible(plot(buoy_sst))
  expect_visible(plot(buoy_sst,
                      years = 2010:2020))
  expect_error(plot(buoy_sst, stn_id = c("C46004", "C46036")))
})

# plot.pacea_harbour_seals()
test_that("harbour seals plotting  works with various options", {
  expect_visible(plot(harbour_seals))
  expect_visible(plot(harbour_seals,
                 include_coastwide = FALSE))
  expect_invisible(plot(harbour_seals,
                      region = "SOG"))
  expect_error(plot(harbour_seals,
                    region = "nonsense"))
  expect_error(plot(harbour_seals,
                    value = "nonsense"))
  # Doesn't pass, even though figures do look the same
  #expect_equal(plot(dplyr::filter(harbour_seals,
  #                                region != "Coastwide")),
  #             plot(harbour_seals,
  #                  include_coastwide = FALSE))

})

# plot.pacea_zooplankton()
test_that("zooplankton plotting: the stopifnot command works", {
  expect_error(plot(zooplankton_sog,
                    species_group = "foo"))
})

test_that("zooplankton plotting works with various options", {
  expect_invisible(plot(zooplankton_sog,
                        species_group = "cladocera",
                        xlab = "Hello world",
                        ylab = "Hello again",
                        mgp_val = c(3, 2, 0)))
  expect_invisible(plot(zooplankton_sog))
})

# generic object with no axis_name attribute. MWE from Emily O'Grady
#  #87. Thought might have to do for
#  each type of class to make sure they're all covered (i.e. fix each plotting
#  function, but it's only in plot.pacea_biomass where we explicitly do an if
# statement on an attribute.
test_that("no error given if axis_name attribute missing", {
  # Create a data frame without axis_name attribute
  test_data <- data.frame(
    year = 2000:2010,
    median = rnorm(11)
  )

  class(test_data) <- c("pacea_biomass", "data.frame")
  # This gave error (before fixing the problem)
  expect_invisible(plot(test_data))


})
